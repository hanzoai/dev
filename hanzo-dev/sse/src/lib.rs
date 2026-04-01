use std::fmt;

/// A parsed SSE event containing optional event name and raw data payload.
#[derive(Debug, Clone, PartialEq)]
pub struct SseEvent {
    pub event: Option<String>,
    pub data: String,
}

/// Errors that can occur during SSE parsing.
#[derive(Debug)]
pub enum SseError {
    InvalidJson(serde_json::Error),
    InvalidFrame(&'static str),
}

impl fmt::Display for SseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SseError::InvalidJson(e) => write!(f, "invalid JSON in SSE frame: {e}"),
            SseError::InvalidFrame(msg) => write!(f, "invalid SSE frame: {msg}"),
        }
    }
}

impl std::error::Error for SseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            SseError::InvalidJson(e) => Some(e),
            SseError::InvalidFrame(_) => None,
        }
    }
}

impl From<serde_json::Error> for SseError {
    fn from(e: serde_json::Error) -> Self {
        SseError::InvalidJson(e)
    }
}

/// Incremental SSE stream parser.
///
/// Feed chunks of bytes via [`push`](SseParser::push) and receive parsed events.
/// Call [`finish`](SseParser::finish) when the stream ends to flush any trailing frame.
#[derive(Debug, Default)]
pub struct SseParser {
    buffer: Vec<u8>,
}

impl SseParser {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Accumulate bytes and extract any complete SSE frames.
    pub fn push(&mut self, chunk: &[u8]) -> Result<Vec<SseEvent>, SseError> {
        self.buffer.extend_from_slice(chunk);
        let mut events = Vec::new();

        while let Some(frame) = self.next_frame() {
            if let Some(event) = parse_frame(&frame)? {
                events.push(event);
            }
        }

        Ok(events)
    }

    /// Flush remaining buffer content as a final frame.
    pub fn finish(&mut self) -> Result<Vec<SseEvent>, SseError> {
        if self.buffer.is_empty() {
            return Ok(Vec::new());
        }

        let trailing = std::mem::take(&mut self.buffer);
        match parse_frame(&String::from_utf8_lossy(&trailing))? {
            Some(event) => Ok(vec![event]),
            None => Ok(Vec::new()),
        }
    }

    fn next_frame(&mut self) -> Option<String> {
        // Check 4-byte separator first to avoid partial match of \r\n as \r + \n.
        let separator = self
            .buffer
            .windows(4)
            .position(|w| w == b"\r\n\r\n")
            .map(|pos| (pos, 4))
            .into_iter()
            .chain(
                self.buffer
                    .windows(2)
                    .position(|w| w == b"\n\n")
                    .map(|pos| (pos, 2)),
            )
            .chain(
                self.buffer
                    .windows(2)
                    .position(|w| w == b"\r\r")
                    .map(|pos| (pos, 2)),
            )
            .min_by_key(|&(pos, _)| pos)?;

        let (position, separator_len) = separator;
        let frame: Vec<u8> = self.buffer.drain(..position + separator_len).collect();
        let frame_len = frame.len().saturating_sub(separator_len);
        Some(String::from_utf8_lossy(&frame[..frame_len]).into_owned())
    }
}

/// Parse a single SSE frame string into an event.
///
/// Returns `Ok(None)` for comment-only frames, ping events, `[DONE]` markers,
/// and frames with no data lines.
pub fn parse_frame(frame: &str) -> Result<Option<SseEvent>, SseError> {
    let trimmed = frame.trim();
    if trimmed.is_empty() {
        return Ok(None);
    }

    let mut data_lines: Vec<&str> = Vec::new();
    let mut event_name: Option<&str> = None;

    for line in trimmed.lines() {
        if line.starts_with(':') {
            continue;
        }
        if let Some(name) = line.strip_prefix("event:") {
            event_name = Some(name.trim());
            continue;
        }
        if let Some(data) = line.strip_prefix("data:") {
            data_lines.push(data.trim_start());
        }
    }

    if matches!(event_name, Some("ping")) {
        return Ok(None);
    }

    if data_lines.is_empty() {
        return Ok(None);
    }

    let payload = data_lines.join("\n");
    if payload == "[DONE]" {
        return Ok(None);
    }

    Ok(Some(SseEvent {
        event: event_name.map(String::from),
        data: payload,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_single_frame() {
        let frame = concat!(
            "event: content_block_start\n",
            "data: {\"type\":\"content_block_start\",\"index\":0}\n\n"
        );

        let event = parse_frame(frame).expect("frame should parse");
        assert_eq!(
            event,
            Some(SseEvent {
                event: Some("content_block_start".to_string()),
                data: "{\"type\":\"content_block_start\",\"index\":0}".to_string(),
            })
        );
    }

    #[test]
    fn parses_chunked_stream() {
        let mut parser = SseParser::new();
        let first = b"event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"text\":\"Hel";
        let second = b"lo\"}\n\n";

        assert!(parser
            .push(first)
            .expect("first chunk should buffer")
            .is_empty());
        let events = parser.push(second).expect("second chunk should parse");

        assert_eq!(events.len(), 1);
        assert_eq!(events[0].event, Some("content_block_delta".to_string()));
        assert_eq!(
            events[0].data,
            "{\"type\":\"content_block_delta\",\"text\":\"Hello\"}"
        );
    }

    #[test]
    fn ignores_ping_and_done() {
        let mut parser = SseParser::new();
        let payload = concat!(
            ": keepalive\n",
            "event: ping\n",
            "data: {\"type\":\"ping\"}\n\n",
            "event: message_delta\n",
            "data: {\"type\":\"message_delta\",\"stop_reason\":\"end_turn\"}\n\n",
            "event: message_stop\n",
            "data: {\"type\":\"message_stop\"}\n\n",
            "data: [DONE]\n\n"
        );

        let events = parser
            .push(payload.as_bytes())
            .expect("parser should succeed");
        assert_eq!(events.len(), 2);
        assert_eq!(events[0].event, Some("message_delta".to_string()));
        assert_eq!(events[1].event, Some("message_stop".to_string()));
    }

    #[test]
    fn ignores_data_less_event_frames() {
        let frame = "event: ping\n\n";
        let event = parse_frame(frame).expect("frame without data should be ignored");
        assert_eq!(event, None);
    }

    #[test]
    fn parses_split_json_across_data_lines() {
        let frame = concat!(
            "event: content_block_delta\n",
            "data: {\"type\":\"content_block_delta\",\"index\":0,\n",
            "data: \"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}\n\n"
        );

        let event = parse_frame(frame).expect("frame should parse");
        assert_eq!(
            event,
            Some(SseEvent {
                event: Some("content_block_delta".to_string()),
                data: "{\"type\":\"content_block_delta\",\"index\":0,\n\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}".to_string(),
            })
        );
    }

    #[test]
    fn skips_comment_only_frame() {
        let frame = ": this is a comment\n\n";
        let event = parse_frame(frame).expect("comment frame should parse");
        assert_eq!(event, None);
    }

    #[test]
    fn handles_crlf_separator() {
        let mut parser = SseParser::new();
        let payload = b"event: test\r\ndata: {\"ok\":true}\r\n\r\n";
        let events = parser.push(payload).expect("crlf should parse");
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].data, "{\"ok\":true}");
    }

    #[test]
    fn finish_flushes_trailing_frame() {
        let mut parser = SseParser::new();
        // No trailing double-newline
        let _ = parser.push(b"event: last\ndata: {\"final\":true}");
        let events = parser.finish().expect("finish should flush");
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].data, "{\"final\":true}");
    }

    #[test]
    fn finish_empty_buffer_returns_empty() {
        let mut parser = SseParser::new();
        let events = parser.finish().expect("finish on empty should succeed");
        assert!(events.is_empty());
    }

    #[test]
    fn done_marker_returns_none() {
        let frame = "data: [DONE]\n\n";
        let event = parse_frame(frame).expect("[DONE] should parse");
        assert_eq!(event, None);
    }

    #[test]
    fn handles_bare_cr_cr_separator() {
        let mut parser = SseParser::new();
        // Lines use \n internally; frames separated by \r\r.
        let payload = b"event: content_block_delta\ndata: {\"type\":\"text\"}\r\r";
        let events = parser.push(payload).expect("should parse");
        assert_eq!(events.len(), 1);
        assert_eq!(
            events[0].event,
            Some("content_block_delta".to_string())
        );
        assert_eq!(events[0].data, "{\"type\":\"text\"}");
    }
}
