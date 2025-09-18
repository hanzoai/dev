//! Streaming utilities for SSE and WebSocket

use futures::{Stream, StreamExt};
use hanzoai::{Result, HanzoError};
use serde::{Deserialize, Serialize};
use std::pin::Pin;

/// SSE event parser
pub struct SSEParser {
    buffer: String,
}

impl SSEParser {
    pub fn new() -> Self {
        Self {
            buffer: String::new(),
        }
    }

    /// Parse chunk of SSE data
    pub fn parse_chunk(&mut self, data: &[u8]) -> Vec<SSEEvent> {
        let text = match std::str::from_utf8(data) {
            Ok(t) => t,
            Err(_) => return vec![],
        };

        self.buffer.push_str(text);

        let mut events = Vec::new();
        while let Some(event) = self.extract_event() {
            events.push(event);
        }

        events
    }

    fn extract_event(&mut self) -> Option<SSEEvent> {
        if let Some(end) = self.buffer.find("\n\n") {
            let event_str = self.buffer.drain(..end + 2).collect::<String>();
            return self.parse_event(&event_str);
        }
        None
    }

    fn parse_event(&self, event_str: &str) -> Option<SSEEvent> {
        let mut event = SSEEvent::default();

        for line in event_str.lines() {
            if let Some(colon) = line.find(':') {
                let field = &line[..colon];
                let value = line[colon + 1..].trim_start();

                match field {
                    "event" => event.event = Some(value.to_string()),
                    "data" => event.data = Some(value.to_string()),
                    "id" => event.id = Some(value.to_string()),
                    "retry" => event.retry = value.parse().ok(),
                    _ => {}
                }
            }
        }

        if event.data.is_some() || event.event.is_some() {
            Some(event)
        } else {
            None
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct SSEEvent {
    pub event: Option<String>,
    pub data: Option<String>,
    pub id: Option<String>,
    pub retry: Option<u64>,
}

/// Stream processor for chat completions
pub struct StreamProcessor {
    parser: SSEParser,
}

impl StreamProcessor {
    pub fn new() -> Self {
        Self {
            parser: SSEParser::new(),
        }
    }

    /// Process a stream of bytes into chat deltas
    pub fn process_stream<S>(&mut self, stream: S) -> impl Stream<Item = Result<ChatDelta>>
    where
        S: Stream<Item = std::result::Result<bytes::Bytes, reqwest::Error>> + Unpin,
    {
        let mut parser = SSEParser::new();

        stream.flat_map(move |chunk| {
            let deltas = match chunk {
                Ok(bytes) => {
                    parser.parse_chunk(&bytes)
                        .into_iter()
                        .filter_map(|event| {
                            if let Some(data) = event.data {
                                if data == "[DONE]" {
                                    Some(Ok(ChatDelta::Done))
                                } else if let Ok(json) = serde_json::from_str::<serde_json::Value>(&data) {
                                    if let Some(content) = json["choices"][0]["delta"]["content"].as_str() {
                                        Some(Ok(ChatDelta::Content(content.to_string())))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>()
                }
                Err(e) => vec![Err(HanzoError::NetworkError(e.to_string()))],
            };

            futures::stream::iter(deltas)
        })
    }
}

#[derive(Debug, Clone)]
pub enum ChatDelta {
    Content(String),
    Done,
}

/// Stream aggregator for collecting full responses
pub struct StreamAggregator {
    content: String,
}

impl StreamAggregator {
    pub fn new() -> Self {
        Self {
            content: String::new(),
        }
    }

    /// Aggregate a stream of deltas into a complete response
    pub async fn aggregate<S>(&mut self, mut stream: Pin<Box<S>>) -> Result<String>
    where
        S: Stream<Item = Result<ChatDelta>> + Unpin,
    {
        while let Some(delta) = stream.next().await {
            match delta? {
                ChatDelta::Content(text) => self.content.push_str(&text),
                ChatDelta::Done => break,
            }
        }

        Ok(self.content.clone())
    }
}

impl Default for SSEParser {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for StreamProcessor {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for StreamAggregator {
    fn default() -> Self {
        Self::new()
    }
}