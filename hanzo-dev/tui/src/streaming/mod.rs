use crate::markdown_stream::AnimatedLineStreamer;
use crate::markdown_stream::MarkdownStreamCollector;
use crate::memory_citation::MemoryCitationParser;
pub(crate) mod controller;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum StreamKind {
    Answer,
    Reasoning,
}

/// Strips `<think>...</think>` blocks from streamed text.
///
/// Since deltas arrive in arbitrary chunks, the filter tracks state across
/// pushes: when inside a `<think>` block, content is suppressed until the
/// closing `</think>` tag is seen.
#[derive(Debug)]
pub(crate) struct ThinkTagFilter {
    inside_think: bool,
    /// Partial tag buffer for when a tag straddles two deltas.
    partial: String,
}

impl ThinkTagFilter {
    pub(crate) fn new() -> Self {
        Self {
            inside_think: false,
            partial: String::new(),
        }
    }

    pub(crate) fn clear(&mut self) {
        self.inside_think = false;
        self.partial.clear();
    }

    /// Filter a delta, returning the visible portion with think blocks removed.
    pub(crate) fn filter(&mut self, delta: &str) -> String {
        let mut input = if self.partial.is_empty() {
            delta.to_string()
        } else {
            let mut s = std::mem::take(&mut self.partial);
            s.push_str(delta);
            s
        };

        let mut output = String::new();

        loop {
            if self.inside_think {
                // Look for </think>
                if let Some(end) = input.find("</think>") {
                    self.inside_think = false;
                    input = input[end + 8..].to_string();
                    continue;
                }
                // Check if input ends with a partial </think> tag
                if could_be_partial_close_tag(&input) {
                    self.partial = input;
                    break;
                }
                // Entire input is inside think block — drop it
                break;
            } else {
                // Look for <think>
                if let Some(start) = input.find("<think>") {
                    output.push_str(&input[..start]);
                    self.inside_think = true;
                    input = input[start + 7..].to_string();
                    continue;
                }
                // Also handle <think with no closing > (attributes variant)
                if let Some(start) = input.find("<think ") {
                    if let Some(gt) = input[start..].find('>') {
                        output.push_str(&input[..start]);
                        self.inside_think = true;
                        input = input[start + gt + 1..].to_string();
                        continue;
                    }
                }
                // Check if input ends with a partial <think tag
                if could_be_partial_open_tag(&input) {
                    // Buffer the potential partial tag, emit the safe prefix
                    let safe_len = input.len() - partial_open_tag_len(&input);
                    output.push_str(&input[..safe_len]);
                    self.partial = input[safe_len..].to_string();
                    break;
                }
                output.push_str(&input);
                break;
            }
        }

        output
    }
}

/// Check if the end of `s` could be the start of `</think>`.
fn could_be_partial_close_tag(s: &str) -> bool {
    let tag = "</think>";
    for i in 1..tag.len() {
        if s.ends_with(&tag[..i]) {
            return true;
        }
    }
    false
}

/// Check if the end of `s` could be the start of `<think>` or `<think `.
fn could_be_partial_open_tag(s: &str) -> bool {
    partial_open_tag_len(s) > 0
}

fn partial_open_tag_len(s: &str) -> usize {
    let tags = ["<think>", "<think "];
    for tag in &tags {
        for i in 1..tag.len() {
            if s.ends_with(&tag[..i]) {
                return i;
            }
        }
    }
    0
}

pub(crate) struct StreamState {
    pub(crate) collector: MarkdownStreamCollector,
    pub(crate) streamer: AnimatedLineStreamer,
    pub(crate) citation_parser: MemoryCitationParser,
    pub(crate) citations: Vec<String>,
    pub(crate) has_seen_delta: bool,
    pub(crate) last_commit_instant: Option<std::time::Instant>,
    pub(crate) tail_chars_since_commit: usize,
    pub(crate) last_sequence_number: Option<u64>,
    pub(crate) think_filter: ThinkTagFilter,
}

impl StreamState {
    pub(crate) fn new_for_kind(kind: StreamKind) -> Self {
        // Bold the first sentence for assistant answers; reasoning stays normal.
        let collector = match kind {
            StreamKind::Answer => MarkdownStreamCollector::new_with_bold_first(),
            StreamKind::Reasoning => MarkdownStreamCollector::new(),
        };
        Self {
            collector,
            streamer: AnimatedLineStreamer::new(),
            citation_parser: MemoryCitationParser::default(),
            citations: Vec::new(),
            has_seen_delta: false,
            last_commit_instant: None,
            tail_chars_since_commit: 0,
            last_sequence_number: None,
            think_filter: ThinkTagFilter::new(),
        }
    }
    pub(crate) fn clear(&mut self) {
        // Preserve bold_first_sentence setting in collector
        self.collector.clear();
        self.streamer.clear();
        self.citation_parser.clear();
        self.citations.clear();
        self.has_seen_delta = false;
        self.last_commit_instant = None;
        self.tail_chars_since_commit = 0;
        self.last_sequence_number = None;
        self.think_filter.clear();
    }
    pub(crate) fn step(&mut self) -> crate::markdown_stream::StepResult {
        self.streamer.step()
    }
    pub(crate) fn drain_all(&mut self) -> crate::markdown_stream::StepResult {
        self.streamer.drain_all()
    }
    pub(crate) fn is_idle(&self) -> bool {
        self.streamer.is_idle()
    }
    pub(crate) fn enqueue(&mut self, lines: Vec<ratatui::text::Line<'static>>) {
        self.streamer.enqueue(lines)
    }
}

/// Strip all `<think>...</think>` blocks from a complete string.
pub(crate) fn strip_think_tags(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut remaining = s;
    while let Some(start) = remaining.find("<think>").or_else(|| {
        remaining.find("<think ").and_then(|i| remaining[i..].find('>').map(|_| i))
    }) {
        result.push_str(&remaining[..start]);
        let after_open = if remaining[start..].starts_with("<think>") {
            &remaining[start + 7..]
        } else if let Some(gt) = remaining[start..].find('>') {
            &remaining[start + gt + 1..]
        } else {
            break;
        };
        if let Some(end) = after_open.find("</think>") {
            remaining = &after_open[end + 8..];
        } else {
            return result;
        }
    }
    result.push_str(remaining);
    result
}

pub(crate) struct HeaderEmitter {
    reasoning_emitted_this_turn: bool,
    answer_emitted_this_turn: bool,
    reasoning_emitted_in_stream: bool,
    answer_emitted_in_stream: bool,
    just_emitted_header: bool,
}

impl HeaderEmitter {
    pub(crate) fn new() -> Self {
        Self {
            reasoning_emitted_this_turn: false,
            answer_emitted_this_turn: false,
            reasoning_emitted_in_stream: false,
            answer_emitted_in_stream: false,
            just_emitted_header: false,
        }
    }

    pub(crate) fn reset_for_new_turn(&mut self) {
        self.reasoning_emitted_this_turn = false;
        self.answer_emitted_this_turn = false;
        self.reasoning_emitted_in_stream = false;
        self.answer_emitted_in_stream = false;
        self.just_emitted_header = false;
    }

    pub(crate) fn reset_for_stream(&mut self, kind: StreamKind) {
        match kind {
            StreamKind::Reasoning => self.reasoning_emitted_in_stream = false,
            StreamKind::Answer => self.answer_emitted_in_stream = false,
        }
        self.just_emitted_header = false;
    }

    pub(crate) fn has_emitted_for_stream(&self, kind: StreamKind) -> bool {
        match kind {
            StreamKind::Reasoning => self.reasoning_emitted_in_stream,
            StreamKind::Answer => self.answer_emitted_in_stream,
        }
    }

    /// Allow emitting the header again for the same kind within the current turn.
    ///
    /// This is used when a stream (e.g., Answer) is finalized and a subsequent
    /// block of the same kind is started within the same turn. Without this,
    /// only the first block would render a header.
    pub(crate) fn allow_reemit_for_same_kind_in_turn(&mut self, kind: StreamKind) {
        match kind {
            StreamKind::Reasoning => self.reasoning_emitted_this_turn = false,
            StreamKind::Answer => self.answer_emitted_this_turn = false,
        }
    }

    pub(crate) fn maybe_emit(
        &mut self,
        kind: StreamKind,
        _out_lines: &mut Vec<ratatui::text::Line<'static>>,
    ) -> bool {
        let already_emitted_this_turn = match kind {
            StreamKind::Reasoning => self.reasoning_emitted_this_turn,
            StreamKind::Answer => self.answer_emitted_this_turn,
        };
        let already_emitted_in_stream = self.has_emitted_for_stream(kind);
        if !already_emitted_in_stream && !already_emitted_this_turn {
            // Do not render a visible header line for either stream kind.
            // We still mark the header as emitted to preserve per-turn gating
            // and stream state, but the UI should not show the "codex" prefix
            // on streaming assistant messages.
            match kind {
                StreamKind::Reasoning => {
                    self.reasoning_emitted_in_stream = true;
                    self.reasoning_emitted_this_turn = true;
                    // Reset opposite header so it may be emitted again this turn
                    self.answer_emitted_this_turn = false;
                }
                StreamKind::Answer => {
                    self.answer_emitted_in_stream = true;
                    self.answer_emitted_this_turn = true;
                    // Reset opposite header so it may be emitted again this turn
                    self.reasoning_emitted_this_turn = false;
                }
            }
            self.just_emitted_header = true;
            true
        } else {
            self.just_emitted_header = false;
            false
        }
    }

    pub(crate) fn consume_header_flag(&mut self) -> bool {
        let was_just_emitted = self.just_emitted_header;
        self.just_emitted_header = false;
        was_just_emitted
    }
}
