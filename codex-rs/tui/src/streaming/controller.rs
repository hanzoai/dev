#![allow(dead_code)]

use codex_core::config::Config;
use ratatui::text::Line;

use super::HeaderEmitter;
use super::StreamKind;
use super::StreamState;

/// Sink for history insertions and animation control.
pub(crate) trait HistorySink {
    fn insert_history(&self, lines: Vec<Line<'static>>);
    fn insert_history_with_kind(&self, kind: StreamKind, lines: Vec<Line<'static>>);
    fn start_commit_animation(&self);
    fn stop_commit_animation(&self);
}

/// Concrete sink backed by `AppEventSender`.
pub(crate) struct AppEventHistorySink(pub(crate) crate::app_event_sender::AppEventSender);

impl HistorySink for AppEventHistorySink {
    fn insert_history(&self, lines: Vec<Line<'static>>) {
        tracing::debug!("insert_history called with {} lines:", lines.len());
        for (i, line) in lines.iter().enumerate() {
            let text: String = line.spans.iter().map(|s| s.content.to_string()).collect();
            if i < 3 || i >= lines.len() - 1 {
                tracing::debug!("  Line {}: {:?}", i, text);
            } else if i == 3 {
                tracing::debug!("  ... ({} more lines) ...", lines.len() - 4);
            }
        }
        self.0
            .send(crate::app_event::AppEvent::InsertHistory(lines))
    }
    fn insert_history_with_kind(&self, kind: StreamKind, lines: Vec<Line<'static>>) {
        tracing::debug!("insert_history_with_kind({:?}) {} lines", kind, lines.len());
        self.0
            .send(crate::app_event::AppEvent::InsertHistoryWithKind { kind, lines })
    }
    fn start_commit_animation(&self) {
        self.0
            .send(crate::app_event::AppEvent::StartCommitAnimation)
    }
    fn stop_commit_animation(&self) {
        self.0.send(crate::app_event::AppEvent::StopCommitAnimation)
    }
}

type Lines = Vec<Line<'static>>;

/// Controller that manages newline-gated streaming, header emission, and
/// commit animation across streams.
pub(crate) struct StreamController {
    config: Config,
    header: HeaderEmitter,
    states: [StreamState; 2],
    current_stream: Option<StreamKind>,
    finishing_after_drain: bool,
    thinking_placeholder_shown: bool,
}

impl StreamController {
    pub(crate) fn new(config: Config) -> Self {
        Self {
            config,
            header: HeaderEmitter::new(),
            states: [StreamState::new_for_kind(StreamKind::Answer), StreamState::new_for_kind(StreamKind::Reasoning)],
            current_stream: None,
            finishing_after_drain: false,
            thinking_placeholder_shown: false,
        }
    }

    pub(crate) fn reset_headers_for_new_turn(&mut self) {
        self.header.reset_for_new_turn();
    }

    pub(crate) fn is_write_cycle_active(&self) -> bool {
        self.current_stream.is_some()
    }
    

    pub(crate) fn clear_all(&mut self) {
        tracing::debug!("clear_all called, current_stream={:?}", self.current_stream);
        self.states.iter_mut().for_each(|s| s.clear());
        self.current_stream = None;
        self.finishing_after_drain = false;
        self.thinking_placeholder_shown = false;
        // leave header state unchanged; caller decides when to reset
    }

    #[inline]
    fn idx(kind: StreamKind) -> usize {
        kind as usize
    }
    fn state(&self, kind: StreamKind) -> &StreamState {
        &self.states[Self::idx(kind)]
    }
    fn state_mut(&mut self, kind: StreamKind) -> &mut StreamState {
        &mut self.states[Self::idx(kind)]
    }

    fn emit_header_if_needed(&mut self, kind: StreamKind, out_lines: &mut Lines) -> bool {
        self.header.maybe_emit(kind, out_lines)
    }

    #[inline]
    fn ensure_single_trailing_blank(_lines: &mut Lines) {
        // Removed - we don't need to add extra blank lines
        // The markdown renderer and section breaks already handle spacing
    }

    /// Begin a stream, flushing previously completed lines from any other
    /// active stream to maintain ordering.
    pub(crate) fn begin(&mut self, kind: StreamKind, sink: &impl HistorySink) {
        tracing::debug!("begin called for {:?}, current_stream={:?}", kind, self.current_stream);
        if let Some(current) = self.current_stream {
            if current != kind {
                tracing::debug!("Switching from {:?} to {:?}, flushing previous", current, kind);
                // Synchronously flush completed lines from previous stream.
                let cfg = self.config.clone();
                let step = {
                    let prev_state = self.state_mut(current);
                    let newly_completed = prev_state.collector.commit_complete_lines(&cfg);
                    if !newly_completed.is_empty() {
                        prev_state.enqueue(newly_completed);
                    }
                    let result = prev_state.drain_all();
                    // Clear the previous stream state to ensure no contamination
                    tracing::debug!("Clearing {:?} stream state", current);
                    prev_state.clear();
                    result
                };
                if !step.history.is_empty() {
                    tracing::debug!("Flushing {} lines from {:?} stream", step.history.len(), current);
                    let mut lines: Lines = Vec::new();
                    self.emit_header_if_needed(current, &mut lines);
                    lines.extend(step.history);
                    // Don't add extra blank line - markdown renderer handles spacing
                    sink.insert_history_with_kind(current, lines);
                }
                self.current_stream = None;
            }
        }

        if self.current_stream != Some(kind) {
            let prev = self.current_stream;
            self.current_stream = Some(kind);
            // Starting a new stream cancels any pending finish-from-previous-stream animation.
            self.finishing_after_drain = false;
            if prev.is_some() {
                self.header.reset_for_stream(kind);
            }
            // Emit header immediately for reasoning; for answers, defer to first commit.
            if matches!(kind, StreamKind::Reasoning) {
                let mut header_lines = Vec::new();
                if self.emit_header_if_needed(kind, &mut header_lines) {
                    sink.insert_history(header_lines);
                    self.thinking_placeholder_shown = true;
                }
            }
        }
    }

    /// Push a delta; if it contains a newline, commit completed lines and start animation.
    pub(crate) fn push_and_maybe_commit(&mut self, delta: &str, sink: &impl HistorySink) {
        let Some(kind) = self.current_stream else {
            tracing::debug!("push_and_maybe_commit called but no current_stream");
            return;
        };
        tracing::debug!("push_and_maybe_commit for {:?}, delta={:?}", kind, delta);
        let cfg = self.config.clone();

        // Check header flag before borrowing state (used only to avoid double headers)
        let _just_emitted_header = self.header.consume_header_flag();
        
        let state = self.state_mut(kind);
        // Record that at least one delta was received for this stream
        if !delta.is_empty() {
            state.has_seen_delta = true;
        }
        state.collector.push_delta(delta);
        if delta.contains('\n') {
            let mut newly_completed = state.collector.commit_complete_lines(&cfg);
            // Reduce leading blanks to at most one across commits
            if !newly_completed.is_empty() {
                let mut skip_count = 0;
                while skip_count < newly_completed.len()
                    && crate::render::line_utils::is_blank_line_trim(&newly_completed[skip_count]) {
                    skip_count += 1;
                }
                if skip_count > 1 {
                    for _ in 0..(skip_count - 1) {
                        newly_completed.remove(0);
                    }
                }
            }
            if !newly_completed.is_empty() {
                // Color reasoning as text_dim and answers as text_bright, preserving span modifiers
                let color = match kind {
                    StreamKind::Reasoning => crate::colors::text_dim(),
                    StreamKind::Answer => crate::colors::text_bright(),
                };
                let mut styled: Vec<Line<'static>> = Vec::with_capacity(newly_completed.len());
                for mut line in newly_completed {
                    line.style = line
                        .style
                        .patch(ratatui::style::Style::default().fg(color));
                    if matches!(kind, StreamKind::Answer) {
                        // Force bold spans in assistant output to use bright text
                        let spans: Vec<ratatui::text::Span<'static>> = line
                            .spans
                            .into_iter()
                            .map(|s| {
                                if s.style.add_modifier.contains(ratatui::style::Modifier::BOLD) {
                                    s.style(ratatui::style::Style::default().fg(crate::colors::text_bright()))
                                } else {
                                    s
                                }
                            })
                            .collect();
                        line.spans = spans;
                    }
                    styled.push(line);
                }
                state.enqueue(styled);
                sink.start_commit_animation();
            }
        }
    }

    /// Insert a reasoning section break and commit any newly completed lines.
    pub(crate) fn insert_reasoning_section_break(&mut self, sink: &impl HistorySink) {
        if self.current_stream != Some(StreamKind::Reasoning) {
            self.begin(StreamKind::Reasoning, sink);
        }
        let cfg = self.config.clone();
        let state = self.state_mut(StreamKind::Reasoning);
        // Insert an explicit section break so upcoming section titles are
        // rendered on a fresh line. Without this, bold titles that arrive
        // mid-line can be glued to the previous sentence and fail to be
        // recognized as titles in collapsed view.
        state.collector.insert_section_break();
        let mut newly_completed = state.collector.commit_complete_lines(&cfg);
        // Reduce leading blanks to at most one after section breaks
        if !newly_completed.is_empty() {
            let mut skip_count = 0;
            while skip_count < newly_completed.len()
                && crate::render::line_utils::is_blank_line_trim(&newly_completed[skip_count]) {
                skip_count += 1;
            }
            if skip_count > 1 {
                for _ in 0..(skip_count - 1) {
                    newly_completed.remove(0);
                }
            }
        }
        if !newly_completed.is_empty() {
            // Reasoning sections use dim text
            let color = crate::colors::text_dim();
            let mut styled: Vec<Line<'static>> = Vec::with_capacity(newly_completed.len());
            for mut line in newly_completed {
                let spans = line
                    .spans
                    .into_iter()
                    .map(|s| s.style(ratatui::style::Style::default().fg(color)))
                    .collect();
                line.spans = spans;
                styled.push(line);
            }
            state.enqueue(styled);
            sink.start_commit_animation();
        }
    }

    /// Finalize the active stream. If `flush_immediately` is true, drain and emit now.
    pub(crate) fn finalize(
        &mut self,
        kind: StreamKind,
        flush_immediately: bool,
        sink: &impl HistorySink,
    ) -> bool {
        if self.current_stream != Some(kind) {
            return false;
        }
        let cfg = self.config.clone();
        // Finalize collector first.
        let remaining = {
            let state = self.state_mut(kind);
            state.collector.finalize_and_drain(&cfg)
        };
        if flush_immediately {
            // Collect all output first to avoid emitting headers when there is no content.
            let mut out_lines: Lines = Vec::new();
            {
                let state = self.state_mut(kind);
                if !remaining.is_empty() {
                    state.enqueue(remaining);
                }
                let step = state.drain_all();
                out_lines.extend(step.history);
            }
            if !out_lines.is_empty() {
                let mut lines_with_header: Lines = Vec::new();
                let _emitted_header = self.emit_header_if_needed(kind, &mut lines_with_header);
                // Reduce leading blanks to at most one
                let mut skip_count = 0;
                while skip_count < out_lines.len()
                    && crate::render::line_utils::is_blank_line_trim(&out_lines[skip_count]) {
                    skip_count += 1;
                }
                if skip_count > 1 {
                    for _ in 0..(skip_count - 1) {
                        out_lines.remove(0);
                    }
                }
                // Apply stream-specific color to body lines
                let color = match kind {
                    StreamKind::Reasoning => crate::colors::text_dim(),
                    StreamKind::Answer => crate::colors::text_bright(),
                };
                let out_lines: Vec<Line<'static>> = out_lines
                    .into_iter()
                    .map(|mut line| {
                        line.style = line
                            .style
                            .patch(ratatui::style::Style::default().fg(color));
                        if matches!(kind, StreamKind::Answer) {
                            let spans: Vec<ratatui::text::Span<'static>> = line
                                .spans
                                .into_iter()
                                .map(|s| {
                                    if s.style.add_modifier.contains(ratatui::style::Modifier::BOLD) {
                                        s.style(ratatui::style::Style::default().fg(crate::colors::text_bright()))
                                    } else {
                                        s
                                    }
                                })
                                .collect();
                            line.spans = spans;
                        }
                        line
                    })
                    .collect();

                lines_with_header.extend(out_lines);
                // Don't add extra blank line - markdown renderer handles spacing
                sink.insert_history_with_kind(kind, lines_with_header);
            }

            // Cleanup
            self.state_mut(kind).clear();
            // Allow a subsequent block of the same kind in this turn to emit its header.
            self.header.allow_reemit_for_same_kind_in_turn(kind);
            // Also clear the per-stream emitted flag so the header can render again.
            self.header.reset_for_stream(kind);
            self.current_stream = None;
            self.finishing_after_drain = false;
            true
        } else {
            if !remaining.is_empty() {
                let state = self.state_mut(kind);
                state.enqueue(remaining);
            }
            // Don't add spacer - causes extra blank lines
            // self.state_mut(kind).enqueue(vec![Line::from("")]);
            self.finishing_after_drain = true;
            sink.start_commit_animation();
            false
        }
    }

    /// Step animation: commit at most one queued line and handle end-of-drain cleanup.
    pub(crate) fn on_commit_tick(&mut self, sink: &impl HistorySink) -> bool {
        let Some(kind) = self.current_stream else {
            return false;
        };
        let step = {
            let state = self.state_mut(kind);
            state.step()
        };
        if !step.history.is_empty() {
            let mut lines: Lines = Vec::new();
            // Emit header if needed for this stream; ignore return value
            self.emit_header_if_needed(kind, &mut lines);
            let mut out = lines;
            let mut history = step.history;
            // Reduce leading blanks to at most one
            if !history.is_empty() {
                let mut skip_count = 0;
                while skip_count < history.len()
                    && crate::render::line_utils::is_blank_line_trim(&history[skip_count]) {
                    skip_count += 1;
                }
                if skip_count > 1 {
                    for _ in 0..(skip_count - 1) {
                        history.remove(0);
                    }
                }
            }
            // Apply stream-specific color to body lines while preserving modifiers
            let color = match kind {
                StreamKind::Reasoning => crate::colors::text_dim(),
                StreamKind::Answer => crate::colors::text_bright(),
            };
            let history: Vec<Line<'static>> = history
                .into_iter()
                .map(|mut line| {
                    line.style = line
                        .style
                        .patch(ratatui::style::Style::default().fg(color));
                    if matches!(kind, StreamKind::Answer) {
                        let spans: Vec<ratatui::text::Span<'static>> = line
                            .spans
                            .into_iter()
                            .map(|s| {
                                if s.style.add_modifier.contains(ratatui::style::Modifier::BOLD) {
                                    s.style(ratatui::style::Style::default().fg(crate::colors::text_bright()))
                                } else {
                                    s
                                }
                            })
                            .collect();
                        line.spans = spans;
                    }
                    line
                })
                .collect();
            out.extend(history);
            sink.insert_history_with_kind(kind, out);
        }

        let is_idle = self.state(kind).is_idle();
        if is_idle {
            sink.stop_commit_animation();
            if self.finishing_after_drain {
                // Reset and notify
                self.state_mut(kind).clear();
                // Allow a subsequent block of the same kind in this turn to emit its header.
                self.header.allow_reemit_for_same_kind_in_turn(kind);
                // Also clear the per-stream emitted flag so the header can render again.
                self.header.reset_for_stream(kind);
                self.current_stream = None;
                self.finishing_after_drain = false;
                return true;
            }
        }
        false
    }

    /// Apply a full final answer: replace queued content with only the remaining tail,
    /// then finalize immediately and notify completion.
    pub(crate) fn apply_final_answer(&mut self, message: &str, sink: &impl HistorySink) -> bool {
        tracing::debug!("apply_final_answer called with: {:?}...", message.chars().take(100).collect::<String>());
        self.apply_full_final(StreamKind::Answer, message, true, sink)
    }

    pub(crate) fn apply_final_reasoning(&mut self, message: &str, sink: &impl HistorySink) -> bool {
        tracing::debug!("apply_final_reasoning called with: {:?}...", message.chars().take(100).collect::<String>());
        self.apply_full_final(StreamKind::Reasoning, message, false, sink)
    }

    fn apply_full_final(
        &mut self,
        kind: StreamKind,
        message: &str,
        immediate: bool,
        sink: &impl HistorySink,
    ) -> bool {
        tracing::debug!("apply_full_final for {:?}, immediate={}, message_len={}, current_stream={:?}", 
            kind, immediate, message.len(), self.current_stream);
        
        // Check if we're already processing this stream
        if self.current_stream == Some(kind) {
            let state = self.state(kind);
            let has_delta = state.has_seen_delta;
            
            if has_delta {
                // This is the final event for content we've been streaming via deltas
                // Just finalize what we have, don't inject the full message
                tracing::debug!("Already streaming {:?} via deltas, finalizing without injection", kind);
                return self.finalize(kind, immediate, sink);
            } else if self.finishing_after_drain {
                // We're already in the process of finishing this stream (animation phase)
                // This is likely a duplicate event - ignore it
                tracing::debug!("Already finishing {:?} stream, ignoring duplicate final event", kind);
                return false;
            }
            // else: We have a stream open but no deltas yet - could be a header-only stream
            // Fall through to inject the message
        }
        
        // This is a new section (no deltas received)
        self.begin(kind, sink);

        {
            let state = self.state_mut(kind);
            tracing::debug!("State for {:?}: has_seen_delta={}, committed_count={}, message_empty={}",
                kind, state.has_seen_delta, 
                state.collector.committed_count(),
                message.is_empty());
            
            // Inject the full message since we haven't been streaming it
            if !message.is_empty() {
                tracing::debug!("Injecting full message into {:?} collector", kind);
                // normalize to end with newline
                let mut msg = message.to_owned();
                if !msg.ends_with('\n') {
                    msg.push('\n');
                }

                // replace while preserving already committed count
                let committed = state.collector.committed_count();
                state
                    .collector
                    .replace_with_and_mark_committed(&msg, committed);
            }
        }

        self.finalize(kind, immediate, sink)
    }
}
