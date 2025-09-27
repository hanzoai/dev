use std::collections::HashSet;
use std::rc::Rc;
use std::time::{Duration, Instant, SystemTime};

use codex_common::elapsed::format_duration;
use codex_core::parse_command::ParsedCommand;
use ratatui::prelude::{Buffer, Rect};
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Padding, Widget};

use crate::exec_command::strip_bash_lc_and_escape;
use crate::history::state::{
    ExecAction,
    ExecRecord,
    ExecStatus,
    ExecStreamChunk,
    ExecWaitNote as RecordExecWaitNote,
    HistoryId,
    TextTone,
};
use crate::insert_history::word_wrap_lines;
use crate::util::buffer::{fill_rect, write_line};

use super::{
    action_enum_from_parsed,
    exec_command_lines,
    emphasize_shell_command_name,
    first_context_path,
    format_inline_script_for_display,
    insert_line_breaks_after_double_ampersand,
    normalize_shell_command_display,
    running_status_line,
    trim_empty_lines,
    exec_render_parts_parsed,
    exec_render_parts_parsed_with_meta,
    CommandOutput,
    ExecKind,
    HistoryCell,
    HistoryCellType,
    output_lines,
};

// ==================== ExecCell ====================

#[derive(Clone, PartialEq, Eq)]
struct ExecWaitNote {
    text: String,
    is_error: bool,
}

#[derive(Clone, Default)]
struct ExecWaitState {
    total_wait: Option<Duration>,
    run_duration: Option<Duration>,
    waiting: bool,
    notes: Vec<ExecWaitNote>,
}

pub(crate) struct ExecCell {
    pub(crate) record: ExecRecord,
    pub(crate) command: Vec<String>,
    pub(crate) parsed: Vec<ParsedCommand>,
    pub(crate) output: Option<CommandOutput>,
    pub(crate) start_time: Option<Instant>,
    pub(crate) stream_preview: Option<CommandOutput>,
    // Caches to avoid recomputing expensive line construction for completed execs
    cached_display_lines: std::cell::RefCell<Option<Vec<Line<'static>>>>,
    cached_pre_lines: std::cell::RefCell<Option<Vec<Line<'static>>>>,
    cached_out_lines: std::cell::RefCell<Option<Vec<Line<'static>>>>,
    // Cached per-width layout (wrapped rows + totals) while content is stable
    cached_layout: std::cell::RefCell<Option<Rc<ExecLayoutCache>>>,
    cached_command_lines: std::cell::RefCell<Option<Vec<Line<'static>>>>,
    cached_wait_extras: std::cell::RefCell<Option<Vec<Line<'static>>>>,
    parsed_meta: Option<ParsedExecMetadata>,
    has_bold_command: bool,
    wait_state: std::cell::RefCell<ExecWaitState>,
}

const STREAMING_EXIT_CODE: i32 = i32::MIN;

#[derive(Clone)]
struct ExecLayoutCache {
    width: u16,
    pre_lines: Vec<Line<'static>>,
    out_lines: Vec<Line<'static>>,
    pre_total: u16,
    out_block_total: u16,
}

#[derive(Clone)]
pub(crate) struct ParsedExecMetadata {
    pub(crate) action: ExecAction,
    pub(crate) ctx_path: Option<String>,
    pub(crate) search_paths: HashSet<String>,
}

impl ParsedExecMetadata {
    pub(crate) fn from_commands(parsed: &[ParsedCommand]) -> Self {
        let action = action_enum_from_parsed(parsed);
        let ctx_path = first_context_path(parsed);
        let mut search_paths: HashSet<String> = HashSet::new();
        for pc in parsed {
            if let ParsedCommand::Search { path: Some(p), .. } = pc {
                search_paths.insert(p.to_string());
            }
        }
        Self {
            action,
            ctx_path,
            search_paths,
        }
    }
}

fn chunks_to_string(chunks: &[ExecStreamChunk]) -> String {
    if chunks.is_empty() {
        return String::new();
    }
    let mut sorted = chunks.to_vec();
    sorted.sort_by_key(|chunk| chunk.offset);
    let mut combined = String::new();
    for chunk in sorted {
        combined.push_str(&chunk.content);
    }
    combined
}

fn wait_notes_from_record(notes: &[RecordExecWaitNote]) -> Vec<ExecWaitNote> {
    notes
        .iter()
        .map(|note| ExecWaitNote {
            text: note.message.clone(),
            is_error: matches!(note.tone, TextTone::Error),
        })
        .collect()
}

fn record_output(record: &ExecRecord) -> Option<CommandOutput> {
    if !matches!(record.status, ExecStatus::Running) {
        let stdout = chunks_to_string(&record.stdout_chunks);
        let stderr = chunks_to_string(&record.stderr_chunks);
        let exit_code = record.exit_code.unwrap_or_default();
        return Some(CommandOutput {
            exit_code,
            stdout,
            stderr,
        });
    }
    None
}

impl HistoryCell for ExecCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
    fn kind(&self) -> HistoryCellType {
        let kind = match self.parsed_action() {
            ExecAction::Read => ExecKind::Read,
            ExecAction::Search => ExecKind::Search,
            ExecAction::List => ExecKind::List,
            ExecAction::Run => ExecKind::Run,
        };
        let status = self.record.status;
        HistoryCellType::Exec { kind, status }
    }
    fn gutter_symbol(&self) -> Option<&'static str> {
        match self.kind() {
            HistoryCellType::Exec {
                kind: ExecKind::Run,
                status,
            } => {
                if matches!(status, ExecStatus::Error) {
                    Some("✖")
                } else if self.has_bold_command {
                    Some("❯")
                } else {
                    None
                }
            }
            HistoryCellType::Exec { .. } => None,
            _ => None,
        }
    }
    fn display_lines(&self) -> Vec<Line<'static>> {
        // Fallback textual representation (used for height measurement only when custom rendering).
        // For completed executions, cache the computed lines since they are immutable.
        if let Some(cached) = self.cached_display_lines.borrow().as_ref() {
            return cached.clone();
        }
        let lines = exec_command_lines(
            &self.command,
            &self.parsed,
            self.output.as_ref(),
            self.stream_preview.as_ref(),
            self.start_time,
        );
        if self.output.is_some() {
            *self.cached_display_lines.borrow_mut() = Some(lines.clone());
        }
        lines
    }
    fn has_custom_render(&self) -> bool {
        true
    }
    fn is_animating(&self) -> bool {
        matches!(self.record.status, ExecStatus::Running) && self.start_time.is_some()
    }
    fn desired_height(&self, width: u16) -> u16 {
        let (pre_total, _out_block_total, out_total_with_status) = self.ensure_wrap_totals(width);
        pre_total.saturating_add(out_total_with_status)
    }
    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        let plan = self.ensure_layout(area.width);
        let plan_ref = plan.as_ref();

        let pre_total = plan_ref.pre_total;
        let out_block_total = plan_ref.out_block_total;

        let pre_skip = skip_rows.min(pre_total);
        let after_pre_skip = skip_rows.saturating_sub(pre_total);
        let block_skip = after_pre_skip.min(out_block_total);
        let after_block_skip = after_pre_skip.saturating_sub(block_skip);

        let pre_height = pre_total
            .saturating_sub(pre_skip)
            .min(area.height);
        let mut remaining_height = area.height.saturating_sub(pre_height);

        let block_height = out_block_total
            .saturating_sub(block_skip)
            .min(remaining_height);
        remaining_height = remaining_height.saturating_sub(block_height);

        let status_line_to_render = if self.output.is_none()
            && after_block_skip == 0
            && remaining_height > 0
        {
            self.streaming_status_line()
        } else {
            None
        };
        let status_height = status_line_to_render.is_some().then_some(1).unwrap_or(0);

        let mut cur_y = area.y;

        if pre_height > 0 {
            let pre_area = Rect {
                x: area.x,
                y: cur_y,
                width: area.width,
                height: pre_height,
            };
            let bg_style = Style::default()
                .bg(crate::colors::background())
                .fg(crate::colors::text());
            fill_rect(buf, pre_area, Some(' '), bg_style);
            for (idx, line) in plan_ref
                .pre_lines
                .iter()
                .skip(pre_skip as usize)
                .take(pre_height as usize)
                .enumerate()
            {
                let y = pre_area.y.saturating_add(idx as u16);
                if y >= pre_area.y.saturating_add(pre_area.height) {
                    break;
                }
                write_line(buf, pre_area.x, y, pre_area.width, line, bg_style);
            }
            cur_y = cur_y.saturating_add(pre_height);
        }

        if block_height > 0 && area.width > 0 {
            let out_area = Rect {
                x: area.x,
                y: cur_y,
                width: area.width,
                height: block_height,
            };
            let bg_style = Style::default()
                .bg(crate::colors::background())
                .fg(crate::colors::text_dim());
            fill_rect(buf, out_area, Some(' '), bg_style);
            let block = Block::default()
                .borders(Borders::LEFT)
                .border_style(
                    Style::default()
                        .fg(crate::colors::border_dim())
                        .bg(crate::colors::background()),
                )
                .style(Style::default().bg(crate::colors::background()))
                .padding(Padding {
                    left: 1,
                    right: 0,
                    top: 0,
                    bottom: 0,
                });
            let inner_rect = block.inner(out_area);
            block.render(out_area, buf);

            if inner_rect.width > 0 {
                for (idx, line) in plan_ref
                    .out_lines
                    .iter()
                    .skip(block_skip as usize)
                    .take(block_height as usize)
                    .enumerate()
                {
                    let y = inner_rect.y.saturating_add(idx as u16);
                    if y >= inner_rect.y.saturating_add(inner_rect.height) {
                        break;
                    }
                    write_line(buf, inner_rect.x, y, inner_rect.width, line, bg_style);
                }
            }
            cur_y = cur_y.saturating_add(block_height);
        }

        if let Some(line) = status_line_to_render {
            if status_height > 0 {
                let status_area = Rect {
                    x: area.x,
                    y: cur_y,
                    width: area.width,
                    height: status_height,
                };
                let bg_style = Style::default().bg(crate::colors::background());
                fill_rect(buf, status_area, Some(' '), bg_style);
                write_line(buf, status_area.x, status_area.y, status_area.width, &line, bg_style);
            }
        }
    }
}

impl ExecCell {
    pub(crate) fn from_record(record: ExecRecord) -> Self {
        let command = record.command.clone();
        let parsed = record.parsed.clone();
        let output = record_output(&record);
        let stream_preview = if matches!(record.status, ExecStatus::Running) {
            let stdout = chunks_to_string(&record.stdout_chunks);
            let stderr = chunks_to_string(&record.stderr_chunks);
            if stdout.is_empty() && stderr.is_empty() {
                None
            } else {
                Some(CommandOutput {
                    exit_code: STREAMING_EXIT_CODE,
                    stdout,
                    stderr,
                })
            }
        } else {
            None
        };
        let has_bold_command = command_has_bold_token(&command);
        let parsed_meta = if parsed.is_empty() {
            None
        } else {
            Some(ParsedExecMetadata::from_commands(&parsed))
        };
        let run_duration = record
            .completed_at
            .and_then(|done| done.duration_since(record.started_at).ok());
        let wait_state = ExecWaitState {
            total_wait: None,
            run_duration,
            waiting: matches!(record.status, ExecStatus::Running),
            notes: wait_notes_from_record(&record.wait_notes),
        };
        let start_time = if matches!(record.status, ExecStatus::Running) {
            Some(Instant::now())
        } else {
            None
        };

        Self {
            record,
            command,
            parsed,
            output,
            start_time,
            stream_preview,
            cached_display_lines: std::cell::RefCell::new(None),
            cached_pre_lines: std::cell::RefCell::new(None),
            cached_out_lines: std::cell::RefCell::new(None),
            cached_layout: std::cell::RefCell::new(None),
            cached_command_lines: std::cell::RefCell::new(None),
            cached_wait_extras: std::cell::RefCell::new(None),
            parsed_meta,
            has_bold_command,
            wait_state: std::cell::RefCell::new(wait_state),
        }
    }

    fn invalidate_render_caches(&self) {
        self.cached_display_lines.borrow_mut().take();
        self.cached_pre_lines.borrow_mut().take();
        self.cached_out_lines.borrow_mut().take();
        self.cached_layout.borrow_mut().take();
        self.cached_wait_extras.borrow_mut().take();
    }

    pub(crate) fn parsed_action(&self) -> ExecAction {
        self
            .parsed_meta
            .as_ref()
            .map(|meta| meta.action)
            .unwrap_or(ExecAction::Run)
    }


    pub(crate) fn set_waiting(&self, waiting: bool) {
        let mut state = self.wait_state.borrow_mut();
        if state.waiting != waiting {
            state.waiting = waiting;
            drop(state);
            self.invalidate_render_caches();
        }
    }

    pub(crate) fn set_wait_total(&self, total: Option<Duration>) {
        let mut state = self.wait_state.borrow_mut();
        if state.total_wait != total {
            state.total_wait = total;
            drop(state);
            self.invalidate_render_caches();
        }
    }

    pub(crate) fn set_run_duration(&self, duration: Option<Duration>) {
        let mut state = self.wait_state.borrow_mut();
        if state.run_duration != duration {
            state.run_duration = duration;
            drop(state);
            self.invalidate_render_caches();
        }
    }

    pub(crate) fn wait_total(&self) -> Option<Duration> {
        self.wait_state.borrow().total_wait
    }

    pub(crate) fn clear_wait_notes(&mut self) {
        let mut state = self.wait_state.borrow_mut();
        if state.notes.is_empty() {
            return;
        }
        state.notes.clear();
        drop(state);
        self.record.wait_notes.clear();
        self.invalidate_render_caches();
    }

    pub(crate) fn push_wait_note(&mut self, text: &str, is_error: bool) {
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return;
        }
        let mut state = self.wait_state.borrow_mut();
        if state
            .notes
            .last()
            .map(|note| note.text == trimmed && note.is_error == is_error)
            .unwrap_or(false)
        {
            return;
        }
        state.notes.push(ExecWaitNote {
            text: trimmed.to_string(),
            is_error,
        });
        drop(state);
        self.record.wait_notes.push(RecordExecWaitNote {
            message: trimmed.to_string(),
            tone: if is_error {
                TextTone::Error
            } else {
                TextTone::Info
            },
            timestamp: SystemTime::now(),
        });
        self.invalidate_render_caches();
    }

    pub(crate) fn set_wait_notes(&mut self, notes: &[(String, bool)]) {
        let mut state = self.wait_state.borrow_mut();
        let mut changed = state.notes.len() != notes.len();
        if !changed {
            for (existing, (text, is_error)) in state.notes.iter().zip(notes.iter()) {
                if existing.text != text.trim() || existing.is_error != *is_error {
                    changed = true;
                    break;
                }
            }
        }
        if !changed {
            return;
        }
        state.notes = notes
            .iter()
            .map(|(text, is_error)| ExecWaitNote {
                text: text.trim().to_string(),
                is_error: *is_error,
            })
            .filter(|note| !note.text.is_empty())
            .collect();
        drop(state);
        self.record.wait_notes = notes
            .iter()
            .filter_map(|(text, is_error)| {
                let trimmed = text.trim();
                if trimmed.is_empty() {
                    None
                } else {
                    Some(RecordExecWaitNote {
                        message: trimmed.to_string(),
                        tone: if *is_error {
                            TextTone::Error
                        } else {
                            TextTone::Info
                        },
                        timestamp: SystemTime::now(),
                    })
                }
            })
            .collect();
        self.invalidate_render_caches();
    }

    fn wait_note_lines(&self, state: &ExecWaitState) -> Vec<Line<'static>> {
        let mut lines = Vec::new();
        for note in &state.notes {
            let mut line = Line::from(note.text.clone());
            let mut style = Style::default().fg(if note.is_error {
                crate::colors::error()
            } else {
                crate::colors::text_dim()
            });
            if note.is_error {
                style = style.add_modifier(Modifier::BOLD);
            }
            for span in line.spans.iter_mut() {
                span.style = style;
            }
            lines.push(line);
        }
        lines
    }

    fn wait_state_snapshot(&self) -> ExecWaitState {
        self.wait_state.borrow().clone()
    }

    fn wait_summary_line(&self, state: &ExecWaitState) -> Option<Line<'static>> {
        if state.waiting {
            return None;
        }
        if let Some(run_duration) = state.run_duration {
            if run_duration.is_zero() {
                return None;
            }
            let text = format!("Ran for {}", format_duration(run_duration));
            return Some(Line::styled(
                text,
                Style::default().fg(crate::colors::text_dim()),
            ));
        }
        let total = state.total_wait?;
        if total.is_zero() {
            return None;
        }
        let text = format!("Waited {}", format_duration(total));
        Some(Line::styled(
            text,
            Style::default().fg(crate::colors::text_dim()),
        ))
    }

    fn wait_extras(&self, state: &ExecWaitState) -> Vec<Line<'static>> {
        if let Some(cached) = self.cached_wait_extras.borrow().as_ref() {
            return cached.clone();
        }
        let mut extra_lines: Vec<Line<'static>> = Vec::new();
        if let Some(summary_line) = self.wait_summary_line(state) {
            extra_lines.push(summary_line);
        }
        extra_lines.extend(self.wait_note_lines(state));
        if self.output.is_some() && !extra_lines.is_empty() {
            *self.cached_wait_extras.borrow_mut() = Some(extra_lines.clone());
        }
        extra_lines
    }

    #[cfg(test)]
    fn has_bold_command(&self) -> bool {
        self.has_bold_command
    }

    pub(crate) fn replace_command_metadata(
        &mut self,
        command: Vec<String>,
        parsed: Vec<ParsedCommand>,
    ) {
        self.command = command;
        self.parsed = parsed;
        self.record.command = self.command.clone();
        self.record.parsed = self.parsed.clone();
        self.record.action = action_enum_from_parsed(&self.record.parsed);
        self.has_bold_command = command_has_bold_token(&self.command);
        self.cached_command_lines.borrow_mut().take();
        self.cached_wait_extras.borrow_mut().take();
        self.parsed_meta = if self.parsed.is_empty() {
            None
        } else {
            Some(ParsedExecMetadata::from_commands(&self.parsed))
        };
        self.invalidate_render_caches();
    }
    /// Compute wrapped row totals for the preamble and the output at the given width.
    /// Delegates to the per-width layout cache to avoid redundant reflow work.
    fn ensure_wrap_totals(&self, width: u16) -> (u16, u16, u16) {
        let layout = self.ensure_layout(width);
        let status_height = if self.output.is_none() {
            self.streaming_status_line().map(|_| 1).unwrap_or(0)
        } else {
            0
        };
        (
            layout.pre_total,
            layout.out_block_total,
            layout
                .out_block_total
                .saturating_add(status_height),
        )
    }

    fn ensure_layout(&self, width: u16) -> Rc<ExecLayoutCache> {
        if let Some(layout) = self.cached_layout.borrow().as_ref() {
            if layout.width == width {
                return layout.clone();
            }
        }

        let (pre_lines_raw, out_lines_raw, _status_line_opt) = self.exec_render_parts();
        let pre_trimmed = trim_empty_lines(pre_lines_raw);
        let out_trimmed = trim_empty_lines(out_lines_raw);

        let pre_wrap_width = width;
        let out_wrap_width = width.saturating_sub(2);

        let pre_wrapped = if pre_wrap_width == 0 {
            Vec::new()
        } else {
            word_wrap_lines(&pre_trimmed, pre_wrap_width)
        };
        let out_wrapped = if out_wrap_width == 0 {
            Vec::new()
        } else {
            word_wrap_lines(&out_trimmed, out_wrap_width)
        };

        let clamp_len = |len: usize| -> u16 { len.min(u16::MAX as usize) as u16 };
        let pre_total = clamp_len(pre_wrapped.len());
        let out_block_total = clamp_len(out_wrapped.len());

        let layout = Rc::new(ExecLayoutCache {
            width,
            pre_lines: pre_wrapped,
            out_lines: out_wrapped,
            pre_total,
            out_block_total,
        });
        *self.cached_layout.borrow_mut() = Some(layout.clone());
        layout
    }
    // Build separate segments: (preamble lines, output lines)
    pub(crate) fn exec_render_parts(
        &self,
    ) -> (
        Vec<Line<'static>>,
        Vec<Line<'static>>,
        Option<Line<'static>>,
    ) {
        if let (Some(pre), Some(out)) = (
            self.cached_pre_lines.borrow().as_ref(),
            self.cached_out_lines.borrow().as_ref(),
        ) {
            if self.output.is_some() {
                return (pre.clone(), out.clone(), None);
            }
            if self.stream_preview.is_some() {
                let wait_state = self.wait_state_snapshot();
                let status_label = if wait_state.waiting { "Waiting" } else { "Running" };
                let status = self.streaming_status_line_for_label(status_label);
                return (pre.clone(), out.clone(), status);
            }
        }

        let wait_state = self.wait_state_snapshot();
        let status_label = if wait_state.waiting { "Waiting" } else { "Running" };

        let (pre, mut out, status) = if self.parsed.is_empty() {
            if let (Some(pre_cached), Some(out_cached)) = (
                self.cached_pre_lines.borrow().as_ref(),
                self.cached_out_lines.borrow().as_ref(),
            ) {
                let status_cached = if self.output.is_none() {
                    self.streaming_status_line_for_label(status_label)
                } else {
                    None
                };
                return (pre_cached.clone(), out_cached.clone(), status_cached);
            }

            self.exec_render_parts_generic(status_label)
        } else {
            if self.output.is_some() {
                if let (Some(pre_cached), Some(out_cached)) = (
                    self.cached_pre_lines.borrow().as_ref(),
                    self.cached_out_lines.borrow().as_ref(),
                ) {
                    return (pre_cached.clone(), out_cached.clone(), None);
                }
            }

            match self.parsed_meta.as_ref() {
                Some(meta) => exec_render_parts_parsed_with_meta(
                    &self.parsed,
                    meta,
                    self.output.as_ref(),
                    self.stream_preview.as_ref(),
                    self.start_time,
                    status_label,
                ),
                None => exec_render_parts_parsed(
                    &self.parsed,
                    self.output.as_ref(),
                    self.stream_preview.as_ref(),
                    self.start_time,
                    status_label,
                ),
            }
        };

        if self.output.is_some() {
            let extra_lines = self.wait_extras(&wait_state);
            if !extra_lines.is_empty() {
                let is_blank_line = |line: &Line<'static>| {
                    line.spans
                        .iter()
                        .all(|span| span.content.as_ref().trim().is_empty())
                };
                let is_error_line = |line: &Line<'static>| {
                    line.spans
                        .first()
                        .map(|span| span.content.as_ref().starts_with("Error (exit code"))
                        .unwrap_or(false)
                };
                let insert_at = if let Some(pos) = out.iter().position(is_error_line) {
                    pos
                } else {
                    out.len()
                };

                let mut block: Vec<Line<'static>> = Vec::new();
                if insert_at > 0 && !is_blank_line(&out[insert_at - 1]) {
                    block.push(Line::from(""));
                }
                block.extend(extra_lines.into_iter());
                if insert_at < out.len() {
                    if !is_blank_line(&out[insert_at]) {
                        block.push(Line::from(""));
                    }
                } else {
                    block.push(Line::from(""));
                }

                out.splice(insert_at..insert_at, block);
            }
            *self.cached_pre_lines.borrow_mut() = Some(pre.clone());
            *self.cached_out_lines.borrow_mut() = Some(out.clone());
        } else if self.output.is_none() {
            *self.cached_pre_lines.borrow_mut() = Some(pre.clone());
            *self.cached_out_lines.borrow_mut() = Some(out.clone());
        }
        (pre, out, status)
    }

    pub(crate) fn sync_from_record(&mut self, record: &ExecRecord) {
        self.record = record.clone();
        self.command = record.command.clone();
        self.parsed = record.parsed.clone();
        self.has_bold_command = command_has_bold_token(&self.command);

        if matches!(record.status, ExecStatus::Running) {
            let stdout = chunks_to_string(&record.stdout_chunks);
            let stderr = chunks_to_string(&record.stderr_chunks);
            if stdout.is_empty() && stderr.is_empty() {
                self.stream_preview = None;
            } else {
                self.stream_preview = Some(CommandOutput {
                    exit_code: STREAMING_EXIT_CODE,
                    stdout,
                    stderr,
                });
            }
        } else {
            self.stream_preview = None;
        }

        // Invalidate cached layouts so the refreshed state re-renders.
        self.cached_display_lines.borrow_mut().take();
        self.cached_pre_lines.borrow_mut().take();
        self.cached_out_lines.borrow_mut().take();
        self.cached_layout.borrow_mut().take();
        self.cached_command_lines.borrow_mut().take();
        self.cached_wait_extras.borrow_mut().take();
    }

    fn exec_render_parts_generic(
        &self,
        status_label: &str,
    ) -> (
        Vec<Line<'static>>,
        Vec<Line<'static>>,
        Option<Line<'static>>,
    ) {
        let mut pre = self.generic_command_lines();
        let display_output = self
            .output
            .as_ref()
            .or(self.stream_preview.as_ref());
        let mut out = output_lines(display_output, false, false);
        let has_output = !trim_empty_lines(out.clone()).is_empty();

        if self.output.is_none() && has_output {
            if let Some(last) = pre.last_mut() {
                last.spans.insert(
                    0,
                    Span::styled(
                        "┌ ",
                        Style::default().fg(crate::colors::text_dim()),
                    ),
                );
            }
        }

        let mut status = None;
        if self.output.is_none() {
            let status_line = self.streaming_status_line_for_label(status_label);
            if status_line.is_some() {
                if let Some(last) = out.last() {
                    let is_blank = last
                        .spans
                        .iter()
                        .all(|sp| sp.content.as_ref().trim().is_empty());
                    if is_blank {
                        out.pop();
                    }
                }
            }
            status = status_line;
        }

        (pre, out, status)
    }

    fn generic_command_lines(&self) -> Vec<Line<'static>> {
        if let Some(cached) = self.cached_command_lines.borrow().as_ref() {
            return cached.clone();
        }

        let command_escaped = strip_bash_lc_and_escape(&self.command);
        let formatted = format_inline_script_for_display(&command_escaped);
        let normalized = normalize_shell_command_display(&formatted);
        let command_display = insert_line_breaks_after_double_ampersand(&normalized);

        let mut highlighted_cmd =
            crate::syntax_highlight::highlight_code_block(&command_display, Some("bash"));
        for (idx, line) in highlighted_cmd.iter_mut().enumerate() {
            emphasize_shell_command_name(line);
            if idx > 0 {
                line.spans.insert(
                    0,
                    Span::styled(
                        "  ",
                        Style::default().fg(crate::colors::text()),
                    ),
                );
            }
        }

        let owned: Vec<Line<'static>> = highlighted_cmd;
        *self.cached_command_lines.borrow_mut() = Some(owned.clone());
        owned
    }

    fn streaming_status_line(&self) -> Option<Line<'static>> {
        if self.output.is_some() {
            return None;
        }
        let wait_state = self.wait_state_snapshot();
        let status_label = if wait_state.waiting { "Waiting" } else { "Running" };
        self.streaming_status_line_for_label(status_label)
    }

    fn streaming_status_line_for_label(&self, status_label: &str) -> Option<Line<'static>> {
        if self.output.is_some() {
            return None;
        }

        if self.parsed.is_empty() {
            let mut message = format!("{status_label}...");
            if let Some(start) = self.start_time {
                let elapsed = start.elapsed();
                if !elapsed.is_zero() {
                    message = format!("{message} ({})", format_duration(elapsed));
                }
            }
            return Some(running_status_line(message));
        }

        let meta = match self.parsed_meta.as_ref() {
            Some(meta) => meta,
            None => return None,
        };
        if !matches!(meta.action, ExecAction::Run) {
            return None;
        }

        let mut message = match meta.ctx_path.as_deref() {
            Some(p) => format!("{status_label}... in {p}"),
            None => format!("{status_label}..."),
        };
        if let Some(start) = self.start_time {
            let elapsed = start.elapsed();
            if !elapsed.is_zero() {
                message = format!("{message} ({})", format_duration(elapsed));
            }
        }
        Some(running_status_line(message))
    }
}

fn chunk_from_text(text: &str) -> Vec<ExecStreamChunk> {
    if text.is_empty() {
        Vec::new()
    } else {
        vec![ExecStreamChunk {
            offset: 0,
            content: text.to_string(),
        }]
    }
}

fn build_exec_record(
    command: Vec<String>,
    parsed: Vec<ParsedCommand>,
    output: Option<CommandOutput>,
) -> ExecRecord {
    let action = action_enum_from_parsed(&parsed);
    let started_at = SystemTime::now();
    match output {
        Some(output) => {
            let exit_code = output.exit_code;
            let status = if exit_code == 0 {
                ExecStatus::Success
            } else {
                ExecStatus::Error
            };
            ExecRecord {
                id: HistoryId::ZERO,
                command,
                parsed,
                action,
                status,
                stdout_chunks: chunk_from_text(&output.stdout),
                stderr_chunks: chunk_from_text(&output.stderr),
                exit_code: Some(exit_code),
                wait_notes: Vec::new(),
                started_at,
                completed_at: Some(SystemTime::now()),
            }
        }
        None => ExecRecord {
            id: HistoryId::ZERO,
            command,
            parsed,
            action,
            status: ExecStatus::Running,
            stdout_chunks: Vec::new(),
            stderr_chunks: Vec::new(),
            exit_code: None,
            wait_notes: Vec::new(),
            started_at,
            completed_at: None,
        },
    }
}

pub(crate) fn new_active_exec_command(
    command: Vec<String>,
    parsed: Vec<ParsedCommand>,
) -> ExecCell {
    let record = build_exec_record(command, parsed, None);
    ExecCell::from_record(record)
}

pub(crate) fn new_completed_exec_command(
    command: Vec<String>,
    parsed: Vec<ParsedCommand>,
    output: CommandOutput,
) -> ExecCell {
    let record = build_exec_record(command, parsed, Some(output));
    ExecCell::from_record(record)
}

fn command_has_bold_token(command: &[String]) -> bool {
    let command_escaped = strip_bash_lc_and_escape(command);
    let normalized = normalize_shell_command_display(&command_escaped);
    let trimmed = normalized.trim_start();
    if trimmed.is_empty() {
        return false;
    }
    trimmed.chars().take_while(|ch| !ch.is_whitespace()).count() > 4
}

// ==================== MergedExecCell ====================
// Represents multiple completed exec results merged into one cell while preserving
// the bordered, dimmed output styling for each command's stdout/stderr preview.
