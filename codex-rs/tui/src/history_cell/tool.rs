//! Tool call history cells driven by structured argument/result state.

use super::*;
use crate::history::state::{ArgumentValue, ToolArgument, ToolResultPreview};
use crate::text_formatting::format_json_compact;
use std::time::{Duration, Instant, SystemTime};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ToolCallStatus {
    Running,
    Success,
    Failed,
}

#[derive(Clone, Debug)]
pub(crate) struct ToolCallCellState {
    pub title: String,
    pub status: ToolCallStatus,
    pub duration: Option<Duration>,
    pub arguments: Vec<ToolArgument>,
    pub result_preview: Option<ToolResultPreview>,
    pub error_message: Option<String>,
}

impl ToolCallCellState {
    pub(crate) fn new(
        title: String,
        status: ToolCallStatus,
        duration: Option<Duration>,
        arguments: Vec<ToolArgument>,
        result_preview: Option<ToolResultPreview>,
        error_message: Option<String>,
    ) -> Self {
        Self {
            title,
            status,
            duration,
            arguments,
            result_preview,
            error_message,
        }
    }
}

pub(crate) struct ToolCallCell {
    state: ToolCallCellState,
}

impl ToolCallCell {
    pub(crate) fn new(state: ToolCallCellState) -> Self {
        Self { state }
    }

    pub(crate) fn retint(&mut self, _old: &crate::theme::Theme, _new: &crate::theme::Theme) {}

    fn header_line(&self) -> Line<'static> {
        let mut spans: Vec<Span<'static>> = Vec::new();
        let mut style = Style::default().add_modifier(Modifier::BOLD);
        style = match self.state.status {
            ToolCallStatus::Running => style.fg(crate::colors::info()),
            ToolCallStatus::Success => style.fg(crate::colors::success()),
            ToolCallStatus::Failed => style.fg(crate::colors::error()),
        };
        spans.push(Span::styled(self.state.title.clone(), style));
        if let Some(duration) = self.state.duration {
            spans.push(Span::styled(
                format!(", duration: {}", format_duration(duration)),
                Style::default().fg(crate::colors::text_dim()),
            ));
        }
        Line::from(spans)
    }
}

impl HistoryCell for ToolCallCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Tool {
            status: match self.state.status {
                ToolCallStatus::Running => ToolStatus::Running,
                ToolCallStatus::Success => ToolStatus::Success,
                ToolCallStatus::Failed => ToolStatus::Failed,
            },
        }
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(self.header_line());
        lines.extend(render_arguments(&self.state.arguments));

        if let Some(result) = &self.state.result_preview {
            if !result.lines.is_empty() {
                lines.push(Line::from(""));
                for line in &result.lines {
                    lines.push(Line::styled(
                        line.clone(),
                        Style::default().fg(crate::colors::text_dim()),
                    ));
                }
                if result.truncated {
                    lines.push(Line::styled(
                        "… truncated ",
                        Style::default().fg(crate::colors::text_dim()),
                    ));
                }
            }
        }

        if let Some(error) = &self.state.error_message {
            if !error.is_empty() {
                lines.push(Line::from(""));
                lines.push(Line::styled(
                    error.clone(),
                    Style::default().fg(crate::colors::error()),
                ));
            }
        }

        lines.push(Line::from(""));
        lines
    }
}

#[derive(Clone, Debug)]
pub(crate) struct RunningToolCallState {
    pub title: String,
    pub started_at: SystemTime,
    pub arguments: Vec<ToolArgument>,
    pub wait_has_target: bool,
    pub wait_has_call_id: bool,
    pub wait_cap_ms: Option<u64>,
}

impl RunningToolCallState {
    pub(crate) fn new(
        title: String,
        started_at: SystemTime,
        arguments: Vec<ToolArgument>,
        wait_has_target: bool,
        wait_has_call_id: bool,
        wait_cap_ms: Option<u64>,
    ) -> Self {
        Self {
            title,
            started_at,
            arguments,
            wait_has_target,
            wait_has_call_id,
            wait_cap_ms,
        }
    }
}

pub(crate) struct RunningToolCallCell {
    state: RunningToolCallState,
    start_clock: Instant,
}

impl RunningToolCallCell {
    pub(crate) fn new(state: RunningToolCallState) -> Self {
        Self {
            state,
            start_clock: Instant::now(),
        }
    }

    fn strip_zero_seconds_suffix(mut duration: String) -> String {
        if duration.ends_with(" 00s") {
            duration.truncate(duration.len().saturating_sub(4));
        }
        duration
    }

    fn spinner_frame(&self) -> &'static str {
        const FRAMES: [&str; 4] = ["◐", "◓", "◑", "◒"];
        let idx = ((self.start_clock.elapsed().as_millis() / 100) as usize) % FRAMES.len();
        FRAMES[idx]
    }

    pub(crate) fn has_title(&self, title: &str) -> bool {
        self.state.title == title
    }

    pub(crate) fn finalize_web_search(
        &self,
        success: bool,
        query: Option<String>,
    ) -> ToolCallCell {
        let duration = self.elapsed_duration();
        let mut arguments: Vec<ToolArgument> = Vec::new();
        if let Some(q) = query {
            arguments.push(ToolArgument {
                name: "query".to_string(),
                value: ArgumentValue::Text(q),
            });
        }
        let status = if success {
            ToolCallStatus::Success
        } else {
            ToolCallStatus::Failed
        };
        let state = ToolCallCellState::new(
            if success {
                "Web Search".to_string()
            } else {
                "Web Search (failed)".to_string()
            },
            status,
            Some(duration),
            arguments,
            None,
            None,
        );
        ToolCallCell::new(state)
    }

    fn elapsed_duration(&self) -> Duration {
        SystemTime::now()
            .duration_since(self.state.started_at)
            .unwrap_or_else(|_| self.start_clock.elapsed())
    }
}

impl HistoryCell for RunningToolCallCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Tool {
            status: ToolStatus::Running,
        }
    }

    fn gutter_symbol(&self) -> Option<&'static str> {
        if self.state.title == "Waiting" {
            if self.state.wait_has_call_id {
                None
            } else {
                Some(self.spinner_frame())
            }
        } else {
            Some("⚙")
        }
    }

    fn is_animating(&self) -> bool {
        true
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        let elapsed = self.elapsed_duration();
        let mut lines: Vec<Line<'static>> = Vec::new();
        if self.state.title == "Waiting" {
            let show_elapsed = !self.state.wait_has_target;
            let mut spans = Vec::new();
            spans.push(
                Span::styled(
                    "Waiting...",
                    Style::default()
                        .fg(crate::colors::text())
                        .add_modifier(Modifier::BOLD),
                ),
            );
            let cap_ms = self.state.wait_cap_ms.unwrap_or(600_000);
            let cap_str = Self::strip_zero_seconds_suffix(
                format_duration(Duration::from_millis(cap_ms)),
            );
            let suffix = if show_elapsed {
                let elapsed_str = Self::strip_zero_seconds_suffix(format_duration(elapsed));
                format!(" ({} / up to {})", elapsed_str, cap_str)
            } else {
                format!(" (up to {})", cap_str)
            };
            spans.push(Span::styled(
                suffix,
                Style::default().fg(crate::colors::text_dim()),
            ));
            lines.push(Line::from(spans));
        } else {
            lines.push(Line::styled(
                format!("{} ({})", self.state.title, format_duration(elapsed)),
                Style::default()
                    .fg(crate::colors::info())
                    .add_modifier(Modifier::BOLD),
            ));
        }
        lines.extend(render_arguments(&self.state.arguments));
        lines.push(Line::from(""));
        lines
    }
}

fn render_arguments(arguments: &[ToolArgument]) -> Vec<Line<'static>> {
    arguments.iter().map(render_argument).collect()
}

fn render_argument(arg: &ToolArgument) -> Line<'static> {
    let dim_style = Style::default().fg(crate::colors::text_dim());
    let mut spans = vec![Span::styled("└ ", dim_style)];
    spans.push(Span::styled(
        format!("{}: ", arg.name),
        dim_style,
    ));
    let value_span = match &arg.value {
        ArgumentValue::Text(text) => Span::styled(text.clone(), Style::default().fg(crate::colors::text())),
        ArgumentValue::Json(json) => {
            let compact = format_json_compact(&json.to_string()).unwrap_or_else(|| json.to_string());
            Span::styled(compact, Style::default().fg(crate::colors::text()))
        }
        ArgumentValue::Secret => Span::styled("(secret)".to_string(), Style::default().fg(crate::colors::text_dim())),
    };
    spans.push(value_span);
    Line::from(spans)
}
