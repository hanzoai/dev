use super::{HistoryCell, HistoryCellType, ToolCellStatus};
use ratatui::text::Line;
use std::path::PathBuf;
use std::time::Duration;

const BOX_WIDTH: usize = 76;
const MAX_ACTIONS: usize = 12;
const MAX_CONSOLE: usize = 8;

#[derive(Clone, Default)]
pub(crate) struct BrowserSessionCell {
    url: Option<String>,
    title: Option<String>,
    actions: Vec<BrowserAction>,
    console_messages: Vec<String>,
    screenshot_path: Option<String>,
    total_duration: Duration,
    completed: bool,
}

#[derive(Clone)]
struct BrowserAction {
    timestamp: Duration,
    description: String,
}

impl BrowserSessionCell {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn set_url(&mut self, url: impl Into<String>) {
        self.url = Some(url.into());
    }

    pub(crate) fn record_action(
        &mut self,
        timestamp: Duration,
        duration: Duration,
        description: String,
    ) {
        let action = BrowserAction {
            timestamp,
            description,
        };
        self.actions.push(action);
        if self.actions.len() > MAX_ACTIONS {
            let overflow = self.actions.len() - MAX_ACTIONS;
            self.actions.drain(0..overflow);
        }
        let finish = timestamp.saturating_add(duration);
        if finish > self.total_duration {
            self.total_duration = finish;
        }
    }

    pub(crate) fn add_console_message(&mut self, message: String) {
        self.console_messages.push(message);
        if self.console_messages.len() > MAX_CONSOLE {
            let overflow = self.console_messages.len() - MAX_CONSOLE;
            self.console_messages.drain(0..overflow);
        }
    }

    pub(crate) fn set_screenshot(&mut self, path: PathBuf) {
        self.screenshot_path = Some(path.display().to_string());
    }

}

impl HistoryCell for BrowserSessionCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        let status = if self.completed {
            ToolCellStatus::Success
        } else {
            ToolCellStatus::Running
        };
        HistoryCellType::Tool { status }
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();

        let inner_border = "─".repeat(BOX_WIDTH);
        lines.push(Line::from(format!("┌{}┐", inner_border)));
        lines.push(Line::from(format!(
            "│{}│",
            build_header_content(
                self.url.as_deref().unwrap_or("(unknown)"),
                self.total_duration,
                self.completed,
            )
        )));
        lines.push(Line::from(format!("└{}┘", inner_border)));

        let title = self.title.clone().unwrap_or_else(|| "(pending)".to_string());
        lines.push(Line::from(format!(
            "  Active URL: {}",
            self.url.as_deref().unwrap_or("(unknown)")
        )));
        lines.push(Line::from(format!("  Title: {}", title)));
        lines.push(Line::from(""));

        lines.push(Line::from("  Actions"));
        if self.actions.is_empty() {
            lines.push(Line::from("    (no browser actions yet)"));
        } else {
            for action in &self.actions {
                lines.push(Line::from(format!(
                    "    {}  {}",
                    format_timestamp(action.timestamp),
                    action.description
                )));
            }
        }
        lines.push(Line::from(""));

        lines.push(Line::from("  Console"));
        if self.console_messages.is_empty() {
            lines.push(Line::from("    (no console messages)"));
        } else {
            for message in &self.console_messages {
                lines.push(Line::from(format!("    {}", message)));
            }
        }
        lines.push(Line::from(""));

        lines.push(Line::from("  Screenshot"));
        if let Some(path) = &self.screenshot_path {
            lines.push(Line::from(format!("    {}", path)));
        } else {
            lines.push(Line::from("    (no screenshot yet)"));
        }

        lines
    }
}

fn build_header_content(url: &str, total_duration: Duration, completed: bool) -> String {
    let inner_width = BOX_WIDTH;
    let mut base = format!(" Browser Session • {}", url);
    if base.len() > inner_width {
        base.truncate(inner_width);
    }

    let duration_label = if total_duration.is_zero() {
        if completed {
            "0s".to_string()
        } else {
            "Running".to_string()
        }
    } else {
        format_elapsed_compact(total_duration)
    };

    let required = duration_label.len();
    let mut max_head = inner_width.saturating_sub(required + 1);
    if max_head == 0 {
        max_head = inner_width;
    }
    if base.len() > max_head {
        base.truncate(max_head);
        if !base.ends_with(' ') {
            base.push(' ');
        }
    }

    let padding = inner_width
        .saturating_sub(base.len())
        .saturating_sub(required);
    if padding > 0 {
        base.push_str(&" ".repeat(padding));
    }
    if required > 0 {
        if !base.ends_with(' ') {
            base.push(' ');
        }
        base.push_str(&duration_label);
    }
    if base.len() < inner_width {
        base.push_str(&" ".repeat(inner_width - base.len()));
    }
    base
}

fn format_elapsed_compact(duration: Duration) -> String {
    let secs = duration.as_secs();
    let minutes = secs / 60;
    let seconds = secs % 60;
    if minutes > 0 {
        format!("{}m{:02}s", minutes, seconds)
    } else {
        format!("{:02}s", seconds)
    }
}

fn format_timestamp(duration: Duration) -> String {
    let secs = duration.as_secs();
    let minutes = secs / 60;
    let seconds = secs % 60;
    format!("{:02}:{:02}", minutes, seconds)
}
