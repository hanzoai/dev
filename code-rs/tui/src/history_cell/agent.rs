use super::{HistoryCell, HistoryCellType, ToolCellStatus};
use ratatui::text::Line;
use std::time::Duration;

const HEADER_WIDTH: usize = 76;

#[derive(Clone, Default)]
pub(crate) struct AgentRunCell {
    agent_name: String,
    status_label: String,
    task: Option<String>,
    duration: Option<Duration>,
    plan: Vec<String>,
    status_rows: Vec<(String, String)>,
    latest_result: Vec<String>,
    completed: bool,
    actions: Vec<String>,
}

impl AgentRunCell {
    pub(crate) fn new(agent_name: String) -> Self {
        Self {
            agent_name,
            status_label: "Running".to_string(),
            ..Default::default()
        }
    }

    pub(crate) fn set_task(&mut self, task: Option<String>) {
        self.task = task;
    }

    pub(crate) fn set_plan(&mut self, plan: Vec<String>) {
        self.plan = plan;
    }

    pub(crate) fn set_status_rows(&mut self, rows: Vec<(String, String)>) {
        self.status_rows = rows;
    }

    pub(crate) fn set_duration(&mut self, duration: Option<Duration>) {
        self.duration = duration;
    }

    pub(crate) fn set_latest_result(&mut self, lines: Vec<String>) {
        self.latest_result = lines;
    }

    pub(crate) fn mark_completed(&mut self) {
        self.completed = true;
    }

    pub(crate) fn mark_failed(&mut self) {
        self.completed = true;
        self.status_label = "Failed".to_string();
    }

    pub(crate) fn set_agent_name(&mut self, name: String) {
        self.agent_name = name;
    }

    pub(crate) fn set_status_label<S: Into<String>>(&mut self, label: S) {
        self.status_label = label.into();
    }

    pub(crate) fn record_action<S: Into<String>>(&mut self, text: S) {
        const MAX_ACTIONS: usize = 20;
        self.actions.push(text.into());
        if self.actions.len() > MAX_ACTIONS {
            let overflow = self.actions.len() - MAX_ACTIONS;
            self.actions.drain(0..overflow);
        }
    }
}

impl HistoryCell for AgentRunCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        let status = if self.completed {
            if self.status_label == "Failed" {
                ToolCellStatus::Failed
            } else {
                ToolCellStatus::Success
            }
        } else {
            ToolCellStatus::Running
        };
        HistoryCellType::Tool { status }
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();
        let header = build_header_content(&self.agent_name, &self.status_label);
        lines.push(Line::from(format!("┌{}┐", "─".repeat(HEADER_WIDTH))));
        lines.push(Line::from(format!("│{}│", header)));
        lines.push(Line::from(format!("└{}┘", "─".repeat(HEADER_WIDTH))));

        if let Some(task) = &self.task {
            lines.push(Line::from(format!("  Task: {}", task)));
        }

        if let Some(duration) = self.duration {
            lines.push(Line::from(format!("  Duration: {}", format_duration(duration))));
        }

        lines.push(Line::from(""));
        lines.push(Line::from("  Plan"));
        if self.plan.is_empty() {
            lines.push(Line::from("    (no plan provided)"));
        } else {
            for step in &self.plan {
                lines.push(Line::from(format!("    • {}", step)));
            }
        }

        lines.push(Line::from(""));
        lines.push(Line::from("  Status"));
        if self.status_rows.is_empty() {
            lines.push(Line::from("    (no status updates yet)"));
        } else {
            for (name, status) in &self.status_rows {
                lines.push(Line::from(format!("    {:<12} {}", name, status)));
            }
        }

        if !self.latest_result.is_empty() {
            lines.push(Line::from(""));
            lines.push(Line::from("  Latest result"));
            for line in &self.latest_result {
                lines.push(Line::from(format!("    {}", line)));
            }
        }

        lines.push(Line::from(""));
        lines.push(Line::from("  Actions"));
        if self.actions.is_empty() {
            lines.push(Line::from("    (no recorded actions yet)"));
        } else {
            for action in &self.actions {
                lines.push(Line::from(format!("    {}", action)));
            }
        }

        lines
    }
}

fn format_duration(duration: Duration) -> String {
    let secs = duration.as_secs();
    let minutes = secs / 60;
    let seconds = secs % 60;
    format!("{}m{:02}s", minutes, seconds)
}

fn build_header_content(agent_name: &str, status_label: &str) -> String {
    let mut base = format!(" Agent Run • {}", agent_name);
    if base.len() > HEADER_WIDTH {
        base.truncate(HEADER_WIDTH);
    }
    let status = format!(" {} ", status_label);
    if base.len() + status.len() > HEADER_WIDTH {
        if status.len() >= HEADER_WIDTH {
            return format!("{}", &status[status.len() - HEADER_WIDTH..]);
        }
        base.truncate(HEADER_WIDTH - status.len());
    }
    let padding = HEADER_WIDTH.saturating_sub(base.len() + status.len());
    format!("{}{}{}", base, " ".repeat(padding), status)
}
