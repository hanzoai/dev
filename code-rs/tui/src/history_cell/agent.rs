use super::card_style::{
    agent_card_style,
    divider_style,
    fill_card_background,
    header_text_style,
    primary_text_style,
    rows_to_lines,
    secondary_text_style,
    section_title_style,
    status_chip_style,
    truncate_to_width,
    truncate_with_ellipsis,
    CardRow,
    CardSegment,
    CardStyle,
    CARD_ACCENT_WIDTH,
};
use super::{HistoryCell, HistoryCellType, ToolCellStatus};
use crate::colors;
use ratatui::buffer::Buffer;
use ratatui::prelude::*;
use ratatui::text::{Line, Text};
use ratatui::widgets::{Paragraph, Wrap};
use std::time::Duration;
use unicode_width::UnicodeWidthStr;

const MAX_PLAN_LINES: usize = 4;
const MAX_STATUS_LINES: usize = 4;
const MAX_RESULT_LINES: usize = 3;
const MAX_ACTION_LINES: usize = 4;

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
    cell_key: Option<String>,
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
        const MAX_ACTIONS_BUFFER: usize = 20;
        let text = text.into();
        if self.actions.last().map_or(false, |last| last == &text) {
            return;
        }
        self.actions.push(text);
        if self.actions.len() > MAX_ACTIONS_BUFFER {
            let overflow = self.actions.len() - MAX_ACTIONS_BUFFER;
            self.actions.drain(0..overflow);
        }
    }

    pub(crate) fn set_cell_key(&mut self, key: Option<String>) {
        self.cell_key = key;
    }

    pub(crate) fn cell_key(&self) -> Option<&str> {
        self.cell_key.as_deref()
    }

    fn build_card_rows(&self, width: u16, style: &CardStyle) -> Vec<CardRow> {
        if width == 0 {
            return Vec::new();
        }

        let accent_width = CARD_ACCENT_WIDTH.min(width as usize);
        let body_width = width.saturating_sub(accent_width as u16) as usize;
        if body_width == 0 {
            return Vec::new();
        }

        let mut rows: Vec<CardRow> = Vec::new();
        let accent_style_header = Style::default()
            .fg(style.accent_fg)
            .bg(style.accent_bg)
            .add_modifier(Modifier::BOLD);
        let accent_style_body = Style::default().fg(style.accent_fg).bg(style.accent_bg);

        rows.push(self.header_row(body_width, style, accent_style_header.clone()));

        if let Some(meta) = self.meta_line() {
            let text = truncate_with_ellipsis(meta.as_str(), body_width);
            let segment = CardSegment::new(text, secondary_text_style(style));
            rows.push(CardRow::new(String::from(""), accent_style_body, vec![segment], None));
        }

        let mut has_prior_section = !rows.is_empty();

        if let Some(plan_rows) = self.plan_section_lines() {
            self.push_section(
                &mut rows,
                style,
                &accent_style_body,
                &mut has_prior_section,
                "Plan",
                plan_rows,
                body_width,
            );
        }

        if let Some(status_rows) = self.status_section_lines() {
            self.push_section(
                &mut rows,
                style,
                &accent_style_body,
                &mut has_prior_section,
                "Status",
                status_rows,
                body_width,
            );
        }

        if let Some(latest_rows) = self.latest_section_lines() {
            self.push_section(
                &mut rows,
                style,
                &accent_style_body,
                &mut has_prior_section,
                "Latest",
                latest_rows,
                body_width,
            );
        }

        if let Some(action_rows) = self.action_section_lines() {
            self.push_section(
                &mut rows,
                style,
                &accent_style_body,
                &mut has_prior_section,
                "Actions",
                action_rows,
                body_width,
            );
        }

        rows
    }

    fn header_row(
        &self,
        body_width: usize,
        style: &CardStyle,
        accent_style: Style,
    ) -> CardRow {
        let mut title = format!("Agent Run • {}", self.agent_name);
        if let Some(duration) = self.duration {
            title.push_str(" · ");
            title.push_str(format_duration(duration).as_str());
        }

        let pill_text = format!(" {} ", self.status_label);
        let pill_width = pill_text_width(&pill_text).min(body_width);
        let left_width = body_width.saturating_sub(pill_width);

        let header_style = header_text_style(style);
        let left_segment = CardSegment::with_fixed_bg(
            truncate_with_ellipsis(title.as_str(), left_width),
            header_style,
        );

        let mut segments = Vec::new();
        segments.push(left_segment);

        if pill_width > 0 {
            let chip_color = self.status_chip_color();
            let chip_style = status_chip_style(chip_color, style);
            let pill_segment = CardSegment::with_fixed_bg(
                truncate_to_width(pill_text.as_str(), pill_width),
                chip_style,
            );
            segments.push(pill_segment);
        }

        CardRow::new(String::from("⚙"), accent_style, segments, Some(style.header_bg))
    }

    fn meta_line(&self) -> Option<String> {
        self.task
            .as_ref()
            .filter(|task| !task.trim().is_empty())
            .map(|task| format!("Task: {}", task))
    }

    fn plan_section_lines(&self) -> Option<Vec<String>> {
        if self.plan.is_empty() {
            return Some(vec!["(no plan provided)".to_string()]);
        }
        let mut lines: Vec<String> = self
            .plan
            .iter()
            .take(MAX_PLAN_LINES)
            .map(|step| format!("• {}", step))
            .collect();
        if self.plan.len() > MAX_PLAN_LINES {
            lines.push(format!("(+{} more)", self.plan.len() - MAX_PLAN_LINES));
        }
        Some(lines)
    }

    fn status_section_lines(&self) -> Option<Vec<String>> {
        if self.status_rows.is_empty() {
            return Some(vec!["(no status updates yet)".to_string()]);
        }
        let mut lines: Vec<String> = self
            .status_rows
            .iter()
            .take(MAX_STATUS_LINES)
            .map(|(name, status)| format!("• {} — {}", name, status))
            .collect();
        if self.status_rows.len() > MAX_STATUS_LINES {
            lines.push(format!("(+{} more)", self.status_rows.len() - MAX_STATUS_LINES));
        }
        Some(lines)
    }

    fn latest_section_lines(&self) -> Option<Vec<String>> {
        if self.latest_result.is_empty() {
            return None;
        }
        let mut lines: Vec<String> = self
            .latest_result
            .iter()
            .take(MAX_RESULT_LINES)
            .map(|line| format!("• {}", line))
            .collect();
        if self.latest_result.len() > MAX_RESULT_LINES {
            lines.push(format!("(+{} more)", self.latest_result.len() - MAX_RESULT_LINES));
        }
        Some(lines)
    }

    fn action_section_lines(&self) -> Option<Vec<String>> {
        if self.actions.is_empty() {
            return None;
        }
        let mut lines: Vec<String> = Vec::new();
        for action in self.actions.iter().take(MAX_ACTION_LINES) {
            let decorated = format!("• {}", action);
            lines.push(decorated);
        }
        if self.actions.len() > MAX_ACTION_LINES {
            lines.push(format!("(+{} more)", self.actions.len() - MAX_ACTION_LINES));
        }
        Some(lines)
    }

    fn push_section(
        &self,
        rows: &mut Vec<CardRow>,
        style: &CardStyle,
        accent_style: &Style,
        has_prior_section: &mut bool,
        title: &str,
        lines: Vec<String>,
        body_width: usize,
    ) {
        if lines.is_empty() {
            return;
        }

        if *has_prior_section {
            rows.push(self.divider_row(accent_style.clone(), style, body_width));
        }
        *has_prior_section = true;

        let title_segment = CardSegment::new(
            truncate_with_ellipsis(&title.to_uppercase(), body_width),
            section_title_style(style),
        );
        rows.push(CardRow::new(String::from(""), accent_style.clone(), vec![title_segment], None));

        for line in lines {
            let trimmed = truncate_with_ellipsis(line.as_str(), body_width);
            let is_placeholder = trimmed.trim().starts_with('(');
            let text_style = if is_placeholder {
                secondary_text_style(style)
            } else {
                primary_text_style(style)
            };
            rows.push(CardRow::new(
                String::from(""),
                accent_style.clone(),
                vec![CardSegment::new(trimmed, text_style)],
                None,
            ));
        }
    }

    fn divider_row(&self, accent_style: Style, style: &CardStyle, body_width: usize) -> CardRow {
        let rule = "─".repeat(body_width);
        let segment = CardSegment::new(rule, divider_style(style));
        CardRow::new(String::from(""), accent_style, vec![segment], None)
    }

    fn status_chip_color(&self) -> Color {
        let lowered = self.status_label.to_ascii_lowercase();
        if lowered.contains("fail") || lowered.contains("error") {
            colors::error()
        } else if lowered.contains("cancel") {
            colors::warning()
        } else if lowered.contains("complete") || lowered.contains("success") {
            colors::success()
        } else if self.completed {
            colors::success()
        } else {
            colors::info()
        }
    }

    fn build_plain_summary(&self) -> Vec<String> {
        let mut lines = Vec::new();
        lines.push(format!("Agent Run: {} [{}]", self.agent_name, self.status_label));
        if let Some(task) = &self.task {
            if !task.trim().is_empty() {
                lines.push(format!("Task: {}", task));
            }
        }
        if let Some(duration) = self.duration {
            lines.push(format!("Duration: {}", format_duration(duration)));
        }
        if !self.plan.is_empty() {
            lines.push(format!("Plan: {}", self.plan.join(" | ")));
        }
        if !self.status_rows.is_empty() {
            let summary = self
                .status_rows
                .iter()
                .map(|(name, status)| format!("{}={}", name, status))
                .collect::<Vec<_>>()
                .join(" | ");
            lines.push(format!("Status: {}", summary));
        }
        if !self.latest_result.is_empty() {
            lines.push(format!("Latest: {}", self.latest_result.join(" | ")));
        }
        if !self.actions.is_empty() {
            let preview = self
                .actions
                .iter()
                .take(MAX_ACTION_LINES)
                .cloned()
                .collect::<Vec<_>>()
                .join(" | ");
            lines.push(format!("Actions: {}", preview));
        }
        lines
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
        self
            .build_plain_summary()
            .into_iter()
            .map(Line::from)
            .collect()
    }

    fn desired_height(&self, width: u16) -> u16 {
        let style = agent_card_style();
        let rows = self.build_card_rows(width, &style);
        rows.len().max(1) as u16
    }

    fn has_custom_render(&self) -> bool {
        true
    }

    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        let style = agent_card_style();
        fill_card_background(buf, area, &style);
        let rows = self.build_card_rows(area.width, &style);
        let lines = rows_to_lines(&rows, &style, area.width);
        let text = Text::from(lines);

        Paragraph::new(text)
            .wrap(Wrap { trim: false })
            .scroll((skip_rows, 0))
            .render(area, buf);
    }
}

fn format_duration(duration: Duration) -> String {
    let secs = duration.as_secs();
    let minutes = secs / 60;
    let seconds = secs % 60;
    format!("{}m{:02}s", minutes, seconds)
}

impl crate::chatwidget::tool_cards::ToolCardCell for AgentRunCell {
    fn tool_card_key(&self) -> Option<&str> {
        self.cell_key()
    }

    fn set_tool_card_key(&mut self, key: Option<String>) {
        self.set_cell_key(key);
    }
}

fn pill_text_width(text: &str) -> usize {
    UnicodeWidthStr::width(text)
}
