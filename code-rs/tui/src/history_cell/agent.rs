use super::card_style::{
    agent_card_style,
    fill_card_background,
    primary_text_style,
    rows_to_lines,
    secondary_text_style,
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
use ratatui::layout::Rect;
use ratatui::prelude::{Color, Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Paragraph, Widget, Wrap};
use std::time::Duration;
use code_protocol::num_format::format_with_separators;

const BORDER_TOP: &str = "╭─";
const BORDER_BODY: &str = "│ ";
const BORDER_BOTTOM: &str = "╰─";
use unicode_width::UnicodeWidthChar;

const MAX_PLAN_LINES: usize = 4;
const MAX_SUMMARY_LINES: usize = 4;
const MAX_AGENT_DETAIL_LINES: usize = 3;
const MAX_AGENT_DISPLAY: usize = 8;

#[derive(Clone, Default)]
pub(crate) struct AgentRunCell {
    agent_name: String,
    status_label: String,
    task: Option<String>,
    duration: Option<Duration>,
    plan: Vec<String>,
    agents: Vec<AgentStatusPreview>,
    summary_lines: Vec<String>,
    completed: bool,
    actions: Vec<String>,
    cell_key: Option<String>,
    batch_label: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct AgentStatusPreview {
    pub name: String,
    pub status: String,
    pub model: Option<String>,
    pub details: Vec<AgentDetail>,
    pub status_kind: AgentStatusKind,
    pub step_progress: Option<StepProgress>,
    pub elapsed: Option<Duration>,
    pub token_count: Option<u64>,
    pub last_update: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct StepProgress {
    pub completed: u32,
    pub total: u32,
}

#[derive(Clone, Copy, Debug, Default)]
pub(crate) enum AgentStatusKind {
    #[default]
    Running,
    Completed,
    Failed,
    Cancelled,
    Pending,
}

impl AgentStatusKind {
    fn glyph(self) -> &'static str {
        match self {
            AgentStatusKind::Running => "▶",
            AgentStatusKind::Completed => "✓",
            AgentStatusKind::Failed => "!",
            AgentStatusKind::Cancelled => "▮",
            AgentStatusKind::Pending => "…",
        }
    }

    fn label(self) -> &'static str {
        match self {
            AgentStatusKind::Running => "Running",
            AgentStatusKind::Completed => "Completed",
            AgentStatusKind::Failed => "Failed",
            AgentStatusKind::Cancelled => "Cancelled",
            AgentStatusKind::Pending => "Pending",
        }
    }

    fn color(self) -> Color {
        match self {
            AgentStatusKind::Running => colors::info(),
            AgentStatusKind::Completed => colors::success(),
            AgentStatusKind::Failed => colors::error(),
            AgentStatusKind::Cancelled => colors::warning(),
            AgentStatusKind::Pending => colors::mix_toward(colors::info(), colors::text(), 0.6),
        }
    }
}

#[derive(Default, Clone, Copy)]
struct AgentCountSummary {
    total: usize,
    running: usize,
    completed: usize,
    failed: usize,
    cancelled: usize,
    pending: usize,
}

impl AgentCountSummary {
    fn observe(&mut self, kind: AgentStatusKind) {
        self.total += 1;
        match kind {
            AgentStatusKind::Running => self.running += 1,
            AgentStatusKind::Completed => self.completed += 1,
            AgentStatusKind::Failed => self.failed += 1,
            AgentStatusKind::Cancelled => self.cancelled += 1,
            AgentStatusKind::Pending => self.pending += 1,
        }
    }

    fn glyph_counts(&self) -> Vec<(AgentStatusKind, usize)> {
        let mut items = Vec::new();
        if self.completed > 0 {
            items.push((AgentStatusKind::Completed, self.completed));
        }
        if self.running > 0 {
            items.push((AgentStatusKind::Running, self.running));
        }
        if self.failed > 0 {
            items.push((AgentStatusKind::Failed, self.failed));
        }
        if self.cancelled > 0 {
            items.push((AgentStatusKind::Cancelled, self.cancelled));
        }
        if self.pending > 0 {
            items.push((AgentStatusKind::Pending, self.pending));
        }
        items
    }
}

#[derive(Clone, Debug)]
pub(crate) enum AgentDetail {
    Progress(String),
    Result(String),
    Error(String),
    Info(String),
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
        self.task = task.and_then(|value| {
            let trimmed = value.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            }
        });
    }

    pub(crate) fn set_plan(&mut self, plan: Vec<String>) {
        self.plan = plan
            .into_iter()
            .map(|line| line.trim().to_string())
            .filter(|line| !line.is_empty())
            .collect();
    }

    pub(crate) fn set_agent_overview(&mut self, agents: Vec<AgentStatusPreview>) {
        self.agents = agents;
    }

    pub(crate) fn set_duration(&mut self, duration: Option<Duration>) {
        self.duration = duration;
    }

    pub(crate) fn set_latest_result(&mut self, lines: Vec<String>) {
        let mut cleaned: Vec<String> = lines
            .into_iter()
            .map(|line| line.trim().to_string())
            .filter(|line| !line.is_empty())
            .collect();
        if cleaned.len() > MAX_SUMMARY_LINES {
            let overflow = cleaned.len() - MAX_SUMMARY_LINES;
            cleaned.drain(0..overflow);
            if let Some(first) = cleaned.first_mut() {
                if !first.starts_with('…') {
                    first.insert(0, ' ');
                    first.insert(0, '…');
                }
            }
        }
        self.summary_lines = cleaned;
    }

    pub(crate) fn mark_completed(&mut self) {
        self.completed = true;
        if self.status_label.trim().is_empty() {
            self.status_label = "Completed".to_string();
        }
    }

    pub(crate) fn mark_failed(&mut self) {
        self.completed = true;
        self.status_label = "Failed".to_string();
    }

    pub(crate) fn set_agent_name(&mut self, name: String) {
        if !name.trim().is_empty() {
            self.agent_name = name;
        }
    }

    pub(crate) fn set_status_label<S: Into<String>>(&mut self, label: S) {
        let label = label.into();
        if !label.trim().is_empty() {
            self.status_label = label;
        }
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

    fn accent_style(style: &CardStyle) -> Style {
        primary_text_style(style)
    }

    fn top_border_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let text = truncate_with_ellipsis(self.header_info_text().as_str(), body_width);
        let segment = CardSegment::new(text, primary_text_style(style));
        CardRow::new(BORDER_TOP.to_string(), Self::accent_style(style), vec![segment], None)
    }

    fn blank_border_row(&self, _body_width: usize, style: &CardStyle) -> CardRow {
        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            vec![CardSegment::new(String::new(), Style::default())],
            None,
        )
    }

    fn body_text_row(
        &self,
        text: impl Into<String>,
        body_width: usize,
        style: &CardStyle,
        text_style: Style,
    ) -> CardRow {
        let text = text.into();
        let segment = CardSegment::new(truncate_with_ellipsis(text.as_str(), body_width), text_style);
        CardRow::new(BORDER_BODY.to_string(), Self::accent_style(style), vec![segment], None)
    }

    fn bottom_border_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let text = truncate_with_ellipsis("[Ctrl+B] Expand · [Esc] Stop", body_width);
        let segment = CardSegment::new(text, secondary_text_style(style));
        CardRow::new(BORDER_BOTTOM.to_string(), Self::accent_style(style), vec![segment], None)
    }

    fn wrap_body_row(&self, mut row: CardRow, style: &CardStyle) -> CardRow {
        row.accent = BORDER_BODY.to_string();
        row.accent_style = Self::accent_style(style);
        row
    }

    pub(crate) fn set_batch_label(&mut self, batch: Option<String>) {
        self.batch_label = batch.and_then(|value| {
            let trimmed = value.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            }
        });
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
        rows.push(self.top_border_row(body_width, style));
        rows.push(self.blank_border_row(body_width, style));

        let mut content_rows: Vec<CardRow> = Vec::new();

        if let Some(task_row) = self.task_row(body_width, style) {
            content_rows.push(self.wrap_body_row(task_row, style));
        }

        let plan_rows = self.plan_rows(body_width, style);
        if !plan_rows.is_empty() {
            if !content_rows.is_empty() {
                content_rows.push(self.blank_border_row(body_width, style));
            }
            content_rows.extend(plan_rows.into_iter().map(|row| self.wrap_body_row(row, style)));
        }

        let agent_rows = self.agent_rows(body_width, style);
        if !agent_rows.is_empty() {
            if !content_rows.is_empty() {
                content_rows.push(self.blank_border_row(body_width, style));
            }
            content_rows.extend(agent_rows.into_iter().map(|row| self.wrap_body_row(row, style)));
        }

        let summary_rows = self.summary_rows(body_width, style);
        if !summary_rows.is_empty() {
            if !content_rows.is_empty() {
                content_rows.push(self.blank_border_row(body_width, style));
            }
            content_rows.extend(summary_rows.into_iter().map(|row| self.wrap_body_row(row, style)));
        }

        if let Some(activity_row) = self.last_activity_row(body_width, style) {
            if !content_rows.is_empty() {
                content_rows.push(self.blank_border_row(body_width, style));
            }
            content_rows.push(self.wrap_body_row(activity_row, style));
        }

        if content_rows.is_empty() {
            content_rows.push(self.body_text_row(
                "No agent updates yet",
                body_width,
                style,
                secondary_text_style(style),
            ));
        }

        rows.extend(content_rows);

        if !rows
            .last()
            .map(|row| row.segments.iter().all(|segment| segment.text.is_empty()))
            .unwrap_or(false)
        {
            rows.push(self.blank_border_row(body_width, style));
        }

        rows.push(self.bottom_border_row(body_width, style));

        rows
    }

    fn task_row(&self, body_width: usize, style: &CardStyle) -> Option<CardRow> {
        let task = self.task.as_ref()?;
        if task.trim().is_empty() {
            return None;
        }
        Some(self.label_row(
            "Task",
            task.as_str(),
            body_width,
            style,
            primary_text_style(style),
        ))
    }

    fn plan_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        if self.plan.is_empty() {
            return Vec::new();
        }
        let mut rows = Vec::new();
        rows.push(self.section_heading("Plan", body_width, style));

        for (index, step) in self.plan.iter().take(MAX_PLAN_LINES).enumerate() {
            let text = format!("{:>2}. {}", index + 1, step);
            rows.push(CardRow::new(
                String::new(),
                Style::default(),
                vec![CardSegment::new(
                    truncate_with_ellipsis(text.as_str(), body_width),
                    primary_text_style(style),
                )],
                None,
            ));
        }

        if self.plan.len() > MAX_PLAN_LINES {
            let more = format!("(+{} more)", self.plan.len() - MAX_PLAN_LINES);
            rows.push(CardRow::new(
                String::new(),
                Style::default(),
                vec![CardSegment::new(
                    truncate_with_ellipsis(more.as_str(), body_width),
                    secondary_text_style(style),
                )],
                None,
            ));
        }

        rows
    }

    fn agent_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        if self.agents.is_empty() {
            return Vec::new();
        }

        const MIN_TABLE_WIDTH: usize = 60;
        if body_width < MIN_TABLE_WIDTH {
            return self.agent_rows_compact(body_width, style);
        }

        self.agent_table_rows(body_width, style)
    }

    fn agent_rows_compact(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        let mut rows = Vec::new();
        for (idx, agent) in self.agents.iter().enumerate() {
            if idx >= MAX_AGENT_DISPLAY {
                let remaining = self.agents.len() - MAX_AGENT_DISPLAY;
                let text = format!("(+{} more agents)", remaining);
                rows.push(CardRow::new(
                    String::new(),
                    Style::default(),
                    vec![CardSegment::new(
                        truncate_with_ellipsis(text.as_str(), body_width),
                        secondary_text_style(style),
                    )],
                    None,
                ));
                break;
            }
            rows.extend(self.compact_agent_entry_rows(agent, body_width, style));
        }
        rows
    }

    fn compact_agent_entry_rows(
        &self,
        agent: &AgentStatusPreview,
        body_width: usize,
        style: &CardStyle,
    ) -> Vec<CardRow> {
        let mut rows = Vec::new();
        if body_width == 0 {
            return rows;
        }

        let pill_text = format!(" {} ", agent.status_kind.label());
        let mut pill_width = pill_text_width(pill_text.as_str()).min(body_width);
        if body_width > 12 && pill_width < 8 {
            pill_width = 8;
        }
        if pill_width >= body_width {
            pill_width = body_width.saturating_sub(2);
        }
        let mut remaining = body_width.saturating_sub(pill_width);
        let mut bullet_width = 2.min(remaining);
        let mut name_width = remaining.saturating_sub(bullet_width);
        if name_width == 0 && pill_width > 6 {
            pill_width -= 1;
            remaining = body_width.saturating_sub(pill_width);
            bullet_width = 2.min(remaining);
            name_width = remaining.saturating_sub(bullet_width);
        }
        if name_width == 0 && bullet_width > 1 {
            bullet_width -= 1;
            name_width = remaining.saturating_sub(bullet_width);
        }

        let mut segments = Vec::new();

        if bullet_width > 0 {
            let bullet_style = Style::default()
                .fg(agent.status_kind.color())
                .add_modifier(Modifier::BOLD);
            let bullet_text = if bullet_width > 1 {
                format!("{} ", agent.status_kind.glyph())
            } else {
                agent.status_kind.glyph().to_string()
            };
            segments.push(CardSegment::new(
                truncate_to_width(bullet_text.as_str(), bullet_width),
                bullet_style,
            ));
        }

        if name_width > 0 {
            let mut name_text = agent.name.clone();
            if let Some(model) = agent.model.as_ref().filter(|m| !m.trim().is_empty()) {
                name_text.push_str("  · ");
                name_text.push_str(model);
            }
            segments.push(CardSegment::new(
                truncate_with_ellipsis(name_text.as_str(), name_width),
                primary_text_style(style),
            ));
        }

        if pill_width > 0 {
            let chip_style = status_chip_style(agent.status_kind.color(), style);
            segments.push(CardSegment::with_fixed_bg(
                truncate_to_width(pill_text.as_str(), pill_width),
                chip_style,
            ));
        }

        rows.push(CardRow::new(String::new(), Style::default(), segments, None));

        let mut details: Vec<AgentDetail> = agent
            .details
            .iter()
            .rev()
            .take(MAX_AGENT_DETAIL_LINES)
            .cloned()
            .collect();
        details.reverse();
        let hidden = agent.details.len().saturating_sub(details.len());

        for detail in details {
            match detail {
                AgentDetail::Progress(text) => {
                    let line = format!("  ↳ {text}");
                    rows.push(CardRow::new(
                        String::new(),
                        Style::default(),
                        vec![CardSegment::new(
                            truncate_with_ellipsis(line.as_str(), body_width),
                            Style::default().fg(colors::info()),
                        )],
                        None,
                    ));
                }
                AgentDetail::Result(text) => {
                    let line = format!("  ✓ {text}");
                    rows.push(CardRow::new(
                        String::new(),
                        Style::default(),
                        vec![CardSegment::new(
                            truncate_with_ellipsis(line.as_str(), body_width),
                            Style::default()
                                .fg(colors::success())
                                .add_modifier(Modifier::BOLD),
                        )],
                        None,
                    ));
                }
                AgentDetail::Error(text) => {
                    let line = format!("  ! {text}");
                    rows.push(CardRow::new(
                        String::new(),
                        Style::default(),
                        vec![CardSegment::new(
                            truncate_with_ellipsis(line.as_str(), body_width),
                            Style::default()
                                .fg(colors::error())
                                .add_modifier(Modifier::BOLD),
                        )],
                        None,
                    ));
                }
                AgentDetail::Info(text) => {
                    let line = format!("  · {text}");
                    rows.push(CardRow::new(
                        String::new(),
                        Style::default(),
                        vec![CardSegment::new(
                            truncate_with_ellipsis(line.as_str(), body_width),
                            secondary_text_style(style),
                        )],
                        None,
                    ));
                }
            }
        }

        if hidden > 0 {
            let text = format!("  (+{} earlier)", hidden);
            rows.push(CardRow::new(
                String::new(),
                Style::default(),
                vec![CardSegment::new(
                    truncate_with_ellipsis(text.as_str(), body_width),
                    secondary_text_style(style),
                )],
                None,
            ));
        }

        rows
    }

    fn agent_table_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        let mut entries: Vec<AgentTableEntry> = Vec::new();
        for (idx, agent) in self.agents.iter().enumerate() {
            if idx >= MAX_AGENT_DISPLAY {
                break;
            }
            entries.push(AgentTableEntry::from(idx, agent));
        }

        if entries.is_empty() {
            return Vec::new();
        }

        let widths = AgentTableWidths::compute(body_width, &entries);

        let mut rows = Vec::new();
        rows.push(CardRow::new(
            String::new(),
            Style::default(),
            widths.render_header(style),
            None,
        ));

        for entry in &entries {
            rows.push(CardRow::new(
                String::new(),
                Style::default(),
                widths.render_row(entry, style),
                None,
            ));
        }

        if self.agents.len() > MAX_AGENT_DISPLAY {
            let remaining = self.agents.len() - MAX_AGENT_DISPLAY;
            let text = format!("(+{} more agents)", remaining);
            rows.push(CardRow::new(
                String::new(),
                Style::default(),
                vec![CardSegment::new(
                    truncate_with_ellipsis(text.as_str(), body_width),
                    secondary_text_style(style),
                )],
                None,
            ));
        }

        rows
    }

    fn agent_counts(&self) -> AgentCountSummary {
        let mut summary = AgentCountSummary::default();
        for agent in &self.agents {
            summary.observe(agent.status_kind);
        }
        summary
    }

    fn header_info_text(&self) -> String {
        let counts = self.agent_counts();
        let mut parts: Vec<String> = Vec::new();

        if let Some(batch) = self.batch_label.as_deref().and_then(format_batch_label) {
            parts.push(format!("BATCH {}", batch));
        } else {
            parts.push("AGENTS".to_string());
        }

        if let Some(task) = self.task.as_ref() {
            if !task.trim().is_empty() {
                parts.push(format!("“{}”", task.trim()));
            }
        }

        parts.push(format!("{} agents", counts.total));

        for (kind, count) in counts.glyph_counts() {
            parts.push(format!("{}{}", kind.glyph(), count));
        }

        let status_upper = self.status_label.to_ascii_uppercase();
        if let Some(duration) = self.duration {
            parts.push(format!("{} {}", status_upper, format_duration_compact(duration)));
        } else {
            parts.push(status_upper);
        }

        parts.join(" · ")
    }

    fn summary_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        if self.summary_lines.is_empty() {
            return Vec::new();
        }
        let mut rows = Vec::new();
        rows.push(self.section_heading("Summary", body_width, style));

        for line in &self.summary_lines {
            rows.push(CardRow::new(
                String::new(),
                Style::default(),
                vec![CardSegment::new(
                    truncate_with_ellipsis(line.as_str(), body_width),
                    primary_text_style(style),
                )],
                None,
            ));
        }

        rows
    }

    fn last_activity_row(&self, body_width: usize, style: &CardStyle) -> Option<CardRow> {
        if self.actions.is_empty() {
            return None;
        }
        let has_details = self
            .agents
            .iter()
            .any(|agent| agent.details.iter().any(|detail| match detail {
                AgentDetail::Progress(text)
                | AgentDetail::Result(text)
                | AgentDetail::Error(text)
                | AgentDetail::Info(text) => !text.trim().is_empty(),
            }));
        if has_details || !self.summary_lines.is_empty() {
            return None;
        }
        let latest = self.actions.last()?;
        Some(self.label_row(
            "Latest",
            latest.as_str(),
            body_width,
            style,
            secondary_text_style(style),
        ))
    }

    fn label_row(
        &self,
        label: &str,
        content: &str,
        body_width: usize,
        style: &CardStyle,
        content_style: Style,
    ) -> CardRow {
        if body_width == 0 {
            return CardRow::new(String::new(), Style::default(), Vec::new(), None);
        }
        let label_text = format!(" {} ", label.to_uppercase());
        let mut label_width = string_width(label_text.as_str()).min(body_width);
        let mut content_width = body_width.saturating_sub(label_width);
        if content_width == 0 && label_width > 4 {
            label_width -= 1;
            content_width = 1;
        }

        let mut segments = Vec::new();
        if label_width > 0 {
            let label_style = Style::default()
                .fg(style.accent_fg)
                .add_modifier(Modifier::BOLD);
            segments.push(CardSegment::new(
                truncate_to_width(label_text.as_str(), label_width),
                label_style,
            ));
        }
        if content_width > 0 {
            segments.push(CardSegment::new(
                truncate_with_ellipsis(content, content_width),
                content_style,
            ));
        }

        CardRow::new(String::new(), Style::default(), segments, None)
    }

    fn section_heading(&self, title: &str, body_width: usize, style: &CardStyle) -> CardRow {
        let heading_text = format!(" {} ", title.to_uppercase());
        let heading_style = Style::default()
            .fg(colors::mix_toward(style.accent_fg, colors::info(), 0.2))
            .add_modifier(Modifier::BOLD);
        let segment = CardSegment::new(
            truncate_to_width(heading_text.as_str(), body_width),
            heading_style,
        );
        CardRow::new(String::new(), Style::default(), vec![segment], None)
    }

    fn build_plain_summary(&self) -> Vec<String> {
        let mut lines = Vec::new();
        lines.push(format!("Agent Run: {} [{}]", self.agent_name, self.status_label));
        if let Some(task) = &self.task {
            if !task.trim().is_empty() {
                lines.push(format!("Task: {}", task.trim()));
            }
        }
        if let Some(duration) = self.duration {
            lines.push(format!("Duration: {}", format_duration_compact(duration)));
        }

        let counts = self.agent_counts();
        let mut count_parts = Vec::new();
        for (kind, count) in counts.glyph_counts() {
            count_parts.push(format!("{}{}", kind.glyph(), count));
        }
        if !count_parts.is_empty() {
            lines.push(format!("Agents: {} ({})", counts.total, count_parts.join(", ")));
        } else {
            lines.push(format!("Agents: {}", counts.total));
        }

        for (idx, agent) in self.agents.iter().enumerate().take(MAX_AGENT_DISPLAY) {
            let last = agent
                .last_update
                .clone()
                .or_else(|| agent.details.last().map(detail_display_text))
                .unwrap_or_else(|| agent.status.clone());
            lines.push(format!(
                "#{:02} {} {} — {}",
                idx + 1,
                agent.status_kind.glyph(),
                agent.name,
                last
            ));
        }

        if self.agents.len() > MAX_AGENT_DISPLAY {
            lines.push(format!(
                "(+{} more agents)",
                self.agents.len() - MAX_AGENT_DISPLAY
            ));
        }
        if !self.summary_lines.is_empty() {
            lines.push(format!("Summary: {}", self.summary_lines.join(" | ")));
        } else if let Some(last) = self.actions.last() {
            lines.push(format!("Last activity: {}", last));
        }
        lines
    }
}

struct AgentTableEntry {
    index: usize,
    status_kind: AgentStatusKind,
    agent_name: String,
    model: String,
    steps: String,
    elapsed: String,
    tokens: String,
    last: String,
}

impl AgentTableEntry {
    fn from(index: usize, preview: &AgentStatusPreview) -> Self {
        let steps = preview
            .step_progress
            .as_ref()
            .map(|progress| format!("{}/{}", progress.completed, progress.total))
            .unwrap_or_else(|| "—".to_string());

        let elapsed = preview
            .elapsed
            .map(format_duration_compact)
            .unwrap_or_else(|| "—".to_string());

        let tokens = preview
            .token_count
            .map(format_token_count)
            .unwrap_or_else(|| "—".to_string());

        let last = preview
            .last_update
            .clone()
            .or_else(|| preview.details.last().map(detail_display_text))
            .unwrap_or_else(|| preview.status.clone());

        Self {
            index: index + 1,
            status_kind: preview.status_kind,
            agent_name: preview.name.clone(),
            model: preview
                .model
                .as_ref()
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .unwrap_or_else(|| "—".to_string()),
            steps,
            elapsed,
            tokens,
            last,
        }
    }
}

#[derive(Clone, Copy)]
enum ColumnAlign {
    Left,
    Right,
}

struct AgentTableWidths {
    widths: [usize; AgentTableWidths::COLUMN_COUNT],
}

impl AgentTableWidths {
    const COLUMN_COUNT: usize = 8;
    const MIN_WIDTHS: [usize; AgentTableWidths::COLUMN_COUNT] = [2, 2, 12, 8, 5, 4, 3, 12];

    fn headers() -> [&'static str; AgentTableWidths::COLUMN_COUNT] {
        ["#", "STATE", "AGENT", "MODEL", "STEPS", "ELAP", "TOK", "LAST"]
    }

    fn compute(body_width: usize, entries: &[AgentTableEntry]) -> Self {
        let mut widths = Self::MIN_WIDTHS;
        let headers = Self::headers();

        for (idx, header) in headers.iter().enumerate() {
            widths[idx] = widths[idx].max(string_width(header));
        }

        for entry in entries {
            widths[0] = widths[0].max(string_width(entry.index.to_string().as_str()));
            widths[1] = widths[1].max(string_width(entry.status_kind.glyph()));
            widths[2] = widths[2].max(string_width(entry.agent_name.as_str()));
            widths[3] = widths[3].max(string_width(entry.model.as_str()));
            widths[4] = widths[4].max(string_width(entry.steps.as_str()));
            widths[5] = widths[5].max(string_width(entry.elapsed.as_str()));
            widths[6] = widths[6].max(string_width(entry.tokens.as_str()));
            widths[7] = widths[7].max(string_width(entry.last.as_str()));
        }

        let spaces = Self::COLUMN_COUNT.saturating_sub(1);
        let mut total = widths.iter().sum::<usize>() + spaces;
        let mut columns = widths;

        while total > body_width {
            let mut reduced = false;
            for &idx in &[7usize, 2usize, 3usize, 4usize] {
                if columns[idx] > Self::MIN_WIDTHS[idx] {
                    columns[idx] -= 1;
                    total -= 1;
                    reduced = true;
                    if total <= body_width {
                        break;
                    }
                }
            }
            if !reduced {
                break;
            }
        }

        if total < body_width {
            let extra = body_width - total;
            columns[2] += extra;
        }

        Self { widths: columns }
    }

    fn render_header(&self, style: &CardStyle) -> Vec<CardSegment> {
        let mut segments = Vec::new();
        let header_style = Style::default()
            .fg(style.accent_fg)
            .add_modifier(Modifier::BOLD);
        for (idx, label) in Self::headers().iter().enumerate() {
            let text = format_cell(label, self.widths[idx], ColumnAlign::Left);
            segments.push(CardSegment::new(text, header_style));
            if idx + 1 != Self::COLUMN_COUNT {
                segments.push(CardSegment::new(" ".to_string(), Style::default()));
            }
        }
        segments
    }

    fn render_row(&self, entry: &AgentTableEntry, style: &CardStyle) -> Vec<CardSegment> {
        let mut segments = Vec::new();
        for idx in 0..Self::COLUMN_COUNT {
            let (text, column_style, align) = match idx {
                0 => (
                    entry.index.to_string(),
                    secondary_text_style(style),
                    ColumnAlign::Right,
                ),
                1 => (
                    entry.status_kind.glyph().to_string(),
                    Style::default()
                        .fg(entry.status_kind.color())
                        .add_modifier(Modifier::BOLD),
                    ColumnAlign::Left,
                ),
                2 => (
                    entry.agent_name.clone(),
                    primary_text_style(style),
                    ColumnAlign::Left,
                ),
                3 => (
                    entry.model.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Left,
                ),
                4 => (
                    entry.steps.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Right,
                ),
                5 => (
                    entry.elapsed.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Right,
                ),
                6 => (
                    entry.tokens.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Right,
                ),
                7 => (
                    entry.last.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Left,
                ),
                _ => continue,
            };
            let text = format_cell(&text, self.widths[idx], align);
            segments.push(CardSegment::new(text, column_style));
            if idx + 1 != Self::COLUMN_COUNT {
                segments.push(CardSegment::new(" ".to_string(), Style::default()));
            }
        }
        segments
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

fn string_width(text: &str) -> usize {
    text
        .chars()
        .map(|ch| UnicodeWidthChar::width(ch).unwrap_or(0))
        .sum()
}

fn format_cell(text: &str, width: usize, align: ColumnAlign) -> String {
    if width == 0 {
        return String::new();
    }
    let shortened = shorten_with_ellipsis(text, width);
    let current_width = string_width(shortened.as_str());
    match align {
        ColumnAlign::Left => {
            if current_width >= width {
                shortened
            } else {
                let mut result = shortened;
                result.push_str(&" ".repeat(width - current_width));
                result
            }
        }
        ColumnAlign::Right => {
            if current_width >= width {
                shortened
            } else {
                format!("{}{}", " ".repeat(width - current_width), shortened)
            }
        }
    }
}

fn shorten_with_ellipsis(text: &str, width: usize) -> String {
    if width == 0 {
        return String::new();
    }
    if string_width(text) <= width {
        return text.to_string();
    }
    const ELLIPSIS: &str = "…";
    let ellipsis_width = string_width(ELLIPSIS);
    if width <= ellipsis_width {
        return slice_to_width(text, width);
    }
    let mut result = String::new();
    let mut used = 0;
    for ch in text.chars() {
        let ch_width = UnicodeWidthChar::width(ch).unwrap_or(1);
        if used + ch_width > width - ellipsis_width {
            break;
        }
        result.push(ch);
        used += ch_width;
    }
    result.push_str(ELLIPSIS);
    result
}

fn slice_to_width(text: &str, width: usize) -> String {
    let mut result = String::new();
    let mut used = 0;
    for ch in text.chars() {
        let ch_width = UnicodeWidthChar::width(ch).unwrap_or(1);
        if used + ch_width > width {
            break;
        }
        result.push(ch);
        used += ch_width;
    }
    result
}

fn detail_display_text(detail: &AgentDetail) -> String {
    match detail {
        AgentDetail::Progress(text)
        | AgentDetail::Result(text)
        | AgentDetail::Error(text)
        | AgentDetail::Info(text) => text.clone(),
    }
}

fn format_token_count(tokens: u64) -> String {
    if tokens >= 10_000 {
        format!("{}k", tokens / 1_000)
    } else {
        format_with_separators(tokens)
    }
}

fn format_batch_label(raw: &str) -> Option<String> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.starts_with('#') {
        return Some(trimmed.to_string());
    }
    let digits: String = trimmed.chars().filter(|c| c.is_ascii_digit()).collect();
    if !digits.is_empty() {
        return Some(format!("#{}", digits));
    }
    Some(trimmed.to_string())
}

fn format_duration_compact(duration: Duration) -> String {
    let total_secs = duration.as_secs();
    let hours = total_secs / 3600;
    let minutes = (total_secs % 3600) / 60;
    let seconds = total_secs % 60;
    if hours > 0 {
        format!("{}h{:02}m", hours, minutes)
    } else {
        format!("{:02}:{:02}", minutes, seconds)
    }
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
    string_width(text)
}
