use super::card_style::{
    agent_card_style,
    fill_card_background,
    primary_text_style,
    rows_to_lines,
    secondary_text_style,
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
use ratatui::prelude::{Color, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Paragraph, Widget, Wrap};
use std::time::{Duration, Instant};

const BORDER_TOP: &str = "╭─";
const BORDER_BODY: &str = "│";
const BORDER_BOTTOM: &str = "╰─";
use unicode_width::UnicodeWidthChar;

const MAX_PLAN_LINES: usize = 4;
const MAX_SUMMARY_LINES: usize = 4;
const MAX_AGENT_DISPLAY: usize = 8;
const ACTION_TIME_COLUMN_WIDTH: usize = 11;

#[derive(Clone, Default)]
pub(crate) struct AgentRunCell {
    agent_name: String,
    status_label: String,
    task: Option<String>,
    context: Option<String>,
    duration: Option<Duration>,
    plan: Vec<String>,
    agents: Vec<AgentStatusPreview>,
    summary_lines: Vec<String>,
    completed: bool,
    actions: Vec<ActionEntry>,
    cell_key: Option<String>,
    batch_label: Option<String>,
    write_enabled: Option<bool>,
    first_action_at: Option<Instant>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct AgentStatusPreview {
    pub id: String,
    pub name: String,
    pub status: String,
    pub model: Option<String>,
    pub details: Vec<AgentDetail>,
    pub status_kind: AgentStatusKind,
    pub step_progress: Option<StepProgress>,
    pub elapsed: Option<Duration>,
    #[allow(dead_code)]
    pub token_count: Option<u64>,
    pub last_update: Option<String>,
    pub elapsed_updated_at: Option<Instant>,
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
            AgentStatusKind::Cancelled => colors::text_dim(),
            AgentStatusKind::Pending => colors::text_dim(),
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

#[derive(Clone)]
struct AgentRowData {
    name: String,
    status: String,
    meta: String,
    color: Color,
    name_width: usize,
    status_width: usize,
    meta_width: usize,
}

impl AgentRowData {
    fn new(name: String, status: String, meta: String, color: Color) -> Self {
        let name_width = string_width(name.as_str());
        let status_width = string_width(status.as_str());
        let meta_width = string_width(meta.as_str());
        Self {
            name,
            status,
            meta,
            color,
            name_width,
            status_width,
            meta_width,
        }
    }
}

#[derive(Clone, Debug)]
struct ActionEntry {
    label: String,
    elapsed: Duration,
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

    pub(crate) fn set_context(&mut self, context: Option<String>) {
        self.context = context.and_then(|value| {
            let trimmed = value.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            }
        });
    }

    pub(crate) fn display_title(&self) -> Option<String> {
        if let Some(label) = self.batch_label.as_ref() {
            let trimmed = label.trim();
            if !trimmed.is_empty() {
                return Some(trimmed.to_string());
            }
        }
        let trimmed_name = self.agent_name.trim();
        if trimmed_name.is_empty() || trimmed_name.eq_ignore_ascii_case("(pending)") {
            None
        } else {
            Some(trimmed_name.to_string())
        }
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

    pub(crate) fn agent_name_for_id(&self, id: &str) -> Option<String> {
        self
            .agents
            .iter()
            .find(|preview| preview.id == id)
            .map(|preview| Self::agent_display_name(preview))
    }

    pub(crate) fn set_write_mode(&mut self, write_enabled: Option<bool>) {
        if write_enabled.is_some() {
            self.write_enabled = write_enabled;
        }
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
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return;
        }
        let text = trimmed.to_string();
        if self
            .actions
            .last()
            .map_or(false, |last| last.label == text)
        {
            return;
        }
        let now = Instant::now();
        let base = self.first_action_at.get_or_insert(now);
        let elapsed = now.saturating_duration_since(*base);
        self.actions.push(ActionEntry { label: text, elapsed });
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
        let dim = colors::mix_toward(style.accent_fg, colors::text_dim(), 0.85);
        Style::default().fg(dim)
    }

    fn top_border_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let mut segments = Vec::new();
        if body_width == 0 {
            return CardRow::new(
                BORDER_TOP.to_string(),
                Self::accent_style(style),
                segments,
                None,
            );
        }

        let mut remaining = body_width;

        let mut title = self
            .batch_label
            .as_deref()
            .map(|value| value.trim())
            .filter(|value| !value.is_empty());
        if title.is_none() {
            let name_trimmed = self.agent_name.trim();
            if !name_trimmed.is_empty() && !name_trimmed.eq_ignore_ascii_case("(pending)") {
                title = Some(name_trimmed);
            }
        }

        if title.is_none() {
            let agents_segment = if remaining >= string_width("Agents") {
                "Agents".to_string()
            } else {
                let truncated = truncate_with_ellipsis("Agents", remaining);
                let trimmed = truncated.trim_end();
                if trimmed.is_empty() {
                    truncated
                } else {
                    trimmed.to_string()
                }
            };
            let agents_width = string_width(agents_segment.as_str());
            if !agents_segment.is_empty() {
                segments.push(CardSegment::new(agents_segment, secondary_text_style(style)));
            }
            remaining = remaining.saturating_sub(agents_width);

            if remaining == 0 {
                return CardRow::new(BORDER_TOP.to_string(), Self::accent_style(style), segments, None);
            }
        }

        if let Some(text_value) = title {
            if remaining == 0 {
                return CardRow::new(BORDER_TOP.to_string(), Self::accent_style(style), segments, None);
            }
            segments.push(CardSegment::new(" ".to_string(), primary_text_style(style)));
            remaining = remaining.saturating_sub(1);
            let write_label = self.write_mode_label();

            let mut available = remaining;
            let mut name_allow = available;
            if let Some(label) = write_label.as_ref() {
                let label_width = string_width(label.as_str());
                if label_width + 1 <= available {
                    name_allow = available.saturating_sub(label_width);
                } else {
                    // Not enough room for label; skip rendering it.
                    name_allow = available;
                }
            }

            let truncated = truncate_with_ellipsis(text_value, name_allow.max(1));
            let name_width = string_width(truncated.as_str());
            if !truncated.is_empty() {
                segments.push(CardSegment::new(truncated, primary_text_style(style)));
            }
            available = available.saturating_sub(name_width);

            if let Some(label) = write_label {
                if available >= string_width(label.as_str()) {
                    segments.push(CardSegment::new(label, secondary_text_style(style)));
                }
            }
        }

        CardRow::new(
            BORDER_TOP.to_string(),
            Self::accent_style(style),
            segments,
            None,
        )
    }

    fn blank_border_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let filler = " ".repeat(body_width);
        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            vec![CardSegment::new(filler, Style::default())],
            None,
        )
    }

    fn body_text_row_with_indent(
        &self,
        text: impl Into<String>,
        body_width: usize,
        style: &CardStyle,
        text_style: Style,
        indent: usize,
    ) -> CardRow {
        if body_width == 0 {
            return CardRow::new(BORDER_BODY.to_string(), Self::accent_style(style), Vec::new(), None);
        }
        if body_width <= indent {
            return CardRow::new(BORDER_BODY.to_string(), Self::accent_style(style), Vec::new(), None);
        }
        let mut segments = Vec::new();
        if indent > 0 {
            segments.push(CardSegment::new(" ".repeat(indent), text_style));
        }
        let available = body_width.saturating_sub(indent);
        let text: String = text.into();
        let display = truncate_with_ellipsis(text.as_str(), available);
        segments.push(CardSegment::new(display, text_style));
        CardRow::new(BORDER_BODY.to_string(), Self::accent_style(style), segments, None)
    }

    fn multiline_body_rows_with_indent(
        &self,
        text: String,
        body_width: usize,
        style: &CardStyle,
        text_style: Style,
        indent: usize,
    ) -> Vec<CardRow> {
        if body_width == 0 {
            return Vec::new();
        }
        if body_width <= indent + 1 {
            return vec![self.body_text_row_with_indent(text, body_width, style, text_style, indent)];
        }

        let content_width = body_width.saturating_sub(indent);
        let lines = wrap_text_to_width(text.as_str(), content_width.max(1));
        lines
            .into_iter()
            .map(|line| {
                let mut segments = Vec::new();
                if indent > 0 {
                    segments.push(CardSegment::new(" ".repeat(indent), text_style));
                }
                let truncated = truncate_with_ellipsis(line.as_str(), content_width);
                segments.push(CardSegment::new(truncated, text_style));
                CardRow::new(
                    BORDER_BODY.to_string(),
                    Self::accent_style(style),
                    segments,
                    None,
                )
            })
            .collect()
    }

    fn bottom_border_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let text_value = format!(" [Ctrl+A] Expand · [Esc] Stop");
        let text = truncate_with_ellipsis(text_value.as_str(), body_width);
        let segment = CardSegment::new(text, secondary_text_style(style));
        CardRow::new(BORDER_BOTTOM.to_string(), Self::accent_style(style), vec![segment], None)
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

        let mut inserted_section = false;

        let agent_rows = self.agent_section_rows(body_width, style);
        if !agent_rows.is_empty() {
            if inserted_section {
                rows.push(self.blank_border_row(body_width, style));
            }
            rows.extend(agent_rows);
            inserted_section = true;
        }

        let prompt_rows = self.prompt_rows(body_width, style);
        if !prompt_rows.is_empty() {
            if inserted_section {
                rows.push(self.blank_border_row(body_width, style));
            }
            rows.extend(prompt_rows);
            inserted_section = true;
        }

        let action_rows = self.actions_section_rows(body_width, style);
        if !action_rows.is_empty() {
            if inserted_section {
                rows.push(self.blank_border_row(body_width, style));
            }
            rows.extend(action_rows);
        }

        rows.push(self.blank_border_row(body_width, style));
        rows.push(self.bottom_border_row(body_width, style));

        rows
    }

    fn write_mode_label(&self) -> Option<String> {
        self.write_enabled.map(|flag| {
            if flag {
                " write ".to_string()
            } else {
                " read only ".to_string()
            }
        })
    }

    fn prompt_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        if body_width == 0 {
            return Vec::new();
        }

        let mut rows = Vec::new();
        let mut lines: Vec<String> = Vec::new();

        if let Some(task) = self.task.as_ref().map(|t| t.trim()).filter(|t| !t.is_empty()) {
            lines.push(task.to_string());
        }

        if let Some(context) = self.context.as_ref().map(|c| c.trim()).filter(|c| !c.is_empty()) {
            lines.push(format!("Context: {}", context));
        }

        if !self.plan.is_empty() {
            for (index, step) in self.plan.iter().take(MAX_PLAN_LINES).enumerate() {
                lines.push(format!("{}. {}", index + 1, step));
            }
            if self.plan.len() > MAX_PLAN_LINES {
                lines.push(format!("(+{} more)", self.plan.len() - MAX_PLAN_LINES));
            }
        }

        if lines.is_empty() {
            return rows;
        }

        rows.push(self.section_heading_row("Prompt", body_width, style));
        for line in lines {
            rows.extend(self.multiline_body_rows_with_indent(
                line,
                body_width,
                style,
                secondary_text_style(style),
                CONTENT_INDENT,
            ));
        }

        rows
    }

    fn agent_section_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        if body_width == 0 {
            return Vec::new();
        }

        let mut rows = Vec::new();
        rows.push(self.section_heading_row("Agents", body_width, style));

        if self.agents.is_empty() {
            rows.push(self.body_text_row_with_indent(
                "No agent updates yet",
                body_width,
                style,
                secondary_text_style(style),
                CONTENT_INDENT,
            ));
            return rows;
        }

        let displayed: Vec<&AgentStatusPreview> = self.agents.iter().take(MAX_AGENT_DISPLAY).collect();
        let indent = " ".repeat(CONTENT_INDENT);
        let bullet = "• ";
        let indent_width = string_width(indent.as_str());
        let bullet_width = string_width(bullet);
        let available_rest = body_width
            .saturating_sub(indent_width)
            .saturating_sub(bullet_width);

        let entries = self.build_agent_display_entries(&displayed);

        if let Some(mut aligned) = self.build_aligned_agent_rows(
            &entries,
            body_width,
            style,
            indent.as_str(),
            bullet,
            available_rest,
        ) {
            rows.append(&mut aligned);
        } else {
            rows.extend(self.build_agent_rows_fallback(
                &entries,
                body_width,
                style,
                indent.as_str(),
                bullet,
            ));
        }

        if self.agents.len() > MAX_AGENT_DISPLAY {
            let remaining = self.agents.len() - MAX_AGENT_DISPLAY;
            rows.push(self.body_text_row_with_indent(
                format!("(+{} more agents)", remaining),
                body_width,
                style,
                secondary_text_style(style),
                CONTENT_INDENT,
            ));
        }

        rows
    }

    fn build_agent_display_entries(
        &self,
        previews: &[&AgentStatusPreview],
    ) -> Vec<AgentRowData> {
        let now = Instant::now();
        previews
            .iter()
            .map(|preview| {
                let mut meta_parts: Vec<String> = Vec::new();
                if let Some(duration_label) = Self::agent_duration_label(preview, now) {
                    meta_parts.push(duration_label);
                }
                if let Some(progress) = preview.step_progress.as_ref() {
                    meta_parts.push(format!("{}/{}", progress.completed, progress.total));
                }
                // Token counts add noise in the compact card view; leave them out here.

                let meta = if meta_parts.is_empty() {
                    String::new()
                } else {
                    format!("({})", meta_parts.join(" · "))
                };

                let name = Self::agent_display_name(preview);
                let status = Self::agent_status_text(preview);
                let color = preview.status_kind.color();

                AgentRowData::new(name, status, meta, color)
            })
            .collect()
    }

    fn build_aligned_agent_rows(
        &self,
        entries: &[AgentRowData],
        _body_width: usize,
        style: &CardStyle,
        indent: &str,
        bullet: &str,
        available_rest: usize,
    ) -> Option<Vec<CardRow>> {
        if entries.is_empty() {
            return Some(Vec::new());
        }
        if available_rest == 0 {
            return None;
        }

        const COLUMN_GAP: usize = 2;

        let has_status = entries.iter().any(|entry| !entry.status.is_empty());
        let mut include_meta = entries.iter().any(|entry| !entry.meta.is_empty());

        let mut max_status_width = if has_status {
            entries
                .iter()
                .map(|entry| entry.status_width)
                .max()
                .unwrap_or(0)
        } else {
            0
        };

        let mut max_meta_width = if include_meta {
            entries
                .iter()
                .map(|entry| entry.meta_width)
                .max()
                .unwrap_or(0)
        } else {
            0
        };

        let mut remaining = available_rest;

        if has_status {
            if remaining <= COLUMN_GAP {
                return None;
            }
            remaining -= COLUMN_GAP;
            max_status_width = max_status_width.min(remaining);
            remaining = remaining.saturating_sub(max_status_width);
        }

        if include_meta {
            if remaining <= COLUMN_GAP {
                include_meta = false;
                max_meta_width = 0;
            } else {
                remaining -= COLUMN_GAP;
                max_meta_width = max_meta_width.min(remaining);
                remaining = remaining.saturating_sub(max_meta_width);
            }
        }

        if include_meta && max_meta_width == 0 {
            include_meta = false;
        }

        if has_status && max_status_width == 0 {
            return None;
        }

        let max_name_width_raw = entries
            .iter()
            .map(|entry| entry.name_width)
            .max()
            .unwrap_or(0);

        let mut name_space = remaining;
        if name_space == 0 {
            if has_status && max_status_width > 1 {
                max_status_width -= 1;
                name_space = 1;
            } else {
                return None;
            }
        }

        let max_name_width = max_name_width_raw.min(name_space).max(1);

        let mut rows = Vec::new();
        let indent_style = primary_text_style(style);
        for entry in entries {
            let mut segments = Vec::new();
            segments.push(CardSegment::new(indent.to_string(), indent_style));
            segments.push(CardSegment::new(
                bullet.to_string(),
                Style::default().fg(entry.color),
            ));

            let mut name_display = truncate_with_ellipsis(entry.name.as_str(), max_name_width);
            let name_width = string_width(name_display.as_str());
            if name_width < max_name_width {
                let padding = " ".repeat(max_name_width - name_width);
                name_display.push_str(&padding);
            }
            segments.push(CardSegment::new(name_display, primary_text_style(style)));

            if has_status {
                segments.push(CardSegment::new(" ".repeat(COLUMN_GAP), Style::default()));
                let status_display = truncate_with_ellipsis(entry.status.as_str(), max_status_width);
                let status_width = string_width(status_display.as_str());
                if status_width > 0 {
                    segments.push(CardSegment::new(
                        status_display,
                        Style::default().fg(entry.color),
                    ));
                }
                if max_status_width > status_width {
                    segments.push(CardSegment::new(
                        " ".repeat(max_status_width - status_width),
                        Style::default(),
                    ));
                }
            }

            if include_meta {
                segments.push(CardSegment::new(" ".repeat(COLUMN_GAP), Style::default()));
                let meta_display = truncate_with_ellipsis(entry.meta.as_str(), max_meta_width);
                let meta_width = string_width(meta_display.as_str());
                if meta_width > 0 {
                    segments.push(CardSegment::new(
                        meta_display,
                        Style::default().fg(entry.color),
                    ));
                }
                if max_meta_width > meta_width {
                    segments.push(CardSegment::new(
                        " ".repeat(max_meta_width - meta_width),
                        Style::default(),
                    ));
                }
            }

            rows.push(CardRow::new(
                BORDER_BODY.to_string(),
                Self::accent_style(style),
                segments,
                None,
            ));
        }

        Some(rows)
    }

    fn build_agent_rows_fallback(
        &self,
        entries: &[AgentRowData],
        body_width: usize,
        style: &CardStyle,
        indent: &str,
        bullet: &str,
    ) -> Vec<CardRow> {
        if entries.is_empty() {
            return Vec::new();
        }

        let indent_width = string_width(indent);
        let bullet_width = string_width(bullet);
        let available_rest = body_width
            .saturating_sub(indent_width)
            .saturating_sub(bullet_width);

        let mut rows = Vec::new();
        let indent_style = primary_text_style(style);

        for entry in entries {
            let mut segments = Vec::new();
            segments.push(CardSegment::new(indent.to_string(), indent_style));
            segments.push(CardSegment::new(
                bullet.to_string(),
                Style::default().fg(entry.color),
            ));

            let mut remaining = available_rest;
            if remaining == 0 {
                rows.push(CardRow::new(
                    BORDER_BODY.to_string(),
                    Self::accent_style(style),
                    segments,
                    None,
                ));
                continue;
            }

            let name_display = truncate_with_ellipsis(entry.name.as_str(), remaining);
            let name_width = string_width(name_display.as_str());
            remaining = remaining.saturating_sub(name_width);
            segments.push(CardSegment::new(name_display, primary_text_style(style)));

            if remaining > 0 && !entry.status.is_empty() {
                segments.push(CardSegment::new(" ".to_string(), Style::default()));
                remaining = remaining.saturating_sub(1);
                if remaining > 0 {
                    let status_display = truncate_with_ellipsis(entry.status.as_str(), remaining);
                    let status_width = string_width(status_display.as_str());
                    segments.push(CardSegment::new(
                        status_display,
                        Style::default().fg(entry.color),
                    ));
                    remaining = remaining.saturating_sub(status_width);
                }
            }

            if remaining > 0 && !entry.meta.is_empty() {
                let gap = 2.min(remaining);
                if gap > 0 {
                    segments.push(CardSegment::new(" ".repeat(gap), Style::default()));
                    remaining = remaining.saturating_sub(gap);
                }
                if remaining > 0 {
                    let meta_display = truncate_with_ellipsis(entry.meta.as_str(), remaining);
                    segments.push(CardSegment::new(
                        meta_display,
                        Style::default().fg(entry.color),
                    ));
                }
            }

            rows.push(CardRow::new(
                BORDER_BODY.to_string(),
                Self::accent_style(style),
                segments,
                None,
            ));
        }

        rows
    }

    fn actions_section_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        if body_width == 0 || self.actions.is_empty() {
            return Vec::new();
        }

        let mut rows = Vec::new();
        rows.push(self.section_heading_row("Actions", body_width, style));

        for entry in &self.actions {
            let elapsed = Self::format_elapsed_label(entry.elapsed);
            let indent_str = " ".repeat(CONTENT_INDENT);
            let indent_style = secondary_text_style(style);
            let time_style = Style::default().fg(colors::text());
            let label_style = secondary_text_style(style);

            if body_width <= CONTENT_INDENT {
                continue;
            }

            let mut segments = Vec::new();
            segments.push(CardSegment::new(indent_str, indent_style));

            let mut remaining = body_width.saturating_sub(CONTENT_INDENT);
            if remaining <= ACTION_TIME_COLUMN_WIDTH {
                continue;
            }
            let padded_time = format!("{:>width$}", elapsed, width = ACTION_TIME_COLUMN_WIDTH);
            let time_width = string_width(padded_time.as_str());
            segments.push(CardSegment::new(padded_time, time_style));
            remaining = remaining.saturating_sub(time_width);

            if remaining > 0 {
                segments.push(CardSegment::new(" ".to_string(), Style::default()));
                remaining = remaining.saturating_sub(1);
            }

            if remaining > 0 {
                let mut desc_display = entry.label.clone();
                if string_width(desc_display.as_str()) > remaining {
                    desc_display = truncate_with_ellipsis(entry.label.as_str(), remaining)
                        .trim_end()
                        .to_string();
                }
                if !desc_display.is_empty() {
                    segments.push(CardSegment::new(desc_display, label_style));
                }
            }

            rows.push(CardRow::new(
                BORDER_BODY.to_string(),
                Self::accent_style(style),
                segments,
                None,
            ));
        }

        rows
    }

    fn section_heading_row(
        &self,
        title: &str,
        body_width: usize,
        style: &CardStyle,
    ) -> CardRow {
        self.body_text_row_with_indent(
            title,
            body_width,
            style,
            primary_text_style(style),
            HEADING_INDENT,
        )
    }

    fn agent_display_name(preview: &AgentStatusPreview) -> String {
        if let Some(model) = preview.model.as_ref() {
            let trimmed = model.trim();
            if !trimmed.is_empty() {
                return trimmed.to_string();
            }
        }
        let name = preview.name.trim();
        if !name.is_empty() {
            return name.to_string();
        }
        preview.id.trim().to_string()
    }

    fn agent_status_text(preview: &AgentStatusPreview) -> String {
        let status = preview.status.trim();
        if status.is_empty() {
            preview.status_kind.label().to_ascii_lowercase()
        } else {
            status.to_ascii_lowercase()
        }
    }

    fn agent_duration_label(preview: &AgentStatusPreview, now: Instant) -> Option<String> {
        preview.elapsed.map(|base| {
            let mut duration = base;
            if matches!(
                preview.status_kind,
                AgentStatusKind::Running | AgentStatusKind::Pending
            ) {
                if let Some(updated_at) = preview.elapsed_updated_at {
                    if let Some(extra) = now.checked_duration_since(updated_at) {
                        duration = duration.saturating_add(extra);
                    }
                }
            }
            if duration.as_secs() == 0 {
                "0s".to_string()
            } else if duration.as_secs() < 60 {
                format!("{}s", duration.as_secs())
            } else {
                let minutes = duration.as_secs() / 60;
                let seconds = duration.as_secs() % 60;
                format!("{}m {:02}s", minutes, seconds)
            }
        })
    }

    fn format_elapsed_label(duration: Duration) -> String {
        let total_secs = duration.as_secs();
        if total_secs < 60 {
            format!("{}s", total_secs)
        } else {
            let minutes = total_secs / 60;
            let seconds = total_secs % 60;
            format!("{}m {:>2}s", minutes, seconds)
        }
    }
    fn agent_counts(&self) -> AgentCountSummary {
        let mut summary = AgentCountSummary::default();
        for agent in &self.agents {
            summary.observe(agent.status_kind);
        }
        summary
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
            lines.push(format!("Last activity: {}", last.label));
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

    fn gutter_symbol(&self) -> Option<&'static str> {
        None
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

fn wrap_text_to_width(text: &str, width: usize) -> Vec<String> {
    if width == 0 {
        return vec![String::new()];
    }

    let mut lines: Vec<String> = Vec::new();
    let mut current = String::new();
    let mut current_width = 0;

    for word in text.split_whitespace() {
        let mut word_parts = if string_width(word) > width {
            split_long_word(word, width)
        } else {
            vec![word.to_string()]
        };

        for part in word_parts.drain(..) {
            let part_width = string_width(part.as_str());
            if current.is_empty() {
                current.push_str(part.as_str());
                current_width = part_width;
            } else if current_width + 1 + part_width > width {
                lines.push(current);
                current = part.clone();
                current_width = part_width;
            } else {
                current.push(' ');
                current.push_str(part.as_str());
                current_width += 1 + part_width;
            }
        }
    }

    if !current.is_empty() {
        lines.push(current);
    }

    if lines.is_empty() {
        lines.push(String::new());
    }
    lines
}

fn split_long_word(word: &str, width: usize) -> Vec<String> {
    if width == 0 {
        return vec![String::new()];
    }

    let mut parts = Vec::new();
    let mut current = String::new();
    let mut current_width = 0;

    for ch in word.chars() {
        let ch_width = UnicodeWidthChar::width(ch).unwrap_or(1);
        if current_width + ch_width > width && !current.is_empty() {
            parts.push(current);
            current = String::new();
            current_width = 0;
        }
        current.push(ch);
        current_width += ch_width;
    }

    if !current.is_empty() {
        parts.push(current);
    }

    if parts.is_empty() {
        parts.push(String::new());
    }
    parts
}

fn detail_display_text(detail: &AgentDetail) -> String {
    match detail {
        AgentDetail::Progress(text)
        | AgentDetail::Result(text)
        | AgentDetail::Error(text)
        | AgentDetail::Info(text) => text.clone(),
    }
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
const HEADING_INDENT: usize = 2;
const CONTENT_INDENT: usize = 4;
