use super::card_style::{
    fill_card_background,
    primary_text_style,
    rows_to_lines,
    secondary_text_style,
    truncate_with_ellipsis,
    web_search_card_style,
    CardRow,
    CardSegment,
    CardStyle,
    CARD_ACCENT_WIDTH,
};
use super::{HistoryCell, HistoryCellType, ToolCellStatus};
use crate::colors;
use code_common::elapsed::format_duration_digital;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Paragraph, Widget, Wrap};
use std::time::Duration;
use unicode_width::UnicodeWidthStr;

const BORDER_TOP: &str = "╭─";
const BORDER_BODY: &str = "│";
const BORDER_BOTTOM: &str = "╰─";
const HINT_TEXT: &str = " [Ctrl+S] Settings · [Esc] Stop";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum WebSearchStatus {
    Running,
    Completed,
    Failed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WebSearchActionKind {
    Info,
    Success,
    Error,
}

#[derive(Clone, Debug)]
struct WebSearchAction {
    text: String,
    kind: WebSearchActionKind,
}

impl WebSearchAction {
    fn new(text: String, kind: WebSearchActionKind) -> Self {
        Self { text, kind }
    }

    fn style(&self, style: &CardStyle) -> Style {
        match self.kind {
            WebSearchActionKind::Info => primary_text_style(style),
            WebSearchActionKind::Success => Style::default().fg(colors::success()),
            WebSearchActionKind::Error => Style::default().fg(colors::error()),
        }
    }
}

#[derive(Clone)]
pub(crate) struct WebSearchSessionCell {
    query: Option<String>,
    status: WebSearchStatus,
    actions: Vec<WebSearchAction>,
    duration: Option<Duration>,
    cell_key: Option<String>,
    signature: Option<String>,
}

impl WebSearchSessionCell {
    pub(crate) fn new() -> Self {
        Self {
            query: None,
            status: WebSearchStatus::Running,
            actions: Vec::new(),
            duration: None,
            cell_key: None,
            signature: None,
        }
    }

    pub(crate) fn set_query(&mut self, query: Option<String>) {
        self.query = query
            .and_then(|value| {
                let trimmed = value.trim();
                if trimmed.is_empty() {
                    None
                } else {
                    Some(trimmed.to_string())
                }
            });
    }

    pub(crate) fn set_status(&mut self, status: WebSearchStatus) {
        self.status = status;
    }

    fn push_action(&mut self, text: impl Into<String>, kind: WebSearchActionKind) {
        self.actions.push(WebSearchAction::new(text.into(), kind));
    }

    pub(crate) fn push_info(&mut self, text: impl Into<String>) {
        self.push_action(text, WebSearchActionKind::Info);
    }

    pub(crate) fn push_success(&mut self, text: impl Into<String>) {
        self.push_action(text, WebSearchActionKind::Success);
    }

    pub(crate) fn push_error(&mut self, text: impl Into<String>) {
        self.push_action(text, WebSearchActionKind::Error);
    }

    pub(crate) fn set_duration(&mut self, duration: Option<Duration>) {
        self.duration = duration;
    }

    pub(crate) fn ensure_started_message(&mut self) {
        if self.actions.is_empty() {
            self.push_info("Searching…");
        }
    }

    pub(crate) fn tool_title(&self) -> &'static str {
        "Web Search"
    }

    pub(crate) fn status_label(&self) -> &'static str {
        match self.status {
            WebSearchStatus::Running => "Running",
            WebSearchStatus::Completed => "Completed",
            WebSearchStatus::Failed => "Failed",
        }
    }

    fn status_style(&self) -> Style {
        match self.status {
            WebSearchStatus::Running => Style::default().fg(colors::info()),
            WebSearchStatus::Completed => Style::default().fg(colors::success()),
            WebSearchStatus::Failed => Style::default().fg(colors::error()),
        }
    }

    fn accent_style(style: &CardStyle) -> Style {
        Style::default().fg(style.accent_fg)
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

        rows.push(self.title_row(body_width, style));

        if let Some(query) = &self.query {
            rows.push(self.description_row(query.as_str(), body_width, style));
        }

        if !self.actions.is_empty() {
            for action in &self.actions {
                rows.push(self.action_row(action, body_width, style));
            }
        }

        if let Some(duration) = self.duration {
            if matches!(self.status, WebSearchStatus::Completed) {
                let duration_text = format!("Completed in {}", format_duration_digital(duration));
                rows.push(self.detail_row(duration_text.as_str(), body_width, style));
            }
        }

        rows.push(self.bottom_border_row(body_width, style));

        rows
    }

    fn title_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let mut segments: Vec<CardSegment> = Vec::new();
        let title_text = " Web Search";
        let status_text = format!("  {}", self.status_label());
        let total = format!("{title_text}{status_text}");

        if UnicodeWidthStr::width(total.as_str()) <= body_width {
            let mut primary = primary_text_style(style);
            primary = primary.add_modifier(Modifier::BOLD);
            segments.push(CardSegment::new(title_text.to_string(), primary));
            segments.push(CardSegment::new(status_text, self.status_style()));
        } else {
            let mut primary = primary_text_style(style);
            primary = primary.add_modifier(Modifier::BOLD);
            let display = truncate_with_ellipsis(title_text, body_width);
            segments.push(CardSegment::new(display, primary));
        }

        CardRow::new(
            BORDER_TOP.to_string(),
            Self::accent_style(style),
            segments,
            None,
        )
    }

    fn description_row(&self, text: &str, body_width: usize, style: &CardStyle) -> CardRow {
        let display = truncate_with_ellipsis(text, body_width);
        let mut segment = CardSegment::new(display, secondary_text_style(style));
        segment.inherit_background = true;
        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            vec![segment],
            None,
        )
    }

    fn detail_row(&self, text: &str, body_width: usize, style: &CardStyle) -> CardRow {
        let display = truncate_with_ellipsis(text, body_width);
        let mut segment = CardSegment::new(display, secondary_text_style(style));
        segment.inherit_background = true;
        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            vec![segment],
            None,
        )
    }

    fn action_row(&self, action: &WebSearchAction, body_width: usize, style: &CardStyle) -> CardRow {
        if body_width == 0 {
            return CardRow::new(
                BORDER_BODY.to_string(),
                Self::accent_style(style),
                Vec::new(),
                None,
            );
        }

        let bullet = "• ";
        let text_value = format!("{bullet}{}", action.text);
        let display = truncate_with_ellipsis(text_value.as_str(), body_width);

        let mut segment = CardSegment::new(display, action.style(style));
        segment.inherit_background = true;

        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            vec![segment],
            None,
        )
    }

    fn bottom_border_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let text = truncate_with_ellipsis(HINT_TEXT, body_width);
        let mut segment = CardSegment::new(text, secondary_text_style(style));
        segment.inherit_background = true;
        CardRow::new(
            BORDER_BOTTOM.to_string(),
            Self::accent_style(style),
            vec![segment],
            None,
        )
    }

    fn desired_rows(&self, width: u16) -> usize {
        let style = web_search_card_style();
        self.build_card_rows(width, &style).len().max(1)
    }

    fn render_rows(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        if area.width == 0 || area.height == 0 {
            return;
        }
        let style = web_search_card_style();
        fill_card_background(buf, area, &style);
        let rows = self.build_card_rows(area.width, &style);
        let lines = rows_to_lines(&rows, &style, area.width);
        let text = Text::from(lines);
        Paragraph::new(text)
            .wrap(Wrap { trim: false })
            .scroll((skip_rows, 0))
            .render(area, buf);
    }

    pub(crate) fn set_tool_card_key_internal(&mut self, key: Option<String>) {
        self.cell_key = key;
    }

    pub(crate) fn set_signature(&mut self, signature: Option<String>) {
        self.signature = signature;
    }

    pub(crate) fn current_tool_card_key(&self) -> Option<&str> {
        self.cell_key.as_deref()
    }

    pub(crate) fn signature(&self) -> Option<&str> {
        self.signature.as_deref()
    }
}

impl crate::chatwidget::tool_cards::ToolCardCell for WebSearchSessionCell {
    fn tool_card_key(&self) -> Option<&str> {
        self.current_tool_card_key()
    }

    fn set_tool_card_key(&mut self, key: Option<String>) {
        self.set_tool_card_key_internal(key);
    }

    fn dedupe_signature(&self) -> Option<String> {
        self.signature().map(|value| value.to_string())
    }
}

impl HistoryCell for WebSearchSessionCell {
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
        let status = match self.status {
            WebSearchStatus::Running => ToolCellStatus::Running,
            WebSearchStatus::Completed => ToolCellStatus::Success,
            WebSearchStatus::Failed => ToolCellStatus::Failed,
        };
        HistoryCellType::Tool { status }
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::styled(
            format!("{} — {}", self.tool_title(), self.status_label()),
            Style::default().add_modifier(Modifier::BOLD),
        ));
        if let Some(query) = &self.query {
            lines.push(Line::from(format!("query: {query}")));
        }
        for action in &self.actions {
            lines.push(Line::from(format!("- {}", action.text)));
        }
        lines
    }

    fn desired_height(&self, width: u16) -> u16 {
        self.desired_rows(width) as u16
    }

    fn has_custom_render(&self) -> bool {
        true
    }

    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        self.render_rows(area, buf, skip_rows);
    }
}
