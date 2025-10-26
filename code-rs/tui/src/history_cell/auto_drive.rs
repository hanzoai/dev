use super::card_style::{
    auto_drive_card_style,
    fill_card_background,
    hint_text_style,
    primary_text_style,
    rows_to_lines,
    secondary_text_style,
    title_text_style,
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
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Paragraph, Widget, Wrap};
use unicode_width::UnicodeWidthStr;

const BORDER_TOP: &str = "╭─";
const BORDER_BODY: &str = "│";
const BORDER_BOTTOM: &str = "╰─";
const HINT_TEXT: &str = " [Ctrl+S] Settings · [Esc] Stop";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum AutoDriveStatus {
    Running,
    Paused,
    Failed,
    Stopped,
}

impl AutoDriveStatus {
    fn label(self) -> &'static str {
        match self {
            AutoDriveStatus::Running => "Running",
            AutoDriveStatus::Paused => "Paused",
            AutoDriveStatus::Failed => "Failed",
            AutoDriveStatus::Stopped => "Stopped",
        }
    }

    fn style(self) -> Style {
        match self {
            AutoDriveStatus::Running => Style::default().fg(colors::info()),
            AutoDriveStatus::Paused => Style::default().fg(colors::warning()),
            AutoDriveStatus::Failed => Style::default().fg(colors::error()),
            AutoDriveStatus::Stopped => Style::default().fg(colors::text_mid()),
        }
    }

    fn tool_status(self) -> ToolCellStatus {
        match self {
            AutoDriveStatus::Running | AutoDriveStatus::Paused => ToolCellStatus::Running,
            AutoDriveStatus::Stopped => ToolCellStatus::Success,
            AutoDriveStatus::Failed => ToolCellStatus::Failed,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum AutoDriveActionKind {
    Info,
    Warning,
    Error,
}

#[derive(Clone, Debug)]
struct AutoDriveAction {
    text: String,
    kind: AutoDriveActionKind,
}

impl AutoDriveAction {
    fn new(text: String, kind: AutoDriveActionKind) -> Self {
        Self { text, kind }
    }

    fn style(&self, style: &CardStyle) -> Style {
        match self.kind {
            AutoDriveActionKind::Info => primary_text_style(style),
            AutoDriveActionKind::Warning => Style::default().fg(colors::warning()),
            AutoDriveActionKind::Error => Style::default().fg(colors::error()),
        }
    }
}

#[derive(Clone)]
pub(crate) struct AutoDriveCardCell {
    goal: Option<String>,
    status: AutoDriveStatus,
    actions: Vec<AutoDriveAction>,
    cell_key: Option<String>,
    signature: Option<String>,
}

impl AutoDriveCardCell {
    pub(crate) fn new(goal: Option<String>) -> Self {
        let mut cell = Self {
            goal: goal.and_then(Self::normalize_text),
            status: AutoDriveStatus::Running,
            actions: Vec::new(),
            cell_key: None,
            signature: None,
        };
        if let Some(goal) = cell.goal.clone() {
            cell.actions.push(AutoDriveAction::new(
                format!("Goal: {goal}"),
                AutoDriveActionKind::Info,
            ));
        }
        cell
    }

    fn normalize_text(value: String) -> Option<String> {
        let trimmed = value.trim();
        if trimmed.is_empty() {
            None
        } else {
            Some(trimmed.to_string())
        }
    }

    pub(crate) fn set_status(&mut self, status: AutoDriveStatus) {
        self.status = status;
    }

    pub(crate) fn push_action(&mut self, text: impl Into<String>, kind: AutoDriveActionKind) {
        self.actions.push(AutoDriveAction::new(text.into(), kind));
    }

    fn accent_style(style: &CardStyle) -> Style {
        let dim = colors::mix_toward(style.accent_fg, style.text_secondary, 0.85);
        Style::default().fg(dim)
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

        if let Some(goal) = &self.goal {
            rows.push(self.description_row(goal.as_str(), body_width, style));
        }

        for action in &self.actions {
            rows.push(self.action_row(action, body_width, style));
        }

        rows.push(self.bottom_border_row(body_width, style));

        rows
    }

    fn title_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let mut segments: Vec<CardSegment> = Vec::new();
        let title_text = " Auto Drive";
        let status_text = format!("  {}", self.status.label());
        let combined = format!("{title_text}{status_text}");

        if UnicodeWidthStr::width(combined.as_str()) <= body_width {
            let mut title_style = title_text_style(style);
            title_style = title_style.add_modifier(Modifier::BOLD);
            segments.push(CardSegment::new(title_text.to_string(), title_style));
            segments.push(CardSegment::new(status_text, self.status.style()));
        } else {
            let mut title_style = title_text_style(style);
            title_style = title_style.add_modifier(Modifier::BOLD);
            let display = truncate_with_ellipsis(title_text, body_width);
            segments.push(CardSegment::new(display, title_style));
        }

        CardRow::new(
            BORDER_TOP.to_string(),
            Self::accent_style(style),
            segments,
            None,
        )
    }

    fn description_row(&self, goal: &str, body_width: usize, style: &CardStyle) -> CardRow {
        let display = truncate_with_ellipsis(goal, body_width);
        let mut segment = CardSegment::new(display, secondary_text_style(style));
        segment.inherit_background = true;
        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            vec![segment],
            None,
        )
    }

    fn action_row(&self, action: &AutoDriveAction, body_width: usize, style: &CardStyle) -> CardRow {
        if body_width == 0 {
            return CardRow::new(
                BORDER_BODY.to_string(),
                Self::accent_style(style),
                Vec::new(),
                None,
            );
        }
        let bullet = match action.kind {
            AutoDriveActionKind::Warning => "! ",
            AutoDriveActionKind::Error => "✗ ",
            AutoDriveActionKind::Info => "• ",
        };
        let value = format!("{bullet}{}", action.text);
        let display = truncate_with_ellipsis(value.as_str(), body_width);
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
        let mut segment = CardSegment::new(text, hint_text_style(style));
        segment.inherit_background = true;
        CardRow::new(
            BORDER_BOTTOM.to_string(),
            Self::accent_style(style),
            vec![segment],
            None,
        )
    }

    fn render_rows(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        if area.width == 0 || area.height == 0 {
            return;
        }
        let style = auto_drive_card_style();
        fill_card_background(buf, area, &style);
        let rows = self.build_card_rows(area.width, &style);
        let lines = rows_to_lines(&rows, &style, area.width);
        let text = Text::from(lines);
        Paragraph::new(text)
            .wrap(Wrap { trim: false })
            .scroll((skip_rows, 0))
            .render(area, buf);
    }

    fn desired_rows(&self, width: u16) -> usize {
        let style = auto_drive_card_style();
        self.build_card_rows(width, &style).len().max(1)
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

impl crate::chatwidget::tool_cards::ToolCardCell for AutoDriveCardCell {
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

impl HistoryCell for AutoDriveCardCell {
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
        HistoryCellType::Tool {
            status: self.status.tool_status(),
        }
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::from(format!("{} — {}", "Auto Drive", self.status.label())));
        if let Some(goal) = &self.goal {
            lines.push(Line::from(format!("goal: {goal}")));
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
