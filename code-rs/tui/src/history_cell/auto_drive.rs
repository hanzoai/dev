use super::card_style::{
    auto_drive_card_style,
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
use crate::card_theme;
use crate::gradient_background::{GradientBackground, RevealRender};
use crate::colors;
use code_common::elapsed::format_duration_digital;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Paragraph, Widget, Wrap};
use unicode_width::UnicodeWidthStr;
use std::time::{Duration, Instant};

const BORDER_TOP: &str = "╭─";
const BORDER_BODY: &str = "│";
const BORDER_BOTTOM: &str = "╰─";
const HINT_TEXT: &str = " [Ctrl+S] Settings · [Esc] Stop";
const ACTION_TIME_INDENT: usize = 1;
const ACTION_TIME_SEPARATOR_WIDTH: usize = 2;
const ACTION_TIME_COLUMN_MIN_WIDTH: usize = 6;

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
    elapsed: Duration,
}

impl AutoDriveAction {
    fn new(text: String, kind: AutoDriveActionKind, elapsed: Duration) -> Self {
        Self { text, kind, elapsed }
    }
}

#[derive(Clone)]
pub(crate) struct AutoDriveCardCell {
    goal: Option<String>,
    status: AutoDriveStatus,
    actions: Vec<AutoDriveAction>,
    cell_key: Option<String>,
    signature: Option<String>,
    reveal_started_at: Option<Instant>,
    first_action_at: Option<Instant>,
}

impl AutoDriveCardCell {
    pub(crate) fn new(goal: Option<String>) -> Self {
        let reveal_started_at = active_auto_drive_theme()
            .theme
            .reveal
            .map(|_| Instant::now());
        let cell = Self {
            goal: goal.and_then(Self::normalize_text),
            status: AutoDriveStatus::Running,
            actions: Vec::new(),
            cell_key: None,
            signature: None,
            reveal_started_at,
            first_action_at: None,
        };
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
        let now = Instant::now();
        let base = self.first_action_at.get_or_insert(now);
        let elapsed = now.saturating_duration_since(*base);
        self.actions
            .push(AutoDriveAction::new(text.into(), kind, elapsed));
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
        rows.push(self.blank_row(body_width, style));

        if let Some(goal) = &self.goal {
            rows.push(self.goal_row(goal.as_str(), body_width, style));
            rows.push(self.blank_row(body_width, style));
        }

        rows.push(self.actions_heading_row(body_width, style));
        let action_rows = self.action_rows(body_width, style);
        if action_rows.is_empty() {
            rows.push(self.actions_placeholder_row(body_width, style));
        } else {
            rows.extend(action_rows);
        }

        rows.push(self.blank_row(body_width, style));
        rows.push(self.bottom_border_row(body_width, style));

        rows
    }

    fn title_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let mut segments: Vec<CardSegment> = Vec::new();
        let title_text = " Auto Drive";
        let status_text = format!(" · {}", self.status.label());
        let combined = format!("{title_text}{status_text}");

        if UnicodeWidthStr::width(combined.as_str()) <= body_width {
            let mut bold_title = title_text_style(style);
            bold_title = bold_title.add_modifier(Modifier::BOLD);
            segments.push(CardSegment::new(
                title_text.to_string(),
                bold_title,
            ));
            segments.push(CardSegment::new(
                status_text,
                secondary_text_style(style),
            ));
        } else {
            let display = truncate_with_ellipsis(title_text, body_width);
            let mut bold_title = title_text_style(style);
            bold_title = bold_title.add_modifier(Modifier::BOLD);
            segments.push(CardSegment::new(display, bold_title));
        }

        CardRow::new(
            BORDER_TOP.to_string(),
            Self::accent_style(style),
            segments,
            None,
        )
    }

    fn blank_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let filler = " ".repeat(body_width);
        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            vec![CardSegment::new(filler, Style::default())],
            None,
        )
    }

    fn goal_row(&self, goal: &str, body_width: usize, style: &CardStyle) -> CardRow {
        let cleaned = goal.trim();
        let value = format!(" {}", cleaned);
        let display = truncate_with_ellipsis(value.as_str(), body_width);
        let mut segment = CardSegment::new(display, secondary_text_style(style));
        segment.inherit_background = true;
        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            vec![segment],
            None,
        )
    }

    fn actions_heading_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        if body_width == 0 {
            return CardRow::new(
                BORDER_BODY.to_string(),
                Self::accent_style(style),
                Vec::new(),
                None,
            );
        }

        let mut segments = Vec::new();
        if ACTION_TIME_INDENT > 0 {
            segments.push(CardSegment::new(
                " ".repeat(ACTION_TIME_INDENT),
                secondary_text_style(style),
            ));
        }

        let available = body_width.saturating_sub(ACTION_TIME_INDENT);
        if available > 0 {
            let title = truncate_with_ellipsis("Actions", available);
            let mut heading = CardSegment::new(title, primary_text_style(style));
            heading.inherit_background = true;
            segments.push(heading);
        }

        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            segments,
            None,
        )
    }

    fn actions_placeholder_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        if body_width == 0 {
            return CardRow::new(
                BORDER_BODY.to_string(),
                Self::accent_style(style),
                Vec::new(),
                None,
            );
        }

        let mut segments = Vec::new();
        if ACTION_TIME_INDENT > 0 {
            segments.push(CardSegment::new(
                " ".repeat(ACTION_TIME_INDENT),
                secondary_text_style(style),
            ));
        }

        let available = body_width.saturating_sub(ACTION_TIME_INDENT);
        if available > 0 {
            let message = truncate_with_ellipsis("Awaiting auto drive activity", available);
            let mut placeholder = CardSegment::new(message, secondary_text_style(style));
            placeholder.inherit_background = true;
            segments.push(placeholder);
        }

        CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            segments,
            None,
        )
    }

    fn action_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        if body_width == 0 {
            return Vec::new();
        }
        if self.actions.is_empty() {
            return Vec::new();
        }

        let elapsed_labels: Vec<String> = self
            .actions
            .iter()
            .map(|action| format!(" {}", format_duration_digital(action.elapsed)))
            .collect();

        let time_width = elapsed_labels
            .iter()
            .map(|label| UnicodeWidthStr::width(label.as_str()))
            .max()
            .unwrap_or(0)
            .max(ACTION_TIME_COLUMN_MIN_WIDTH);

        let indent_text = " ".repeat(ACTION_TIME_INDENT);
        let indent_style = secondary_text_style(style);
        let time_style = primary_text_style(style);
        let separator_text = if ACTION_TIME_SEPARATOR_WIDTH > 0 {
            Some(" ".repeat(ACTION_TIME_SEPARATOR_WIDTH))
        } else {
            None
        };

        let mut rows = Vec::new();

        for (action, elapsed) in self.actions.iter().zip(elapsed_labels.iter()) {
            let mut segments = Vec::new();
            if ACTION_TIME_INDENT > 0 {
                segments.push(CardSegment::new(indent_text.clone(), indent_style));
            }

            let mut remaining = body_width.saturating_sub(ACTION_TIME_INDENT);
            if remaining == 0 {
                rows.push(CardRow::new(
                    BORDER_BODY.to_string(),
                    Self::accent_style(style),
                    segments,
                    None,
                ));
                continue;
            }

            let padded_time = format!("{elapsed:<width$}", width = time_width);
            segments.push(CardSegment::new(padded_time, time_style));
            remaining = remaining.saturating_sub(time_width);

            if let Some(separator) = separator_text.as_ref() {
                if remaining < ACTION_TIME_SEPARATOR_WIDTH {
                    rows.push(CardRow::new(
                        BORDER_BODY.to_string(),
                        Self::accent_style(style),
                        segments,
                        None,
                    ));
                    continue;
                }
                segments.push(CardSegment::new(separator.clone(), Style::default()));
                remaining = remaining.saturating_sub(ACTION_TIME_SEPARATOR_WIDTH);
            }

            if remaining > 0 {
                let description = match action.kind {
                    AutoDriveActionKind::Info => action.text.trim().to_string(),
                    AutoDriveActionKind::Warning => format!("! {}", action.text.trim()),
                    AutoDriveActionKind::Error => format!("✗ {}", action.text.trim()),
                };
                let display = truncate_with_ellipsis(description.as_str(), remaining);
                let mut description_segment =
                    CardSegment::new(display, secondary_text_style(style));
                description_segment.inherit_background = true;
                segments.push(description_segment);
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
        let is_dark = is_dark_theme_active();
        let theme = active_auto_drive_theme();
        let style = auto_drive_card_style();

        let reveal = theme.theme.reveal.map(|config| {
            let progress = self
                .reveal_started_at
                .map(|started| {
                    let elapsed = started.elapsed().as_secs_f32();
                    (elapsed / config.duration.as_secs_f32()).clamp(0.0, 1.0)
                })
                .unwrap_or(1.0);
            RevealRender {
                progress,
                variant: config.variant,
                intro_light: !is_dark,
            }
        });

        GradientBackground::render(buf, area, &style.gradient, style.text_primary, reveal);

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

fn is_dark_theme_active() -> bool {
    let (r, g, b) = colors::color_to_rgb(colors::background());
    let luminance = (0.2126 * r as f32 + 0.7152 * g as f32 + 0.0722 * b as f32) / 255.0;
    luminance < 0.5
}

fn active_auto_drive_theme() -> card_theme::CardThemeDefinition {
    if is_dark_theme_active() {
        card_theme::auto_drive_dark_theme()
    } else {
        card_theme::auto_drive_light_theme()
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
