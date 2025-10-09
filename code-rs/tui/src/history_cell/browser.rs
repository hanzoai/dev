use super::card_style::{
    browser_card_style,
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
use std::path::PathBuf;
use std::time::Duration;
use unicode_width::UnicodeWidthStr;

const MAX_ACTIONS: usize = 12;
const MAX_CONSOLE: usize = 8;
const MAX_ACTION_DISPLAY: usize = 6;
const MAX_CONSOLE_DISPLAY: usize = 5;

#[derive(Clone, Default)]
pub(crate) struct BrowserSessionCell {
    url: Option<String>,
    title: Option<String>,
    actions: Vec<BrowserAction>,
    console_messages: Vec<String>,
    screenshot_path: Option<String>,
    total_duration: Duration,
    completed: bool,
    cell_key: Option<String>,
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
        if self
            .actions
            .last()
            .map_or(false, |last| last.description == description)
        {
            return;
        }
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

        for line in self.detail_lines(body_width, style) {
            rows.push(CardRow::new(String::new(), accent_style_body.clone(), vec![line], None));
        }

        let mut has_prior_section = !rows.is_empty();

        if let Some(lines) = self.actions_section_lines(body_width, style) {
            self.push_section(
                &mut rows,
                style,
                &accent_style_body,
                &mut has_prior_section,
                "Actions",
                lines,
                body_width,
            );
        }

        if let Some(lines) = self.console_section_lines(body_width, style) {
            self.push_section(
                &mut rows,
                style,
                &accent_style_body,
                &mut has_prior_section,
                "Console",
                lines,
                body_width,
            );
        }

        if let Some(lines) = self.screenshot_section_lines(body_width, style) {
            self.push_section(
                &mut rows,
                style,
                &accent_style_body,
                &mut has_prior_section,
                "Screenshot",
                lines,
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
        let url = self.url.as_deref().unwrap_or("(unknown)");
        let title = format!("Browser Session • {}", url);

        let status_label = if self.completed { "Done" } else { "Running" };
        let chip_color = if self.completed {
            colors::success()
        } else {
            colors::info()
        };
        let pill_text = format!(" {} ", status_label);
        let pill_width = UnicodeWidthStr::width(pill_text.as_str()).min(body_width);
        let left_width = body_width.saturating_sub(pill_width);

        let header_style = header_text_style(style);
        let left_segment = CardSegment::with_fixed_bg(
            truncate_with_ellipsis(title.as_str(), left_width),
            header_style,
        );

        let mut segments = Vec::new();
        segments.push(left_segment);

        if pill_width > 0 {
            let pill_segment = CardSegment::with_fixed_bg(
                truncate_to_width(pill_text.as_str(), pill_width),
                status_chip_style(chip_color, style),
            );
            segments.push(pill_segment);
        }

        CardRow::new(String::from("🌐"), accent_style, segments, Some(style.header_bg))
    }

    fn detail_lines(&self, body_width: usize, style: &CardStyle) -> Vec<CardSegment> {
        let mut segments = Vec::new();
        let url = self.url.as_deref().unwrap_or("(unknown)");
        let title = self.title.as_deref().unwrap_or("(pending)");
        let elapsed = self.elapsed_label();

        let detail_lines = [
            format!("URL: {}", url),
            format!("Title: {}", title),
            format!("Elapsed: {}", elapsed),
        ];

        for line in detail_lines {
            let text = truncate_with_ellipsis(line.as_str(), body_width);
            segments.push(CardSegment::new(text, secondary_text_style(style)));
        }

        segments
    }

    fn actions_section_lines(&self, body_width: usize, style: &CardStyle) -> Option<Vec<CardSegment>> {
        let mut lines: Vec<CardSegment> = Vec::new();
        if self.actions.is_empty() {
            lines.push(CardSegment::new(
                truncate_with_ellipsis("(no browser actions yet)", body_width),
                secondary_text_style(style),
            ));
        } else {
            for action in self.actions.iter().take(MAX_ACTION_DISPLAY) {
                let formatted = format!(
                    "• {}  {}",
                    format_timestamp(action.timestamp),
                    action.description
                );
                lines.push(CardSegment::new(
                    truncate_with_ellipsis(formatted.as_str(), body_width),
                    primary_text_style(style),
                ));
            }
            if self.actions.len() > MAX_ACTION_DISPLAY {
                lines.push(CardSegment::new(
                    truncate_with_ellipsis(
                        format!("(+{} more)", self.actions.len() - MAX_ACTION_DISPLAY).as_str(),
                        body_width,
                    ),
                    secondary_text_style(style),
                ));
            }
        }
        Some(lines)
    }

    fn console_section_lines(&self, body_width: usize, style: &CardStyle) -> Option<Vec<CardSegment>> {
        let mut lines: Vec<CardSegment> = Vec::new();
        if self.console_messages.is_empty() {
            lines.push(CardSegment::new(
                truncate_with_ellipsis("(no console messages)", body_width),
                secondary_text_style(style),
            ));
        } else {
            for message in self.console_messages.iter().take(MAX_CONSOLE_DISPLAY) {
                let decorated = format!("• {}", message);
                lines.push(CardSegment::new(
                    truncate_with_ellipsis(decorated.as_str(), body_width),
                    primary_text_style(style),
                ));
            }
            if self.console_messages.len() > MAX_CONSOLE_DISPLAY {
                lines.push(CardSegment::new(
                    truncate_with_ellipsis(
                        format!("(+{} more)", self.console_messages.len() - MAX_CONSOLE_DISPLAY).as_str(),
                        body_width,
                    ),
                    secondary_text_style(style),
                ));
            }
        }
        Some(lines)
    }

    fn screenshot_section_lines(&self, body_width: usize, style: &CardStyle) -> Option<Vec<CardSegment>> {
        let text = if let Some(path) = &self.screenshot_path {
            format!("• Path: {}", path)
        } else {
            "(no screenshot yet)".to_string()
        };
        let segment = CardSegment::new(
            truncate_with_ellipsis(text.as_str(), body_width),
            secondary_text_style(style),
        );
        Some(vec![segment])
    }

    fn push_section(
        &self,
        rows: &mut Vec<CardRow>,
        style: &CardStyle,
        accent_style: &Style,
        has_prior_section: &mut bool,
        title: &str,
        lines: Vec<CardSegment>,
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
            truncate_with_ellipsis(title.to_uppercase().as_str(), body_width),
            section_title_style(style),
        );
        rows.push(CardRow::new(String::new(), accent_style.clone(), vec![title_segment], None));

        for line in lines {
            rows.push(CardRow::new(String::new(), accent_style.clone(), vec![line], None));
        }
    }

    fn divider_row(&self, accent_style: Style, style: &CardStyle, body_width: usize) -> CardRow {
        let rule = "─".repeat(body_width);
        let segment = CardSegment::new(rule, divider_style(style));
        CardRow::new(String::new(), accent_style, vec![segment], None)
    }

    fn elapsed_label(&self) -> String {
        if self.total_duration.is_zero() {
            if self.completed {
                "0s".to_string()
            } else {
                "Running".to_string()
            }
        } else {
            format_elapsed_compact(self.total_duration)
        }
    }

    fn build_plain_summary(&self) -> Vec<String> {
        let url = self.url.as_deref().unwrap_or("(unknown)");
        let mut lines = Vec::new();
        lines.push(format!(
            "Browser Session: {} [{}]",
            url,
            if self.completed { "done" } else { "running" }
        ));
        if let Some(title) = &self.title {
            if !title.trim().is_empty() {
                lines.push(format!("Title: {}", title));
            }
        }
        if let Some(path) = &self.screenshot_path {
            lines.push(format!("Screenshot: {}", path));
        }
        if let Some(last) = self.actions.last() {
            lines.push(format!("Last action: {}", last.description));
        }
        lines
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
        self.build_plain_summary().into_iter().map(Line::from).collect()
    }

    fn desired_height(&self, width: u16) -> u16 {
        let style = browser_card_style();
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

        let style = browser_card_style();
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

impl crate::chatwidget::tool_cards::ToolCardCell for BrowserSessionCell {
    fn tool_card_key(&self) -> Option<&str> {
        self.cell_key()
    }

    fn set_tool_card_key(&mut self, key: Option<String>) {
        self.set_cell_key(key);
    }
}
