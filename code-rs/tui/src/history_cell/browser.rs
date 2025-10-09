use super::card_style::{
    browser_card_style,
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
use ratatui::prelude::{Modifier, Style};
use ratatui::text::{Line, Text};
use ratatui::widgets::{Paragraph, Widget, Wrap};
use std::path::PathBuf;
use url::Url;
use std::time::Duration;
use unicode_width::UnicodeWidthChar;

const BORDER_TOP: &str = "╭─";
const BORDER_BODY: &str = "│ ";
const BORDER_BOTTOM: &str = "╰─";

const MAX_ACTIONS: usize = 24;
const MAX_CONSOLE: usize = 12;
const ACTION_TAIL: usize = 6;
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
    headless: Option<bool>,
    status_code: Option<String>,
}

#[derive(Clone)]
struct BrowserAction {
    duration: Duration,
    action: String,
    target: Option<String>,
    value: Option<String>,
    outcome: Option<String>,
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
        action: String,
        target: Option<String>,
        value: Option<String>,
        outcome: Option<String>,
    ) {
        if self.actions.last().map_or(false, |last| {
            last.action == action
                && last.target == target
                && last.value == value
                && last.outcome == outcome
        }) {
            return;
        }
        let action_entry = BrowserAction {
            duration,
            action,
            target,
            value,
            outcome: outcome.clone(),
        };
        self.actions.push(action_entry);
        if self.actions.len() > MAX_ACTIONS {
            let overflow = self.actions.len() - MAX_ACTIONS;
            self.actions.drain(0..overflow);
        }
        let finish = timestamp.saturating_add(duration);
        if finish > self.total_duration {
            self.total_duration = finish;
        }
        if let Some(outcome) = outcome {
            if let Some(code) = extract_status_code(&outcome) {
                self.status_code = Some(code);
            }
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

    pub(crate) fn set_headless(&mut self, headless: Option<bool>) {
        self.headless = headless;
    }

    pub(crate) fn set_status_code(&mut self, code: Option<String>) {
        self.status_code = code;
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

    fn normalized_title(&self) -> Option<String> {
        self.title
            .as_ref()
            .map(|value| value.trim())
            .filter(|value| !value.is_empty() && !value.eq_ignore_ascii_case("(pending)"))
            .map(|value| value.to_string())
    }

    fn header_summary_text(&self) -> String {
        let label = if self.headless.unwrap_or(true) {
            "BROWSER (headless)"
        } else {
            "BROWSER"
        };

        let mut primary = format!("{}: {}", label, self.display_label());
        if let Some(code) = &self.status_code {
            primary.push(' ');
            primary.push('[');
            primary.push_str(code);
            primary.push(']');
        }

        let mut parts = vec![primary];
        let action_count = self.actions.len();
        parts.push(format!(
            "{} action{}",
            action_count,
            if action_count == 1 { "" } else { "s" }
        ));
        parts.push(format_duration_seconds(self.total_duration));
        parts.join(" · ")
    }

    fn top_border_row(&self, body_width: usize, style: &CardStyle) -> CardRow {
        let text = truncate_with_ellipsis(self.header_summary_text().as_str(), body_width);
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

    fn display_host(&self) -> Option<String> {
        self
            .url
            .as_ref()
            .and_then(|url| Url::parse(url).ok())
            .and_then(|parsed| parsed.host_str().map(|host| host.to_string()))
    }

    fn display_label(&self) -> String {
        if let Some(title) = self.normalized_title() {
            return title;
        }
        if let Some(host) = self.display_host() {
            return host;
        }
        self
            .url
            .as_ref()
            .map(|url| url.clone())
            .unwrap_or_else(|| "Browser Session".to_string())
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

        let mut content_rows = self.actions_rows(body_width, style);
        if content_rows.is_empty() {
            content_rows.push(self.body_text_row(
                "No browser actions yet",
                body_width,
                style,
                secondary_text_style(style),
            ));
        }

        if let Some(console) = self.console_row(body_width, style) {
            content_rows.push(self.blank_border_row(body_width, style));
            content_rows.push(console);
        }

        if let Some(screenshot) = self.screenshot_row(body_width, style) {
            content_rows.push(self.blank_border_row(body_width, style));
            content_rows.push(screenshot);
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

    fn actions_compact_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        self
            .actions
            .iter()
            .rev()
            .take(ACTION_TAIL)
            .rev()
            .enumerate()
            .map(|(idx, action)| {
                let summary = format!(
                    "#{} {}",
                    idx + 1,
                    format_action_summary(action)
                );
                self.body_text_row(summary, body_width, style, primary_text_style(style))
            })
            .collect()
    }

    fn console_row(&self, body_width: usize, style: &CardStyle) -> Option<CardRow> {
        if self.console_messages.is_empty() {
            return None;
        }
        let last = self.console_messages.last()?.clone();
        let style_color = if last.contains('⚠') {
            Style::default().fg(colors::warning())
        } else {
            secondary_text_style(style)
        };
        let text = format!("Console: {}", last);
        Some(self.body_text_row(text, body_width, style, style_color))
    }

    fn screenshot_row(&self, body_width: usize, style: &CardStyle) -> Option<CardRow> {
        let path = self.screenshot_path.as_ref()?;
        let label = format!("Screenshot: {}", path);
        Some(self.body_text_row(
            label,
            body_width,
            style,
            secondary_text_style(style),
        ))
    }

    fn actions_rows(&self, body_width: usize, style: &CardStyle) -> Vec<CardRow> {
        const MIN_TABLE_WIDTH: usize = 56;
        if body_width < MIN_TABLE_WIDTH {
            return self.actions_compact_rows(body_width, style);
        }

        let mut entries = Vec::new();
        for (idx, action) in self.actions.iter().enumerate() {
            entries.push(BrowserTableEntry::from(idx, action));
        }
        if entries.is_empty() {
            return Vec::new();
        }

        let widths = BrowserTableWidths::compute(body_width, &entries);
        let mut rows = Vec::new();
        rows.push(CardRow::new(
            BORDER_BODY.to_string(),
            Self::accent_style(style),
            widths.render_header(style),
            None,
        ));

        for entry in &entries {
            rows.push(CardRow::new(
                BORDER_BODY.to_string(),
                Self::accent_style(style),
                widths.render_row(entry, style),
                None,
            ));
        }

        rows
    }

    fn build_plain_summary(&self) -> Vec<String> {
        let mut lines = Vec::new();
        let status = if self.completed { "done" } else { "running" };
        lines.push(format!(
            "Browser Session: {} [{}]",
            self.display_label(), status
        ));
        if let Some(code) = &self.status_code {
            lines.push(format!("Status: {}", code));
        }
        lines.push(format!("Actions: {}", self.actions.len()));
        if let Some(last) = self.actions.last() {
            lines.push(format!("Last action: {}", format_action_summary(last)));
        }
        if let Some(path) = &self.screenshot_path {
            lines.push(format!("Screenshot: {}", path));
        }
        lines
    }
}

struct BrowserTableEntry {
    index: usize,
    action: String,
    target: String,
    value: String,
    outcome: String,
    duration: String,
}

impl BrowserTableEntry {
    fn from(index: usize, action: &BrowserAction) -> Self {
        Self {
            index: index + 1,
            action: action.action.clone(),
            target: action
                .target
                .clone()
                .unwrap_or_else(|| "—".to_string()),
            value: action
                .value
                .clone()
                .map(|v| format!("\"{}\"", v))
                .unwrap_or_else(|| "—".to_string()),
            outcome: action
                .outcome
                .clone()
                .unwrap_or_else(|| "—".to_string()),
            duration: format_duration_seconds(action.duration),
        }
    }
}

struct BrowserTableWidths {
    widths: [usize; BrowserTableWidths::COLUMN_COUNT],
}

impl BrowserTableWidths {
    const COLUMN_COUNT: usize = 6;
    const MIN_WIDTHS: [usize; BrowserTableWidths::COLUMN_COUNT] = [2, 10, 12, 10, 12, 5];

    fn headers() -> [&'static str; BrowserTableWidths::COLUMN_COUNT] {
        ["#", "ACTION", "TARGET", "VALUE", "RESULT", "t"]
    }

    fn compute(body_width: usize, entries: &[BrowserTableEntry]) -> Self {
        let mut widths = Self::MIN_WIDTHS;

        for (idx, header) in Self::headers().iter().enumerate() {
            widths[idx] = widths[idx].max(string_width(header));
        }

        for entry in entries {
            widths[0] = widths[0].max(string_width(entry.index.to_string().as_str()));
            widths[1] = widths[1].max(string_width(entry.action.as_str()));
            widths[2] = widths[2].max(string_width(entry.target.as_str()));
            widths[3] = widths[3].max(string_width(entry.value.as_str()));
            widths[4] = widths[4].max(string_width(entry.outcome.as_str()));
            widths[5] = widths[5].max(string_width(entry.duration.as_str()));
        }

        let spaces = Self::COLUMN_COUNT.saturating_sub(1);
        let mut total = widths.iter().sum::<usize>() + spaces;
        let mut columns = widths;

        while total > body_width {
            let mut reduced = false;
            for &idx in &[4usize, 2usize, 3usize, 1usize] {
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
            columns[1] += extra;
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

    fn render_row(&self, entry: &BrowserTableEntry, style: &CardStyle) -> Vec<CardSegment> {
        let mut segments = Vec::new();
        for idx in 0..Self::COLUMN_COUNT {
            let (text, column_style, align) = match idx {
                0 => (
                    entry.index.to_string(),
                    secondary_text_style(style),
                    ColumnAlign::Right,
                ),
                1 => (
                    entry.action.clone(),
                    primary_text_style(style),
                    ColumnAlign::Left,
                ),
                2 => (
                    entry.target.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Left,
                ),
                3 => (
                    entry.value.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Left,
                ),
                4 => (
                    entry.outcome.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Left,
                ),
                5 => (
                    entry.duration.clone(),
                    secondary_text_style(style),
                    ColumnAlign::Right,
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

fn format_action_summary(action: &BrowserAction) -> String {
    match (&action.target, &action.value, &action.outcome) {
        (Some(target), Some(value), Some(outcome)) => {
            format!("{} {} → {}", action.action, target, outcome_for_display(outcome, value))
        }
        (Some(target), Some(value), None) => {
            format!("{} {} = {}", action.action, target, value)
        }
        (Some(target), None, Some(outcome)) => {
            format!("{} {} → {}", action.action, target, outcome)
        }
        (Some(target), None, None) => format!("{} {}", action.action, target),
        (None, Some(value), Some(outcome)) => {
            format!("{} {} → {}", action.action, value, outcome)
        }
        (None, Some(value), None) => format!("{} {}", action.action, value),
        (None, None, Some(outcome)) => format!("{} → {}", action.action, outcome),
        _ => action.action.clone(),
    }
}

fn outcome_for_display(outcome: &str, value: &str) -> String {
    if outcome == "value set" {
        value.to_string()
    } else {
        outcome.to_string()
    }
}

fn format_duration_seconds(duration: Duration) -> String {
    format!("{:.1}s", duration.as_secs_f32())
}

fn extract_status_code(outcome: &str) -> Option<String> {
    let trimmed = outcome.trim();
    if trimmed.len() < 3 {
        return None;
    }
    let code: String = trimmed
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .collect();
    if code.len() == 3 {
        Some(code)
    } else {
        None
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

#[derive(Clone, Copy)]
enum ColumnAlign {
    Left,
    Right,
}

impl crate::chatwidget::tool_cards::ToolCardCell for BrowserSessionCell {
    fn tool_card_key(&self) -> Option<&str> {
        self.cell_key()
    }

    fn set_tool_card_key(&mut self, key: Option<String>) {
        self.set_cell_key(key);
    }
}
