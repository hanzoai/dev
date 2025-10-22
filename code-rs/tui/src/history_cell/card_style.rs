use ratatui::buffer::Buffer;
use ratatui::prelude::*;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

use crate::colors;

#[derive(Clone, Copy)]
pub(crate) struct CardStyle {
    pub accent_fg: Color,
    pub background_top: Color,
    pub background_bottom: Color,
    pub text_primary: Color,
    pub text_secondary: Color,
}

#[derive(Clone, Debug)]
pub(crate) struct CardSegment {
    pub text: String,
    pub style: Style,
    pub inherit_background: bool,
}

impl CardSegment {
    pub fn new(text: String, style: Style) -> Self {
        Self {
            text,
            style,
            inherit_background: true,
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct CardRow {
    pub accent: String,
    pub accent_style: Style,
    pub segments: Vec<CardSegment>,
    pub body_bg: Option<Color>,
}

impl CardRow {
    pub fn new(
        accent: String,
        accent_style: Style,
        segments: Vec<CardSegment>,
        body_bg: Option<Color>,
    ) -> Self {
        Self {
            accent,
            accent_style,
            segments,
            body_bg,
        }
    }
}

pub(crate) const CARD_ACCENT_WIDTH: usize = 2;

pub(crate) fn agent_card_style() -> CardStyle {
    let bg = colors::background();
    let info = colors::info();
    let accent = colors::mix_toward(info, bg, 0.15);
    let accent_fg = colors::mix_toward(colors::text_bright(), accent, 0.40);
    let secondary = colors::text_dim();
    let background_bottom = colors::mix_toward(bg, info, 0.10);
    let background_top = colors::mix_toward(bg, info, 0.05);

    CardStyle {
        accent_fg,
        background_top,
        background_bottom,
        text_primary: colors::text(),
        text_secondary: secondary,
    }
}

pub(crate) fn browser_card_style() -> CardStyle {
    let bg = colors::background();
    let primary = colors::primary();
    let accent = colors::mix_toward(primary, bg, 0.20);
    let accent_fg = colors::mix_toward(colors::text_bright(), accent, 0.35);
    let secondary = colors::text_mid();
    let background_bottom = colors::mix_toward(bg, primary, 0.09);
    let background_top = colors::mix_toward(bg, primary, 0.04);

    CardStyle {
        accent_fg,
        background_top,
        background_bottom,
        text_primary: colors::text(),
        text_secondary: secondary,
    }
}

pub(crate) fn fill_card_background(buf: &mut Buffer, area: Rect, style: &CardStyle) {
    let height = area.height.max(1);
    for row in 0..area.height {
        let color = gradient_color(style, row as usize, height as usize);
        for col in 0..area.width {
            let cell = &mut buf[(area.x + col, area.y + row)];
            cell.set_symbol(" ");
            cell.set_style(Style::default().bg(color).fg(style.text_primary));
        }
    }
}

pub(crate) fn gradient_color(style: &CardStyle, position: usize, total: usize) -> Color {
    if total <= 1 {
        return style.background_bottom;
    }
    let t = position as f32 / ((total - 1) as f32);
    colors::mix_toward(style.background_top, style.background_bottom, t)
}

pub(crate) fn pad_icon(icon: &str, width: usize) -> String {
    if width == 0 {
        return String::new();
    }
    let trimmed = truncate_to_width(icon, width);
    let current = UnicodeWidthStr::width(trimmed.as_str());
    if current < width {
        let mut result = trimmed;
        result.push_str(&" ".repeat(width - current));
        return result;
    }
    trimmed
}

pub(crate) fn truncate_to_width(text: &str, width: usize) -> String {
    if width == 0 {
        return String::new();
    }
    let mut result = String::new();
    let mut used = 0;
    for ch in text.chars() {
        let w = UnicodeWidthChar::width(ch).unwrap_or(1);
        if used + w > width {
            break;
        }
        result.push(ch);
        used += w;
    }
    if used < width {
        result.push_str(&" ".repeat(width - used));
    }
    result
}

pub(crate) fn truncate_with_ellipsis(text: &str, width: usize) -> String {
    if width == 0 {
        return String::new();
    }
    if UnicodeWidthStr::width(text) <= width {
        return truncate_to_width(text, width);
    }
    let ellipsis = "...";
    let ellipsis_width = UnicodeWidthStr::width(ellipsis);
    if width <= ellipsis_width {
        return truncate_to_width(text, width);
    }
    let mut result = String::new();
    let mut used = 0;
    let limit = width - ellipsis_width;
    for ch in text.chars() {
        let w = UnicodeWidthChar::width(ch).unwrap_or(1);
        if used + w > limit {
            break;
        }
        result.push(ch);
        used += w;
    }
    result.push_str(ellipsis);
    let current = UnicodeWidthStr::width(result.as_str());
    if current < width {
        result.push_str(&" ".repeat(width - current));
    }
    result
}

pub(crate) fn rows_to_lines(rows: &[CardRow], style: &CardStyle, total_width: u16) -> Vec<Line<'static>> {
    if total_width == 0 {
        return Vec::new();
    }
    let has_accent = rows.iter().any(|row| !row.accent.trim().is_empty());
    let accent_width = if has_accent {
        CARD_ACCENT_WIDTH.min(total_width as usize)
    } else {
        0
    };
    let body_width = total_width.saturating_sub(accent_width as u16) as usize;
    let mut lines: Vec<Line<'static>> = Vec::new();
    let total_rows = rows.len();
    for (idx, row) in rows.iter().enumerate() {
        let mut spans: Vec<Span<'static>> = Vec::new();
        if accent_width > 0 {
            let accent_text = pad_icon(row.accent.as_str(), accent_width);
            let accent_span = Span::styled(accent_text, row.accent_style);
            spans.push(accent_span);
        }

        let row_bg = row
            .body_bg
            .unwrap_or_else(|| gradient_color(style, idx, total_rows.max(1)));
        let mut used_width = 0;
        for segment in &row.segments {
            let mut seg_style = segment.style;
            if segment.inherit_background {
                seg_style = seg_style.bg(row_bg);
            }
            let width = UnicodeWidthStr::width(segment.text.as_str());
            used_width += width;
            spans.push(Span::styled(segment.text.clone(), seg_style));
        }
        if used_width < body_width {
            let filler = " ".repeat(body_width - used_width);
            spans.push(Span::styled(filler, Style::default().bg(row_bg)));
        }
        lines.push(Line::from(spans));
    }
    lines
}

pub(crate) fn primary_text_style(style: &CardStyle) -> Style {
    Style::default().fg(style.text_primary)
}

pub(crate) fn secondary_text_style(style: &CardStyle) -> Style {
    Style::default().fg(style.text_secondary)
}
