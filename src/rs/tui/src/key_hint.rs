//! Key hint display module

use ratatui::prelude::*;

pub fn render_key_hint(area: Rect, buf: &mut Buffer, hint: &str) {
    let text = Text::from(hint);
    let paragraph = ratatui::widgets::Paragraph::new(text);
    ratatui::widgets::Widget::render(paragraph, area, buf);
}

pub fn format_key_hint(key: &str, description: &str) -> String {
    format!("{} {}", key, description)
}

/// Format an Alt key combination hint
pub fn alt(key: &str) -> Span<'static> {
    Span::styled(format!("Alt+{}", key), Style::default().bold())
}