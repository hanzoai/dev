use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::Style;

/// Fill the given rectangular area with the provided style and optional character.
///
/// The rectangle is clipped to the buffer's bounds before being applied. When `fill_char`
/// is `Some(_)`, each cell's symbol is replaced; otherwise only the style is updated.
pub fn fill_rect(buf: &mut Buffer, area: Rect, fill_char: Option<char>, style: Style) {
    let rect = buf.area.intersection(area);
    if rect.width == 0 || rect.height == 0 {
        return;
    }

    let buf_width = buf.area.width as usize;
    let offset_x = rect.x.saturating_sub(buf.area.x) as usize;
    let offset_y = rect.y.saturating_sub(buf.area.y) as usize;
    let row_span = rect.width as usize;

    for row in 0..rect.height as usize {
        let start = (offset_y + row) * buf_width + offset_x;
        let end = start + row_span;
        let row = &mut buf.content[start..end];
        for cell in row {
            cell.set_style(style);
            if let Some(ch) = fill_char {
                cell.set_char(ch);
            }
        }
    }
}
