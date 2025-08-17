use crate::theme::current_theme;
use ratatui::style::Color;

// Legacy color constants - now redirect to theme
pub(crate) fn light_blue() -> Color {
    current_theme().primary
}

pub(crate) fn success_green() -> Color {
    current_theme().success
}

pub(crate) fn success() -> Color {
    current_theme().success
}

pub(crate) fn warning() -> Color {
    current_theme().warning
}

pub(crate) fn error() -> Color {
    current_theme().error
}

// Convenience functions for common theme colors
pub(crate) fn primary() -> Color {
    current_theme().primary
}

#[allow(dead_code)]
pub(crate) fn secondary() -> Color {
    current_theme().secondary
}

pub(crate) fn border() -> Color {
    current_theme().border
}

pub(crate) fn border_focused() -> Color {
    current_theme().border_focused
}

pub(crate) fn text() -> Color {
    current_theme().text
}

pub(crate) fn text_dim() -> Color {
    current_theme().text_dim
}

pub(crate) fn text_bright() -> Color {
    current_theme().text_bright
}

pub(crate) fn info() -> Color {
    current_theme().info
}

// Alias for text_dim
pub(crate) fn dim() -> Color {
    text_dim()
}

pub(crate) fn background() -> Color {
    current_theme().background
}

#[allow(dead_code)]
pub(crate) fn selection() -> Color {
    current_theme().selection
}

// Syntax/special helpers
pub(crate) fn function() -> Color {
    current_theme().function
}

// Overlay/scrim helper: a dimmed background used behind modal overlays.
// We derive it from the current theme background so it looks consistent for
// both light and dark themes.
fn color_to_rgb(c: Color) -> (u8, u8, u8) {
    match c {
        Color::Rgb(r, g, b) => (r, g, b),
        Color::Black => (0, 0, 0),
        Color::White => (255, 255, 255),
        Color::Gray => (192, 192, 192),
        Color::DarkGray => (128, 128, 128),
        Color::Red => (205, 49, 49),
        Color::Green => (13, 188, 121),
        Color::Yellow => (229, 229, 16),
        Color::Blue => (36, 114, 200),
        Color::Magenta => (188, 63, 188),
        Color::Cyan => (17, 168, 205),
        Color::LightRed => (255, 102, 102),
        Color::LightGreen => (102, 255, 178),
        Color::LightYellow => (255, 255, 102),
        Color::LightBlue => (102, 153, 255),
        Color::LightMagenta => (255, 102, 255),
        Color::LightCyan => (102, 255, 255),
        Color::Indexed(i) => (i, i, i),
        Color::Reset => color_to_rgb(current_theme().background),
    }
}

fn blend_with_black(rgb: (u8, u8, u8), alpha: f32) -> (u8, u8, u8) {
    // target = bg*(1-alpha) + black*alpha => bg*(1-alpha)
    let inv = 1.0 - alpha;
    let r = (rgb.0 as f32 * inv).round() as u8;
    let g = (rgb.1 as f32 * inv).round() as u8;
    let b = (rgb.2 as f32 * inv).round() as u8;
    (r, g, b)
}

fn is_light(rgb: (u8, u8, u8)) -> bool {
    let l = (0.2126 * rgb.0 as f32 + 0.7152 * rgb.1 as f32 + 0.0722 * rgb.2 as f32) / 255.0;
    l >= 0.6
}

pub(crate) fn overlay_scrim() -> Color {
    let bg = current_theme().background;
    let rgb = color_to_rgb(bg);
    // For light themes, use a slightly stronger darkening; for dark themes, a gentler one.
    let alpha = if is_light(rgb) { 0.18 } else { 0.10 };
    let (r, g, b) = blend_with_black(rgb, alpha);
    Color::Rgb(r, g, b)
}
