use ratatui::style::Color;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

static DEFAULT_PALETTE_VERSION: AtomicU64 = AtomicU64::new(0);

fn bump_palette_version() {
    DEFAULT_PALETTE_VERSION.fetch_add(1, Ordering::Relaxed);
}

/// Returns the closest color to the target color that the terminal can display.
pub fn best_color(target: (u8, u8, u8)) -> Color {
    let Some(color_level) = supports_color::on_cached(supports_color::Stream::Stdout) else {
        return Color::default();
    };
    if color_level.has_16m {
        let (r, g, b) = target;
        Color::Rgb(r, g, b)
    } else if color_level.has_256 {
        // Use our existing quantization logic
        crate::theme::quantize_color_for_palette(Color::Rgb(target.0, target.1, target.2))
    } else {
        Color::default()
    }
}

pub fn requery_default_colors() {
    // Implementation can be added later if needed
    bump_palette_version();
}

#[derive(Clone, Copy)]
pub struct DefaultColors {
    pub fg: (u8, u8, u8),
    pub bg: (u8, u8, u8),
}

pub fn default_colors() -> Option<DefaultColors> {
    // For now, return reasonable defaults
    // This could be enhanced to query actual terminal colors
    Some(DefaultColors {
        fg: (255, 255, 255), // white text
        bg: (0, 0, 0),       // black background  
    })
}

pub fn default_bg() -> Option<(u8, u8, u8)> {
    default_colors().map(|dc| dc.bg)
}

pub fn default_fg() -> Option<(u8, u8, u8)> {
    default_colors().map(|dc| dc.fg)
}