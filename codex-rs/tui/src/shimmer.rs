use std::sync::OnceLock;
use std::time::Duration;
use std::time::Instant;

use ratatui::style::Color;
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::text::Span;

static PROCESS_START: OnceLock<Instant> = OnceLock::new();

fn elapsed_since_start() -> Duration {
    let start = PROCESS_START.get_or_init(Instant::now);
    start.elapsed()
}

pub(crate) fn shimmer_spans(text: &str) -> Vec<Span<'static>> {
    let chars: Vec<char> = text.chars().collect();
    if chars.is_empty() {
        return Vec::new();
    }
    // Use time-based sweep synchronized to process start.
    let padding = 10usize;
    let period = chars.len() + padding * 2;
    let sweep_seconds = 2.5f32;
    let pos_f =
        (elapsed_since_start().as_secs_f32() % sweep_seconds) / sweep_seconds * (period as f32);
    let pos = pos_f as usize;
    // Prefer our unified terminal capability detection so Windows Terminal and
    // other modern emulators that support truecolor aren't mistakenly treated
    // as 256‑color only (which breaks smooth gradients).
    let has_true_color = crate::theme::has_truecolor_terminal();
    let band_half_width = 3.0;

    let mut spans: Vec<Span<'static>> = Vec::with_capacity(chars.len());
    for (i, ch) in chars.iter().enumerate() {
        let i_pos = i as isize + padding as isize;
        let pos = pos as isize;
        let dist = (i_pos - pos).abs() as f32;

        let t = if dist <= band_half_width {
            let x = std::f32::consts::PI * (dist / band_half_width);
            0.5 * (1.0 + x.cos())
        } else {
            0.0
        };
        let brightness = 0.4 + 0.6 * t;
        let level = (brightness * 255.0).clamp(0.0, 255.0) as u8;
        let style = if has_true_color {
            // Allow custom RGB colors, as the implementation is thoughtfully
            // adjusting the level of the default foreground color.
            #[allow(clippy::disallowed_methods)]
            {
                Style::default()
                    .fg(Color::Rgb(level, level, level))
                    .add_modifier(Modifier::BOLD)
            }
        } else {
            Style::default()
                .fg(color_for_level(level))
                .add_modifier(Modifier::BOLD)
        };
        spans.push(Span::styled(ch.to_string(), style));
    }
    spans
}

fn color_for_level(level: u8) -> Color {
    // For shimmer effect, we map brightness levels to theme colors
    if level < 128 {
        crate::colors::text_dim()
    } else if level < 192 {
        crate::colors::text()
    } else {
        crate::colors::text_bright()
    }
}
