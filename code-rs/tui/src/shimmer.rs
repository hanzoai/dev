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

/// Blend two RGB colors by factor t (0.0 = a, 1.0 = b)
fn blend(a: (u8, u8, u8), b: (u8, u8, u8), t: f32) -> (u8, u8, u8) {
    let inv = 1.0 - t;
    let r = (a.0 as f32 * inv + b.0 as f32 * t).round() as u8;
    let g = (a.1 as f32 * inv + b.1 as f32 * t).round() as u8;
    let bl = (a.2 as f32 * inv + b.2 as f32 * t).round() as u8;
    (r, g, bl)
}

pub(crate) fn shimmer_spans(text: &str) -> Vec<Span<'static>> {
    let chars: Vec<char> = text.chars().collect();
    if chars.is_empty() {
        return Vec::new();
    }

    // Time-based sweep synchronized to process start (Codex style: 2.0s period)
    let padding = 10usize;
    let period = chars.len() + padding * 2;
    let sweep_seconds = 2.0f32;
    let pos_f =
        (elapsed_since_start().as_secs_f32() % sweep_seconds) / sweep_seconds * (period as f32);
    let pos = pos_f as usize;

    let has_true_color = crate::theme::has_truecolor_terminal();
    // Wider band for smoother gradient (Codex uses 5.0)
    let band_half_width = 5.0;

    // Get theme colors for gradient
    let theme = crate::theme::current_theme();
    let base_color = crate::colors::color_to_rgb(theme.text_dim);
    let highlight_color = crate::colors::color_to_rgb(theme.text_bright);

    let mut spans: Vec<Span<'static>> = Vec::with_capacity(chars.len());
    for (i, ch) in chars.iter().enumerate() {
        let i_pos = i as isize + padding as isize;
        let pos = pos as isize;
        let dist = (i_pos - pos).abs() as f32;

        // Cosine falloff for smooth gradient (same as Codex)
        let t = if dist <= band_half_width {
            let x = std::f32::consts::PI * (dist / band_half_width);
            0.5 * (1.0 + x.cos())
        } else {
            0.0
        };

        let style = if has_true_color {
            // Blend from dim to bright based on wave position
            let highlight = t.clamp(0.0, 1.0);
            let (r, g, b) = blend(base_color, highlight_color, highlight * 0.9);
            #[allow(clippy::disallowed_methods)]
            Style::default().fg(Color::Rgb(r, g, b))
        } else {
            // Fallback: use modifiers for non-truecolor terminals
            color_for_level(t)
        };
        spans.push(Span::styled(ch.to_string(), style));
    }
    spans
}

fn color_for_level(intensity: f32) -> Style {
    // Simple fallback: dim -> normal -> bold based on intensity
    if intensity < 0.2 {
        Style::default().add_modifier(Modifier::DIM)
    } else if intensity < 0.6 {
        Style::default()
    } else {
        Style::default().add_modifier(Modifier::BOLD)
    }
}
