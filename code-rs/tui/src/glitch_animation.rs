//! Color ramps shared by the animated surfaces (Auto Drive, the gradient
//! background).
use ratatui::prelude::*;

fn lerp_u8(a: u8, b: u8, t: f32) -> u8 {
    (a as f32 + (b as f32 - a as f32) * t).round() as u8
}

pub(crate) fn mix_rgb(a: Color, b: Color, t: f32) -> Color {
    match (a, b) {
        (Color::Rgb(ar, ag, ab), Color::Rgb(br, bg, bb)) => {
            Color::Rgb(lerp_u8(ar, br, t), lerp_u8(ag, bg, t), lerp_u8(ab, bb, t))
        }
        _ => b,
    }
}

// vibrant cyan -> magenta -> amber across the word
pub(crate) fn gradient_multi(t: f32) -> Color {
    let t = t.clamp(0.0, 1.0);
    let (r1, g1, b1) = (0u8, 224u8, 255u8); // #00E0FF
    let (r2, g2, b2) = (255u8, 78u8, 205u8); // #FF4ECD
    let (r3, g3, b3) = (255u8, 181u8, 0u8); // #FFB500
    if t < 0.5 {
        Color::Rgb(
            lerp_u8(r1, r2, t * 2.0),
            lerp_u8(g1, g2, t * 2.0),
            lerp_u8(b1, b2, t * 2.0),
        )
    } else {
        Color::Rgb(
            lerp_u8(r2, r3, (t - 0.5) * 2.0),
            lerp_u8(g2, g3, (t - 0.5) * 2.0),
            lerp_u8(b2, b3, (t - 0.5) * 2.0),
        )
    }
}

