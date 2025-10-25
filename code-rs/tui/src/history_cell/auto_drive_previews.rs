use super::*;
use crate::colors;
use crate::glitch_animation::{gradient_multi, mix_rgb};
use crate::util::buffer::write_line;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::prelude::{Color, Line, Span, Style};
use ratatui::style::Modifier;
use std::time::{Duration, Instant};
use textwrap::{Options as TwOptions, WordSplitter};
use unicode_width::UnicodeWidthStr;

#[derive(Clone)]
struct PreviewSpec {
    body: &'static [&'static str],
    gradient: GradientSpec,
    border_color: Color,
    text_color: Color,
    title_color: Color,
    footer_color: Color,
    reveal: Option<RevealConfig>,
}

#[derive(Clone, Copy)]
struct GradientSpec {
    left: Color,
    right: Color,
    bias: f32,
}

#[derive(Clone, Copy)]
struct RevealConfig {
    duration: Duration,
    variant: RevealVariant,
}

#[derive(Clone, Copy)]
enum RevealVariant {
    GlitchSweep,
    VertDrift,
    DiagonalPulse,
    ChromaticScan,
    SparkleFade,
    RainbowBloom,
    AuroraBridge,
    PrismRise,
    NeonRoad,
    HorizonRush,
}

struct CardRevealAnimation {
    started_at: Instant,
    duration: Duration,
    variant: RevealVariant,
}

impl CardRevealAnimation {
    fn new(duration: Duration, variant: RevealVariant) -> Self {
        Self {
            started_at: Instant::now(),
            duration,
            variant,
        }
    }

    fn progress(&self) -> f32 {
        let elapsed = self.started_at.elapsed();
        if elapsed >= self.duration {
            1.0
        } else {
            (elapsed.as_secs_f32() / self.duration.as_secs_f32()).clamp(0.0, 1.0)
        }
    }

    fn is_active(&self) -> bool {
        self.started_at.elapsed() < self.duration
    }
}

/// Generate a set of experimental history cells for Auto Drive.
pub(crate) fn auto_drive_preview_cells() -> Vec<Box<dyn HistoryCell>> {
    LEGACY_PREVIEWS
        .iter()
        .enumerate()
        .map(|(idx, spec)| {
            let name = LEGACY_NAMES[idx];
            Box::new(AutoDrivePreviewCell::new(name, spec)) as Box<dyn HistoryCell>
        })
        .chain(
            EXPERIMENTAL_RAINBOW_ROAD_PREVIEWS
                .iter()
                .enumerate()
                .map(|(idx, spec)| {
                    let name = EXPERIMENTAL_NAMES[idx];
                    Box::new(AutoDrivePreviewCell::new(name, spec)) as Box<dyn HistoryCell>
                }),
        )
        .collect()
}

struct AutoDrivePreviewCell {
    name: &'static str,
    spec: &'static PreviewSpec,
    animation: Option<CardRevealAnimation>,
}

impl AutoDrivePreviewCell {
    fn new(name: &'static str, spec: &'static PreviewSpec) -> Self {
        let animation = spec
            .reveal
            .map(|config| CardRevealAnimation::new(config.duration, config.variant));
        Self {
            name,
            spec,
            animation,
        }
    }

    fn layout_lines(&self, width: u16) -> Vec<Line<'static>> {
        const INDENT: &str = "";
        const CARD_PADDING: &str = "  ";
        const CARD_TITLE: &str = "Started Auto Drive";
        const CARD_FOOTER: &str = "[Ctrl+S] Settings · [Esc] Stop";

        let indent_width = UnicodeWidthStr::width(INDENT);
        let content_width = width.saturating_sub(indent_width as u16) as usize;
        let text_width = content_width
            .saturating_sub(1 + CARD_PADDING.len())
            .max(1);

        let mut lines: Vec<Line<'static>> = Vec::new();

        let base_text = self.spec.text_color;
        let border_style = Style::default().fg(self.spec.border_color);
        let body_style = Style::default().fg(base_text);
        let title_style = Style::default()
            .fg(self.spec.title_color)
            .add_modifier(Modifier::BOLD);
        let footer_style = Style::default().fg(self.spec.footer_color);

        let with_indent = |mut spans: Vec<Span<'static>>| {
            let mut parts = Vec::with_capacity(spans.len() + 1);
            parts.push(Span::raw(INDENT));
            parts.append(&mut spans);
            Line::from(parts)
        };

        lines.push(with_indent(vec![
            Span::styled("╭─ ".to_string(), border_style),
            Span::styled(CARD_TITLE.to_string(), title_style),
        ]));

        lines.push(with_indent(vec![
            Span::styled("│".to_string(), border_style),
            Span::raw(CARD_PADDING),
            Span::styled(self.name.to_string(), body_style),
        ]));

        lines.push(with_indent(vec![Span::styled("│".to_string(), border_style)]));

        for (idx, paragraph) in self.spec.body.iter().enumerate() {
            let mut wrapped = Self::wrap_text(paragraph, text_width);
            if wrapped.is_empty() {
                wrapped.push(String::new());
            }
            for line in wrapped.drain(..) {
                lines.push(with_indent(vec![
                    Span::styled("│".to_string(), border_style),
                    Span::raw(CARD_PADDING),
                    Span::styled(line, body_style),
                ]));
            }
            if idx + 1 < self.spec.body.len() {
                lines.push(with_indent(vec![Span::styled("│".to_string(), border_style)]));
            }
        }

        lines.push(with_indent(vec![Span::styled("│".to_string(), border_style)]));

        lines.push(with_indent(vec![
            Span::styled("╰─ ".to_string(), border_style),
            Span::styled(CARD_FOOTER.to_string(), footer_style),
        ]));

        lines
    }

    fn wrap_text(text: &str, width: usize) -> Vec<String> {
        if text.trim().is_empty() {
            return Vec::new();
        }
        let opts = TwOptions::new(width.max(1))
            .word_splitter(WordSplitter::NoHyphenation)
            .break_words(false);
        textwrap::wrap(text, &opts)
            .into_iter()
            .map(|cow| cow.into_owned())
            .collect()
    }
}

impl HistoryCell for AutoDrivePreviewCell {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn kind(&self) -> HistoryCellType {
        HistoryCellType::Notice
    }

    fn display_lines(&self) -> Vec<Line<'static>> {
        self.layout_lines(80)
    }

    fn desired_height(&self, width: u16) -> u16 {
        self.layout_lines(width).len() as u16
    }

    fn has_custom_render(&self) -> bool {
        true
    }

    fn custom_render_with_skip(&self, area: Rect, buf: &mut Buffer, skip_rows: u16) {
        if area.width == 0 || area.height == 0 {
            return;
        }

        let base_style = Style::default().fg(self.spec.text_color);

        if let Some(anim) = &self.animation {
            let progress = anim.progress();
            paint_reveal(
                buf,
                area,
                self.spec.gradient.left,
                self.spec.gradient.right,
                self.spec.gradient.bias,
                self.spec.text_color,
                progress,
                anim.variant,
            );
        } else {
            paint_horizontal(
                buf,
                area,
                self.spec.gradient.left,
                self.spec.gradient.right,
                self.spec.gradient.bias,
                self.spec.text_color,
            );
        }

        let lines = self.layout_lines(area.width);
        let start = skip_rows as usize;
        let end = (start + area.height as usize).min(lines.len());

        if start >= end {
            return;
        }

        for (idx, line) in lines[start..end].iter().enumerate() {
            let y = area.y + idx as u16;
            write_line(buf, area.x, y, area.width, line, base_style);
        }
    }

    fn is_animating(&self) -> bool {
        if let Some(anim) = &self.animation {
            anim.is_active()
        } else {
            false
        }
    }
}

fn paint_horizontal(
    buf: &mut Buffer,
    area: Rect,
    left: Color,
    right: Color,
    bias: f32,
    fg: Color,
) {
    let width = area.width.max(1) as f32;
    for row in 0..area.height {
        for col in 0..area.width {
            let x = col as f32 / width;
            let t = (x + bias).clamp(0.0, 1.0);
            let color = colors::mix_toward(left, right, t);
            let cell = &mut buf[(area.x + col, area.y + row)];
            cell.set_symbol(" ");
            cell.set_fg(fg);
            cell.set_bg(color);
        }
    }
}

fn paint_reveal(
    buf: &mut Buffer,
    area: Rect,
    left: Color,
    right: Color,
    bias: f32,
    fg: Color,
    progress: f32,
    variant: RevealVariant,
) {
    if progress >= 0.999 {
        paint_horizontal(buf, area, left, right, bias, fg);
        return;
    }

    let width = area.width.max(1);
    let height = area.height.max(1);
    let width_f = width as f32;
    let height_f = height as f32;

    for row in 0..height {
        for col in 0..width {
            let x_norm = (col as f32 + 0.5) / width_f;
            let y_norm = (row as f32 + 0.5) / height_f;
            let gradient_pos = ((col as f32 / width_f) + bias).clamp(0.0, 1.0);
            let final_color = colors::mix_toward(left, right, gradient_pos);

            let coverage = reveal_coverage(
                progress,
                x_norm,
                y_norm,
                col as u16,
                row as u16,
                variant,
            );
            let blend = smoothstep(0.0, 1.0, coverage);

            let accent = accent_color(
                variant,
                progress,
                x_norm,
                y_norm,
                col as u16,
                row as u16,
                final_color,
            );
            let bg_color = mix_rgb(accent, final_color, blend);
            let text_color = mix_rgb(fg, accent, (1.0 - blend) * 0.35);

            let cell = &mut buf[(area.x + col, area.y + row)];
            cell.set_symbol(" ");
            cell.set_bg(bg_color);
            cell.set_fg(text_color);
        }
    }
}

fn reveal_coverage(
    progress: f32,
    x: f32,
    y: f32,
    col: u16,
    row: u16,
    variant: RevealVariant,
) -> f32 {
    let p = progress.clamp(0.0, 1.0);
    let base = match variant {
        RevealVariant::GlitchSweep => {
            let sweep = p * 1.35 - x - 0.1;
            let wave = ((y * 6.0 + p * 12.0).sin()) * 0.1;
            sweep + wave
        }
        RevealVariant::VertDrift => {
            let sweep = p * 1.28 - x - 0.12;
            let sway = ((y * 5.0 + p * 6.0).cos()) * 0.09;
            sweep + sway
        }
        RevealVariant::DiagonalPulse => {
            let diag = x * 0.85 + y * 0.2;
            let sweep = p * 1.4 - diag - 0.1;
            let pulse = (((x + y) * std::f32::consts::PI * 2.0) + p * 7.5).sin() * 0.1;
            sweep + pulse
        }
        RevealVariant::ChromaticScan => {
            let sweep = p * 1.22 - x - 0.18 - (0.5 - y).abs() * 0.22;
            let ripple = ((p * 10.0 + y * 5.0).cos()) * 0.07;
            sweep + ripple
        }
        RevealVariant::SparkleFade => {
            let sparkle = hash_noise(col, row);
            let flicker = ((p * 14.0 + (x + y) * 6.0).sin()) * 0.07;
            p * 1.55 - x - 0.22 + sparkle * 0.32 + flicker
        }
        RevealVariant::RainbowBloom => {
            let center = (x - 0.5).abs();
            let sweep = (p * 1.4 - center).powf(1.2) - 0.2;
            let ripple = ((y * 4.0 + p * 9.0).sin()) * 0.08;
            sweep + ripple
        }
        RevealVariant::AuroraBridge => {
            let arch = (x - 0.5).abs() * 1.4 + (y - 0.5).abs() * 0.6;
            let sweep = p * 1.35 - arch - 0.15;
            let shimmer = ((x * 12.0 + p * 10.0).cos()) * 0.05;
            sweep + shimmer
        }
        RevealVariant::PrismRise => {
            let radial = ((x - 0.5).powi(2) + (y - 0.5).powi(2)).sqrt();
            let sweep = p * 1.45 - radial - 0.1;
            sweep + ((y * 8.0 + p * 6.0).sin()) * 0.06
        }
        RevealVariant::NeonRoad => {
            let road = (x - 0.5).abs() * 1.1 + (y - 0.65).abs() * 0.3;
            let sweep = (p * 1.5 - road) - 0.18;
            sweep + ((x * 6.0 + p * 8.0).sin()) * 0.04
        }
        RevealVariant::HorizonRush => {
            let band = (x - 0.5).abs() * 0.9 + (y * 0.45);
            let horizon = (p * 1.32 - band) - 0.12;
            horizon + ((y * 7.0 + p * 5.0).cos()) * 0.05
        }
    };

    smoothstep(-0.15, 0.95, base)
}

fn accent_color(
    variant: RevealVariant,
    progress: f32,
    x: f32,
    y: f32,
    col: u16,
    row: u16,
    final_color: Color,
) -> Color {
    let seed = (x * 0.7 + y * 0.3 + progress * 0.9).fract();
    let base = gradient_multi(seed);

    match variant {
        RevealVariant::GlitchSweep => {
            let jitter = ((y * 12.0 + progress * 16.0).sin() + 1.0) * 0.25;
            mix_rgb(base, final_color, 0.3 + jitter.clamp(0.0, 0.4))
        }
        RevealVariant::VertDrift => {
            let lift = ((x * 8.0 + progress * 10.0).cos() + 1.0) * 0.2;
            mix_rgb(base, final_color, 0.35 + lift.clamp(0.0, 0.3))
        }
        RevealVariant::DiagonalPulse => {
            let pulse = (((x + y) * std::f32::consts::PI * 2.0) + progress * 6.0).sin().abs();
            let glow = mix_rgb(base, Color::Rgb(255, 255, 255), 0.4 * pulse);
            mix_rgb(glow, final_color, 0.45)
        }
        RevealVariant::ChromaticScan => {
            let scan = ((progress * 12.0 + x * 10.0).cos() + 1.0) * 0.25;
            let cool = mix_rgb(base, Color::Rgb(180, 220, 255), 0.5);
            mix_rgb(cool, final_color, 0.4 + scan.clamp(0.0, 0.3))
        }
        RevealVariant::SparkleFade => {
            let sparkle = hash_noise(col, row);
            let white = Color::Rgb(255, 255, 255);
            let glint = mix_rgb(base, white, (0.6 - progress * 0.4).max(0.0) * sparkle);
            mix_rgb(glint, final_color, 0.5)
        }
        RevealVariant::RainbowBloom => {
            let bloom = ((progress * 9.0 + x * 6.0).cos() + 1.0) * 0.35;
            let neon = mix_rgb(base, Color::Rgb(255, 180, 255), bloom * 0.3);
            mix_rgb(neon, final_color, 0.45)
        }
        RevealVariant::AuroraBridge => {
            let arch = ((y * 9.0 + progress * 8.0).sin() + 1.0) * 0.28;
            let glow = mix_rgb(base, Color::Rgb(140, 200, 255), arch * 0.4);
            mix_rgb(glow, final_color, 0.4)
        }
        RevealVariant::PrismRise => {
            let shimmer = ((x * 7.0 + y * 5.0 + progress * 6.0).sin() + 1.0) * 0.3;
            let prism = mix_rgb(base, Color::Rgb(255, 210, 120), shimmer * 0.4);
            mix_rgb(prism, final_color, 0.42)
        }
        RevealVariant::NeonRoad => {
            let strip = ((x * 12.0 + progress * 14.0).sin() + 1.0) * 0.25;
            let warm = mix_rgb(base, Color::Rgb(255, 120, 80), strip * 0.35);
            mix_rgb(warm, final_color, 0.38)
        }
        RevealVariant::HorizonRush => {
            let wash = ((y * 6.0 + progress * 7.0).cos() + 1.0) * 0.22;
            let dawn = mix_rgb(base, Color::Rgb(255, 200, 150), wash * 0.4);
            mix_rgb(dawn, final_color, 0.36)
        }
    }
}

fn hash_noise(x: u16, y: u16) -> f32 {
    let mut n = (x as u32).wrapping_mul(73856093) ^ (y as u32).wrapping_mul(19349663);
    n ^= n >> 13;
    n = n.wrapping_mul(1274126177);
    ((n >> 10) & 0xffff) as f32 / 65535.0
}

fn smoothstep(e0: f32, e1: f32, x: f32) -> f32 {
    if (e1 - e0).abs() < f32::EPSILON {
        return 0.0;
    }
    let t = ((x - e0) / (e1 - e0)).clamp(0.0, 1.0);
    t * t * (3.0 - 2.0 * t)
}

const BODY_PARAGRAPHS: &[&str] = &[
    "Scan the codebase to identify all tracing targets and log statements related to diagnostics. Produce a short guide with exact RUST_LOG filters (for example, module targets), expected example log lines when diagnostics are active, and a brief note on why no LLM content appears by design.",
];

const LEGACY_NAMES: &[&str] = &[
    "Legacy 01",
    "Legacy 02",
    "Legacy 03",
    "Legacy 04",
    "Legacy 05",
    "Legacy 06",
    "Legacy 07",
    "Legacy 08",
    "Legacy 09",
    "Legacy 10",
    "Legacy 11",
    "Legacy 12",
    "Legacy 13",
    "Legacy 14",
    "Legacy 15",
    "Legacy 16",
    "Legacy 17",
    "Legacy 18",
    "Legacy 19",
    "Legacy 20",
    "Legacy 21",
    "Legacy 22",
    "Legacy 23",
    "Legacy 24",
    "Legacy 25",
    "Legacy 26",
    "Legacy 27",
    "Legacy 28",
    "Legacy 29",
    "Legacy 30",
    "Legacy 31",
    "Legacy 32",
    "Legacy 33",
    "Legacy 34",
    "Legacy 35",
    "Legacy 36",
];

const EXPERIMENTAL_NAMES: &[&str] = &[
    "Rainbow 01",
    "Rainbow 02",
    "Rainbow 03",
    "Rainbow 04",
    "Rainbow 05",
];

#[allow(dead_code)]
const LEGACY_PREVIEWS: &[PreviewSpec] = &[
    // Static variants first so animated entries render last (closest to viewport)
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(255, 210, 190),
            right: Color::Rgb(255, 245, 228),
            bias: -0.1,
        },
        border_color: Color::Rgb(134, 72, 28),
        text_color: Color::Rgb(96, 36, 12),
        title_color: Color::Rgb(108, 44, 16),
        footer_color: Color::Rgb(116, 50, 20),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(190, 192, 198),
            right: Color::Rgb(198, 200, 206),
            bias: 0.3,
        },
        border_color: Color::Rgb(60, 90, 70),
        text_color: Color::Rgb(20, 22, 28),
        title_color: Color::Rgb(30, 32, 38),
        footer_color: Color::Rgb(38, 42, 48),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(220, 160, 232),
            right: Color::Rgb(170, 110, 190),
            bias: 0.0,
        },
        border_color: Color::Rgb(80, 36, 122),
        text_color: Color::Rgb(40, 16, 60),
        title_color: Color::Rgb(48, 20, 70),
        footer_color: Color::Rgb(56, 24, 80),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(240, 200, 255),
            right: Color::Rgb(150, 90, 170),
            bias: -0.05,
        },
        border_color: Color::Rgb(92, 42, 132),
        text_color: Color::Rgb(50, 22, 78),
        title_color: Color::Rgb(58, 28, 90),
        footer_color: Color::Rgb(66, 34, 100),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(214, 216, 222),
            right: Color::Rgb(220, 222, 230),
            bias: 0.25,
        },
        border_color: Color::Rgb(90, 94, 102),
        text_color: Color::Rgb(34, 36, 46),
        title_color: Color::Rgb(44, 46, 56),
        footer_color: Color::Rgb(52, 54, 64),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(92, 64, 181),
            right: Color::Rgb(150, 80, 196),
            bias: 0.1,
        },
        border_color: Color::Rgb(206, 173, 255),
        text_color: Color::Rgb(239, 223, 255),
        title_color: Color::Rgb(206, 173, 255),
        footer_color: Color::Rgb(206, 173, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(32, 86, 132),
            right: Color::Rgb(24, 148, 182),
            bias: 0.15,
        },
        border_color: Color::Rgb(122, 247, 255),
        text_color: Color::Rgb(208, 255, 255),
        title_color: Color::Rgb(122, 247, 255),
        footer_color: Color::Rgb(122, 247, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(255, 160, 90),
            right: Color::Rgb(120, 35, 10),
            bias: 0.2,
        },
        border_color: Color::Rgb(255, 136, 85),
        text_color: Color::Rgb(255, 216, 188),
        title_color: Color::Rgb(255, 136, 85),
        footer_color: Color::Rgb(255, 136, 85),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(30, 60, 115),
            right: Color::Rgb(10, 20, 45),
            bias: -0.05,
        },
        border_color: Color::Rgb(96, 156, 255),
        text_color: Color::Rgb(170, 210, 255),
        title_color: Color::Rgb(96, 156, 255),
        footer_color: Color::Rgb(96, 156, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(36, 96, 80),
            right: Color::Rgb(120, 225, 140),
            bias: 0.05,
        },
        border_color: Color::Rgb(196, 255, 208),
        text_color: Color::Rgb(206, 255, 210),
        title_color: Color::Rgb(196, 255, 208),
        footer_color: Color::Rgb(196, 255, 208),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(255, 135, 70),
            right: Color::Rgb(255, 220, 180),
            bias: -0.1,
        },
        border_color: Color::Rgb(255, 210, 180),
        text_color: Color::Rgb(255, 230, 205),
        title_color: Color::Rgb(255, 210, 180),
        footer_color: Color::Rgb(255, 210, 180),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(22, 22, 24),
            right: Color::Rgb(30, 34, 32),
            bias: 0.3,
        },
        border_color: Color::Rgb(140, 255, 155),
        text_color: Color::Rgb(195, 252, 210),
        title_color: Color::Rgb(140, 255, 155),
        footer_color: Color::Rgb(140, 255, 155),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(120, 40, 130),
            right: Color::Rgb(52, 8, 62),
            bias: 0.0,
        },
        border_color: Color::Rgb(224, 160, 255),
        text_color: Color::Rgb(240, 215, 255),
        title_color: Color::Rgb(224, 160, 255),
        footer_color: Color::Rgb(224, 160, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(168, 88, 255),
            right: Color::Rgb(28, 0, 60),
            bias: -0.05,
        },
        border_color: Color::Rgb(255, 180, 255),
        text_color: Color::Rgb(246, 214, 255),
        title_color: Color::Rgb(255, 180, 255),
        footer_color: Color::Rgb(255, 180, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(42, 44, 48),
            right: Color::Rgb(48, 50, 56),
            bias: 0.25,
        },
        border_color: Color::Rgb(134, 138, 144),
        text_color: Color::Rgb(190, 194, 202),
        title_color: Color::Rgb(134, 138, 144),
        footer_color: Color::Rgb(134, 138, 144),
        reveal: None,
    },
    // Animated variants moved to the end so they appear near the user's viewport
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(255, 210, 190),
            right: Color::Rgb(255, 245, 228),
            bias: -0.1,
        },
        border_color: Color::Rgb(134, 72, 28),
        text_color: Color::Rgb(96, 36, 12),
        title_color: Color::Rgb(108, 44, 16),
        footer_color: Color::Rgb(116, 50, 20),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(190, 192, 198),
            right: Color::Rgb(198, 200, 206),
            bias: 0.3,
        },
        border_color: Color::Rgb(60, 90, 70),
        text_color: Color::Rgb(20, 22, 28),
        title_color: Color::Rgb(30, 32, 38),
        footer_color: Color::Rgb(38, 42, 48),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(220, 160, 232),
            right: Color::Rgb(170, 110, 190),
            bias: 0.0,
        },
        border_color: Color::Rgb(80, 36, 122),
        text_color: Color::Rgb(40, 16, 60),
        title_color: Color::Rgb(48, 20, 70),
        footer_color: Color::Rgb(56, 24, 80),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(240, 200, 255),
            right: Color::Rgb(150, 90, 170),
            bias: -0.05,
        },
        border_color: Color::Rgb(92, 42, 132),
        text_color: Color::Rgb(50, 22, 78),
        title_color: Color::Rgb(58, 28, 90),
        footer_color: Color::Rgb(66, 34, 100),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(214, 216, 222),
            right: Color::Rgb(220, 222, 230),
            bias: 0.25,
        },
        border_color: Color::Rgb(90, 94, 102),
        text_color: Color::Rgb(34, 36, 46),
        title_color: Color::Rgb(44, 46, 56),
        footer_color: Color::Rgb(52, 54, 64),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(92, 64, 181),
            right: Color::Rgb(150, 80, 196),
            bias: 0.1,
        },
        border_color: Color::Rgb(206, 173, 255),
        text_color: Color::Rgb(239, 223, 255),
        title_color: Color::Rgb(206, 173, 255),
        footer_color: Color::Rgb(206, 173, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(32, 86, 132),
            right: Color::Rgb(24, 148, 182),
            bias: 0.15,
        },
        border_color: Color::Rgb(122, 247, 255),
        text_color: Color::Rgb(208, 255, 255),
        title_color: Color::Rgb(122, 247, 255),
        footer_color: Color::Rgb(122, 247, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(255, 160, 90),
            right: Color::Rgb(120, 35, 10),
            bias: 0.2,
        },
        border_color: Color::Rgb(255, 136, 85),
        text_color: Color::Rgb(255, 216, 188),
        title_color: Color::Rgb(255, 136, 85),
        footer_color: Color::Rgb(255, 136, 85),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(30, 60, 115),
            right: Color::Rgb(10, 20, 45),
            bias: -0.05,
        },
        border_color: Color::Rgb(96, 156, 255),
        text_color: Color::Rgb(170, 210, 255),
        title_color: Color::Rgb(96, 156, 255),
        footer_color: Color::Rgb(96, 156, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(36, 96, 80),
            right: Color::Rgb(120, 225, 140),
            bias: 0.05,
        },
        border_color: Color::Rgb(196, 255, 208),
        text_color: Color::Rgb(206, 255, 210),
        title_color: Color::Rgb(196, 255, 208),
        footer_color: Color::Rgb(196, 255, 208),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(255, 135, 70),
            right: Color::Rgb(255, 220, 180),
            bias: -0.1,
        },
        border_color: Color::Rgb(255, 210, 180),
        text_color: Color::Rgb(255, 230, 205),
        title_color: Color::Rgb(255, 210, 180),
        footer_color: Color::Rgb(255, 210, 180),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(22, 22, 24),
            right: Color::Rgb(30, 34, 32),
            bias: 0.3,
        },
        border_color: Color::Rgb(140, 255, 155),
        text_color: Color::Rgb(195, 252, 210),
        title_color: Color::Rgb(140, 255, 155),
        footer_color: Color::Rgb(140, 255, 155),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(120, 40, 130),
            right: Color::Rgb(52, 8, 62),
            bias: 0.0,
        },
        border_color: Color::Rgb(224, 160, 255),
        text_color: Color::Rgb(240, 215, 255),
        title_color: Color::Rgb(224, 160, 255),
        footer_color: Color::Rgb(224, 160, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(168, 88, 255),
            right: Color::Rgb(28, 0, 60),
            bias: -0.05,
        },
        border_color: Color::Rgb(255, 180, 255),
        text_color: Color::Rgb(246, 214, 255),
        title_color: Color::Rgb(255, 180, 255),
        footer_color: Color::Rgb(255, 180, 255),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(42, 44, 48),
            right: Color::Rgb(48, 50, 56),
            bias: 0.25,
        },
        border_color: Color::Rgb(134, 138, 144),
        text_color: Color::Rgb(190, 194, 202),
        title_color: Color::Rgb(134, 138, 144),
        footer_color: Color::Rgb(134, 138, 144),
        reveal: None,
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(216, 190, 255),
            right: Color::Rgb(248, 228, 255),
            bias: 0.1,
        },
        border_color: Color::Rgb(82, 48, 164),
        text_color: Color::Rgb(20, 12, 54),
        title_color: Color::Rgb(30, 18, 72),
        footer_color: Color::Rgb(36, 22, 86),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(650),
            variant: RevealVariant::GlitchSweep,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(160, 210, 240),
            right: Color::Rgb(184, 232, 255),
            bias: 0.15,
        },
        border_color: Color::Rgb(32, 96, 140),
        text_color: Color::Rgb(12, 34, 52),
        title_color: Color::Rgb(18, 48, 70),
        footer_color: Color::Rgb(24, 60, 86),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(700),
            variant: RevealVariant::VertDrift,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(255, 220, 200),
            right: Color::Rgb(220, 130, 100),
            bias: 0.2,
        },
        border_color: Color::Rgb(140, 64, 30),
        text_color: Color::Rgb(78, 26, 4),
        title_color: Color::Rgb(92, 34, 6),
        footer_color: Color::Rgb(104, 40, 10),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(680),
            variant: RevealVariant::DiagonalPulse,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(150, 190, 230),
            right: Color::Rgb(110, 140, 190),
            bias: -0.05,
        },
        border_color: Color::Rgb(40, 70, 120),
        text_color: Color::Rgb(16, 22, 44),
        title_color: Color::Rgb(26, 34, 70),
        footer_color: Color::Rgb(32, 42, 82),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(720),
            variant: RevealVariant::ChromaticScan,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(170, 210, 190),
            right: Color::Rgb(220, 255, 230),
            bias: 0.05,
        },
        border_color: Color::Rgb(48, 102, 82),
        text_color: Color::Rgb(18, 40, 32),
        title_color: Color::Rgb(26, 52, 40),
        footer_color: Color::Rgb(34, 64, 50),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(760),
            variant: RevealVariant::SparkleFade,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(160, 210, 240),
            right: Color::Rgb(184, 232, 255),
            bias: 0.15,
        },
        border_color: Color::Rgb(32, 96, 140),
        text_color: Color::Rgb(12, 34, 52),
        title_color: Color::Rgb(18, 48, 70),
        footer_color: Color::Rgb(24, 60, 86),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(680),
            variant: RevealVariant::DiagonalPulse,
        }),
    },
];

const EXPERIMENTAL_RAINBOW_ROAD_PREVIEWS: &[PreviewSpec] = &[
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(48, 12, 96),
            right: Color::Rgb(210, 120, 255),
            bias: -0.05,
        },
        border_color: Color::Rgb(255, 240, 255),
        text_color: Color::Rgb(250, 230, 255),
        title_color: Color::Rgb(255, 244, 255),
        footer_color: Color::Rgb(255, 244, 255),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(720),
            variant: RevealVariant::RainbowBloom,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(18, 38, 78),
            right: Color::Rgb(34, 168, 210),
            bias: 0.1,
        },
        border_color: Color::Rgb(146, 224, 255),
        text_color: Color::Rgb(220, 244, 255),
        title_color: Color::Rgb(190, 236, 255),
        footer_color: Color::Rgb(190, 236, 255),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(680),
            variant: RevealVariant::AuroraBridge,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(16, 22, 18),
            right: Color::Rgb(190, 230, 140),
            bias: 0.2,
        },
        border_color: Color::Rgb(210, 255, 200),
        text_color: Color::Rgb(220, 255, 220),
        title_color: Color::Rgb(232, 255, 232),
        footer_color: Color::Rgb(232, 255, 232),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(640),
            variant: RevealVariant::PrismRise,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(28, 20, 10),
            right: Color::Rgb(255, 135, 70),
            bias: -0.08,
        },
        border_color: Color::Rgb(255, 182, 120),
        text_color: Color::Rgb(255, 230, 210),
        title_color: Color::Rgb(255, 210, 178),
        footer_color: Color::Rgb(255, 210, 178),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(700),
            variant: RevealVariant::NeonRoad,
        }),
    },
    PreviewSpec {
        body: BODY_PARAGRAPHS,
        gradient: GradientSpec {
            left: Color::Rgb(8, 26, 42),
            right: Color::Rgb(255, 190, 110),
            bias: 0.0,
        },
        border_color: Color::Rgb(255, 220, 160),
        text_color: Color::Rgb(255, 244, 222),
        title_color: Color::Rgb(255, 230, 194),
        footer_color: Color::Rgb(255, 230, 194),
        reveal: Some(RevealConfig {
            duration: Duration::from_millis(760),
            variant: RevealVariant::HorizonRush,
        }),
    },
];
