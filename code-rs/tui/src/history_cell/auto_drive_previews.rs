use super::*;
use crate::card_theme::{self, CardPreviewSpec, RevealVariant};
use crate::gradient_background::{GradientBackground, RevealRender};
use crate::util::buffer::write_line;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::prelude::{Line, Span, Style};
use ratatui::style::Modifier;
use std::time::{Duration, Instant};
use textwrap::{Options as TwOptions, WordSplitter};
use unicode_width::UnicodeWidthStr;

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

pub(crate) fn auto_drive_preview_cells() -> Vec<Box<dyn HistoryCell>> {
    preview_specs()
        .into_iter()
        .map(|spec| Box::new(AutoDrivePreviewCell::new(spec)) as Box<dyn HistoryCell>)
        .collect()
}

pub(crate) fn experimental_auto_drive_preview_count() -> usize {
    card_theme::auto_drive_theme_catalog().len()
}

fn preview_specs() -> Vec<CardPreviewSpec> {
    let static_defs = vec![
        card_theme::search_dark_theme(),
        card_theme::agent_read_only_dark_theme(),
        card_theme::browser_dark_theme(),
        card_theme::agent_write_dark_theme(),
        card_theme::search_light_theme(),
        card_theme::agent_read_only_light_theme(),
        card_theme::browser_light_theme(),
        card_theme::agent_write_light_theme(),
    ];

    let mut specs = static_defs
        .into_iter()
        .map(|definition| definition.preview(BODY_PARAGRAPHS))
        .collect::<Vec<_>>();

    specs.extend(
        card_theme::auto_drive_theme_catalog()
            .into_iter()
            .map(|definition| definition.preview(BODY_PARAGRAPHS)),
    );

    specs
}

struct AutoDrivePreviewCell {
    spec: CardPreviewSpec,
    animation: Option<CardRevealAnimation>,
}

impl AutoDrivePreviewCell {
    fn new(spec: CardPreviewSpec) -> Self {
        let animation = spec
            .theme
            .reveal
            .map(|config| CardRevealAnimation::new(config.duration, config.variant));
        Self { spec, animation }
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

        let palette = self.spec.theme.palette;
        let base_text = palette.text;
        let border_style = Style::default().fg(palette.border);
        let body_style = Style::default().fg(base_text);
        let title_style = Style::default()
            .fg(palette.title)
            .add_modifier(Modifier::BOLD);
        let footer_style = Style::default().fg(palette.footer);

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
            Span::styled(self.spec.name.to_string(), body_style),
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

        let palette = self.spec.theme.palette;
        let base_style = Style::default().fg(palette.text);

        let reveal = self.animation.as_ref().map(|anim| RevealRender {
            progress: anim.progress(),
            variant: anim.variant,
            intro_light: self.spec.name.contains("Light"),
        });

        GradientBackground::render(
            buf,
            area,
            &self.spec.theme.gradient,
            palette.text,
            reveal,
        );

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

const BODY_PARAGRAPHS: &[&str] = &[
    "Scan the codebase to identify all tracing targets and log statements related to diagnostics. Produce a short guide with exact RUST_LOG filters (for example, module targets), expected example log lines when diagnostics are active, and a brief note on why no LLM content appears by design.",
];
