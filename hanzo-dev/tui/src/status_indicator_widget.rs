//! A live status indicator that shows the *latest* log line emitted by the
//! application while the agent is processing a long‑running task.

use std::cell::Cell;
use std::time::Duration;
use std::time::Instant;

use hanzo_core::protocol::Op;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::text::Line;
use ratatui::text::Span;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Widget;
use ratatui::widgets::WidgetRef;

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use textwrap::Options as TwOptions;
use textwrap::WordSplitter;

#[allow(dead_code)]
pub(crate) struct StatusIndicatorWidget {
    /// Animated header text (defaults to "Working").
    header: String,
    /// Queued user messages to display under the status line.
    queued_messages: Vec<String>,

    start_time: Instant,
    /// Last time we scheduled a follow-up frame; used to throttle redraws.
    last_schedule: Cell<Instant>,
    last_rendered_second: Cell<u64>,
    app_event_tx: AppEventSender,
}

#[allow(dead_code)]
impl StatusIndicatorWidget {
    pub(crate) fn new(app_event_tx: AppEventSender) -> Self {
        Self {
            header: String::from("Working"),
            queued_messages: Vec::new(),
            start_time: Instant::now(),
            last_schedule: Cell::new(Instant::now()),
            last_rendered_second: Cell::new(u64::MAX),
            app_event_tx,
        }
    }

    pub fn desired_height(&self, width: u16) -> u16 {
        let inner_width = width.max(1) as usize;
        let mut total: u16 = 2; // leading blank row + status line
        if !self.queued_messages.is_empty() {
            total = total.saturating_add(1);
        }
        let text_width = inner_width.saturating_sub(3);
        if text_width > 0 {
            let opts = TwOptions::new(text_width)
                .break_words(false)
                .word_splitter(WordSplitter::NoHyphenation);
            for q in &self.queued_messages {
                let wrapped = textwrap::wrap(q, &opts);
                let lines = wrapped.len().min(3) as u16;
                total = total.saturating_add(lines);
                if wrapped.len() > 3 {
                    total = total.saturating_add(1);
                }
            }
            if !self.queued_messages.is_empty() {
                total = total.saturating_add(1);
            }
        } else {
            total = total.saturating_add(self.queued_messages.len() as u16);
        }
        total.saturating_add(1)
    }

    pub(crate) fn interrupt(&self) {
        self.app_event_tx.send(AppEvent::CodexOp(Op::Interrupt));
    }

    pub(crate) fn update_header(&mut self, header: String) {
        if self.header != header {
            self.header = header;
        }
    }

    pub(crate) fn set_queued_messages(&mut self, queued: Vec<String>) {
        self.queued_messages = queued;
        self.app_event_tx.send(AppEvent::RequestRedraw);
    }
}

fn fmt_elapsed_compact(elapsed_secs: u64) -> String {
    if elapsed_secs < 60 {
        return format!("{elapsed_secs}s");
    }
    if elapsed_secs < 3600 {
        let minutes = elapsed_secs / 60;
        let seconds = elapsed_secs % 60;
        return format!("{minutes}m {seconds:02}s");
    }
    let hours = elapsed_secs / 3600;
    let minutes = (elapsed_secs % 3600) / 60;
    let seconds = elapsed_secs % 60;
    format!("{hours}h {minutes:02}m {seconds:02}s")
}

/// Build shimmer spans for text: a brightness band sweeps across the characters.
/// Uses the accent color at the peak and text_dim everywhere else, creating a
/// subtle pulsing/sweeping effect on the whole line.
fn shimmer_text(
    text: &str,
    elapsed_ms: u128,
    accent: ratatui::style::Color,
    dim: ratatui::style::Color,
) -> Vec<Span<'static>> {
    let chars: Vec<char> = text.chars().collect();
    if chars.is_empty() {
        return Vec::new();
    }
    let padding = 6usize;
    let period = chars.len() + padding * 2;
    let sweep_secs = 2.0f32;
    let pos_f = ((elapsed_ms as f32 / 1000.0) % sweep_secs) / sweep_secs * (period as f32);
    let band_half = 4.0f32;

    let mut spans = Vec::with_capacity(chars.len());
    for (i, ch) in chars.iter().enumerate() {
        let dist = ((i as f32 + padding as f32) - pos_f).abs();
        let intensity = if dist <= band_half {
            0.5 * (1.0 + (std::f32::consts::PI * dist / band_half).cos())
        } else {
            0.0
        };
        let style = if intensity > 0.5 {
            Style::default().fg(accent).add_modifier(Modifier::BOLD)
        } else if intensity > 0.15 {
            Style::default().fg(accent)
        } else {
            Style::default().fg(dim)
        };
        spans.push(Span::styled(ch.to_string(), style));
    }
    spans
}

#[cfg(all(not(test), target_os = "macos"))]
const ALT_PREFIX: &str = "\u{2315} + ";
#[cfg(all(not(test), not(target_os = "macos")))]
const ALT_PREFIX: &str = "Alt+";
#[cfg(test)]
const ALT_PREFIX: &str = "Alt+";

fn alt_up_hint() -> String {
    format!("{ALT_PREFIX}\u{2191}")
}

impl WidgetRef for StatusIndicatorWidget {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        if area.is_empty() {
            return;
        }

        let viewport = buf.area();
        let area_right = area.x.saturating_add(area.width);
        let area_bottom = area.y.saturating_add(area.height);
        let view_right = viewport.x.saturating_add(viewport.width);
        let view_bottom = viewport.y.saturating_add(viewport.height);
        let intersects = area.x < view_right
            && viewport.x < area_right
            && area.y < view_bottom
            && viewport.y < area_bottom;
        if !intersects {
            return;
        }

        // Schedule animation redraws at ~30fps for smooth shimmer
        let now = Instant::now();
        let elapsed_since_start = self.start_time.elapsed();
        let elapsed = elapsed_since_start.as_secs();
        let elapsed_ms = elapsed_since_start.as_millis();
        let anim_interval = Duration::from_millis(33);

        if elapsed != self.last_rendered_second.get()
            || now.duration_since(self.last_schedule.get()) >= anim_interval
        {
            self.last_schedule.set(now);
            self.last_rendered_second.set(elapsed);
            self.app_event_tx
                .send(AppEvent::ScheduleFrameIn(anim_interval));
        }

        let bg = crate::colors::background();
        let text = crate::colors::text();
        let text_dim = crate::colors::text_dim();
        let accent = crate::colors::text_bright();
        let secondary = Style::default().fg(text_dim).add_modifier(Modifier::DIM);

        let pretty_elapsed = fmt_elapsed_compact(elapsed);
        // Build header spans with shimmer on the header text
        let mut spans: Vec<Span<'static>> = vec![Span::raw(if crate::theme::show_gutter() {
            " "
        } else {
            ""
        })];

        // Simple bullet — hidden when gutter is off
        if crate::theme::show_gutter() {
            // Blink the bullet: alternate • / ◦ every 600ms
            let blink_on = (elapsed_ms / 600) % 2 == 0;
            if blink_on {
                spans.push(Span::styled("• ", Style::default().fg(accent)));
            } else {
                spans.push(Span::styled("• ", secondary));
            }
        }

        // Shimmer the header text for a subtle pulse effect
        spans.extend(shimmer_text(&self.header, elapsed_ms, accent, text_dim));

        // Elapsed time and interrupt hint
        spans.push(Span::raw(" "));
        spans.push(Span::raw(format!("({pretty_elapsed} ")).style(secondary));
        spans.push(Span::raw("• ").style(secondary));
        spans
            .push(Span::raw("esc").style(Style::default().fg(accent).add_modifier(Modifier::BOLD)));
        spans.push(Span::raw(" to interrupt)").style(secondary));

        // Build lines — leading blank row separates status from history above
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::from(""));
        lines.push(Line::from(spans));
        if !self.queued_messages.is_empty() {
            lines.push(Line::from(""));
        }
        let text_width = area.width.saturating_sub(3);
        let opts = TwOptions::new(text_width as usize)
            .break_words(false)
            .word_splitter(WordSplitter::NoHyphenation);
        for q in &self.queued_messages {
            let wrapped = textwrap::wrap(q, &opts);
            for (i, piece) in wrapped.iter().take(3).enumerate() {
                let prefix = if i == 0 { " ↳ " } else { "   " };
                let content = format!("{prefix}{piece}");
                lines.push(
                    Line::from(content).style(
                        Style::default()
                            .fg(text_dim)
                            .add_modifier(Modifier::DIM)
                            .add_modifier(Modifier::ITALIC),
                    ),
                );
            }
            if wrapped.len() > 3 {
                lines.push(
                    Line::from("   …").style(
                        Style::default()
                            .fg(text_dim)
                            .add_modifier(Modifier::DIM)
                            .add_modifier(Modifier::ITALIC),
                    ),
                );
            }
        }
        if !self.queued_messages.is_empty() {
            lines.push(
                Line::from(vec![
                    Span::raw("   "),
                    Span::raw(alt_up_hint())
                        .style(Style::default().fg(accent).add_modifier(Modifier::BOLD)),
                    Span::raw(" edit").style(secondary),
                ])
                .style(Style::default()),
            );
        }

        let paragraph = Paragraph::new(lines).style(Style::default().bg(bg).fg(text));
        paragraph.render(area, buf);
    }
}
