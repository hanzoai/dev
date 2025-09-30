use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::auto_drive_strings;
use crate::colors;
use crate::header_wave::{HeaderBorderWeaveEffect, HeaderWaveEffect};
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph, Widget, Wrap};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};
use std::borrow::Cow;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

const DRIVE_SPINNER_FRAMES: [&str; 10] = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
const DRIVE_SPINNER_INTERVAL_MS: u64 = 120;

static DRIVE_SPINNER_TICK: AtomicUsize = AtomicUsize::new(0);

use super::bottom_pane_view::BottomPaneView;

#[derive(Clone, Debug)]
pub(crate) struct CountdownState {
    pub remaining: u8,
}

#[derive(Clone, Debug)]
pub(crate) struct AutoCoordinatorButton {
    pub label: String,
    pub enabled: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct AutoCoordinatorViewModel {
    #[allow(dead_code)]
    pub goal: Option<String>,
    pub status_lines: Vec<String>,
    pub prompt: Option<String>,
    pub awaiting_submission: bool,
    pub waiting_for_response: bool,
    pub coordinator_waiting: bool,
    pub countdown: Option<CountdownState>,
    pub button: Option<AutoCoordinatorButton>,
    pub manual_hint: Option<String>,
    pub ctrl_switch_hint: String,
    pub cli_running: bool,
}

struct VariantContext {
    button: Option<(String, bool)>,
    ctrl_hint: String,
    manual_hint: Option<String>,
}

pub(crate) struct AutoCoordinatorView {
    model: AutoCoordinatorViewModel,
    app_event_tx: AppEventSender,
    header_wave: HeaderWaveEffect,
    header_border: HeaderBorderWeaveEffect,
}

impl AutoCoordinatorView {
    pub fn new(model: AutoCoordinatorViewModel, app_event_tx: AppEventSender) -> Self {
        let now = Instant::now();
        let header_wave = {
            let effect = HeaderWaveEffect::new();
            effect.set_enabled(false, now);
            effect
        };
        let header_border = {
            let effect = HeaderBorderWeaveEffect::new();
            effect.set_enabled(false, now);
            effect
        };
        Self {
            model,
            app_event_tx,
            header_wave,
            header_border,
        }
    }

    fn build_context(&self) -> VariantContext {
        let button = self
            .model
            .button
            .as_ref()
            .map(|btn| (format_button_text(btn), btn.enabled));
        VariantContext {
            button,
            ctrl_hint: self.model.ctrl_switch_hint.clone(),
            manual_hint: self.model.manual_hint.clone(),
        }
    }

    fn build_title(
        &self,
        area_width: u16,
        spinner_symbol: Option<&str>,
        status_text: Option<&str>,
    ) -> String {
        const BASE_TITLE: &str = " Auto Drive ";
        let base_width = UnicodeWidthStr::width(BASE_TITLE);
        let mut title = String::from(BASE_TITLE);

        let Some(spinner) = spinner_symbol else {
            return title;
        };
        let Some(status) = status_text.map(str::trim) else {
            return title;
        };
        if status.is_empty() {
            return title;
        }

        let inner_width = area_width.saturating_sub(2) as usize;
        if inner_width <= base_width {
            return title;
        }

        let spinner_width = UnicodeWidthStr::width(spinner);
        let max_segment_width = inner_width.saturating_sub(base_width);
        if max_segment_width <= spinner_width + 1 {
            return title;
        }

        let mut final_status = String::new();
        let mut truncated = false;
        let mut used_width = spinner_width + 1; // spinner + single separating space

        for ch in status.chars() {
            let ch_width = UnicodeWidthChar::width(ch).unwrap_or(0);
            if ch_width == 0 {
                continue;
            }
            if used_width + ch_width > max_segment_width {
                truncated = true;
                break;
            }
            final_status.push(ch);
            used_width += ch_width;
        }

        if final_status.is_empty() {
            // Nothing fit besides the spinner – keep default title.
            return title;
        }

        if truncated {
            // Attempt to append an ellipsis without exceeding the available width.
            let ellipsis_width = UnicodeWidthStr::width("…");
            if used_width + ellipsis_width <= max_segment_width {
                final_status.push('…');
            } else if let Some(last) = final_status.pop() {
                // Replace last character if adding ellipsis would overflow.
                let last_width = UnicodeWidthChar::width(last).unwrap_or(0);
                let used_without_last = used_width.saturating_sub(last_width);
                if used_without_last + ellipsis_width <= max_segment_width {
                    final_status.push('…');
                } else {
                    // No room for ellipsis; restore last char to avoid dropping information.
                    final_status.push(last);
                }
            }
        }

        let segment = format!("{spinner} {final_status}");
        let segment_width = UnicodeWidthStr::width(segment.as_str());
        let inner_center = inner_width.saturating_sub(segment_width) / 2;
        let spinner_start = inner_center.max(base_width + 1);
        let pad_width = spinner_start.saturating_sub(base_width);
        title.push_str(&" ".repeat(pad_width));
        title.push_str(&segment);
        title.push(' ');
        title
    }

    fn render_frame(&self, area: Rect, buf: &mut Buffer, title: &str, now: Instant) -> Option<Rect> {
        if area.width < 3 || area.height < 3 {
            return None;
        }
        let title_span = Span::styled(
            title,
            Style::default()
                .fg(colors::text())
                .add_modifier(Modifier::BOLD),
        );
        Block::default()
            .title(title_span)
            .borders(Borders::ALL)
            .border_style(Style::default().fg(colors::border()))
            .render(area, buf);
        if self.header_wave.is_enabled() {
            self.header_wave.render(area, buf, now);
        }
        if self.header_border.is_enabled() {
            self.header_border.render(area, buf, now);
        }
        // Reapply static title styling so animation never recolors it
        let title_style = Style::default()
            .fg(colors::text())
            .add_modifier(Modifier::BOLD);
        let title_y = area.y;
        let title_start = area.x + 1;
        for (offset, ch) in title.chars().enumerate() {
            let x = title_start + offset as u16;
            if x >= area.x && x < area.x.saturating_add(area.width) {
                let mut ch_buf = [0u8; 4];
                let symbol = ch.encode_utf8(&mut ch_buf);
                let cell = &mut buf[(x, title_y)];
                cell.set_symbol(symbol);
                cell.set_style(title_style);
            }
        }
        Some(Rect {
            x: area.x + 1,
            y: area.y + 1,
            width: area.width.saturating_sub(2),
            height: area.height.saturating_sub(2),
        })
    }

    fn derived_status_entries(&self) -> Vec<(String, Style)> {
        let mut entries: Vec<(String, Style)> = Vec::new();

        if self.model.awaiting_submission {
            let text = if let Some(countdown) = &self.model.countdown {
                format!("Auto continue in {}s", countdown.remaining)
            } else {
                "Awaiting confirmation".to_string()
            };
            entries.push((text, Style::default().fg(colors::text_dim())));
        }

        for status in &self.model.status_lines {
            let trimmed = status.trim();
            if trimmed.is_empty() {
                continue;
            }
            entries.push((
                status.clone(),
                Style::default().fg(colors::text_dim()),
            ));
        }

        if entries.is_empty() {
            entries.push((
                auto_drive_strings::next_auto_drive_phrase().to_string(),
                Style::default().fg(colors::text_dim()),
            ));
        }

        entries
    }

    fn status_lines_with_entries(
        &self,
        entries: &[(String, Style)],
        spinner_symbol: Option<&str>,
    ) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();
        for (index, (text, style)) in entries.iter().enumerate() {
            if index == 0 {
                if let Some(symbol) = spinner_symbol {
                    lines.push(Line::from(vec![
                        Span::styled(
                            symbol.to_string(),
                            Style::default()
                                .fg(colors::spinner())
                                .add_modifier(Modifier::BOLD),
                        ),
                        Span::raw("  "),
                        Span::styled(text.clone(), *style),
                    ]));
                } else {
                    lines.push(Line::from(vec![
                        Span::raw("   "),
                        Span::styled(text.clone(), *style),
                    ]));
                }
            } else {
                lines.push(Line::from(Span::styled(text.clone(), *style)));
            }
        }
        lines
    }

    fn prompt_lines(&self, style: Style) -> Option<Vec<Line<'static>>> {
        self.model.prompt.as_ref().map(|prompt| {
            prompt
                .lines()
                .map(|line| Line::from(Span::styled(line.trim_end().to_string(), style)))
                .collect()
        })
    }

    fn button_line(&self, ctx: &VariantContext) -> Option<Line<'static>> {
        ctx.button.as_ref().map(|(text, enabled)| {
            let style = if *enabled {
                Style::default()
                    .fg(colors::primary())
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(colors::text_dim())
            };
            Line::from(Span::styled(text.clone(), style))
        })
    }

    fn manual_hint_line(&self, ctx: &VariantContext) -> Option<Line<'static>> {
        ctx.manual_hint.as_ref().map(|hint| {
            Line::from(Span::styled(
                hint.clone(),
                Style::default().fg(colors::warning()),
            ))
        })
    }

    fn ctrl_hint_line(&self, ctx: &VariantContext) -> Option<Line<'static>> {
        if ctx.ctrl_hint.trim().is_empty() {
            return None;
        }
        Some(Line::from(Span::styled(
            ctx.ctrl_hint.clone(),
            Style::default()
                .fg(colors::text_dim())
                .add_modifier(Modifier::ITALIC),
        )))
    }

    fn inner_width(&self, width: u16) -> u16 {
        width
            .saturating_sub(3) // borders + left padding
            .max(1)
    }

    fn wrap_count(text: &str, width: u16) -> usize {
        if width == 0 {
            return text.lines().count().max(1);
        }
        let max_width = width as usize;
        text.lines()
            .map(|line| {
                let trimmed = line.trim_end();
                let w = UnicodeWidthStr::width(trimmed);
                let lines = if w == 0 {
                    1
                } else {
                    (w + max_width - 1) / max_width
                };
                lines.max(1)
            })
            .sum()
    }

    fn estimated_height(&self, width: u16, ctx: &VariantContext) -> u16 {
        let inner_width = self.inner_width(width);
        let button_height = ctx
            .button
            .as_ref()
            .map(|(text, _)| Self::wrap_count(text, inner_width))
            .unwrap_or(0);
        let hint_height = ctx
            .manual_hint
            .as_ref()
            .map(|text| Self::wrap_count(text, inner_width))
            .unwrap_or(0);
        let ctrl_hint = ctx.ctrl_hint.trim();
        let ctrl_height = if ctrl_hint.is_empty() {
            0
        } else {
            Self::wrap_count(ctrl_hint, inner_width)
        };

        let awaiting = self.model.awaiting_submission;
        let spinner_active = !awaiting && self.model.coordinator_waiting;

        let mut total = 0;

        if awaiting {
            if let Some(prompt) = &self.model.prompt {
                total += Self::wrap_count(prompt, inner_width).max(1);
            }
            if ctx.button.is_some() {
                total += 1; // spacer before button
                total += button_height.max(1);
            }
            if ctrl_height > 0 {
                total += 1; // spacer before ctrl hint
                total += ctrl_height.max(1);
            }
        } else {
            let status_entries = self.derived_status_entries();
            for (index, (text, _)) in status_entries.iter().enumerate() {
                let measure: Cow<'_, str> = if index == 0 {
                    Cow::Owned(format!("{}  {}", DRIVE_SPINNER_FRAMES[0], text))
                } else {
                    Cow::Borrowed(text.as_str())
                };
                total += Self::wrap_count(measure.as_ref(), inner_width);
            }
            if ctx.button.is_some() {
                total += 1; // spacer before button
                total += button_height.max(1);
            }
            if ctx.manual_hint.is_some() {
                total += hint_height.max(1);
            }
            if ctrl_height > 0 {
                total += 1; // spacer before ctrl hint
                total += ctrl_height.max(1);
            }
        }

        if spinner_active {
            total = total.saturating_add(2);
        }

        total
            .saturating_add(2) // frame borders
            .min(u16::MAX as usize) as u16
    }

    fn render_internal(&self, area: Rect, buf: &mut Buffer, ctx: &VariantContext) {
        self.render_classic(area, buf, ctx);
    }

    fn render_classic(&self, area: Rect, buf: &mut Buffer, ctx: &VariantContext) {
        let now = Instant::now();
        let waiting = self.model.waiting_for_response;
        let mut frame_needed = false;

        let should_enable_border = waiting && !self.model.cli_running;
        let mut border_enabled = self.header_border.is_enabled();
        if should_enable_border && !border_enabled {
            self.header_border.set_enabled(true, now);
            border_enabled = true;
        } else if !should_enable_border && border_enabled {
            self.header_border.set_enabled(false, now);
            border_enabled = false;
        }

        if border_enabled && self.header_border.schedule_if_needed(now) {
            frame_needed = true;
        }

        let spinner_active = !self.model.awaiting_submission && self.model.coordinator_waiting;
        let spinner_symbol = spinner_active.then(|| {
            let frame_idx = DRIVE_SPINNER_TICK.fetch_add(1, Ordering::Relaxed);
            DRIVE_SPINNER_FRAMES[frame_idx % DRIVE_SPINNER_FRAMES.len()].to_string()
        });
        let status_entries = self.derived_status_entries();
        let status_title_text = status_entries.first().map(|(text, _)| text.clone());
        let title = self.build_title(
            area.width,
            spinner_symbol.as_deref(),
            status_title_text.as_deref(),
        );

        let Some(inner) = self.render_frame(area, buf, &title, now) else {
            return;
        };
        let inner = self.apply_left_padding(inner, buf);
        if inner.height == 0 {
            return;
        }

        let mut lines: Vec<Line<'static>> = Vec::new();

        if spinner_active {
            lines.push(Line::default());
        }

        if self.model.awaiting_submission {
            if let Some(prompt_lines) =
                self.prompt_lines(Style::default().fg(colors::text_dim()))
            {
                lines.extend(prompt_lines);
            }

            if let Some(button_line) = self.button_line(ctx) {
                lines.push(Line::default());
                lines.push(button_line);
            }

            if let Some(ctrl_hint_line) = self.ctrl_hint_line(ctx) {
                lines.push(Line::default());
                lines.push(ctrl_hint_line);
            }
        } else {
            let status_lines =
                self.status_lines_with_entries(&status_entries, spinner_symbol.as_deref());
            lines.extend(status_lines);

            if let Some(button_line) = self.button_line(ctx) {
                lines.push(Line::default());
                lines.push(button_line);
            }

            if let Some(hint_line) = self.manual_hint_line(ctx) {
                lines.push(hint_line);
            }

            if let Some(ctrl_hint_line) = self.ctrl_hint_line(ctx) {
                lines.push(Line::default());
                lines.push(ctrl_hint_line);
            }
        }

        if spinner_active {
            lines.push(Line::default());
        }

        Paragraph::new(lines)
            .wrap(Wrap { trim: true })
            .render(inner, buf);

        let mut next_interval = Duration::from_millis(DRIVE_SPINNER_INTERVAL_MS);
        if frame_needed {
            next_interval = next_interval.min(HeaderBorderWeaveEffect::FRAME_INTERVAL);
        }
        self.app_event_tx
            .send(AppEvent::ScheduleFrameIn(next_interval));
    }

    fn apply_left_padding(&self, area: Rect, buf: &mut Buffer) -> Rect {
        if area.width <= 1 {
            return area;
        }
        let bg_style = Style::default()
            .bg(colors::background())
            .fg(colors::text());
        for y in area.y..area.y.saturating_add(area.height) {
            let cell = &mut buf[(area.x, y)];
            cell.set_symbol(" ");
            cell.set_style(bg_style);
        }
        Rect {
            x: area.x + 1,
            y: area.y,
            width: area.width.saturating_sub(1),
            height: area.height,
        }
    }
}

fn format_button_text(button: &AutoCoordinatorButton) -> String {
    if button.enabled {
        format!("[{}]", button.label)
    } else {
        button.label.clone()
    }
}

impl<'a> BottomPaneView<'a> for AutoCoordinatorView {
    fn desired_height(&self, width: u16) -> u16 {
        let ctx = self.build_context();
        self.estimated_height(width, &ctx)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let ctx = self.build_context();
        if area.height == 0 {
            return;
        }

        self.render_internal(area, buf, &ctx);
    }
}
