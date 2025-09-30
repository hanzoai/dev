use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::auto_drive_strings;
use crate::colors;
use crate::header_wave::{HeaderBorderWeaveEffect, HeaderWaveEffect};
use crate::spinner;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph, Widget, Wrap};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use super::{
    bottom_pane_view::{BottomPaneView, ConditionalUpdate},
    chat_composer::ChatComposer,
};

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
    status_message: Option<String>,
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
            status_message: None,
        }
    }

    pub fn update_model(&mut self, model: AutoCoordinatorViewModel) {
        self.model = model;
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

    fn normalize_status_message(message: &str) -> Option<String> {
        let mapped = ChatComposer::map_status_message(message);
        let trimmed = mapped.trim();
        if trimmed.is_empty() {
            None
        } else {
            Some(trimmed.to_string())
        }
    }

    fn update_status_message(&mut self, message: String) -> bool {
        let new_value = Self::normalize_status_message(&message);
        if self.status_message.as_deref() == new_value.as_deref() {
            return false;
        }
        self.status_message = new_value;
        true
    }

    fn status_message_for_display(message: &str) -> Option<String> {
        let trimmed = message.trim();
        if trimmed.is_empty() {
            None
        } else if trimmed.ends_with("...") || trimmed.ends_with('…') {
            Some(trimmed.to_string())
        } else {
            Some(format!("{trimmed}..."))
        }
    }

    fn spinner_should_run(&self) -> bool {
        self.model.cli_running
    }

    fn overlay_text(&self, spinner_symbol: &str) -> Option<String> {
        let message = self
            .status_message
            .as_ref()
            .and_then(|msg| Self::status_message_for_display(msg))
            .unwrap_or_else(|| "Working...".to_string());
        if message.is_empty() {
            None
        } else {
            Some(format!(" {spinner_symbol} {message} "))
        }
    }

    fn render_frame(
        &self,
        area: Rect,
        buf: &mut Buffer,
        now: Instant,
        overlay: Option<&str>,
    ) -> Option<Rect> {
        const BASE_TITLE: &str = " Auto Drive ";
        if area.width < 3 || area.height < 3 {
            return None;
        }
        let title_span = Span::styled(
            BASE_TITLE,
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
        for (offset, ch) in BASE_TITLE.chars().enumerate() {
            let x = title_start + offset as u16;
            if x >= area.x && x < area.x.saturating_add(area.width) {
                let mut ch_buf = [0u8; 4];
                let symbol = ch.encode_utf8(&mut ch_buf);
                let cell = &mut buf[(x, title_y)];
                cell.set_symbol(symbol);
                cell.set_style(title_style);
            }
        }

        if let Some(text) = overlay {
            self.render_title_overlay(area, buf, text);
        }

        Some(Rect {
            x: area.x + 1,
            y: area.y + 1,
            width: area.width.saturating_sub(2),
            height: area.height.saturating_sub(2),
        })
    }

    fn render_title_overlay(&self, area: Rect, buf: &mut Buffer, text: &str) {
        if area.width <= 2 {
            return;
        }
        const BASE_TITLE: &str = " Auto Drive ";
        let overlay_width = UnicodeWidthStr::width(text);
        if overlay_width == 0 {
            return;
        }
        let available = area.width.saturating_sub(2) as usize;
        let trimmed = if overlay_width > available {
            let mut acc = String::new();
            let mut used = 0usize;
            for ch in text.chars() {
                let w = UnicodeWidthChar::width(ch).unwrap_or(0);
                if used + w > available {
                    acc.push('…');
                    break;
                }
                acc.push(ch);
                used += w;
            }
            acc
        } else {
            text.to_string()
        };
        let draw_width = UnicodeWidthStr::width(trimmed.as_str());
        if draw_width == 0 {
            return;
        }
        let base_width = UnicodeWidthStr::width(BASE_TITLE) as u16;
        let base_end = area.x + 1 + base_width;
        let mut start_x = area.x + (area.width.saturating_sub(draw_width as u16)) / 2;
        start_x = start_x.max(base_end);
        if start_x + draw_width as u16 >= area.x.saturating_add(area.width) {
            if area.width > draw_width as u16 + 1 {
                start_x = area.x + area.width - draw_width as u16 - 1;
            } else {
                start_x = area.x + 1;
            }
        }
        let title_y = area.y;
        let style = Style::default()
            .fg(colors::info())
            .add_modifier(Modifier::BOLD);
        let mut x = start_x;
        for ch in trimmed.chars() {
            if x >= area.x.saturating_add(area.width) {
                break;
            }
            let mut ch_buf = [0u8; 4];
            let symbol = ch.encode_utf8(&mut ch_buf);
            let cell = &mut buf[(x, title_y)];
            cell.set_symbol(symbol);
            cell.set_style(style);
            x += UnicodeWidthChar::width(ch).unwrap_or(1) as u16;
        }
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

    fn status_lines_with_entries(&self, entries: &[(String, Style)]) -> Vec<Line<'static>> {
        let mut lines: Vec<Line<'static>> = Vec::new();
        for (index, (text, style)) in entries.iter().enumerate() {
            if index == 0 {
                lines.push(Line::from(vec![
                    Span::raw("   "),
                    Span::styled(text.clone(), *style),
                ]));
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
            for (_, (text, _)) in status_entries.iter().enumerate() {
                total += Self::wrap_count(text, inner_width);
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

        let spinner_active = self.spinner_should_run();
        let spinner_def = spinner::current_spinner();
        let spinner_symbol = if spinner_active {
            let now_ms = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis();
            Some(spinner::frame_at_time(spinner_def, now_ms))
        } else {
            None
        };
        let overlay_text = spinner_symbol
            .as_ref()
            .and_then(|symbol| self.overlay_text(symbol));

        let Some(inner) = self.render_frame(area, buf, now, overlay_text.as_deref()) else {
            return;
        };
        let inner = self.apply_left_padding(inner, buf);
        if inner.height == 0 {
            return;
        }

        let status_entries = self.derived_status_entries();
        let mut lines: Vec<Line<'static>> = Vec::new();

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
            let status_lines = self.status_lines_with_entries(&status_entries);
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

        Paragraph::new(lines)
            .wrap(Wrap { trim: true })
            .render(inner, buf);

        let mut next_interval = if spinner_active {
            Duration::from_millis(spinner_def.interval_ms.max(80))
        } else {
            Duration::from_millis(200)
        };
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

    fn update_status_text(&mut self, text: String) -> ConditionalUpdate {
        if self.update_status_message(text) {
            ConditionalUpdate::NeedsRedraw
        } else {
            ConditionalUpdate::NoRedraw
        }
    }

    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(self)
    }
}
