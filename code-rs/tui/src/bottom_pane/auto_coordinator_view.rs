use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::auto_drive_strings;
use crate::colors;
use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::prelude::Widget;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph, WidgetRef, Wrap};
use unicode_width::UnicodeWidthStr;
use std::time::{Duration, Instant};

use super::{
    bottom_pane_view::{BottomPaneView, ConditionalUpdate},
    chat_composer::ChatComposer,
    BottomPane,
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
pub(crate) struct AutoActiveViewModel {
    #[allow(dead_code)]
    pub goal: Option<String>,
    pub status_lines: Vec<String>,
    pub cli_prompt: Option<String>,
    pub awaiting_submission: bool,
    pub waiting_for_response: bool,
    pub countdown: Option<CountdownState>,
    pub button: Option<AutoCoordinatorButton>,
    pub manual_hint: Option<String>,
    pub ctrl_switch_hint: String,
    pub cli_running: bool,
    pub review_enabled: bool,
    pub agents_enabled: bool,
    pub turns_completed: usize,
    pub started_at: Option<Instant>,
    pub elapsed: Option<Duration>,
    pub progress_past: Option<String>,
    pub progress_current: Option<String>,
}

#[derive(Clone, Debug)]
pub(crate) enum AutoCoordinatorViewModel {
    Active(AutoActiveViewModel),
}

struct ButtonContext {
    label: String,
    enabled: bool,
}

struct VariantContext {
    button: Option<ButtonContext>,
    ctrl_hint: String,
    manual_hint: Option<String>,
}

pub(crate) struct AutoCoordinatorView {
    model: AutoCoordinatorViewModel,
    app_event_tx: AppEventSender,
    status_message: Option<String>,
}

impl AutoCoordinatorView {
    pub fn new(model: AutoCoordinatorViewModel, app_event_tx: AppEventSender) -> Self {
        Self {
            model,
            app_event_tx,
            status_message: None,
        }
    }

    pub fn update_model(&mut self, model: AutoCoordinatorViewModel) {
        self.model = model;
    }

    fn build_context(model: &AutoActiveViewModel) -> VariantContext {
        let button = model.button.as_ref().map(|btn| ButtonContext {
            label: btn.label.clone(),
            enabled: btn.enabled,
        });
        VariantContext {
            button,
            ctrl_hint: model.ctrl_switch_hint.clone(),
            manual_hint: model.manual_hint.clone(),
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

    pub(crate) fn handle_active_key_event(
        &mut self,
        _pane: &mut BottomPane<'_>,
        key_event: KeyEvent,
    ) -> bool {
        if !matches!(key_event.kind, KeyEventKind::Press | KeyEventKind::Repeat) {
            return false;
        }

        if key_event
            .modifiers
            .contains(KeyModifiers::CONTROL)
            && matches!(key_event.code, KeyCode::Char('s') | KeyCode::Char('S'))
        {
            self.app_event_tx.send(AppEvent::ShowAutoDriveSettings);
            return true;
        }

        matches!(key_event.code, KeyCode::Up | KeyCode::Down)
    }

    fn render_frame(&self, area: Rect, buf: &mut Buffer) -> Option<Rect> {
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
        Some(Rect {
            x: area.x + 1,
            y: area.y + 1,
            width: area.width.saturating_sub(2),
            height: area.height.saturating_sub(2),
        })
    }

    fn derived_status_entries(&self, model: &AutoActiveViewModel) -> Vec<(String, Style)> {
        let mut entries: Vec<(String, Style)> = Vec::new();

        if model.awaiting_submission {
            let text = if let Some(countdown) = &model.countdown {
                format!("Auto continue in {}s", countdown.remaining)
            } else {
                "Awaiting confirmation".to_string()
            };
            entries.push((text, Style::default().fg(colors::text_dim())));
        }

        for status in &model.status_lines {
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

    fn cli_prompt_lines(&self, model: &AutoActiveViewModel, style: Style) -> Option<Vec<Line<'static>>> {
        model.cli_prompt.as_ref().map(|prompt| {
            prompt
                .lines()
                .map(|line| Line::from(Span::styled(line.trim_end().to_string(), style)))
                .collect()
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

    fn button_block_lines(&self, ctx: &VariantContext) -> Option<Vec<Line<'static>>> {
        let button = ctx.button.as_ref()?;
        let label = button.label.trim();
        if label.is_empty() {
            return None;
        }

        let inner = format!(" {label} ");
        let inner_width = UnicodeWidthStr::width(inner.as_str());
        let horizontal = "─".repeat(inner_width);
        let top = format!("╭{horizontal}╮");
        let middle = format!("│{inner}│");
        let bottom = format!("╰{horizontal}╯");

        let base_style = if button.enabled {
            Style::default()
                .fg(colors::primary())
                .add_modifier(Modifier::BOLD)
        } else {
            Style::default().fg(colors::text_dim())
        };

        let mut lines = Vec::with_capacity(3);
        lines.push(Line::from(Span::styled(top, base_style)));

        let mut middle_spans: Vec<Span<'static>> = vec![Span::styled(middle, base_style)];
        if let Some(mut hint_spans) = Self::ctrl_hint_spans(ctx.ctrl_hint.as_str()) {
            if !hint_spans.is_empty() {
                middle_spans.push(Span::raw("   "));
                middle_spans.append(&mut hint_spans);
            }
        }
        lines.push(Line::from(middle_spans));

        lines.push(Line::from(Span::styled(bottom, base_style)));
        Some(lines)
    }

    fn ctrl_hint_spans(hint: &str) -> Option<Vec<Span<'static>>> {
        let trimmed = hint.trim();
        if trimmed.is_empty() {
            return None;
        }

        let normal_style = Style::default().fg(colors::text());
        let bold_style = Style::default()
            .fg(colors::text())
            .add_modifier(Modifier::BOLD);

        let lower = trimmed.to_ascii_lowercase();
        if lower.starts_with("esc") {
            let rest = &trimmed[3..];
            let mut use_prefix = rest.is_empty();
            if let Some(ch) = rest.chars().next() {
                if ch.is_whitespace() || matches!(ch, ':' | '-' | ',' | ';') {
                    use_prefix = true;
                }
            }

            if use_prefix {
                let prefix = &trimmed[..3];
                let mut spans = Vec::new();
                spans.push(Span::styled(prefix.to_string(), bold_style));
                if !rest.is_empty() {
                    spans.push(Span::styled(rest.to_string(), normal_style));
                }
                return Some(spans);
            }
        }

        Some(vec![Span::styled(trimmed.to_string(), normal_style)])
    }

    fn ctrl_hint_line(&self, ctx: &VariantContext) -> Option<Line<'static>> {
        if ctx.button.is_some() {
            return None;
        }
        Self::ctrl_hint_spans(ctx.ctrl_hint.as_str()).map(Line::from)
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

    fn estimated_height_active(
        &self,
        width: u16,
        ctx: &VariantContext,
        model: &AutoActiveViewModel,
    ) -> u16 {
        let inner_width = self.inner_width(width);
        let button_height = if ctx.button.is_some() { 3 } else { 0 };
        let hint_height = ctx
            .manual_hint
            .as_ref()
            .map(|text| Self::wrap_count(text, inner_width))
            .unwrap_or(0);
        let ctrl_hint = ctx.ctrl_hint.trim();
        let ctrl_height = if ctx.button.is_some() {
            0
        } else if ctrl_hint.is_empty() {
            0
        } else {
            Self::wrap_count(ctrl_hint, inner_width)
        };

        let mut total = 0;

        if model.awaiting_submission {
            if let Some(prompt) = &model.cli_prompt {
                total += Self::wrap_count(prompt, inner_width).max(1);
            }
            if ctx.button.is_some() {
                total += button_height;
            }
            if ctrl_height > 0 {
                total += 1; // spacer before ctrl hint
                total += ctrl_height.max(1);
            }
        } else {
            let status_entries = self.derived_status_entries(model);
            for (_, (text, _)) in status_entries.iter().enumerate() {
                total += Self::wrap_count(text, inner_width);
            }
            if ctx.button.is_some() {
                total += button_height;
            }
            if ctx.manual_hint.is_some() {
                total += hint_height.max(1);
            }
            if ctrl_height > 0 {
                total += 1; // spacer before ctrl hint
                total += ctrl_height.max(1);
            }
        }

        let composer_min = 5usize; // input block with surrounding padding
        let summary_height = 1usize; // status summary line at bottom

        total = total
            .saturating_add(composer_min)
            .saturating_add(summary_height);

        total
            .saturating_add(2) // frame borders
            .min(u16::MAX as usize) as u16
    }

    fn render_active(
        &self,
        area: Rect,
        buf: &mut Buffer,
        model: &AutoActiveViewModel,
        composer: &ChatComposer,
    ) {
        let Some(inner) = self.render_frame(area, buf) else {
            return;
        };
        let inner = self.apply_left_padding(inner, buf);
        if inner.height == 0 || inner.width == 0 {
            return;
        }

        let ctx = Self::build_context(model);
        let mut top_lines: Vec<Line<'static>> = Vec::new();
        let mut after_lines: Vec<Line<'static>> = Vec::new();

        if model.awaiting_submission {
            if let Some(prompt_lines) =
                self.cli_prompt_lines(model, Style::default().fg(colors::text()))
            {
                top_lines.extend(prompt_lines);
            }

            if let Some(button_block) = self.button_block_lines(&ctx) {
                after_lines.extend(button_block);
            }

            if let Some(ctrl_hint_line) = self.ctrl_hint_line(&ctx) {
                if !after_lines.is_empty() {
                    after_lines.push(Line::default());
                }
                after_lines.push(ctrl_hint_line);
            }
        } else {
            let status_entries = self.derived_status_entries(model);
            top_lines.extend(self.status_lines_with_entries(&status_entries));

            if let Some(button_block) = self.button_block_lines(&ctx) {
                after_lines.extend(button_block);
            }

            if let Some(hint_line) = self.manual_hint_line(&ctx) {
                after_lines.push(hint_line);
            }

            if let Some(ctrl_hint_line) = self.ctrl_hint_line(&ctx) {
                if !after_lines.is_empty() {
                    after_lines.push(Line::default());
                }
                after_lines.push(ctrl_hint_line);
            }
        }

        if model.waiting_for_response || model.awaiting_submission || model.cli_running {
            if let Some(progress_text) = Self::compose_progress_line(model) {
                let line = Line::from(Span::styled(
                    progress_text,
                    Style::default().fg(colors::text()),
                ));
                if top_lines.is_empty() {
                    top_lines.push(line);
                } else {
                    top_lines.insert(0, line);
                }
            }
        }

        let summary_line = self.build_status_summary(model);
        let summary_height = if summary_line.is_some() { 1 } else { 0 };

        let mut top_height = Self::lines_height(&top_lines, inner.width);
        let mut after_height = Self::lines_height(&after_lines, inner.width);
        let min_composer_height = 3u16;

        let mut composer_height = inner
            .height
            .saturating_sub(top_height)
            .saturating_sub(after_height)
            .saturating_sub(summary_height);

        if composer_height < min_composer_height {
            let deficit = min_composer_height.saturating_sub(composer_height);
            let reduce_after = after_height.min(deficit);
            after_height -= reduce_after;
            let remaining = deficit - reduce_after;
            let reduce_top = top_height.min(remaining);
            top_height -= reduce_top;
            composer_height = inner
                .height
                .saturating_sub(top_height)
                .saturating_sub(after_height)
                .saturating_sub(summary_height);
        }

        if composer_height == 0 {
            composer_height = inner
                .height
                .saturating_sub(top_height)
                .saturating_sub(after_height)
                .saturating_sub(summary_height);
        }

        if composer_height == 0 {
            composer_height = 1;
        }

        let mut cursor_y = inner.y;
        if top_height > 0 {
            let max_height = inner.y + inner.height - cursor_y;
            let rect_height = top_height.min(max_height);
            if rect_height > 0 {
                let top_rect = Rect {
                    x: inner.x,
                    y: cursor_y,
                    width: inner.width,
                    height: rect_height,
                };
                Paragraph::new(top_lines.clone())
                    .wrap(Wrap { trim: true })
                    .render(top_rect, buf);
                cursor_y = cursor_y.saturating_add(rect_height);
            }
        }

        if composer_height > 0 && cursor_y < inner.y + inner.height {
            let max_height = inner.y + inner.height - cursor_y;
            let rect_height = composer_height.min(max_height);
            if rect_height > 0 {
                let composer_rect = Rect {
                    x: inner.x,
                    y: cursor_y,
                    width: inner.width,
                    height: rect_height,
                };
                composer.render_ref(composer_rect, buf);
                cursor_y = cursor_y.saturating_add(rect_height);
            }
        }

        if after_height > 0 && cursor_y < inner.y + inner.height {
            let max_height = inner
                .y
                .saturating_add(inner.height)
                .saturating_sub(cursor_y)
                .saturating_sub(summary_height);
            let rect_height = after_height.min(max_height);
            if rect_height > 0 {
                let after_rect = Rect {
                    x: inner.x,
                    y: cursor_y,
                    width: inner.width,
                    height: rect_height,
                };
                Paragraph::new(after_lines.clone())
                    .wrap(Wrap { trim: true })
                    .render(after_rect, buf);
                cursor_y = cursor_y.saturating_add(rect_height);
            }
        }

        if let Some(line) = summary_line {
            if cursor_y < inner.y + inner.height {
                let rect_height = (inner.y + inner.height - cursor_y).max(1);
                let summary_rect = Rect {
                    x: inner.x,
                    y: cursor_y,
                    width: inner.width,
                    height: rect_height,
                };
                Paragraph::new(line).render(summary_rect, buf);
            }
        }
    }

    fn lines_height(lines: &[Line<'static>], width: u16) -> u16 {
        if lines.is_empty() {
            return 0;
        }
        if width == 0 {
            return lines.len() as u16;
        }
        lines.iter().fold(0u16, |acc, line| {
            let line_width = line.width() as u16;
            let segments = if line_width == 0 {
                1
            } else {
                (line_width + width - 1) / width
            };
            acc.saturating_add(segments.max(1))
        })
    }

    fn build_status_summary(&self, model: &AutoActiveViewModel) -> Option<Line<'static>> {
        let status_label = if model.waiting_for_response {
            "Running"
        } else if model.awaiting_submission {
            "Awaiting input"
        } else if model.elapsed.is_some() && model.started_at.is_none() {
            "Stopped"
        } else {
            "Ready"
        };

        let elapsed = if let Some(duration) = model.elapsed {
            Some(duration)
        } else if let Some(started_at) = model.started_at {
            Some(Instant::now().saturating_duration_since(started_at))
        } else {
            None
        };

        let mut primary = status_label.to_string();
        let mut details: Vec<String> = Vec::new();

        if let Some(duration) = elapsed {
            details.push(Self::format_elapsed(duration));
        }

        details.push(Self::format_turns(model.turns_completed));

        if !details.is_empty() {
            primary.push_str(" (");
            primary.push_str(&details.join(", "));
            primary.push(')');
        }

        let mut spans: Vec<Span<'static>> = Vec::new();
        spans.push(Span::styled(
            primary,
            Style::default()
                .fg(colors::text())
                .add_modifier(Modifier::BOLD),
        ));

        let secondary_style = Style::default().fg(colors::text_dim());

        let agents_text = if model.agents_enabled {
            "Agents Enabled"
        } else {
            "Agents Disabled"
        };
        let review_text = if model.review_enabled {
            "Review Enabled"
        } else {
            "Review Disabled"
        };

        spans.push(Span::styled("  •  ", secondary_style));
        spans.push(Span::styled(agents_text.to_string(), secondary_style));
        spans.push(Span::styled("  •  ", secondary_style));
        spans.push(Span::styled(review_text.to_string(), secondary_style));

        Some(Line::from(spans))
    }

    fn format_elapsed(duration: Duration) -> String {
        let total_seconds = duration.as_secs();
        let hours = total_seconds / 3600;
        let minutes = (total_seconds % 3600) / 60;
        let seconds = total_seconds % 60;

        if hours > 0 {
            if minutes > 0 {
                format!("{}h {:02}m", hours, minutes)
            } else {
                format!("{}h", hours)
            }
        } else if minutes > 0 {
            if seconds > 0 {
                format!("{}m {:02}s", minutes, seconds)
            } else {
                format!("{}m", minutes)
            }
        } else {
            format!("{}s", seconds)
        }
    }

    fn format_turns(turns: usize) -> String {
        let label = if turns == 1 { "turn" } else { "turns" };
        format!("{} {}", turns, label)
    }

    fn compose_progress_line(model: &AutoActiveViewModel) -> Option<String> {
        let mut parts: Vec<String> = Vec::new();
        if let Some(past) = model.progress_past.as_ref() {
            let trimmed = past.trim();
            if !trimmed.is_empty() {
                parts.push(trimmed.to_string());
            }
        }
        if let Some(current) = model.progress_current.as_ref() {
            let trimmed = current.trim();
            if !trimmed.is_empty() {
                parts.push(trimmed.to_string());
            }
        }
        if parts.is_empty() {
            None
        } else {
            Some(parts.join(" "))
        }
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

impl<'a> BottomPaneView<'a> for AutoCoordinatorView {
    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, key_event: KeyEvent) {
        if !matches!(key_event.kind, KeyEventKind::Press | KeyEventKind::Repeat) {
            return;
        }

        if key_event
            .modifiers
            .contains(KeyModifiers::CONTROL)
            && matches!(key_event.code, KeyCode::Char('s') | KeyCode::Char('S'))
        {
            self.app_event_tx.send(AppEvent::ShowAutoDriveSettings);
        }
    }

    fn desired_height(&self, width: u16) -> u16 {
        let AutoCoordinatorViewModel::Active(model) = &self.model;
        let ctx = Self::build_context(model);
        self.estimated_height_active(width, &ctx, model)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        if area.height == 0 {
            return;
        }

        // Fallback path when the composer is not available: draw the outer
        // frame so the layout remains stable.
        let _ = self.render_frame(area, buf);
    }

    fn render_with_composer(
        &self,
        area: Rect,
        buf: &mut Buffer,
        composer: &ChatComposer,
    ) {
        if area.height == 0 {
            return;
        }

        let AutoCoordinatorViewModel::Active(model) = &self.model;
        self.render_active(area, buf, model, composer);
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
