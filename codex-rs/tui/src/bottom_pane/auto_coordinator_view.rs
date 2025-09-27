use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::colors;
use crate::auto_drive_strings;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Paragraph, Widget, Wrap};
use unicode_width::UnicodeWidthStr;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Duration;

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
    #[allow(dead_code)]
    pub waiting_for_response: bool,
    pub countdown: Option<CountdownState>,
    pub button: Option<AutoCoordinatorButton>,
    pub manual_hint: Option<String>,
    pub ctrl_switch_hint: String,
}

struct VariantContext {
    button: Option<(String, bool)>,
    ctrl_hint: String,
    manual_hint: Option<String>,
}

pub(crate) struct AutoCoordinatorView {
    model: AutoCoordinatorViewModel,
    app_event_tx: AppEventSender,
}

impl AutoCoordinatorView {
    pub fn new(model: AutoCoordinatorViewModel, app_event_tx: AppEventSender) -> Self {
        Self { model, app_event_tx }
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

    fn render_frame(&self, area: Rect, buf: &mut Buffer, title: &str) -> Option<Rect> {
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

    fn status_lines(&self) -> Vec<Line<'static>> {
        let frame_idx = DRIVE_SPINNER_TICK.fetch_add(1, Ordering::Relaxed);
        let spinner_symbol = DRIVE_SPINNER_FRAMES[frame_idx % DRIVE_SPINNER_FRAMES.len()].to_string();

        let mut lines: Vec<Line<'static>> = Vec::new();
        for (index, (text, style)) in self.derived_status_entries().into_iter().enumerate() {
            if index == 0 {
                lines.push(Line::from(vec![
                    Span::styled(spinner_symbol.clone(), Style::default().fg(colors::spinner()).add_modifier(Modifier::BOLD)),
                    Span::raw("  "),
                    Span::styled(text, style),
                ]));
            } else {
                lines.push(Line::from(Span::styled(text, style)));
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

        let mut total = 1; // top padding line

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
            for (text, _) in &status_entries {
                total += Self::wrap_count(&format!("• {}", text), inner_width);
            }
            if status_entries.len() == 1 {
                total = total.saturating_add(1); // spacer below single status line
            }
            if ctx.button.is_some() {
                total += 1; // spacer before button
                total += button_height.max(1);
            }
            if ctx.manual_hint.is_some() {
                total += 1; // spacer before manual hint
                total += hint_height.max(1);
            }
            if ctrl_height > 0 {
                total += 1; // spacer before ctrl hint
                total += ctrl_height.max(1);
            }
        }
        total += 1; // bottom padding line

        total
            .saturating_add(2) // frame borders
            .min(u16::MAX as usize) as u16
    }

    fn render_internal(&self, area: Rect, buf: &mut Buffer, ctx: &VariantContext) {
        self.render_classic(area, buf, ctx);
    }

    fn render_classic(&self, area: Rect, buf: &mut Buffer, ctx: &VariantContext) {
        let Some(inner) = self.render_frame(area, buf, " Auto Drive ") else {
            return;
        };
        let inner = self.apply_left_padding(inner, buf);
        if inner.height == 0 {
            return;
        }

        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::default());

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
            let status_lines = self.status_lines();
            let awaiting_goal = status_lines.len() == 1;
            lines.extend(status_lines);
            if awaiting_goal {
                lines.push(Line::default());
            }

            if let Some(button_line) = self.button_line(ctx) {
                lines.push(Line::default());
                lines.push(button_line);
            }

            if let Some(hint_line) = self.manual_hint_line(ctx) {
                lines.push(Line::default());
                lines.push(hint_line);
            }

            if let Some(ctrl_hint_line) = self.ctrl_hint_line(ctx) {
                lines.push(Line::default());
                lines.push(ctrl_hint_line);
            }
        }

        lines.push(Line::default());

        Paragraph::new(lines)
            .wrap(Wrap { trim: true })
            .render(inner, buf);

        self.app_event_tx
            .send(AppEvent::ScheduleFrameIn(Duration::from_millis(DRIVE_SPINNER_INTERVAL_MS)));
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

        let spacer_style = Style::default()
            .bg(colors::background())
            .fg(colors::text());
        for x in area.x..area.x.saturating_add(area.width) {
            let cell = &mut buf[(x, area.y)];
            cell.set_symbol(" ");
            cell.set_style(spacer_style);
        }

        let inner_area = Rect {
            x: area.x,
            y: area.y + 1,
            width: area.width,
            height: area.height.saturating_sub(1),
        };

        if inner_area.height == 0 {
            return;
        }

        self.render_internal(inner_area, buf, &ctx);
    }
}
