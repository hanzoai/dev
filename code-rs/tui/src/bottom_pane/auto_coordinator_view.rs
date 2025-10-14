use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::auto_drive_style::{AutoDriveStyle, FrameStyle};
use crate::colors;
use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use ratatui::prelude::Widget;
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Clear, Paragraph};
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

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub(crate) struct AutoCoordinatorButton {
    pub label: String,
    pub enabled: bool,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub(crate) struct AutoActiveViewModel {
    #[allow(dead_code)]
    pub goal: Option<String>,
    pub status_lines: Vec<String>,
    pub cli_prompt: Option<String>,
    pub cli_context: Option<String>,
    pub show_composer: bool,
    pub awaiting_submission: bool,
    pub waiting_for_response: bool,
    pub waiting_for_review: bool,
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

pub(crate) struct AutoCoordinatorView {
    model: AutoCoordinatorViewModel,
    app_event_tx: AppEventSender,
    status_message: Option<String>,
    style: AutoDriveStyle,
}

const PUN_FALLBACK: &str = "Recalculating route…";

impl AutoCoordinatorView {
    pub fn new(
        model: AutoCoordinatorViewModel,
        app_event_tx: AppEventSender,
        style: AutoDriveStyle,
    ) -> Self {
        Self {
            model,
            app_event_tx,
            status_message: None,
            style,
        }
    }

    pub fn update_model(&mut self, model: AutoCoordinatorViewModel) {
        self.model = model;
    }

    pub fn set_style(&mut self, style: AutoDriveStyle) {
        self.style = style;
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

        let awaiting_without_input = matches!(
            &self.model,
            AutoCoordinatorViewModel::Active(model)
                if model.awaiting_submission && !model.show_composer
        );
        if awaiting_without_input {
            // Allow approval keys to bubble so ChatWidget handles them.
            let allow_passthrough = matches!(
                key_event.code,
                KeyCode::Esc | KeyCode::Enter | KeyCode::Char(' ') | KeyCode::Char('e') | KeyCode::Char('E')
            );
            if !allow_passthrough {
                return true;
            }
        }

        matches!(key_event.code, KeyCode::Up | KeyCode::Down)
    }

    fn frame_style_for_model(&self, _model: &AutoActiveViewModel) -> FrameStyle {
        FrameStyle {
            title_prefix: "",
            title_text: "",
            title_suffix: "",
            title_style: Style::default().fg(colors::text()),
            border_style: Style::default().fg(colors::text_dim()),
            border_type: BorderType::Plain,
            accent: None,
        }
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

    fn body_inner_lines(model: &AutoActiveViewModel) -> u16 {
        if model.awaiting_submission {
            1
        } else {
            0
        }
    }

    fn body_block_height(model: &AutoActiveViewModel) -> u16 {
        // Two lines for the block borders plus the interior content.
        2 + Self::body_inner_lines(model)
    }

    fn estimated_height_active(&self, model: &AutoActiveViewModel) -> u16 {
        let header: u16 = 1;
        let footer: u16 = 1;
        header
            .saturating_add(Self::body_block_height(model))
            .saturating_add(footer)
    }

    fn effective_elapsed(model: &AutoActiveViewModel) -> Option<Duration> {
        model.elapsed
    }

    fn status_label(model: &AutoActiveViewModel) -> &'static str {
        if model.waiting_for_review {
            "Awaiting review"
        } else if model.waiting_for_response || model.cli_running {
            "Running"
        } else if model.awaiting_submission {
            "Awaiting input"
        } else if model.elapsed.is_some() && model.started_at.is_none() {
            "Stopped"
        } else {
            "Ready"
        }
    }

    fn resolve_display_message(&self, model: &AutoActiveViewModel) -> String {
        if let Some(message) = self.status_message.as_ref() {
            let trimmed = message.trim();
            if !trimmed.is_empty() {
                return trimmed.to_string();
            }
        }

        if let Some(current) = model.progress_current.as_ref() {
            let trimmed = current.trim();
            if !trimmed.is_empty() {
                return trimmed.to_string();
            }
        }

        if model.awaiting_submission {
            if let Some(countdown) = &model.countdown {
                return format!("Awaiting confirmation ({}s)", countdown.remaining);
            }
            if let Some(button) = &model.button {
                let trimmed = button.label.trim();
                if !trimmed.is_empty() {
                    return trimmed.to_string();
                }
            }
        }

        for status in &model.status_lines {
            let trimmed = status.trim();
            if !trimmed.is_empty() {
                return trimmed.to_string();
            }
        }

        PUN_FALLBACK.to_string()
    }

    fn runtime_text(&self, model: &AutoActiveViewModel) -> String {
        let label = Self::status_label(model);
        let mut details: Vec<String> = Vec::new();
        if let Some(duration) = Self::effective_elapsed(model) {
            details.push(Self::format_elapsed(duration));
        }
        details.push(Self::format_turns(model.turns_completed));
        if details.is_empty() {
            label.to_string()
        } else {
            format!("{} ({})", label, details.join(", "))
        }
    }

    fn footer_left_text(&self, model: &AutoActiveViewModel) -> String {
        if model.awaiting_submission {
            let agents = if model.agents_enabled {
                "Agents Enabled"
            } else {
                "Agents Disabled"
            };
            let review = if model.review_enabled {
                "Review Enabled"
            } else {
                "Review Disabled"
            };
            return format!(
                "Awaiting approval  •  {}  •  {}  •  Ctrl+S Settings",
                agents, review
            );
        }
        let agents = if model.agents_enabled {
            "Agents Enabled"
        } else {
            "Agents Disabled"
        };
        let review = if model.review_enabled {
            "Review Enabled"
        } else {
            "Review Disabled"
        };
        format!("{}  •  {}  •  Ctrl+S Settings", agents, review)
    }

    fn footer_right_text(&self, model: &AutoActiveViewModel) -> &'static str {
        if model.awaiting_submission {
            "Enter approve  •  Esc cancel"
        } else {
            "Esc stop Auto Drive"
        }
    }

    fn render_header(
        &self,
        area: Rect,
        buf: &mut Buffer,
        model: &AutoActiveViewModel,
        frame_style: &FrameStyle,
    ) {
        if area.height == 0 {
            return;
        }

        let message = self.resolve_display_message(model);
        let left_spans = vec![
            Span::styled("Auto Drive", frame_style.title_style.clone()),
            Span::styled(" > ", Style::default().fg(colors::text_dim())),
            Span::styled(message, Style::default().fg(colors::text())),
        ];
        let left_line = Line::from(left_spans);

        let runtime = self.runtime_text(model);
        let right_width = UnicodeWidthStr::width(runtime.as_str()).min(area.width as usize) as u16;
        let constraints = if right_width == 0 {
            vec![Constraint::Fill(1)]
        } else {
            vec![Constraint::Fill(1), Constraint::Length(right_width)]
        };
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(constraints)
            .split(area);

        Paragraph::new(left_line).render(chunks[0], buf);

        if right_width > 0 {
            Paragraph::new(Line::from(Span::styled(
                runtime,
                self.style.summary_style.clone(),
            )))
            .alignment(Alignment::Right)
            .render(chunks[chunks.len() - 1], buf);
        }
    }

    fn render_body(
        &self,
        area: Rect,
        buf: &mut Buffer,
        model: &AutoActiveViewModel,
        frame_style: &FrameStyle,
    ) {
        if area.height == 0 {
            return;
        }

        Block::default()
            .borders(Borders::ALL)
            .border_style(frame_style.border_style.clone())
            .border_type(frame_style.border_type)
            .render(area, buf);

        let inner = Rect {
            x: area.x.saturating_add(1),
            y: area.y.saturating_add(1),
            width: area.width.saturating_sub(2),
            height: area.height.saturating_sub(2),
        };

        if inner.width > 0 && inner.height > 0 {
            Clear.render(inner, buf);
            if model.awaiting_submission {
                let notice = Line::from(Span::styled(
                    "Approval required — review and press Enter to send",
                    Style::default().fg(colors::text_dim()),
                ));
                let paragraph = Paragraph::new(notice);
                let line_area = Rect {
                    x: inner.x,
                    y: inner.y,
                    width: inner.width,
                    height: inner.height.min(1),
                };
                paragraph.render(line_area, buf);
            }
        }
    }

    fn render_footer(&self, area: Rect, buf: &mut Buffer, model: &AutoActiveViewModel) {
        if area.height == 0 {
            return;
        }

        let left_text = self.footer_left_text(model);
        let right_text = self.footer_right_text(model);
        let right_width = UnicodeWidthStr::width(right_text).min(area.width as usize) as u16;
        let constraints = if right_width == 0 {
            vec![Constraint::Fill(1)]
        } else {
            vec![Constraint::Fill(1), Constraint::Length(right_width)]
        };
        let chunks = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(constraints)
            .split(area);

        Paragraph::new(Line::from(Span::styled(
            left_text,
            Style::default().fg(colors::text_dim()),
        )))
        .render(chunks[0], buf);

        if right_width > 0 {
            Paragraph::new(Line::from(Span::styled(
                right_text.to_string(),
                Style::default().fg(colors::text()),
            )))
            .alignment(Alignment::Right)
            .render(chunks[chunks.len() - 1], buf);
        }
    }

    fn draw_view(
        &self,
        area: Rect,
        buf: &mut Buffer,
        model: &AutoActiveViewModel,
    ) {
        if area.height == 0 {
            return;
        }

        let frame_style = self.frame_style_for_model(model);
        let constraints = [
            Constraint::Length(1),
            Constraint::Length(Self::body_block_height(model)),
            Constraint::Length(1),
        ];

        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints(constraints)
            .split(area);

        let header_area = chunks[0];
        let body_area = chunks[1];
        let footer_area = chunks[2];

        self.render_header(header_area, buf, model, &frame_style);
        self.render_body(body_area, buf, model, &frame_style);
        self.render_footer(footer_area, buf, model);
    }
}

impl<'a> BottomPaneView<'a> for AutoCoordinatorView {
    fn as_any(&self) -> Option<&dyn std::any::Any> {
        Some(self)
    }

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

    fn desired_height(&self, _width: u16) -> u16 {
        let AutoCoordinatorViewModel::Active(model) = &self.model;
        self.estimated_height_active(model)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        if area.height == 0 {
            return;
        }

        let AutoCoordinatorViewModel::Active(model) = &self.model;
        self.draw_view(area, buf, model);
    }

    fn render_with_composer(
        &self,
        area: Rect,
        buf: &mut Buffer,
        _composer: &ChatComposer,
    ) {
        if area.height == 0 {
            return;
        }

        let AutoCoordinatorViewModel::Active(model) = &self.model;
        self.draw_view(area, buf, model);
    }

    fn update_status_text(&mut self, text: String) -> ConditionalUpdate {
        if self.update_status_message(text) {
            ConditionalUpdate::NeedsRedraw
        } else {
            ConditionalUpdate::NoRedraw
        }
    }

    fn handle_paste_with_composer(
        &mut self,
        composer: &mut ChatComposer,
        pasted: String,
    ) -> ConditionalUpdate {
        if composer.handle_paste(pasted) {
            ConditionalUpdate::NeedsRedraw
        } else {
            ConditionalUpdate::NoRedraw
        }
    }

    fn as_any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        Some(self)
    }
}
