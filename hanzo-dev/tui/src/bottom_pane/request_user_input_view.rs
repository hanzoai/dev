// Request-user-input view: shows elicitation questions from the model.

use crossterm::event::KeyEvent;
use hanzo_protocol::request_user_input::RequestUserInputQuestion;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Paragraph, Widget, Wrap};
use std::any::Any;

use super::bottom_pane_view::BottomPaneView;
use super::{BottomPane, CancellationEvent};
use crate::app_event_sender::AppEventSender;

pub(crate) struct RequestUserInputView {
    _turn_id: String,
    questions: Vec<RequestUserInputQuestion>,
    _app_event_tx: AppEventSender,
}

impl RequestUserInputView {
    pub fn new(
        turn_id: String,
        questions: Vec<RequestUserInputQuestion>,
        app_event_tx: AppEventSender,
    ) -> Self {
        Self {
            _turn_id: turn_id,
            questions,
            _app_event_tx: app_event_tx,
        }
    }
}

impl<'a> BottomPaneView<'a> for RequestUserInputView {
    fn handle_key_event(&mut self, _pane: &mut BottomPane<'a>, _key_event: KeyEvent) {
        // Stub: user-input views are not yet interactive in this UI.
    }

    fn on_ctrl_c(&mut self, _pane: &mut BottomPane<'a>) -> CancellationEvent {
        CancellationEvent::Ignored
    }

    fn desired_height(&self, _width: u16) -> u16 {
        // One line per question + header.
        (self.questions.len() as u16 + 2).min(10)
    }

    fn render(&self, area: Rect, buf: &mut Buffer) {
        let mut lines: Vec<Line<'_>> = Vec::new();
        lines.push(Line::from(Span::styled(
            "User input requested:",
            Style::default().add_modifier(Modifier::BOLD),
        )));
        for q in &self.questions {
            lines.push(Line::from(format!("  {} - {}", q.header, q.question)));
        }
        Paragraph::new(lines)
            .wrap(Wrap { trim: false })
            .render(area, buf);
    }

    fn as_any(&self) -> Option<&dyn Any> {
        Some(self)
    }
}
