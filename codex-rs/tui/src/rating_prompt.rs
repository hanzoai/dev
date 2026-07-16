//! End-of-session rating prompt.
//!
//! A one-line prompt shown at most once per session as the TUI tears down. It
//! collects a content-free reward signal: `1`/`2`/`3` are quality ratings;
//! `0`, `Esc`, or `Enter` dismiss ("no opinion"). Ctrl+C / Ctrl+D also dismiss.

use crate::render::Insets;
use crate::render::renderable::ColumnRenderable;
use crate::render::renderable::Renderable;
use crate::render::renderable::RenderableExt as _;
use crate::tui::Tui;
use crate::tui::TuiEvent;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyEventKind;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::prelude::Widget;
use ratatui::style::Stylize as _;
use ratatui::text::Line;
use ratatui::widgets::Clear;
use ratatui::widgets::WidgetRef;
use tokio_stream::StreamExt;

/// Outcome of the exit rating prompt.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RatingOutcome {
    /// A quality rating in `1..=3`.
    Rated(u8),
    /// User dismissed (`0` / `Esc` / `Enter`): "no opinion".
    Dismissed,
}

/// Show the one-line rating prompt and return the user's choice.
///
/// `1`/`2`/`3` rate; `0`, `Esc`, and `Enter` dismiss. The event stream closing
/// also dismisses.
pub(crate) async fn run_exit_rating_prompt(tui: &mut Tui) -> RatingOutcome {
    let mut screen = RatingPromptScreen::default();
    let _ = tui.draw(u16::MAX, |frame| {
        frame.render_widget_ref(&screen, frame.area());
    });

    let events = tui.event_stream();
    tokio::pin!(events);

    while screen.outcome.is_none() {
        match events.next().await {
            Some(TuiEvent::Key(key_event)) => screen.handle_key(key_event),
            Some(TuiEvent::Draw) => {
                let _ = tui.draw(u16::MAX, |frame| {
                    frame.render_widget_ref(&screen, frame.area());
                });
            }
            Some(_) => {}
            None => break,
        }
    }

    screen.outcome.unwrap_or(RatingOutcome::Dismissed)
}

#[derive(Default)]
struct RatingPromptScreen {
    outcome: Option<RatingOutcome>,
}

impl RatingPromptScreen {
    fn handle_key(&mut self, key_event: KeyEvent) {
        if !matches!(key_event.kind, KeyEventKind::Press | KeyEventKind::Repeat) {
            return;
        }
        let outcome = match key_event.code {
            KeyCode::Char('1') => RatingOutcome::Rated(1),
            KeyCode::Char('2') => RatingOutcome::Rated(2),
            KeyCode::Char('3') => RatingOutcome::Rated(3),
            // `0`, Esc, Enter (and anything else) dismiss with "no opinion".
            _ => RatingOutcome::Dismissed,
        };
        self.outcome = Some(outcome);
    }
}

impl WidgetRef for &RatingPromptScreen {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        Clear.render(area, buf);
        let mut column = ColumnRenderable::new();
        column.push("");
        column.push(
            Line::from(vec![
                "Rate this session:  ".into(),
                "1".bold(),
                " · ".dim(),
                "2".bold(),
                " · ".dim(),
                "3".bold(),
                "   (0 to dismiss)".dim(),
            ])
            .inset(Insets::tlbr(0, 1, 0, 0)),
        );
        column.render(area, buf);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::KeyModifiers;

    fn press(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    #[test]
    fn digits_1_through_3_rate() {
        for (ch, n) in [('1', 1u8), ('2', 2), ('3', 3)] {
            let mut screen = RatingPromptScreen::default();
            screen.handle_key(press(KeyCode::Char(ch)));
            assert_eq!(screen.outcome, Some(RatingOutcome::Rated(n)));
        }
    }

    #[test]
    fn zero_esc_and_enter_dismiss() {
        for code in [KeyCode::Char('0'), KeyCode::Esc, KeyCode::Enter] {
            let mut screen = RatingPromptScreen::default();
            screen.handle_key(press(code));
            assert_eq!(screen.outcome, Some(RatingOutcome::Dismissed));
        }
    }

    #[test]
    fn key_release_is_ignored() {
        let mut screen = RatingPromptScreen::default();
        let mut ev = press(KeyCode::Char('2'));
        ev.kind = KeyEventKind::Release;
        screen.handle_key(ev);
        assert_eq!(screen.outcome, None);
    }
}
