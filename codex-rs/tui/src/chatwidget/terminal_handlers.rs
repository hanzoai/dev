use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

use super::{ChatWidget, PendingCommandAction};
use crate::app_event::AppEvent;

pub(super) fn handle_terminal_key(chat: &mut ChatWidget<'_>, key_event: KeyEvent) -> bool {
    let Some(id) = chat.terminal_overlay_id() else {
        return false;
    };

    if chat.terminal_handle_pending_key(key_event) {
        return true;
    }

    match key_event.code {
        KeyCode::Up => {
            chat.terminal_scroll_lines(-1);
            true
        }
        KeyCode::Down => {
            chat.terminal_scroll_lines(1);
            true
        }
        KeyCode::PageUp => {
            chat.terminal_scroll_page(-1);
            true
        }
        KeyCode::PageDown => {
            chat.terminal_scroll_page(1);
            true
        }
        KeyCode::Home => {
            chat.terminal_scroll_to_top();
            true
        }
        KeyCode::End => {
            chat.terminal_scroll_to_bottom();
            true
        }
        KeyCode::Esc => {
            if chat.terminal_is_running() {
                chat.request_terminal_cancel(id);
                true
            } else {
                chat.close_terminal_overlay();
                true
            }
        }
        KeyCode::Char('c') if key_event.modifiers.contains(KeyModifiers::CONTROL) => {
            if chat.terminal_is_running() {
                chat.app_event_tx.send(AppEvent::TerminalCancel { id });
                true
            } else {
                false
            }
        }
        KeyCode::Char('r') | KeyCode::Char('R') => {
            if chat.terminal_is_running() {
                true
            } else if chat.terminal_prepare_rerun(id) {
                chat.app_event_tx.send(AppEvent::TerminalRerun { id });
                true
            } else {
                true
            }
        }
        KeyCode::Enter => {
            if let Some(action) = chat.terminal_accept_pending_command() {
                if let PendingCommandAction::Manual(command) = action {
                    chat.terminal_execute_manual_command(id, command);
                }
                true
            } else if chat.terminal_has_pending_command() {
                true
            } else {
                false
            }
        }
        _ => false,
    }
}
