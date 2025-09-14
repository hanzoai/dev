use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use ratatui::{buffer::Buffer, layout::Rect, style::Style, widgets::StatefulWidgetRef};

use super::textarea::{TextArea, TextAreaState};

/// Lightweight text field wrapper for bottom‑pane forms.
///
/// - Centralizes key handling (Shift‑modified chars, Enter/newlines, undo, nav)
/// - Renders via the same TextArea engine used by the chat composer, so
///   wrapping and height match exactly.
/// - Supports single‑line mode (ignores Enter pastes/newlines) and multi‑line
///   mode (Enter inserts a newline; paste preserves newlines).
#[derive(Debug)]
pub struct FormTextField {
    textarea: TextArea,
    state: TextAreaState,
    single_line: bool,
}

impl FormTextField {
    pub fn new_single_line() -> Self {
        Self { textarea: TextArea::new(), state: TextAreaState::default(), single_line: true }
    }
    pub fn new_multi_line() -> Self {
        Self { textarea: TextArea::new(), state: TextAreaState::default(), single_line: false }
    }

    pub fn set_text(&mut self, text: &str) {
        if self.single_line {
            let mut t = text.replace('\r', "\n");
            t = t.replace('\n', " ");
            self.textarea.set_text(&t);
        } else {
            self.textarea.set_text(&text.replace('\r', "\n"));
        }
        self.textarea.set_cursor(self.textarea.text().len());
    }

    pub fn text(&self) -> &str { self.textarea.text() }

    pub fn handle_key(&mut self, key: KeyEvent) -> bool {
        // For single-line inputs, swallow Enter (treat as no-op here; the form
        // can decide to move focus or trigger an action). Also convert any
        // Control-Enter to newline only in multi-line mode.
        match key.code {
            KeyCode::Enter if self.single_line => return true, // consumed
            _ => {}
        }

        // Block Alt-modified printable chars from being inserted as text since
        // many terminals map Option/Meta to Alt for word navigation shortcuts.
        if matches!(key.code, KeyCode::Char(_))
            && key.modifiers.contains(KeyModifiers::ALT)
            && !key.modifiers.contains(KeyModifiers::CONTROL)
        {
            return false; // let parent handle (e.g., form navigation)
        }

        // Delegate remaining keys to TextArea which already handles:
        // - Shift-modified chars
        // - Enter/newline (multi-line only)
        // - Undo (Ctrl+Z), word nav, Home/End, etc.
        self.textarea.input(key);
        true
    }

    pub fn handle_paste(&mut self, mut pasted: String) {
        pasted = pasted.replace('\r', "\n");
        if self.single_line {
            pasted = pasted.replace('\n', " ");
        }
        self.textarea.insert_str(&pasted);
    }

    pub fn desired_height(&self, width: u16) -> u16 {
        if self.single_line { 1 } else { self.textarea.desired_height(width).max(1) }
    }

    /// Render the field text within `area`. When `focused` is true, draw a thin
    /// caret marker at the logical cursor position (the real terminal cursor is
    /// hidden while overlays are active).
    pub fn render(&self, area: Rect, buf: &mut Buffer, focused: bool) {
        // Paint text using the TextArea renderer for exact wrapping
        let mut state = self.state;
        StatefulWidgetRef::render_ref(&(&self.textarea), area, buf, &mut state);

        // Draw a pseudo-caret when focused.
        if focused {
            if let Some((cx, cy)) = self.textarea.cursor_pos_with_state(area, &state) {
                if cy >= area.y && cy < area.y + area.height && cx >= area.x && cx < area.x + area.width {
                    // Use theme info color for caret; overwrite a single cell
                    let caret_style = Style::default().fg(crate::colors::info());
                    buf.set_string(cx, cy, "▏", caret_style);
                }
            }
        }
        // Note: We intentionally do not persist state.scroll changes from a
        // read-only &self; form owns a &mut when calling this in practice.
    }

    /// Mutable render variant used by form views to keep scroll state stable.
    pub fn render_mut(&mut self, area: Rect, buf: &mut Buffer, focused: bool) {
        StatefulWidgetRef::render_ref(&(&self.textarea), area, buf, &mut self.state);
        if focused {
            if let Some((cx, cy)) = self.textarea.cursor_pos_with_state(area, &self.state) {
                if cy >= area.y && cy < area.y + area.height && cx >= area.x && cx < area.x + area.width {
                    let caret_style = Style::default().fg(crate::colors::info());
                    buf.set_string(cx, cy, "▏", caret_style);
                }
            }
        }
    }
}
