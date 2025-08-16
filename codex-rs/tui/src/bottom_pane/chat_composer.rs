use codex_core::protocol::TokenUsage;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyModifiers;
use ratatui::buffer::Buffer;
use ratatui::layout::Constraint;
use ratatui::layout::Layout;
use ratatui::layout::Margin;
use ratatui::layout::Rect;
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::style::Stylize;
use ratatui::text::Line;
use ratatui::text::Span;
use ratatui::widgets::Block;
use ratatui::widgets::Borders;
use ratatui::widgets::StatefulWidgetRef;
use ratatui::widgets::WidgetRef;

use super::chat_composer_history::ChatComposerHistory;
use super::command_popup::CommandPopup;
use super::file_search_popup::FileSearchPopup;

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::bottom_pane::textarea::TextArea;
use crate::bottom_pane::textarea::TextAreaState;
use codex_file_search::FileMatch;
use std::cell::RefCell;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::thread;
use std::time::Duration;

const BASE_PLACEHOLDER_TEXT: &str = "What are we coding today?";
/// If the pasted content exceeds this number of characters, replace it with a
/// placeholder in the UI.
const LARGE_PASTE_CHAR_THRESHOLD: usize = 1000;

/// Result returned when the user interacts with the text area.
pub enum InputResult {
    Submitted(String),
    ScrollUp,
    ScrollDown,
    None,
}

struct TokenUsageInfo {
    total_token_usage: TokenUsage,
    last_token_usage: TokenUsage,
    model_context_window: Option<u64>,
}

// Format an integer with thousands separators (e.g., 125,654).
fn format_with_thousands(n: u64) -> String {
    let s = n.to_string();
    let mut out = String::with_capacity(s.len() + s.len() / 3);
    let mut count = 0usize;
    for ch in s.chars().rev() {
        if count != 0 && count % 3 == 0 {
            out.push(',');
        }
        out.push(ch);
        count += 1;
    }
    out.chars().rev().collect()
}

pub(crate) struct ChatComposer {
    textarea: TextArea,
    textarea_state: RefCell<TextAreaState>,
    active_popup: ActivePopup,
    app_event_tx: AppEventSender,
    history: ChatComposerHistory,
    ctrl_c_quit_hint: bool,
    #[allow(dead_code)]
    use_shift_enter_hint: bool,
    dismissed_file_popup_token: Option<String>,
    current_file_query: Option<String>,
    pending_pastes: Vec<(String, String)>,
    token_usage_info: Option<TokenUsageInfo>,
    has_focus: bool,
    has_chat_history: bool,
    last_esc_time: Option<std::time::Instant>,
    is_task_running: bool,
    // Current status message to display when task is running
    status_message: String,
    // Animation thread for spinning icon when task is running
    animation_running: Option<Arc<AtomicBool>>,
    using_chatgpt_auth: bool,
    // Ephemeral footer notice and its expiry
    footer_notice: Option<(String, std::time::Instant)>,
    // Footer hint visibility flags
    show_reasoning_hint: bool,
    show_diffs_hint: bool,
}

/// Popup state – at most one can be visible at any time.
enum ActivePopup {
    None,
    Command(CommandPopup),
    File(FileSearchPopup),
}

impl ChatComposer {
    pub fn new(
        has_input_focus: bool,
        app_event_tx: AppEventSender,
        enhanced_keys_supported: bool,
        using_chatgpt_auth: bool,
    ) -> Self {
        let use_shift_enter_hint = enhanced_keys_supported;

        Self {
            textarea: TextArea::new(),
            textarea_state: RefCell::new(TextAreaState::default()),
            active_popup: ActivePopup::None,
            app_event_tx,
            history: ChatComposerHistory::new(),
            ctrl_c_quit_hint: false,
            use_shift_enter_hint,
            dismissed_file_popup_token: None,
            current_file_query: None,
            pending_pastes: Vec::new(),
            token_usage_info: None,
            has_focus: has_input_focus,
            has_chat_history: false,
            last_esc_time: None,
            is_task_running: false,
            status_message: String::from("coding"),
            animation_running: None,
            using_chatgpt_auth,
            footer_notice: None,
            show_reasoning_hint: false,
            show_diffs_hint: false,
        }
    }

    pub fn set_has_chat_history(&mut self, has_history: bool) {
        self.has_chat_history = has_history;
    }

    pub fn set_task_running(&mut self, running: bool) {
        self.is_task_running = running;

        if running {
            // Start animation thread if not already running
            if self.animation_running.is_none() {
                let animation_flag = Arc::new(AtomicBool::new(true));
                let animation_flag_clone = Arc::clone(&animation_flag);
                let app_event_tx_clone = self.app_event_tx.clone();

                thread::spawn(move || {
                    while animation_flag_clone.load(Ordering::Relaxed) {
                        thread::sleep(Duration::from_millis(200)); // Slower animation
                        app_event_tx_clone.send(AppEvent::RequestRedraw);
                    }
                });

                self.animation_running = Some(animation_flag);
            }
        } else {
            // Stop animation thread
            if let Some(animation_flag) = self.animation_running.take() {
                animation_flag.store(false, Ordering::Relaxed);
            }
        }
    }

    pub fn update_status_message(&mut self, message: String) {
        self.status_message = Self::map_status_message(&message);
    }

    pub fn flash_footer_notice(&mut self, text: String) {
        let expiry = std::time::Instant::now() + std::time::Duration::from_secs(2);
        self.footer_notice = Some((text, expiry));
    }

    // Control footer hint visibility
    pub fn set_show_reasoning_hint(&mut self, show: bool) {
        if self.show_reasoning_hint != show {
            self.show_reasoning_hint = show;
        }
    }

    pub fn set_show_diffs_hint(&mut self, show: bool) {
        if self.show_diffs_hint != show {
            self.show_diffs_hint = show;
        }
    }

    // Map technical status messages to user-friendly ones
    fn map_status_message(technical_message: &str) -> String {
        let lower = technical_message.to_lowercase();

        // Thinking/reasoning patterns
        if lower.contains("reasoning")
            || lower.contains("thinking")
            || lower.contains("planning")
            || lower.contains("waiting for model")
            || lower.contains("model")
        {
            "Thinking".to_string()
        }
        // Tool/command execution patterns
        else if lower.contains("tool")
            || lower.contains("command")
            || lower.contains("running command")
            || lower.contains("running")
            || lower.contains("bash")
            || lower.contains("shell")
        {
            "Using tools".to_string()
        }
        // Response generation patterns
        else if lower.contains("generating")
            || lower.contains("responding")
            || lower.contains("streaming")
            || lower.contains("writing response")
            || lower.contains("assistant")
            || lower.contains("chat completions")
            || lower.contains("completion")
        {
            "Responding".to_string()
        }
        // File/code editing patterns
        else if lower.contains("editing")
            || lower.contains("writing")
            || lower.contains("modifying")
            || lower.contains("creating file")
            || lower.contains("updating")
            || lower.contains("patch")
        {
            "Coding".to_string()
        }
        // Catch some common technical terms
        else if lower.contains("processing") || lower.contains("analyzing") {
            "Thinking".to_string()
        } else if lower.contains("reading") || lower.contains("searching") {
            "Reading".to_string()
        } else {
            // Default fallback - use "working" for unknown status
            "Working".to_string()
        }
    }

    pub fn desired_height(&self, width: u16) -> u16 {
        // Calculate hint/popup height
        let hint_height = match &self.active_popup {
            ActivePopup::None => 1u16,
            ActivePopup::Command(c) => c.calculate_required_height(),
            ActivePopup::File(c) => c.calculate_required_height(),
        };

        // Calculate actual content height of textarea
        // Account for: 2 border + 2 horizontal padding (1 left + 1 right)
        let content_width = width.saturating_sub(4); // 2 border + 2 horizontal padding
        let content_lines = self.textarea.desired_height(content_width).max(1); // At least 1 line

        // Total input height: content + border (2) only, no vertical padding
        // Minimum of 3 ensures at least 1 visible line with border
        let input_height = (content_lines + 2).max(3).min(15);

        input_height + hint_height
    }

    pub fn cursor_pos(&self, area: Rect) -> Option<(u16, u16)> {
        // Split area: textarea with border at top, hints/popup at bottom
        let hint_height = if matches!(self.active_popup, ActivePopup::None) {
            1
        } else {
            match &self.active_popup {
                ActivePopup::Command(popup) => popup.calculate_required_height(),
                ActivePopup::File(popup) => popup.calculate_required_height(),
                ActivePopup::None => 1,
            }
        };
        // Calculate dynamic height based on content
        let content_width = area.width.saturating_sub(4); // Account for border and padding
        let content_lines = self.textarea.desired_height(content_width).max(1);
        let desired_input_height = (content_lines + 2).max(3).min(15); // Dynamic with min/max

        // Use desired height but don't exceed available space
        let input_height = desired_input_height.min(area.height.saturating_sub(hint_height));
        let [input_area, _] = Layout::vertical([
            Constraint::Length(input_height),
            Constraint::Length(hint_height),
        ])
        .areas(area);

        // Get inner area of the bordered input box
        let input_block = Block::default().borders(Borders::ALL);
        let textarea_rect = input_block.inner(input_area);

        // Apply same padding as in render (1 char horizontal only, no vertical padding)
        let padded_textarea_rect = textarea_rect.inner(Margin::new(1, 0));

        let state = self.textarea_state.borrow();
        self.textarea
            .cursor_pos_with_state(padded_textarea_rect, &state)
    }

    /// Returns true if the composer currently contains no user input.
    #[allow(dead_code)]
    pub(crate) fn is_empty(&self) -> bool {
        self.textarea.is_empty()
    }

    /// Update the cached *context-left* percentage and refresh the placeholder
    /// text. The UI relies on the placeholder to convey the remaining
    /// context when the composer is empty.
    pub(crate) fn set_token_usage(
        &mut self,
        total_token_usage: TokenUsage,
        last_token_usage: TokenUsage,
        model_context_window: Option<u64>,
    ) {
        self.token_usage_info = Some(TokenUsageInfo {
            total_token_usage,
            last_token_usage,
            model_context_window,
        });
    }

    /// Record the history metadata advertised by `SessionConfiguredEvent` so
    /// that the composer can navigate cross-session history.
    pub(crate) fn set_history_metadata(&mut self, log_id: u64, entry_count: usize) {
        self.history.set_metadata(log_id, entry_count);
    }

    /// Integrate an asynchronous response to an on-demand history lookup. If
    /// the entry is present and the offset matches the current cursor we
    /// immediately populate the textarea.
    pub(crate) fn on_history_entry_response(
        &mut self,
        log_id: u64,
        offset: usize,
        entry: Option<String>,
    ) -> bool {
        let Some(text) = self.history.on_entry_response(log_id, offset, entry) else {
            return false;
        };
        self.textarea.set_text(&text);
        self.textarea.set_cursor(0);
        true
    }

    pub fn handle_paste(&mut self, pasted: String) -> bool {
        let char_count = pasted.chars().count();
        if char_count > LARGE_PASTE_CHAR_THRESHOLD {
            let placeholder = format!("[Pasted Content {char_count} chars]");
            self.textarea.insert_str(&placeholder);
            self.pending_pastes.push((placeholder, pasted));
        } else {
            self.textarea.insert_str(&pasted);
        }
        self.sync_command_popup();
        self.sync_file_search_popup();
        true
    }

    /// Integrate results from an asynchronous file search.
    pub(crate) fn on_file_search_result(&mut self, query: String, matches: Vec<FileMatch>) {
        // Only apply if user is still editing a token starting with `query`.
        let current_opt = Self::current_at_token(&self.textarea);
        let Some(current_token) = current_opt else {
            return;
        };

        if !current_token.starts_with(&query) {
            return;
        }

        if let ActivePopup::File(popup) = &mut self.active_popup {
            popup.set_matches(&query, matches);
        }
    }

    pub fn set_ctrl_c_quit_hint(&mut self, show: bool, has_focus: bool) {
        self.ctrl_c_quit_hint = show;
        self.set_has_focus(has_focus);
    }

    pub(crate) fn insert_str(&mut self, text: &str) {
        self.textarea.insert_str(text);
        self.sync_command_popup();
        self.sync_file_search_popup();
    }

    /// Handle a key event coming from the main UI.
    pub fn handle_key_event(&mut self, key_event: KeyEvent) -> (InputResult, bool) {
        let result = match &mut self.active_popup {
            ActivePopup::Command(_) => self.handle_key_event_with_slash_popup(key_event),
            ActivePopup::File(_) => self.handle_key_event_with_file_popup(key_event),
            ActivePopup::None => self.handle_key_event_without_popup(key_event),
        };

        // Update (or hide/show) popup after processing the key.
        self.sync_command_popup();
        if matches!(self.active_popup, ActivePopup::Command(_)) {
            self.dismissed_file_popup_token = None;
        } else {
            self.sync_file_search_popup();
        }

        result
    }

    /// Handle key event when the slash-command popup is visible.
    fn handle_key_event_with_slash_popup(&mut self, key_event: KeyEvent) -> (InputResult, bool) {
        let ActivePopup::Command(popup) = &mut self.active_popup else {
            unreachable!();
        };

        match key_event {
            KeyEvent {
                code: KeyCode::Up, ..
            } => {
                popup.move_up();
                (InputResult::None, true)
            }
            KeyEvent {
                code: KeyCode::Down,
                ..
            } => {
                popup.move_down();
                (InputResult::None, true)
            }
            KeyEvent {
                code: KeyCode::Tab, ..
            } => {
                if let Some(cmd) = popup.selected_command() {
                    let first_line = self.textarea.text().lines().next().unwrap_or("");

                    let starts_with_cmd = first_line
                        .trim_start()
                        .starts_with(&format!("/{}", cmd.command()));

                    if !starts_with_cmd {
                        self.textarea.set_text(&format!("/{} ", cmd.command()));
                    }
                    // After completing, place the cursor at the end of the
                    // slash command so the user can immediately type args.
                    let new_cursor = self.textarea.text().len();
                    self.textarea.set_cursor(new_cursor);
                }
                (InputResult::None, true)
            }
            KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                if let Some(cmd) = popup.selected_command() {
                    // Get the full command text before clearing
                    let command_text = self.textarea.text().to_string();

                    // Record the exact slash command that was typed
                    self.history.record_local_submission(&command_text);

                    // Check if this is a prompt-expanding command that will trigger agents
                    if cmd.is_prompt_expanding() {
                        self.app_event_tx.send(AppEvent::PrepareAgents);
                    }

                    // Send command to the app layer with full text.
                    self.app_event_tx
                        .send(AppEvent::DispatchCommand(*cmd, command_text.clone()));

                    // Clear textarea so no residual text remains.
                    self.textarea.set_text("");

                    // Hide popup since the command has been dispatched.
                    self.active_popup = ActivePopup::None;
                    return (InputResult::None, true);
                }
                // Fallback to default newline handling if no command selected.
                self.handle_key_event_without_popup(key_event)
            }
            input => self.handle_input_basic(input),
        }
    }

    /// Handle key events when file search popup is visible.
    fn handle_key_event_with_file_popup(&mut self, key_event: KeyEvent) -> (InputResult, bool) {
        let ActivePopup::File(popup) = &mut self.active_popup else {
            unreachable!();
        };

        match key_event {
            KeyEvent {
                code: KeyCode::Up, ..
            } => {
                popup.move_up();
                (InputResult::None, true)
            }
            KeyEvent {
                code: KeyCode::Down,
                ..
            } => {
                popup.move_down();
                (InputResult::None, true)
            }
            KeyEvent {
                code: KeyCode::Esc, ..
            } => {
                // Hide popup without modifying text, remember token to avoid immediate reopen.
                if let Some(tok) = Self::current_at_token(&self.textarea) {
                    self.dismissed_file_popup_token = Some(tok.to_string());
                }
                self.active_popup = ActivePopup::None;
                (InputResult::None, true)
            }
            KeyEvent {
                code: KeyCode::Tab, ..
            }
            | KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                if let Some(sel) = popup.selected_match() {
                    let sel_path = sel.to_string();
                    // Drop popup borrow before using self mutably again.
                    self.insert_selected_path(&sel_path);
                    self.active_popup = ActivePopup::None;
                    return (InputResult::None, true);
                }
                (InputResult::None, false)
            }
            input => self.handle_input_basic(input),
        }
    }

    /// Extract the `@token` that the cursor is currently positioned on, if any.
    ///
    /// The returned string **does not** include the leading `@`.
    ///
    /// Behavior:
    /// - The cursor may be anywhere *inside* the token (including on the
    ///   leading `@`). It does **not** need to be at the end of the line.
    /// - A token is delimited by ASCII whitespace (space, tab, newline).
    /// - If the token under the cursor starts with `@`, that token is
    ///   returned without the leading `@`. This includes the case where the
    ///   token is just "@" (empty query), which is used to trigger a UI hint
    fn current_at_token(textarea: &TextArea) -> Option<String> {
        let cursor_offset = textarea.cursor();
        let text = textarea.text();

        // Adjust the provided byte offset to the nearest valid char boundary at or before it.
        let mut safe_cursor = cursor_offset.min(text.len());
        // If we're not on a char boundary, move back to the start of the current char.
        if safe_cursor < text.len() && !text.is_char_boundary(safe_cursor) {
            // Find the last valid boundary <= cursor_offset.
            safe_cursor = text
                .char_indices()
                .map(|(i, _)| i)
                .take_while(|&i| i <= cursor_offset)
                .last()
                .unwrap_or(0);
        }

        // Split the line around the (now safe) cursor position.
        let before_cursor = &text[..safe_cursor];
        let after_cursor = &text[safe_cursor..];

        // Detect whether we're on whitespace at the cursor boundary.
        let at_whitespace = if safe_cursor < text.len() {
            text[safe_cursor..]
                .chars()
                .next()
                .map(|c| c.is_whitespace())
                .unwrap_or(false)
        } else {
            false
        };

        // Left candidate: token containing the cursor position.
        let start_left = before_cursor
            .char_indices()
            .rfind(|(_, c)| c.is_whitespace())
            .map(|(idx, c)| idx + c.len_utf8())
            .unwrap_or(0);
        let end_left_rel = after_cursor
            .char_indices()
            .find(|(_, c)| c.is_whitespace())
            .map(|(idx, _)| idx)
            .unwrap_or(after_cursor.len());
        let end_left = safe_cursor + end_left_rel;
        let token_left = if start_left < end_left {
            Some(&text[start_left..end_left])
        } else {
            None
        };

        // Right candidate: token immediately after any whitespace from the cursor.
        let ws_len_right: usize = after_cursor
            .chars()
            .take_while(|c| c.is_whitespace())
            .map(|c| c.len_utf8())
            .sum();
        let start_right = safe_cursor + ws_len_right;
        let end_right_rel = text[start_right..]
            .char_indices()
            .find(|(_, c)| c.is_whitespace())
            .map(|(idx, _)| idx)
            .unwrap_or(text.len() - start_right);
        let end_right = start_right + end_right_rel;
        let token_right = if start_right < end_right {
            Some(&text[start_right..end_right])
        } else {
            None
        };

        let left_at = token_left
            .filter(|t| t.starts_with('@'))
            .map(|t| t[1..].to_string());
        let right_at = token_right
            .filter(|t| t.starts_with('@'))
            .map(|t| t[1..].to_string());

        if at_whitespace {
            if right_at.is_some() {
                return right_at;
            }
            if token_left.is_some_and(|t| t == "@") {
                return None;
            }
            return left_at;
        }
        if after_cursor.starts_with('@') {
            return right_at.or(left_at);
        }
        left_at.or(right_at)
    }

    /// Replace the active `@token` (the one under the cursor) with `path`.
    ///
    /// The algorithm mirrors `current_at_token` so replacement works no matter
    /// where the cursor is within the token and regardless of how many
    /// `@tokens` exist in the line.
    fn insert_selected_path(&mut self, path: &str) {
        let cursor_offset = self.textarea.cursor();
        let text = self.textarea.text();

        let before_cursor = &text[..cursor_offset];
        let after_cursor = &text[cursor_offset..];

        // Determine token boundaries.
        let start_idx = before_cursor
            .char_indices()
            .rfind(|(_, c)| c.is_whitespace())
            .map(|(idx, c)| idx + c.len_utf8())
            .unwrap_or(0);

        let end_rel_idx = after_cursor
            .char_indices()
            .find(|(_, c)| c.is_whitespace())
            .map(|(idx, _)| idx)
            .unwrap_or(after_cursor.len());
        let end_idx = cursor_offset + end_rel_idx;

        // Replace the slice `[start_idx, end_idx)` with the chosen path and a trailing space.
        let mut new_text =
            String::with_capacity(text.len() - (end_idx - start_idx) + path.len() + 1);
        new_text.push_str(&text[..start_idx]);
        new_text.push_str(path);
        new_text.push(' ');
        new_text.push_str(&text[end_idx..]);

        self.textarea.set_text(&new_text);
        let new_cursor = start_idx.saturating_add(path.len()).saturating_add(1);
        self.textarea.set_cursor(new_cursor);
    }

    /// Handle key event when no popup is visible.
    fn handle_key_event_without_popup(&mut self, key_event: KeyEvent) -> (InputResult, bool) {
        match key_event {
            // -------------------------------------------------------------
            // Handle Esc key for double-press clearing
            // -------------------------------------------------------------
            KeyEvent {
                code: KeyCode::Esc, ..
            } => {
                // Check if this is a double-Esc press
                const DOUBLE_ESC_THRESHOLD: std::time::Duration =
                    std::time::Duration::from_millis(500);
                let now = std::time::Instant::now();

                if let Some(last_time) = self.last_esc_time {
                    if now.duration_since(last_time) < DOUBLE_ESC_THRESHOLD
                        && !self.textarea.is_empty()
                    {
                        // Double-Esc detected, clear the input field
                        self.textarea.set_text("");
                        self.pending_pastes.clear();
                        self.last_esc_time = None;
                        self.history.reset_navigation();
                        return (InputResult::None, true);
                    }
                }

                // Single Esc - just record the time
                self.last_esc_time = Some(now);
                (InputResult::None, false)
            }
            // -------------------------------------------------------------
            // Up/Down key handling - check modifiers to determine action
            // -------------------------------------------------------------
            KeyEvent {
                code: KeyCode::Up | KeyCode::Down,
                modifiers,
                ..
            } => {
                // Check if Shift is held for history navigation
                if modifiers.contains(KeyModifiers::SHIFT) {
                    // History navigation with Shift+Up/Down
                    if self
                        .history
                        .should_handle_navigation(self.textarea.text(), self.textarea.cursor())
                    {
                        let replace_text = match key_event.code {
                            KeyCode::Up => self
                                .history
                                .navigate_up(self.textarea.text(), &self.app_event_tx),
                            KeyCode::Down => self.history.navigate_down(&self.app_event_tx),
                            _ => None,
                        };
                        if let Some(text) = replace_text {
                            self.textarea.set_text(&text);
                            self.textarea.set_cursor(0);
                            return (InputResult::None, true);
                        }
                    }
                    // If history navigation didn't happen, just ignore the key
                    (InputResult::None, false)
                } else {
                    // No Shift modifier — move cursor within the input first.
                    // Only when already at the top-left/bottom-right should Up/Down scroll chat.
                    if self.textarea.is_empty() {
                        return match key_event.code {
                            KeyCode::Up => (InputResult::ScrollUp, false),
                            KeyCode::Down => (InputResult::ScrollDown, false),
                            _ => (InputResult::None, false),
                        };
                    }

                    let before = self.textarea.cursor();
                    let len = self.textarea.text().len();
                    match key_event.code {
                        KeyCode::Up => {
                            if before == 0 {
                                (InputResult::ScrollUp, false)
                            } else {
                                // Move up a visual/logical line; if already on first line, TextArea moves to start.
                                self.textarea.input(KeyEvent::new(KeyCode::Up, KeyModifiers::NONE));
                                (InputResult::None, true)
                            }
                        }
                        KeyCode::Down => {
                            if before == len {
                                (InputResult::ScrollDown, false)
                            } else {
                                // Move down a visual/logical line; if already on last line, TextArea moves to end.
                                self.textarea.input(KeyEvent::new(KeyCode::Down, KeyModifiers::NONE));
                                (InputResult::None, true)
                            }
                        }
                        _ => (InputResult::None, false),
                    }
                }
            }
            KeyEvent {
                code: KeyCode::Enter,
                modifiers: KeyModifiers::NONE,
                ..
            } => {
                // Record the exact text that was typed (before replacement)
                let original_text = self.textarea.text().to_string();

                let mut text = self.textarea.text().to_string();
                self.textarea.set_text("");

                // Replace all pending pastes in the text
                for (placeholder, actual) in &self.pending_pastes {
                    if text.contains(placeholder) {
                        text = text.replace(placeholder, actual);
                    }
                }
                self.pending_pastes.clear();

                if text.is_empty() {
                    (InputResult::None, true)
                } else {
                    // Check if this is a prompt-expanding command that will trigger agents
                    let trimmed = original_text.trim();
                    if trimmed.starts_with("/plan ")
                        || trimmed.starts_with("/solve ")
                        || trimmed.starts_with("/code ")
                    {
                        self.app_event_tx.send(AppEvent::PrepareAgents);
                    }

                    self.history.record_local_submission(&original_text);
                    (InputResult::Submitted(text), true)
                }
            }
            input => self.handle_input_basic(input),
        }
    }

    /// Handle generic Input events that modify the textarea content.
    fn handle_input_basic(&mut self, input: KeyEvent) -> (InputResult, bool) {
        let text_before = self.textarea.text().to_string();

        // Special handling for backspace on placeholders
        if let KeyEvent {
            code: KeyCode::Backspace,
            ..
        } = input
        {
            if self.try_remove_placeholder_at_cursor() {
                // Text was modified, reset history navigation
                self.history.reset_navigation();
                return (InputResult::None, true);
            }
        }

        // Normal input handling
        self.textarea.input(input);
        let text_after = self.textarea.text();

        // If text changed, reset history navigation state
        if text_before != text_after {
            self.history.reset_navigation();
        }

        // Check if any placeholders were removed and remove their corresponding pending pastes
        self.pending_pastes
            .retain(|(placeholder, _)| text_after.contains(placeholder));

        (InputResult::None, true)
    }

    /// Attempts to remove a placeholder if the cursor is at the end of one.
    /// Returns true if a placeholder was removed.
    fn try_remove_placeholder_at_cursor(&mut self) -> bool {
        let p = self.textarea.cursor();
        let text = self.textarea.text();

        // Find any placeholder that ends at the cursor position
        let placeholder_to_remove = self.pending_pastes.iter().find_map(|(ph, _)| {
            if p < ph.len() {
                return None;
            }
            let potential_ph_start = p - ph.len();
            if text[potential_ph_start..p] == *ph {
                Some(ph.clone())
            } else {
                None
            }
        });

        if let Some(placeholder) = placeholder_to_remove {
            self.textarea.replace_range(p - placeholder.len()..p, "");
            self.pending_pastes.retain(|(ph, _)| ph != &placeholder);
            true
        } else {
            false
        }
    }

    /// Synchronize `self.command_popup` with the current text in the
    /// textarea. This must be called after every modification that can change
    /// the text so the popup is shown/updated/hidden as appropriate.
    fn sync_command_popup(&mut self) {
        let first_line = self.textarea.text().lines().next().unwrap_or("");
        let input_starts_with_slash = first_line.starts_with('/');
        match &mut self.active_popup {
            ActivePopup::Command(popup) => {
                if input_starts_with_slash {
                    popup.on_composer_text_change(first_line.to_string());
                } else {
                    self.active_popup = ActivePopup::None;
                }
            }
            _ => {
                if input_starts_with_slash {
                    let mut command_popup = CommandPopup::new_with_filter(self.using_chatgpt_auth);
                    command_popup.on_composer_text_change(first_line.to_string());
                    self.active_popup = ActivePopup::Command(command_popup);
                }
            }
        }
    }

    /// Synchronize `self.file_search_popup` with the current text in the textarea.
    /// Note this is only called when self.active_popup is NOT Command.
    fn sync_file_search_popup(&mut self) {
        // Determine if there is an @token underneath the cursor.
        let query = match Self::current_at_token(&self.textarea) {
            Some(token) => token,
            None => {
                self.active_popup = ActivePopup::None;
                self.dismissed_file_popup_token = None;
                return;
            }
        };

        // If user dismissed popup for this exact query, don't reopen until text changes.
        if self.dismissed_file_popup_token.as_ref() == Some(&query) {
            return;
        }

        // Only trigger file search when the token has at least 2 characters to
        // reduce churn while typing the first character.
        if query.chars().count() >= 2 {
            self.app_event_tx
                .send(AppEvent::StartFileSearch(query.clone()));
        }

        match &mut self.active_popup {
            ActivePopup::File(popup) => {
                if query.is_empty() {
                    popup.set_empty_prompt();
                } else {
                    popup.set_query(&query);
                }
            }
            _ => {
                let mut popup = FileSearchPopup::new();
                if query.is_empty() {
                    popup.set_empty_prompt();
                } else {
                    popup.set_query(&query);
                }
                self.active_popup = ActivePopup::File(popup);
            }
        }

        self.current_file_query = Some(query);
        self.dismissed_file_popup_token = None;
    }

    fn set_has_focus(&mut self, has_focus: bool) {
        self.has_focus = has_focus;
    }
}

impl WidgetRef for &ChatComposer {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        let popup_height = match &self.active_popup {
            ActivePopup::Command(popup) => popup.calculate_required_height(),
            ActivePopup::File(popup) => popup.calculate_required_height(),
            ActivePopup::None => 1,
        };
        // Split area: textarea with border at top, hints/popup at bottom
        let hint_height = if matches!(self.active_popup, ActivePopup::None) {
            1
        } else {
            popup_height
        };

        // Calculate dynamic height based on content
        let content_width = area.width.saturating_sub(4); // Account for border and padding
        let content_lines = self.textarea.desired_height(content_width).max(1);
        let desired_input_height = (content_lines + 2).max(3).min(15); // Dynamic with min/max

        // Use desired height but don't exceed available space
        let input_height = desired_input_height.min(area.height.saturating_sub(hint_height));
        let [input_area, hint_area] = Layout::vertical([
            Constraint::Length(input_height),
            Constraint::Length(hint_height),
        ])
        .areas(area);
        match &self.active_popup {
            ActivePopup::Command(popup) => {
                popup.render_ref(hint_area, buf);
            }
            ActivePopup::File(popup) => {
                popup.render_ref(hint_area, buf);
            }
            ActivePopup::None => {
                let bottom_line_rect = hint_area;

                let key_hint_style = Style::default().fg(crate::colors::function());
                let label_style = Style::default().fg(crate::colors::text_dim());
                // Left side: padding + notices (and Ctrl+C again-to-quit notice if active)
                let mut left_spans: Vec<Span> = Vec::new();
                left_spans.push(Span::from(" "));

                if self.ctrl_c_quit_hint {
                    // Treat as a notice; keep on the left
                    left_spans.push(Span::from("Ctrl+C").style(key_hint_style));
                    left_spans.push(Span::from(" again to quit").style(label_style));
                }

                // Append ephemeral footer notice if present and not expired
                if let Some((msg, until)) = &self.footer_notice {
                    if std::time::Instant::now() <= *until {
                        if !self.ctrl_c_quit_hint { left_spans.push(Span::from("   ")); }
                        left_spans.push(Span::from(msg.clone()).style(Style::default().add_modifier(Modifier::DIM)));
                    }
                }

                // Right side: command key hints (Ctrl+R/D/C) followed by token usage if available
                let mut right_spans: Vec<Span> = Vec::new();
                let mut first_right = true;

                // Only show command hints on the right when not in the special Ctrl+C confirmation state
                if !self.ctrl_c_quit_hint {
                    if self.show_reasoning_hint {
                        if !first_right { right_spans.push(Span::from("  •  ").style(Style::default())); } else { first_right = false; }
                        right_spans.push(Span::from("Ctrl+R").style(key_hint_style));
                        right_spans.push(Span::from(" toggle reasoning").style(label_style));
                    }
                    if self.show_diffs_hint {
                        if !first_right { right_spans.push(Span::from("  •  ").style(Style::default())); } else { first_right = false; }
                        right_spans.push(Span::from("Ctrl+D").style(key_hint_style));
                        right_spans.push(Span::from(" diff viewer").style(label_style));
                    }
                    // Always show quit at the end of the command hints
                    if !first_right { right_spans.push(Span::from("  •  ").style(Style::default())); } else { first_right = false; }
                    right_spans.push(Span::from("Ctrl+C").style(key_hint_style));
                    right_spans.push(Span::from(" quit").style(label_style));
                }

                // Token usage (always shown to the far right when available)
                if let Some(token_usage_info) = &self.token_usage_info {
                    if !right_spans.is_empty() { right_spans.push(Span::from("  •  ").style(Style::default())); }

                    let token_usage = &token_usage_info.total_token_usage;
                    let used_str = format_with_thousands(token_usage.blended_total());
                    right_spans.push(Span::from(used_str).style(Style::default().add_modifier(Modifier::BOLD)));
                    right_spans.push(Span::from(" tokens used").style(Style::default()));

                    // Context remaining, if known
                    let last_token_usage = &token_usage_info.last_token_usage;
                    if let Some(context_window) = token_usage_info.model_context_window {
                        let percent_remaining: u8 = if context_window > 0 {
                            let percent = 100.0
                                - (last_token_usage.tokens_in_context_window() as f32
                                    / context_window as f32
                                    * 100.0);
                            percent.clamp(0.0, 100.0) as u8
                        } else {
                            100
                        };
                        right_spans.push(Span::from("  •  ").style(Style::default()));
                        right_spans.push(
                            Span::from(percent_remaining.to_string()).style(Style::default().add_modifier(Modifier::BOLD)),
                        );
                        right_spans.push(Span::from("% context left").style(Style::default()));
                    }
                }

                // Compute spacer between left and right to make right content right-aligned
                let left_len: usize = left_spans.iter().map(|s| s.content.chars().count()).sum();
                let right_len: usize = right_spans.iter().map(|s| s.content.chars().count()).sum();
                let total_width = bottom_line_rect.width as usize;
                let trailing_pad = 1usize; // one space on the right edge
                let spacer = if total_width > left_len + right_len + trailing_pad {
                    " ".repeat(total_width - left_len - right_len - trailing_pad)
                } else {
                    String::from(" ")
                };

                let mut line_spans = left_spans;
                line_spans.push(Span::from(spacer));
                line_spans.extend(right_spans);
                line_spans.push(Span::from(" "));

                Line::from(line_spans)
                    .style(Style::default().dim())
                    .render_ref(bottom_line_rect, buf);
            }
        }
        // Draw border around input area with optional "Coding" title when task is running
        let mut input_block = Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(crate::colors::border()));

        if self.is_task_running {
            use std::time::{SystemTime, UNIX_EPOCH};

            // Call this when a task starts; store it on self (e.g. self.task_seed)
            fn make_task_seed() -> u64 {
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_nanos() as u64
            }

            // Mix bits so low bits aren't parity-biased
            fn mix(mut x: u64) -> u64 {
                x ^= x >> 30;
                x = x.wrapping_mul(0xbf58476d1ce4e5b9);
                x ^= x >> 27;
                x = x.wrapping_mul(0x94d049bb133111eb);
                x ^ (x >> 31)
            }

            // Generate a per-render seed; good enough for varied spinners
            let seed = make_task_seed();
            let r = (mix(seed) % 200) as u64;

            // 1% ✨, 49.5% star, 49.5% diamond-family
            let selected_spinner: &[char] = if r < 2 {
                &['✨']
            } else if r < 101 {
                &['✧', '✦', '✧']
            } else {
                match r % 3 {
                    0 => &['✶'],
                    1 => &['◇'],
                    _ => &['◆'],
                }
            };

            let frame_idx = (SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis()
                / 150) as usize;

            let spinner = selected_spinner[frame_idx % selected_spinner.len()];

            // Create centered title with spinner and spaces
            let title_line = Line::from(vec![
                Span::raw(" "), // Space before spinner
                Span::styled(
                    spinner.to_string(),
                    Style::default().fg(crate::colors::secondary()),
                ),
                Span::styled(
                    format!(" {}... ", self.status_message),
                    Style::default().fg(crate::colors::secondary()),
                ), // Space after spinner and after text
            ])
            .centered();
            input_block = input_block.title(title_line);
        }

        let textarea_rect = input_block.inner(input_area);
        input_block.render_ref(input_area, buf);

        // Add padding inside the text area (1 char horizontal only, no vertical padding)
        let padded_textarea_rect = textarea_rect.inner(Margin::new(1, 0));

        let mut state = self.textarea_state.borrow_mut();
        StatefulWidgetRef::render_ref(&(&self.textarea), padded_textarea_rect, buf, &mut state);
        // Only show placeholder if there's no chat history AND no text typed
        if !self.has_chat_history && self.textarea.text().is_empty() {
            Line::from(BASE_PLACEHOLDER_TEXT)
                .style(Style::default().dim())
                .render_ref(padded_textarea_rect, buf);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::app_event::AppEvent;
    use crate::bottom_pane::AppEventSender;
    use crate::bottom_pane::ChatComposer;
    use crate::bottom_pane::InputResult;
    use crate::bottom_pane::chat_composer::LARGE_PASTE_CHAR_THRESHOLD;
    use crate::bottom_pane::textarea::TextArea;

    #[test]
    fn test_current_at_token_basic_cases() {
        let test_cases = vec![
            // Valid @ tokens
            ("@hello", 3, Some("hello".to_string()), "Basic ASCII token"),
            (
                "@file.txt",
                4,
                Some("file.txt".to_string()),
                "ASCII with extension",
            ),
            (
                "hello @world test",
                8,
                Some("world".to_string()),
                "ASCII token in middle",
            ),
            (
                "@test123",
                5,
                Some("test123".to_string()),
                "ASCII with numbers",
            ),
            // Unicode examples
            ("@İstanbul", 3, Some("İstanbul".to_string()), "Turkish text"),
            (
                "@testЙЦУ.rs",
                8,
                Some("testЙЦУ.rs".to_string()),
                "Mixed ASCII and Cyrillic",
            ),
            ("@诶", 2, Some("诶".to_string()), "Chinese character"),
            ("@👍", 2, Some("👍".to_string()), "Emoji token"),
            // Invalid cases (should return None)
            ("hello", 2, None, "No @ symbol"),
            (
                "@",
                1,
                Some("".to_string()),
                "Only @ symbol triggers empty query",
            ),
            ("@ hello", 2, None, "@ followed by space"),
            ("test @ world", 6, None, "@ with spaces around"),
        ];

        for (input, cursor_pos, expected, description) in test_cases {
            let mut textarea = TextArea::new();
            textarea.insert_str(input);
            textarea.set_cursor(cursor_pos);

            let result = ChatComposer::current_at_token(&textarea);
            assert_eq!(
                result, expected,
                "Failed for case: {description} - input: '{input}', cursor: {cursor_pos}"
            );
        }
    }

    #[test]
    fn test_current_at_token_cursor_positions() {
        let test_cases = vec![
            // Different cursor positions within a token
            ("@test", 0, Some("test".to_string()), "Cursor at @"),
            ("@test", 1, Some("test".to_string()), "Cursor after @"),
            ("@test", 5, Some("test".to_string()), "Cursor at end"),
            // Multiple tokens - cursor determines which token
            ("@file1 @file2", 0, Some("file1".to_string()), "First token"),
            (
                "@file1 @file2",
                8,
                Some("file2".to_string()),
                "Second token",
            ),
            // Edge cases
            ("@", 0, Some("".to_string()), "Only @ symbol"),
            ("@a", 2, Some("a".to_string()), "Single character after @"),
            ("", 0, None, "Empty input"),
        ];

        for (input, cursor_pos, expected, description) in test_cases {
            let mut textarea = TextArea::new();
            textarea.insert_str(input);
            textarea.set_cursor(cursor_pos);

            let result = ChatComposer::current_at_token(&textarea);
            assert_eq!(
                result, expected,
                "Failed for cursor position case: {description} - input: '{input}', cursor: {cursor_pos}",
            );
        }
    }

    #[test]
    fn test_current_at_token_whitespace_boundaries() {
        let test_cases = vec![
            // Space boundaries
            (
                "aaa@aaa",
                4,
                None,
                "Connected @ token - no completion by design",
            ),
            (
                "aaa @aaa",
                5,
                Some("aaa".to_string()),
                "@ token after space",
            ),
            (
                "test @file.txt",
                7,
                Some("file.txt".to_string()),
                "@ token after space",
            ),
            // Full-width space boundaries
            (
                "test　@İstanbul",
                8,
                Some("İstanbul".to_string()),
                "@ token after full-width space",
            ),
            (
                "@ЙЦУ　@诶",
                10,
                Some("诶".to_string()),
                "Full-width space between Unicode tokens",
            ),
            // Tab and newline boundaries
            (
                "test\t@file",
                6,
                Some("file".to_string()),
                "@ token after tab",
            ),
        ];

        for (input, cursor_pos, expected, description) in test_cases {
            let mut textarea = TextArea::new();
            textarea.insert_str(input);
            textarea.set_cursor(cursor_pos);

            let result = ChatComposer::current_at_token(&textarea);
            assert_eq!(
                result, expected,
                "Failed for whitespace boundary case: {description} - input: '{input}', cursor: {cursor_pos}",
            );
        }
    }

    #[test]
    fn handle_paste_small_inserts_text() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;

        let (tx, _rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut composer = ChatComposer::new(true, sender, false);

        let needs_redraw = composer.handle_paste("hello".to_string());
        assert!(needs_redraw);
        assert_eq!(composer.textarea.text(), "hello");
        assert!(composer.pending_pastes.is_empty());

        let (result, _) =
            composer.handle_key_event(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
        match result {
            InputResult::Submitted(text) => assert_eq!(text, "hello"),
            _ => panic!("expected Submitted"),
        }
    }

    #[test]
    fn handle_paste_large_uses_placeholder_and_replaces_on_submit() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;

        let (tx, _rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut composer = ChatComposer::new(true, sender, false);

        let large = "x".repeat(LARGE_PASTE_CHAR_THRESHOLD + 10);
        let needs_redraw = composer.handle_paste(large.clone());
        assert!(needs_redraw);
        let placeholder = format!("[Pasted Content {} chars]", large.chars().count());
        assert_eq!(composer.textarea.text(), placeholder);
        assert_eq!(composer.pending_pastes.len(), 1);
        assert_eq!(composer.pending_pastes[0].0, placeholder);
        assert_eq!(composer.pending_pastes[0].1, large);

        let (result, _) =
            composer.handle_key_event(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
        match result {
            InputResult::Submitted(text) => assert_eq!(text, large),
            _ => panic!("expected Submitted"),
        }
        assert!(composer.pending_pastes.is_empty());
    }

    #[test]
    fn edit_clears_pending_paste() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;

        let large = "y".repeat(LARGE_PASTE_CHAR_THRESHOLD + 1);
        let (tx, _rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut composer = ChatComposer::new(true, sender, false);

        composer.handle_paste(large);
        assert_eq!(composer.pending_pastes.len(), 1);

        // Any edit that removes the placeholder should clear pending_paste
        composer.handle_key_event(KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE));
        assert!(composer.pending_pastes.is_empty());
    }

    #[test]
    fn ui_snapshots() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;
        use insta::assert_snapshot;
        use ratatui::Terminal;
        use ratatui::backend::TestBackend;

        let (tx, _rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut terminal = match Terminal::new(TestBackend::new(100, 10)) {
            Ok(t) => t,
            Err(e) => panic!("Failed to create terminal: {e}"),
        };

        let test_cases = vec![
            ("empty", None),
            ("small", Some("short".to_string())),
            ("large", Some("z".repeat(LARGE_PASTE_CHAR_THRESHOLD + 5))),
            ("multiple_pastes", None),
            ("backspace_after_pastes", None),
        ];

        for (name, input) in test_cases {
            // Create a fresh composer for each test case
            let mut composer = ChatComposer::new(true, sender.clone(), false);

            if let Some(text) = input {
                composer.handle_paste(text);
            } else if name == "multiple_pastes" {
                // First large paste
                composer.handle_paste("x".repeat(LARGE_PASTE_CHAR_THRESHOLD + 3));
                // Second large paste
                composer.handle_paste("y".repeat(LARGE_PASTE_CHAR_THRESHOLD + 7));
                // Small paste
                composer.handle_paste(" another short paste".to_string());
            } else if name == "backspace_after_pastes" {
                // Three large pastes
                composer.handle_paste("a".repeat(LARGE_PASTE_CHAR_THRESHOLD + 2));
                composer.handle_paste("b".repeat(LARGE_PASTE_CHAR_THRESHOLD + 4));
                composer.handle_paste("c".repeat(LARGE_PASTE_CHAR_THRESHOLD + 6));
                // Move cursor to end and press backspace
                composer.textarea.set_cursor(composer.textarea.text().len());
                composer.handle_key_event(KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE));
            }

            terminal
                .draw(|f| f.render_widget_ref(&composer, f.area()))
                .unwrap_or_else(|e| panic!("Failed to draw {name} composer: {e}"));

            assert_snapshot!(name, terminal.backend());
        }
    }

    #[test]
    fn slash_init_dispatches_command_and_does_not_submit_literal_text() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;
        use std::sync::mpsc::TryRecvError;

        let (tx, rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut composer = ChatComposer::new(true, sender, false);

        // Type the slash command.
        for ch in [
            '/', 'i', 'n', 'i', 't', // "/init"
        ] {
            let _ = composer.handle_key_event(KeyEvent::new(KeyCode::Char(ch), KeyModifiers::NONE));
        }

        // Press Enter to dispatch the selected command.
        let (result, _needs_redraw) =
            composer.handle_key_event(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));

        // When a slash command is dispatched, the composer should not submit
        // literal text and should clear its textarea.
        match result {
            InputResult::None | InputResult::ScrollUp | InputResult::ScrollDown => {}
            InputResult::Submitted(text) => {
                panic!("expected command dispatch, but composer submitted literal text: {text}")
            }
        }
        assert!(composer.textarea.is_empty(), "composer should be cleared");

        // Verify a DispatchCommand event for the "init" command was sent.
        match rx.try_recv() {
            Ok(AppEvent::DispatchCommand(cmd, text)) => {
                assert_eq!(cmd.command(), "init");
                assert_eq!(text, "/init");
            }
            Ok(_other) => panic!("unexpected app event"),
            Err(TryRecvError::Empty) => panic!("expected a DispatchCommand event for '/init'"),
            Err(TryRecvError::Disconnected) => panic!("app event channel disconnected"),
        }
    }

    #[test]
    fn slash_mention_dispatches_command_and_inserts_at() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;
        use std::sync::mpsc::TryRecvError;

        let (tx, rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut composer = ChatComposer::new(true, sender, false);

        for ch in ['/', 'm', 'e', 'n', 't', 'i', 'o', 'n'] {
            let _ = composer.handle_key_event(KeyEvent::new(KeyCode::Char(ch), KeyModifiers::NONE));
        }

        let (result, _needs_redraw) =
            composer.handle_key_event(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));

        match result {
            InputResult::None | InputResult::ScrollUp | InputResult::ScrollDown => {}
            InputResult::Submitted(text) => {
                panic!("expected command dispatch, but composer submitted literal text: {text}")
            }
        }
        assert!(composer.textarea.is_empty(), "composer should be cleared");

        match rx.try_recv() {
            Ok(AppEvent::DispatchCommand(cmd, _)) => {
                assert_eq!(cmd.command(), "mention");
                composer.insert_str("@");
            }
            Ok(_other) => panic!("unexpected app event"),
            Err(TryRecvError::Empty) => panic!("expected a DispatchCommand event for '/mention'"),
            Err(TryRecvError::Disconnected) => {
                panic!("app event channel disconnected")
            }
        }
        assert_eq!(composer.textarea.text(), "@");
    }

    #[test]
    fn test_multiple_pastes_submission() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;

        let (tx, _rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut composer = ChatComposer::new(true, sender, false);

        // Define test cases: (paste content, is_large)
        let test_cases = [
            ("x".repeat(LARGE_PASTE_CHAR_THRESHOLD + 3), true),
            (" and ".to_string(), false),
            ("y".repeat(LARGE_PASTE_CHAR_THRESHOLD + 7), true),
        ];

        // Expected states after each paste
        let mut expected_text = String::new();
        let mut expected_pending_count = 0;

        // Apply all pastes and build expected state
        let states: Vec<_> = test_cases
            .iter()
            .map(|(content, is_large)| {
                composer.handle_paste(content.clone());
                if *is_large {
                    let placeholder = format!("[Pasted Content {} chars]", content.chars().count());
                    expected_text.push_str(&placeholder);
                    expected_pending_count += 1;
                } else {
                    expected_text.push_str(content);
                }
                (expected_text.clone(), expected_pending_count)
            })
            .collect();

        // Verify all intermediate states were correct
        assert_eq!(
            states,
            vec![
                (
                    format!("[Pasted Content {} chars]", test_cases[0].0.chars().count()),
                    1
                ),
                (
                    format!(
                        "[Pasted Content {} chars] and ",
                        test_cases[0].0.chars().count()
                    ),
                    1
                ),
                (
                    format!(
                        "[Pasted Content {} chars] and [Pasted Content {} chars]",
                        test_cases[0].0.chars().count(),
                        test_cases[2].0.chars().count()
                    ),
                    2
                ),
            ]
        );

        // Submit and verify final expansion
        let (result, _) =
            composer.handle_key_event(KeyEvent::new(KeyCode::Enter, KeyModifiers::NONE));
        if let InputResult::Submitted(text) = result {
            assert_eq!(text, format!("{} and {}", test_cases[0].0, test_cases[2].0));
        } else {
            panic!("expected Submitted");
        }
    }

    #[test]
    fn test_placeholder_deletion() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;

        let (tx, _rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut composer = ChatComposer::new(true, sender, false);

        // Define test cases: (content, is_large)
        let test_cases = [
            ("a".repeat(LARGE_PASTE_CHAR_THRESHOLD + 5), true),
            (" and ".to_string(), false),
            ("b".repeat(LARGE_PASTE_CHAR_THRESHOLD + 6), true),
        ];

        // Apply all pastes
        let mut current_pos = 0;
        let states: Vec<_> = test_cases
            .iter()
            .map(|(content, is_large)| {
                composer.handle_paste(content.clone());
                if *is_large {
                    let placeholder = format!("[Pasted Content {} chars]", content.chars().count());
                    current_pos += placeholder.len();
                } else {
                    current_pos += content.len();
                }
                (
                    composer.textarea.text().to_string(),
                    composer.pending_pastes.len(),
                    current_pos,
                )
            })
            .collect();

        // Delete placeholders one by one and collect states
        let mut deletion_states = vec![];

        // First deletion
        composer.textarea.set_cursor(states[0].2);
        composer.handle_key_event(KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE));
        deletion_states.push((
            composer.textarea.text().to_string(),
            composer.pending_pastes.len(),
        ));

        // Second deletion
        composer.textarea.set_cursor(composer.textarea.text().len());
        composer.handle_key_event(KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE));
        deletion_states.push((
            composer.textarea.text().to_string(),
            composer.pending_pastes.len(),
        ));

        // Verify all states
        assert_eq!(
            deletion_states,
            vec![
                (" and [Pasted Content 1006 chars]".to_string(), 1),
                (" and ".to_string(), 0),
            ]
        );
    }

    #[test]
    fn test_partial_placeholder_deletion() {
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;

        let (tx, _rx) = std::sync::mpsc::channel();
        let sender = AppEventSender::new(tx);
        let mut composer = ChatComposer::new(true, sender, false);

        // Define test cases: (cursor_position_from_end, expected_pending_count)
        let test_cases = [
            5, // Delete from middle - should clear tracking
            0, // Delete from end - should clear tracking
        ];

        let paste = "x".repeat(LARGE_PASTE_CHAR_THRESHOLD + 4);
        let placeholder = format!("[Pasted Content {} chars]", paste.chars().count());

        let states: Vec<_> = test_cases
            .into_iter()
            .map(|pos_from_end| {
                composer.handle_paste(paste.clone());
                composer
                    .textarea
                    .set_cursor((placeholder.len() - pos_from_end) as usize);
                composer.handle_key_event(KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE));
                let result = (
                    composer.textarea.text().contains(&placeholder),
                    composer.pending_pastes.len(),
                );
                composer.textarea.set_text("");
                result
            })
            .collect();

        assert_eq!(
            states,
            vec![
                (false, 0), // After deleting from middle
                (false, 0), // After deleting from end
            ]
        );
    }
}
