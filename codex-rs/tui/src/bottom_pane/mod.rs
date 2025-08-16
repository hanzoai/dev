//! Bottom pane: shows the ChatComposer or a BottomPaneView, if one is active.

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::user_approval_widget::ApprovalRequest;
use bottom_pane_view::BottomPaneView;
use codex_core::protocol::TokenUsage;
use codex_file_search::FileMatch;
use crossterm::event::KeyEvent;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::widgets::WidgetRef;

mod approval_modal_view;
mod bottom_pane_view;
mod chat_composer;
mod chat_composer_history;
pub mod chrome_selection_view;
mod diff_popup;
mod command_popup;
mod file_search_popup;
mod live_ring_widget;
mod popup_consts;
mod reasoning_selection_view;
mod scroll_state;
mod selection_popup_common;
mod textarea;
mod theme_selection_view;
mod verbosity_selection_view;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CancellationEvent {
    Ignored,
    Handled,
}

pub(crate) use chat_composer::ChatComposer;
pub(crate) use chat_composer::InputResult;

use approval_modal_view::ApprovalModalView;
use codex_core::config_types::ReasoningEffort;
use codex_core::config_types::TextVerbosity;
use codex_core::config_types::ThemeName;
use reasoning_selection_view::ReasoningSelectionView;
use theme_selection_view::ThemeSelectionView;
use verbosity_selection_view::VerbositySelectionView;

/// Pane displayed in the lower half of the chat UI.
pub(crate) struct BottomPane<'a> {
    /// Composer is retained even when a BottomPaneView is displayed so the
    /// input state is retained when the view is closed.
    composer: ChatComposer,

    /// If present, this is displayed instead of the `composer`.
    active_view: Option<Box<dyn BottomPaneView<'a> + 'a>>,

    app_event_tx: AppEventSender,
    has_input_focus: bool,
    is_task_running: bool,
    ctrl_c_quit_hint: bool,

    /// Optional transient ring shown above the composer. This is a rendering-only
    /// container used during development before we wire it to ChatWidget events.
    live_ring: Option<live_ring_widget::LiveRingWidget>,

    /// True if the active view is the StatusIndicatorView that replaces the
    /// composer during a running task.
    status_view_active: bool,

    /// Whether to reserve an empty spacer line above the input composer.
    /// Defaults to true for visual breathing room, but can be disabled when
    /// the chat history is scrolled up to allow history to reclaim that row.
    top_spacer_enabled: bool,
}

pub(crate) struct BottomPaneParams {
    pub(crate) app_event_tx: AppEventSender,
    pub(crate) has_input_focus: bool,
    pub(crate) enhanced_keys_supported: bool,
    pub(crate) using_chatgpt_auth: bool,
}

impl BottomPane<'_> {
    // Reduce bottom padding so footer sits one line lower
    const BOTTOM_PAD_LINES: u16 = 1;
    pub fn new(params: BottomPaneParams) -> Self {
        let enhanced_keys_supported = params.enhanced_keys_supported;
        Self {
            composer: ChatComposer::new(
                params.has_input_focus,
                params.app_event_tx.clone(),
                enhanced_keys_supported,
                params.using_chatgpt_auth,
            ),
            active_view: None,
            app_event_tx: params.app_event_tx,
            has_input_focus: params.has_input_focus,
            is_task_running: false,
            ctrl_c_quit_hint: false,
            live_ring: None,
            status_view_active: false,
            top_spacer_enabled: true,
        }
    }

    pub fn set_has_chat_history(&mut self, has_history: bool) {
        self.composer.set_has_chat_history(has_history);
    }

    pub fn desired_height(&self, width: u16) -> u16 {
        let ring_h = self
            .live_ring
            .as_ref()
            .map(|r| r.desired_height(width))
            .unwrap_or(0);

        let view_height = if let Some(view) = self.active_view.as_ref() {
            view.desired_height(width)
        } else {
            // Optionally add 1 for the empty line above the composer
            let spacer = if self.top_spacer_enabled { 1 } else { 0 };
            spacer + self.composer.desired_height(width)
        };

        ring_h
            .saturating_add(view_height)
            .saturating_add(Self::BOTTOM_PAD_LINES)
    }

    pub fn cursor_pos(&self, area: Rect) -> Option<(u16, u16)> {
        // Hide the cursor whenever an overlay view is active (e.g. approval modal).
        // But keep cursor visible when only status overlay is shown.
        if self.active_view.is_some() {
            None
        } else {
            // Account for the optional empty line above the composer
            let y_offset = if self.top_spacer_enabled { 1u16 } else { 0u16 };

            // Adjust composer area to account for empty line and padding
            let horizontal_padding = 1u16; // Message input uses 1 char padding
            let composer_rect = Rect {
                x: area.x + horizontal_padding,
                y: area.y + y_offset,
                width: area.width.saturating_sub(horizontal_padding * 2),
                height: (area.height.saturating_sub(y_offset))
                    - BottomPane::BOTTOM_PAD_LINES
                        .min((area.height.saturating_sub(y_offset)).saturating_sub(1)),
            };
            self.composer.cursor_pos(composer_rect)
        }
    }

    /// Forward a key event to the active view or the composer.
    pub fn handle_key_event(&mut self, key_event: KeyEvent) -> InputResult {
        if let Some(mut view) = self.active_view.take() {
            view.handle_key_event(self, key_event);
            if !view.is_complete() {
                self.active_view = Some(view);
            }
            // Don't create a status view - keep composer visible
            self.request_immediate_redraw();
            InputResult::None
        } else {
            let (input_result, needs_redraw) = self.composer.handle_key_event(key_event);
            if needs_redraw {
                self.request_immediate_redraw();
            }
            input_result
        }
    }

    /// Handle Ctrl-C in the bottom pane. If a modal view is active it gets a
    /// chance to consume the event (e.g. to dismiss itself).
    pub(crate) fn on_ctrl_c(&mut self) -> CancellationEvent {
        let mut view = match self.active_view.take() {
            Some(view) => view,
            None => return CancellationEvent::Ignored,
        };

        let event = view.on_ctrl_c(self);
        match event {
            CancellationEvent::Handled => {
                if !view.is_complete() {
                    self.active_view = Some(view);
                }
                // Don't create a status view - keep composer visible
                self.show_ctrl_c_quit_hint();
            }
            CancellationEvent::Ignored => {
                self.active_view = Some(view);
            }
        }
        event
    }

    pub fn handle_paste(&mut self, pasted: String) {
        if self.active_view.is_none() {
            let needs_redraw = self.composer.handle_paste(pasted);
            if needs_redraw {
                self.request_immediate_redraw();
            }
        }
    }

    pub(crate) fn insert_str(&mut self, text: &str) {
        self.composer.insert_str(text);
        self.request_redraw();
    }

    /// Enable or disable compact compose mode. When enabled, the spacer line
    /// above the input composer is removed so the history can scroll into that
    /// row. This is typically toggled when the user scrolls up.
    pub(crate) fn set_compact_compose(&mut self, compact: bool) {
        let new_enabled = !compact;
        if self.top_spacer_enabled != new_enabled {
            self.top_spacer_enabled = new_enabled;
            self.request_redraw();
        }
    }

    /// Update the status indicator text. Shows status as overlay above composer
    /// to allow continued input while processing.
    pub(crate) fn update_status_text(&mut self, text: String) {
        // If there's an active modal view that can handle status updates, let it
        if let Some(view) = self.active_view.as_mut() {
            if matches!(
                view.update_status_text(text.clone()),
                bottom_pane_view::ConditionalUpdate::NeedsRedraw
            ) {
                self.request_redraw();
                return;
            }
        }

        // Pass status message to composer for dynamic title display
        self.composer.update_status_message(text);
        self.request_redraw();
    }

    pub(crate) fn show_ctrl_c_quit_hint(&mut self) {
        self.ctrl_c_quit_hint = true;
        self.composer
            .set_ctrl_c_quit_hint(true, self.has_input_focus);
        self.request_redraw();
    }

    pub(crate) fn clear_ctrl_c_quit_hint(&mut self) {
        if self.ctrl_c_quit_hint {
            self.ctrl_c_quit_hint = false;
            self.composer
                .set_ctrl_c_quit_hint(false, self.has_input_focus);
            self.request_redraw();
        }
    }

    pub(crate) fn ctrl_c_quit_hint_visible(&self) -> bool {
        self.ctrl_c_quit_hint
    }

    pub fn set_task_running(&mut self, running: bool) {
        self.is_task_running = running;
        self.composer.set_task_running(running);

        if running {
            // No longer need separate status widget - title shows in composer
            self.request_redraw();
        } else {
            // Status now shown in composer title
            // Drop the status view when a task completes, but keep other
            // modal views (e.g. approval dialogs).
            if let Some(mut view) = self.active_view.take() {
                if !view.should_hide_when_task_is_done() {
                    self.active_view = Some(view);
                }
                self.status_view_active = false;
            }
        }
    }

    #[allow(dead_code)]
    pub(crate) fn composer_is_empty(&self) -> bool {
        self.composer.is_empty()
    }

    pub(crate) fn is_task_running(&self) -> bool {
        self.is_task_running
    }

    /// Update the *context-window remaining* indicator in the composer. This
    /// is forwarded directly to the underlying `ChatComposer`.
    pub(crate) fn set_token_usage(
        &mut self,
        total_token_usage: TokenUsage,
        last_token_usage: TokenUsage,
        model_context_window: Option<u64>,
    ) {
        self.composer
            .set_token_usage(total_token_usage, last_token_usage, model_context_window);
        self.request_redraw();
    }

    /// Called when the agent requests user approval.
    pub fn push_approval_request(&mut self, request: ApprovalRequest) {
        let request = if let Some(view) = self.active_view.as_mut() {
            match view.try_consume_approval_request(request) {
                Some(request) => request,
                None => {
                    self.request_redraw();
                    return;
                }
            }
        } else {
            request
        };

        // Otherwise create a new approval modal overlay.
        let modal = ApprovalModalView::new(request, self.app_event_tx.clone());
        self.active_view = Some(Box::new(modal));
        // Hide any overlay status while a modal is visible.
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the reasoning selection UI
    pub fn show_reasoning_selection(&mut self, current_effort: ReasoningEffort) {
        let view = ReasoningSelectionView::new(current_effort, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the theme selection UI
    pub fn show_theme_selection(&mut self, current_theme: ThemeName) {
        let view = ThemeSelectionView::new(current_theme, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the Chrome launch options UI
    pub fn show_chrome_selection(&mut self, port: Option<u16>) {
        use chrome_selection_view::ChromeSelectionView;
        let view = ChromeSelectionView::new(self.app_event_tx.clone(), port);
        self.active_view = Some(Box::new(view));
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the diffs popup with tabs for each file.
    #[allow(dead_code)]
    pub fn show_diff_popup(&mut self, tabs: Vec<(String, Vec<ratatui::text::Line<'static>>)>) {
        let view = diff_popup::DiffPopupView::new(tabs);
        self.active_view = Some(Box::new(view));
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the verbosity selection UI
    pub fn show_verbosity_selection(&mut self, current_verbosity: TextVerbosity) {
        let view = VerbositySelectionView::new(current_verbosity, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Height (terminal rows) required by the current bottom pane.
    pub(crate) fn request_redraw(&self) {
        self.app_event_tx.send(AppEvent::RequestRedraw)
    }

    pub(crate) fn request_immediate_redraw(&self) {
        self.app_event_tx.send(AppEvent::ImmediateRedraw)
    }

    pub(crate) fn flash_footer_notice(&mut self, text: String) {
        self.composer.flash_footer_notice(text);
        // Ask app to schedule a redraw shortly to clear the notice automatically
        self.app_event_tx
            .send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(2100)));
        self.request_redraw();
    }

    /// Control footer hint visibility: whether to show Ctrl+R (reasoning) and Ctrl+D (diffs)
    #[allow(dead_code)]
    pub(crate) fn set_footer_hints(&mut self, show_reasoning: bool, show_diffs: bool) {
        self.composer.set_show_reasoning_hint(show_reasoning);
        self.composer.set_show_diffs_hint(show_diffs);
        self.request_redraw();
    }

    /// Convenience setters for individual hints
    pub(crate) fn set_reasoning_hint(&mut self, show: bool) {
        self.composer.set_show_reasoning_hint(show);
        self.request_redraw();
    }

    pub(crate) fn set_reasoning_state(&mut self, shown: bool) {
        self.composer.set_reasoning_state(shown);
        self.request_redraw();
    }

    pub(crate) fn set_diffs_hint(&mut self, show: bool) {
        self.composer.set_show_diffs_hint(show);
        self.request_redraw();
    }

    // --- History helpers ---

    pub(crate) fn set_history_metadata(&mut self, log_id: u64, entry_count: usize) {
        self.composer.set_history_metadata(log_id, entry_count);
    }

    pub(crate) fn on_history_entry_response(
        &mut self,
        log_id: u64,
        offset: usize,
        entry: Option<String>,
    ) {
        let updated = self
            .composer
            .on_history_entry_response(log_id, offset, entry);

        if updated {
            self.request_redraw();
        }
    }

    pub(crate) fn on_file_search_result(&mut self, query: String, matches: Vec<FileMatch>) {
        self.composer.on_file_search_result(query, matches);
        self.request_redraw();
    }

    pub(crate) fn clear_live_ring(&mut self) {
        self.live_ring = None;
    }
    
    /// Set the live ring rows and maximum rows to display
    #[cfg(test)]
    pub(crate) fn set_live_ring_rows(&mut self, max_rows: usize, rows: Vec<Line<'static>>) {
        self.live_ring = Some(live_ring_widget::LiveRingWidget::new(max_rows, rows));
    }

    /// Ensure input focus is maintained, especially after redraws or content updates
    pub(crate) fn ensure_input_focus(&mut self) {
        // Only ensure focus if there's no active modal view
        if self.active_view.is_none() {
            self.has_input_focus = true;
            // Reset any transient state that might affect focus
            // Clear any temporary status overlays that might interfere
            if !self.is_task_running {
                // Status now shown in composer title
            }
            // Ensure composer knows it has focus
            self.composer
                .set_ctrl_c_quit_hint(self.ctrl_c_quit_hint, self.has_input_focus);
        }
    }

    // Removed restart_live_status_with_text – no longer used by the current streaming UI.
}

impl WidgetRef for &BottomPane<'_> {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        let mut y_offset = 0u16;
        if let Some(ring) = &self.live_ring {
            let live_h = ring.desired_height(area.width).min(area.height);
            if live_h > 0 {
                let live_rect = Rect {
                    x: area.x,
                    y: area.y,
                    width: area.width,
                    height: live_h,
                };
                ring.render_ref(live_rect, buf);
                y_offset = live_h;
            }
        }
        // Spacer between live ring and status view when active
        if self.live_ring.is_some() && self.status_view_active && y_offset < area.height {
            // Leave one empty line
            y_offset = y_offset.saturating_add(1);
        }

        if let Some(view) = &self.active_view {
            if y_offset < area.height {
                // Reserve bottom padding lines; keep at least 1 line for the view.
                let avail = area.height - y_offset;
                let pad = BottomPane::BOTTOM_PAD_LINES.min(avail.saturating_sub(1));
                // Add horizontal padding (2 chars on each side) for views
                let horizontal_padding = 1u16;
                let view_rect = Rect {
                    x: area.x + horizontal_padding,
                    y: area.y + y_offset,
                    width: area.width.saturating_sub(horizontal_padding * 2),
                    height: avail - pad,
                };
                view.render(view_rect, buf);
            }
        } else if y_offset < area.height {
            // Optionally add an empty line above the input box
            if self.top_spacer_enabled {
                y_offset = y_offset.saturating_add(1);
            }

            // Add horizontal padding (2 chars on each side) for Message input
            let horizontal_padding = 1u16;
            let composer_rect = Rect {
                x: area.x + horizontal_padding,
                y: area.y + y_offset,
                width: area.width.saturating_sub(horizontal_padding * 2),
                // Reserve bottom padding
                height: (area.height - y_offset)
                    - BottomPane::BOTTOM_PAD_LINES.min((area.height - y_offset).saturating_sub(1)),
            };
            (&self.composer).render_ref(composer_rect, buf);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::app_event::AppEvent;
    use ratatui::buffer::Buffer;
    use ratatui::layout::Rect;
    use std::sync::mpsc::channel;

    fn exec_request() -> ApprovalRequest {
        ApprovalRequest::Exec {
            id: "1".to_string(),
            command: vec!["echo".into(), "ok".into()],
            reason: None,
        }
    }

    #[test]
    fn ctrl_c_on_modal_consumes_and_shows_quit_hint() {
        let (tx_raw, _rx) = channel::<AppEvent>();
        let tx = AppEventSender::new(tx_raw);
        let mut pane = BottomPane::new(BottomPaneParams {
            app_event_tx: tx,
            has_input_focus: true,
            enhanced_keys_supported: false,
            using_chatgpt_auth: false,
        });
        pane.push_approval_request(exec_request());
        assert_eq!(CancellationEvent::Handled, pane.on_ctrl_c());
        assert!(pane.ctrl_c_quit_hint_visible());
        assert_eq!(CancellationEvent::Ignored, pane.on_ctrl_c());
    }

    #[test]
    fn live_ring_renders_above_composer() {
        let (tx_raw, _rx) = channel::<AppEvent>();
        let tx = AppEventSender::new(tx_raw);
        let mut pane = BottomPane::new(BottomPaneParams {
            app_event_tx: tx,
            has_input_focus: true,
            enhanced_keys_supported: false,
            using_chatgpt_auth: false,
        });

        // Provide 4 rows with max_rows=3; only the last 3 should be visible.
        pane.set_live_ring_rows(
            3,
            vec![
                Line::from("one".to_string()),
                Line::from("two".to_string()),
                Line::from("three".to_string()),
                Line::from("four".to_string()),
            ],
        );

        let area = Rect::new(0, 0, 10, 5);
        let mut buf = Buffer::empty(area);
        (&pane).render_ref(area, &mut buf);

        // Extract the first 3 rows and assert they contain the last three lines.
        let mut lines: Vec<String> = Vec::new();
        for y in 0..3 {
            let mut s = String::new();
            for x in 0..area.width {
                s.push(buf[(x, y)].symbol().chars().next().unwrap_or(' '));
            }
            lines.push(s.trim_end().to_string());
        }
        assert_eq!(lines, vec!["two", "three", "four"]);
    }

    #[test]
    fn status_indicator_visible_with_live_ring() {
        let (tx_raw, _rx) = channel::<AppEvent>();
        let tx = AppEventSender::new(tx_raw);
        let mut pane = BottomPane::new(BottomPaneParams {
            app_event_tx: tx,
            has_input_focus: true,
            enhanced_keys_supported: false,
            using_chatgpt_auth: false,
        });

        // Simulate task running which replaces composer with the status indicator.
        pane.set_task_running(true);
        pane.update_status_text("waiting for model".to_string());

        // Provide 2 rows in the live ring (e.g., streaming CoT) and ensure the
        // status indicator remains visible below them.
        pane.set_live_ring_rows(
            2,
            vec![
                Line::from("cot1".to_string()),
                Line::from("cot2".to_string()),
            ],
        );

        // Allow some frames so the dot animation is present.
        std::thread::sleep(std::time::Duration::from_millis(120));

        // Height should include both ring rows, 1 spacer, and the 1-line status.
        let area = Rect::new(0, 0, 30, 4);
        let mut buf = Buffer::empty(area);
        (&pane).render_ref(area, &mut buf);

        // Top two rows are the live ring.
        let mut r0 = String::new();
        let mut r1 = String::new();
        for x in 0..area.width {
            r0.push(buf[(x, 0)].symbol().chars().next().unwrap_or(' '));
            r1.push(buf[(x, 1)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(r0.contains("cot1"), "expected first live row: {r0:?}");
        assert!(r1.contains("cot2"), "expected second live row: {r1:?}");

        // Row 2 is the spacer (blank)
        let mut r2 = String::new();
        for x in 0..area.width {
            r2.push(buf[(x, 2)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(r2.trim().is_empty(), "expected blank spacer line: {r2:?}");

        // Bottom row is the status line; it should contain the "Coding" header.
        let mut r3 = String::new();
        for x in 0..area.width {
            r3.push(buf[(x, 3)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(
            r3.contains("Coding"),
            "expected Coding header in status line: {r3:?}"
        );
    }
    // live ring removed; related tests deleted.

    #[test]
    fn overlay_not_shown_above_approval_modal() {
        let (tx_raw, _rx) = channel::<AppEvent>();
        let tx = AppEventSender::new(tx_raw);
        let mut pane = BottomPane::new(BottomPaneParams {
            app_event_tx: tx,
            has_input_focus: true,
            enhanced_keys_supported: false,
            using_chatgpt_auth: false,
        });

        // Create an approval modal (active view).
        pane.push_approval_request(exec_request());
        // Attempt to update status; this should NOT create an overlay while modal is visible.
        pane.update_status_text("running command".to_string());

        // Render and verify the top row does not include the Coding header overlay.
        let area = Rect::new(0, 0, 60, 6);
        let mut buf = Buffer::empty(area);
        (&pane).render_ref(area, &mut buf);

        let mut r0 = String::new();
        for x in 0..area.width {
            r0.push(buf[(x, 0)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(
            !r0.contains("Coding"),
            "overlay Coding header should not render above modal"
        );
    }

    #[test]
    fn composer_not_shown_after_denied_if_task_running() {
        let (tx_raw, rx) = channel::<AppEvent>();
        let tx = AppEventSender::new(tx_raw);
        let mut pane = BottomPane::new(BottomPaneParams {
            app_event_tx: tx.clone(),
            has_input_focus: true,
            enhanced_keys_supported: false,
        });

        // Start a running task so the status indicator replaces the composer.
        pane.set_task_running(true);
        pane.update_status_text("waiting for model".to_string());

        // Push an approval modal (e.g., command approval) which should hide the status view.
        pane.push_approval_request(exec_request());

        // Simulate pressing 'n' (deny) on the modal.
        use crossterm::event::KeyCode;
        use crossterm::event::KeyEvent;
        use crossterm::event::KeyModifiers;
        pane.handle_key_event(KeyEvent::new(KeyCode::Char('n'), KeyModifiers::NONE));

        // After denial, since the task is still running, the status indicator
        // should be restored as the active view; the composer should NOT be visible.
        assert!(
            pane.status_view_active,
            "status view should be active after denial"
        );
        assert!(pane.active_view.is_some(), "active view should be present");

        // Render and ensure the top row includes the Coding header instead of the composer.
        // Give the animation thread a moment to tick.
        std::thread::sleep(std::time::Duration::from_millis(120));
        let area = Rect::new(0, 0, 40, 3);
        let mut buf = Buffer::empty(area);
        (&pane).render_ref(area, &mut buf);
        let mut row0 = String::new();
        for x in 0..area.width {
            row0.push(buf[(x, 0)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(
            row0.contains("Coding"),
            "expected Coding header after denial: {row0:?}"
        );

        // Drain the channel to avoid unused warnings.
        drop(rx);
    }

    #[test]
    fn status_indicator_visible_during_command_execution() {
        let (tx_raw, _rx) = channel::<AppEvent>();
        let tx = AppEventSender::new(tx_raw);
        let mut pane = BottomPane::new(BottomPaneParams {
            app_event_tx: tx,
            has_input_focus: true,
            enhanced_keys_supported: false,
            using_chatgpt_auth: false,
        });

        // Begin a task: show initial status.
        pane.set_task_running(true);
        pane.update_status_text("waiting for model".to_string());

        // As a long-running command begins (post-approval), ensure the status
        // indicator is visible while we wait for the command to run.
        pane.update_status_text("running command".to_string());

        // Allow some frames so the animation thread ticks.
        std::thread::sleep(std::time::Duration::from_millis(120));

        // Render and confirm the line contains the "Coding" header.
        let area = Rect::new(0, 0, 40, 3);
        let mut buf = Buffer::empty(area);
        (&pane).render_ref(area, &mut buf);

        let mut row0 = String::new();
        for x in 0..area.width {
            row0.push(buf[(x, 0)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(row0.contains("Coding"), "expected Coding header: {row0:?}");
    }

    #[test]
    fn bottom_padding_present_for_status_view() {
        let (tx_raw, _rx) = channel::<AppEvent>();
        let tx = AppEventSender::new(tx_raw);
        let mut pane = BottomPane::new(BottomPaneParams {
            app_event_tx: tx,
            has_input_focus: true,
            enhanced_keys_supported: false,
            using_chatgpt_auth: false,
        });

        // Activate spinner (status view replaces composer) with no live ring.
        pane.set_task_running(true);
        pane.update_status_text("waiting for model".to_string());

        // Use height == desired_height; expect 1 status row at top and 2 bottom padding rows.
        let height = pane.desired_height(30);
        assert!(
            height >= 3,
            "expected at least 3 rows with bottom padding; got {height}"
        );
        let area = Rect::new(0, 0, 30, height);
        let mut buf = Buffer::empty(area);
        (&pane).render_ref(area, &mut buf);

        // Top row contains the status header
        let mut top = String::new();
        for x in 0..area.width {
            top.push(buf[(x, 0)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(
            top.contains("Coding"),
            "expected Coding header on top row: {top:?}"
        );

        // Bottom two rows are blank padding
        let mut r_last = String::new();
        let mut r_last2 = String::new();
        for x in 0..area.width {
            r_last.push(buf[(x, height - 1)].symbol().chars().next().unwrap_or(' '));
            r_last2.push(buf[(x, height - 2)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(
            r_last.trim().is_empty(),
            "expected last row blank: {r_last:?}"
        );
        assert!(
            r_last2.trim().is_empty(),
            "expected second-to-last row blank: {r_last2:?}"
        );
    }

    #[test]
    fn bottom_padding_shrinks_when_tiny() {
        let (tx_raw, _rx) = channel::<AppEvent>();
        let tx = AppEventSender::new(tx_raw);
        let mut pane = BottomPane::new(BottomPaneParams {
            app_event_tx: tx,
            has_input_focus: true,
            enhanced_keys_supported: false,
            using_chatgpt_auth: false,
        });

        pane.set_task_running(true);
        pane.update_status_text("waiting for model".to_string());

        // Height=2 → pad shrinks to 1; bottom row is blank, top row has spinner.
        let area2 = Rect::new(0, 0, 20, 2);
        let mut buf2 = Buffer::empty(area2);
        (&pane).render_ref(area2, &mut buf2);
        let mut row0 = String::new();
        let mut row1 = String::new();
        for x in 0..area2.width {
            row0.push(buf2[(x, 0)].symbol().chars().next().unwrap_or(' '));
            row1.push(buf2[(x, 1)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(
            row0.contains("Coding"),
            "expected Coding header on row 0: {row0:?}"
        );
        assert!(
            row1.trim().is_empty(),
            "expected bottom padding on row 1: {row1:?}"
        );

        // Height=1 → no padding; single row is the spinner.
        let area1 = Rect::new(0, 0, 20, 1);
        let mut buf1 = Buffer::empty(area1);
        (&pane).render_ref(area1, &mut buf1);
        let mut only = String::new();
        for x in 0..area1.width {
            only.push(buf1[(x, 0)].symbol().chars().next().unwrap_or(' '));
        }
        assert!(
            only.contains("Coding"),
            "expected Coding header with no padding: {only:?}"
        );
    }
}
