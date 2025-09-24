use std::sync::mpsc::Sender;

use crate::app_event::AppEvent;
use crate::session_log;

#[derive(Clone, Debug)]
pub(crate) struct AppEventSender {
    // High‑priority events (input, resize, redraw scheduling) are routed here.
    high_tx: Sender<AppEvent>,
    // Bulk/streaming events (history inserts, commit ticks, file search, etc.).
    bulk_tx: Sender<AppEvent>,
}

impl AppEventSender {
    /// Create a sender that splits events by priority across two channels.
    pub(crate) fn new_dual(high_tx: Sender<AppEvent>, bulk_tx: Sender<AppEvent>) -> Self {
        Self { high_tx, bulk_tx }
    }
    /// Backward‑compatible constructor for tests/fixtures that expect a single
    /// channel. Routes both high‑priority and bulk events to the same sender.
    #[allow(dead_code)]
    pub(crate) fn new(app_event_tx: Sender<AppEvent>) -> Self {
        Self { high_tx: app_event_tx.clone(), bulk_tx: app_event_tx }
    }


    /// Send an event to the app event channel. If it fails, we swallow the
    /// error and log it.
    pub(crate) fn send(&self, event: AppEvent) {
        // Record inbound events for high-fidelity session replay.
        // Avoid double-logging Ops; those are logged at the point of submission.
        if !matches!(event, AppEvent::CodexOp(_)) {
            session_log::log_inbound_app_event(&event);
        }
        let is_high = matches!(
            event,
            AppEvent::KeyEvent(_)
                | AppEvent::MouseEvent(_)
                | AppEvent::Paste(_)
                | AppEvent::RequestRedraw
                | AppEvent::Redraw
                | AppEvent::ExitRequest
                | AppEvent::SetTerminalTitle { .. }
        );

        let tx = if is_high { &self.high_tx } else { &self.bulk_tx };
        if let Err(e) = tx.send(event) {
            tracing::error!("failed to send event: {e}");
        }
    }

    /// Emit a background event initiated by the UI near the top of the current
    /// request window. This keeps ordering logic in one place (history
    /// insertion) instead of forging `Event` structs with ad-hoc sequencing.
    ///
    /// IMPORTANT: UI code should call this (or other history helpers) rather
    /// than constructing `Event { event_seq: 0, .. }` manually. Protocol events
    /// must come from `codex-core` via `Session::make_event` so the per-turn
    /// sequence stays consistent.
    pub(crate) fn send_background_event(&self, message: impl Into<String>) {
        self.send(AppEvent::InsertBackgroundEventEarly(message.into()));
    }

    /// Emit a background event appended to the current request window so it
    /// shows up after previously rendered content.
    pub(crate) fn send_background_event_late(&self, message: impl Into<String>) {
        self.send(AppEvent::InsertBackgroundEventLate(message.into()));
    }

}
