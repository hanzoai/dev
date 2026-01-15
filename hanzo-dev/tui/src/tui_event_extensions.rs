//! Fork-specific TUI event helpers kept behind the `hanzo-fork` feature flag.
#![cfg(feature = "hanzo-fork")]

use crate::app_event_sender::AppEventSender;
use hanzo_core::protocol::{BrowserScreenshotUpdateEvent, RateLimitSnapshotEvent};

/// Forward a rate-limit snapshot into the main event loop.
#[inline]
pub fn handle_rate_limit(_event: &RateLimitSnapshotEvent, _sender: &AppEventSender) {
    // Intentionally no-op: downstream wrappers can forward to AppEvent if needed.
}

/// Forward a browser screenshot update into the main event loop.
#[inline]
pub fn handle_browser_screenshot(_event: &BrowserScreenshotUpdateEvent, _sender: &AppEventSender) {
    // Intentionally no-op: downstream wrappers can forward to AppEvent if needed.
}
