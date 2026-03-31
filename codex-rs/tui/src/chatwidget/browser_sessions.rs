//! Browser session tracking stubs for the TUI.
//!
//! The browser session grouping logic requires ChatWidget integration points
//! (tool card slots, order keys, session maps) that have not yet been ported
//! from the upstream just-every codebase. This module provides the public API
//! surface so that callers inside chatwidget compile, but operations are
//! currently no-ops.
//!
//! When the chatwidget gains browser-session infrastructure, fill in the
//! implementations here.

use crate::history_cell::BrowserSessionCell;
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Tracks an active browser session's state within the chat widget.
pub(super) struct BrowserSessionTracker {
    pub cell: BrowserSessionCell,
    pub elapsed: Duration,
    pub anchor_inserted: bool,
    pub first_screenshot_instant: Option<Instant>,
    pub last_screenshot_timestamp: Duration,
}

/// Result of attempting to group a screenshot into an existing browser session.
pub(super) struct BrowserScreenshotUpdateResult {
    pub grouped: bool,
    pub session_key: Option<String>,
}

/// State bag for browser session tracking, stored in ChatWidget.
#[derive(Default)]
pub(super) struct BrowserToolsState {
    pub browser_sessions: HashMap<String, BrowserSessionTracker>,
    pub browser_session_by_call: HashMap<String, String>,
    pub browser_last_key: Option<String>,
}
