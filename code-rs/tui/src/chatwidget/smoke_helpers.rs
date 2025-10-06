#![allow(dead_code)]

use super::ChatWidget;
use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::tui::TerminalInfo;
use code_core::history::state::HistoryRecord;
use code_core::config::{Config, ConfigOverrides, ConfigToml};
use code_core::protocol::Event;
use once_cell::sync::Lazy;
use std::sync::mpsc::{self, Receiver};
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;

static TEST_RUNTIME: Lazy<Runtime> = Lazy::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()
        .expect("test runtime")
});

pub struct ChatWidgetHarness {
    chat: ChatWidget<'static>,
    events: Receiver<AppEvent>,
}

impl ChatWidgetHarness {
    pub fn new() -> Self {
        let cfg = Config::load_from_base_config_with_overrides(
            ConfigToml::default(),
            ConfigOverrides::default(),
            std::env::temp_dir(),
        )
        .expect("config");

        let (tx_raw, rx) = mpsc::channel::<AppEvent>();
        let app_event_tx = AppEventSender::new(tx_raw);
        let terminal_info = TerminalInfo {
            picker: None,
            font_size: (8, 16),
        };

        let runtime = &*TEST_RUNTIME;
        let _guard = runtime.enter();

        let chat = ChatWidget::new(
            cfg,
            app_event_tx,
            None,
            Vec::new(),
            false,
            terminal_info,
            false,
            None,
        );

        Self { chat, events: rx }
    }

    pub fn handle_event(&mut self, event: Event) {
        self.chat.handle_code_event(event);
    }

    pub(crate) fn drain_events(&self) -> Vec<AppEvent> {
        let mut out = Vec::new();
        while let Ok(ev) = self.events.try_recv() {
            out.push(ev);
        }
        out
    }

    pub(crate) fn chat(&mut self) -> &mut ChatWidget<'static> {
        &mut self.chat
    }

    pub(crate) fn poll_until<F>(&mut self, mut predicate: F, timeout: Duration) -> Vec<AppEvent>
    where
        F: FnMut(&[AppEvent]) -> bool,
    {
        let deadline = Instant::now() + timeout;
        let mut collected = Vec::new();

        loop {
            while let Ok(event) = self.events.try_recv() {
                if !matches!(event, AppEvent::RequestRedraw) {
                    collected.push(event);
                }
            }

            if predicate(&collected) {
                break;
            }

            if Instant::now() >= deadline {
                break;
            }

            std::thread::sleep(Duration::from_millis(5));
        }

        collected
    }

    pub(crate) fn history_records(&self) -> Vec<HistoryRecord> {
        self.chat.history_state.records.clone()
    }
}

pub fn assert_has_insert_history(events: &[AppEvent]) {
    let found = events.iter().any(|event| {
        matches!(
            event,
            AppEvent::InsertHistory(_) | AppEvent::InsertHistoryWithKind { .. } | AppEvent::InsertFinalAnswer { .. }
        )
    });
    assert!(found, "expected InsertHistory-like event, got: {events:#?}");
}

pub fn assert_has_background_event_containing(events: &[AppEvent], needle: &str) {
    let found = events.iter().any(|event| {
        matches!(event, AppEvent::InsertBackgroundEvent { message, .. } if message.contains(needle))
    });
    assert!(
        found,
        "expected InsertBackgroundEvent containing '{needle}', got: {events:#?}"
    );
}

pub fn assert_has_terminal_chunk_containing(events: &[AppEvent], needle: &str) {
    let found = events.iter().any(|event| {
        if let AppEvent::TerminalChunk { chunk, .. } = event {
            String::from_utf8_lossy(chunk).contains(needle)
        } else {
            false
        }
    });
    assert!(
        found,
        "expected TerminalChunk containing '{needle}', got: {events:#?}"
    );
}

pub fn assert_has_codex_event(events: &[AppEvent]) {
    assert!(
        events.iter().any(|event| matches!(event, AppEvent::CodexEvent(_))),
        "expected CodexEvent, got: {events:#?}"
    );
}

pub fn assert_no_events(events: &[AppEvent]) {
    assert!(events.is_empty(), "expected no events, got: {events:#?}");
}
