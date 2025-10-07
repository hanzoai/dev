#![allow(dead_code)]

use super::ChatWidget;
use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::history_cell::{self, HistoryCellType};
use crate::markdown_render::render_markdown_text;
use crate::tui::TerminalInfo;
use code_core::config::{Config, ConfigOverrides, ConfigToml};
use code_core::history::state::HistoryRecord;
use code_core::protocol::{BackgroundEventEvent, Event, EventMsg, OrderMeta};
use once_cell::sync::Lazy;
use ratatui::text::Line;
use std::collections::VecDeque;
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
    helper_seq: u64,
}

#[derive(Debug, Clone, Copy)]
pub struct LayoutMetrics {
    pub scroll_offset: u16,
    pub last_viewport_height: u16,
    pub last_max_scroll: u16,
}

impl ChatWidgetHarness {
    pub fn new() -> Self {
        // Stabilize time-of-day dependent greeting so VT100 snapshots remain deterministic.
        // Safe: tests run single-threaded by design.
        unsafe { std::env::set_var("CODEX_TUI_FAKE_HOUR", "12"); }

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

        Self {
            chat,
            events: rx,
            helper_seq: 0,
        }
    }

    pub fn handle_event(&mut self, event: Event) {
        self.chat.handle_code_event(event);
    }

    pub(crate) fn flush_into_widget(&mut self) {
        let mut queue: VecDeque<AppEvent> = self
            .drain_events()
            .into_iter()
            .filter(|event| !matches!(event, AppEvent::RequestRedraw))
            .collect();

        while let Some(event) = queue.pop_front() {
            match event {
                AppEvent::InsertHistory(lines) => {
                    self.chat.insert_history_lines(lines);
                }
                AppEvent::InsertHistoryWithKind { id, kind, lines } => {
                    self.chat.insert_history_lines_with_kind(kind, id, lines);
                }
                AppEvent::InsertFinalAnswer { id, lines, source } => {
                    self.chat.insert_final_answer_with_id(id, lines, source);
                }
                AppEvent::InsertBackgroundEvent { message, placement, order } => {
                    self.chat
                        .insert_background_event_with_placement(message, placement, order);
                }
                AppEvent::CommitTick => {
                    self.chat.on_commit_tick();
                    let newly_emitted = self.drain_events();
                    if !newly_emitted.is_empty() {
                        queue.extend(
                            newly_emitted
                                .into_iter()
                                .filter(|event| !matches!(event, AppEvent::RequestRedraw)),
                        );
                    }
                }
                AppEvent::StartCommitAnimation
                | AppEvent::StopCommitAnimation
                | AppEvent::ScheduleFrameIn(_)
                | AppEvent::SetTerminalTitle { .. }
                | AppEvent::EmitTuiNotification { .. }
                | AppEvent::RequestRedraw
                | AppEvent::Redraw
                | AppEvent::PreviewTheme(_)
                | AppEvent::PreviewSpinner(_) => {}
                // Other events are either no-ops for VT100 rendering or handled elsewhere.
                _ => {}
            }
            if !queue.is_empty() {
                continue;
            }
            let newly_emitted = self.drain_events();
            if !newly_emitted.is_empty() {
                queue.extend(
                    newly_emitted
                        .into_iter()
                        .filter(|event| !matches!(event, AppEvent::RequestRedraw)),
                );
            }
        }
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

    pub(crate) fn layout_metrics(&self) -> LayoutMetrics {
        LayoutMetrics {
            scroll_offset: self.chat.layout.scroll_offset,
            last_viewport_height: self.chat.layout.last_history_viewport_height.get(),
            last_max_scroll: self.chat.layout.last_max_scroll.get(),
        }
    }

    pub fn push_user_prompt(&mut self, message: impl Into<String>) {
        let state = history_cell::new_user_prompt(message.into());
        self.chat.history_push_plain_state(state);
    }

    pub fn push_background_event(&mut self, message: impl Into<String>) {
        let seq = self.next_helper_seq();
        self.handle_event(Event {
            id: format!("bg-helper-{seq}"),
            event_seq: 0,
            msg: EventMsg::BackgroundEvent(BackgroundEventEvent {
                message: message.into(),
            }),
            order: Some(OrderMeta {
                request_ordinal: 0,
                output_index: Some(u32::MAX),
                sequence_number: Some(seq),
            }),
        });
    }

    pub fn push_assistant_markdown(&mut self, markdown: impl Into<String>) {
        let markdown = markdown.into();
        let mut rendered = render_markdown_text(&markdown);
        let mut lines: Vec<Line<'static>> = Vec::with_capacity(rendered.lines.len() + 1);
        lines.push(Line::from("assistant"));
        lines.extend(rendered.lines.drain(..));
        let state = history_cell::plain_message_state_from_lines(lines, HistoryCellType::Assistant);
        self.chat.history_push_plain_state(state);
    }

    pub(crate) fn set_standard_terminal_mode(&mut self, enabled: bool) {
        self.chat.set_standard_terminal_mode(enabled);
    }

    pub(crate) fn force_scroll_offset(&mut self, offset: u16) {
        self.chat.layout.scroll_offset = offset;
    }

    pub(crate) fn scroll_offset(&self) -> u16 {
        self.chat.layout.scroll_offset
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

    pub(crate) fn history_records(&mut self) -> Vec<HistoryRecord> {
        self.flush_into_widget();
        self.chat.history_state.records.clone()
    }

    fn next_helper_seq(&mut self) -> u64 {
        let next = self.helper_seq;
        self.helper_seq = self.helper_seq.saturating_add(1);
        next
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
