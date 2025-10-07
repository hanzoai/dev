#![cfg(test)]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use code_core::protocol::{Event, EventMsg, ReplayHistoryEvent};
use code_protocol::models::{ContentItem, ResponseItem};
use code_tui::test_helpers::{render_chat_widget_to_vt100, ChatWidgetHarness};

fn assistant_cell_count(screen: &str) -> usize {
    screen
        .lines()
        .filter(|line| line.trim_start().starts_with("• "))
        .count()
}

fn message(role: &str, text: &str) -> ResponseItem {
    let content = match role {
        "assistant" => ContentItem::OutputText { text: text.to_string() },
        _ => ContentItem::InputText { text: text.to_string() },
    };

    ResponseItem::Message {
        id: None,
        role: role.to_string(),
        content: vec![content],
    }
}

#[test]
fn replay_history_duplicates_short_assistant_messages() {
    let mut harness = ChatWidgetHarness::new();

    let items = vec![
        message("user", "Please summarize the plan."),
        message("assistant", "Working."),
        message("assistant", "Working. Done."),
    ];

    harness.handle_event(Event {
        id: "resume-replay".to_string(),
        event_seq: 0,
        msg: EventMsg::ReplayHistory(ReplayHistoryEvent {
            items,
            history_snapshot: None,
        }),
        order: None,
    });

    let screen = render_chat_widget_to_vt100(&mut harness, 80, 24);

    assert_eq!(
        1,
        assistant_cell_count(&screen),
        "expected a single restored assistant message but saw: {screen}"
    );
    assert!(screen.contains("Working. Done."));
}

#[test]
fn replay_history_handles_prefixed_revisions() {
    let mut harness = ChatWidgetHarness::new();

    let items = vec![
        message("user", "Please summarize the plan."),
        message("assistant", "Working."),
        message("assistant", "Update:\nWorking."),
    ];

    harness.handle_event(Event {
        id: "resume-replay".to_string(),
        event_seq: 0,
        msg: EventMsg::ReplayHistory(ReplayHistoryEvent {
            items,
            history_snapshot: None,
        }),
        order: None,
    });

    let screen = render_chat_widget_to_vt100(&mut harness, 80, 24);

    assert_eq!(
        1,
        assistant_cell_count(&screen),
        "expected a single restored assistant message but saw: {screen}"
    );
    assert!(screen.contains("Update:"));
}
