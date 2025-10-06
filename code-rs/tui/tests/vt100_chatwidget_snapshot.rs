//! VT100-backed snapshot tests for ChatWidget
//!
//! These tests render ChatWidget into a VT100Backend terminal at a fixed size
//! and snapshot the screen contents using insta.
//!
//! ## Overview
//!
//! This test harness provides a minimal VT100-backed TUI snapshot testing infrastructure
//! for the chat history widget. It uses:
//!
//! - `ChatWidgetHarness`: Test helper for managing ChatWidget state
//! - `insta`: Snapshot testing framework for comparing rendered output
//!
//! ## Usage
//!
//! To run these tests:
//! ```bash
//! cargo test --package code-tui --test vt100_chatwidget_snapshot --features test-helpers
//! ```
//!
//! To update snapshots after making changes:
//! ```bash
//! # Review and accept new snapshots
//! cargo insta review --test vt100_chatwidget_snapshot
//! # Or automatically accept all changes (use with caution)
//! cargo insta accept --test vt100_chatwidget_snapshot
//! ```
//!
//! ## Architecture
//!
//! `render_chat_widget_to_vt100()` flushes pending history events into the widget,
//! flattens the transcript into a fixed-width buffer, and returns it as a string so
//! the snapshots capture exactly what would be visible on a terminal screen.
//! Snapshots live in `tests/snapshots/` with a `.snap` extension.
//!
//! ## Feature Flags
//!
//! - `test-helpers`: Required to enable the test harness and VT100Backend
//! - `unstable-backend-writer`: Required for VT100Backend (enabled in Cargo.toml)

#![cfg(test)]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use code_core::protocol::{
    AgentMessageDeltaEvent, AgentMessageEvent, Event, EventMsg, OrderMeta,
};
use code_tui::test_helpers::{
    render_chat_widget_frames_to_vt100, render_chat_widget_to_vt100, ChatWidgetHarness,
};

#[test]
fn baseline_empty_chat() {
    let mut harness = ChatWidgetHarness::new();

    let output = render_chat_widget_to_vt100(&mut harness, 80, 24);
    insta::assert_snapshot!("empty_chat", output);
}

#[test]
fn baseline_simple_conversation() {
    let mut harness = ChatWidgetHarness::new();

    harness.handle_event(Event {
        id: "msg-1".into(),
        event_seq: 0,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "Hello! ".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(0),
            sequence_number: Some(0),
        }),
    });

    harness.handle_event(Event {
        id: "msg-1".into(),
        event_seq: 1,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "How can I help you today?".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(0),
            sequence_number: Some(1),
        }),
    });

    harness.handle_event(Event {
        id: "msg-1".into(),
        event_seq: 2,
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "Hello! How can I help you today?".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(0),
            sequence_number: Some(2),
        }),
    });

    harness.handle_event(Event {
        id: "msg-2".into(),
        event_seq: 0,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "I can help with ".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 2,
            output_index: Some(0),
            sequence_number: Some(0),
        }),
    });

    harness.handle_event(Event {
        id: "msg-2".into(),
        event_seq: 1,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "various tasks including:\n\n".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 2,
            output_index: Some(0),
            sequence_number: Some(1),
        }),
    });

    harness.handle_event(Event {
        id: "msg-2".into(),
        event_seq: 2,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "- Writing code\n".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 2,
            output_index: Some(0),
            sequence_number: Some(2),
        }),
    });

    harness.handle_event(Event {
        id: "msg-2".into(),
        event_seq: 3,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "- Reading files\n".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 2,
            output_index: Some(0),
            sequence_number: Some(3),
        }),
    });

    harness.handle_event(Event {
        id: "msg-2".into(),
        event_seq: 4,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "- Running commands".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 2,
            output_index: Some(0),
            sequence_number: Some(4),
        }),
    });

    harness.handle_event(Event {
        id: "msg-2".into(),
        event_seq: 5,
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "I can help with various tasks including:\n\n- Writing code\n- Reading files\n- Running commands".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 2,
            output_index: Some(0),
            sequence_number: Some(5),
        }),
    });

    let records = code_tui::test_helpers::history_records(&mut harness);
    dbg!(&records);
    assert!(!records.is_empty(), "history should contain records after events");
    assert!(
        records.iter().any(|record| match record {
            code_core::history::state::HistoryRecord::AssistantMessage(state) => {
                state.markdown.contains("How can I help you today")
            }
            _ => false,
        }),
        "assistant message should be recorded"
    );

    let output = render_chat_widget_to_vt100(&mut harness, 80, 24);
    insta::assert_snapshot!("simple_conversation", output);
}

#[test]
fn baseline_multiline_formatting() {
    let mut harness = ChatWidgetHarness::new();

    harness.handle_event(Event {
        id: "msg-code".into(),
        event_seq: 0,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "Here's a simple function:\n\n```rust\n".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(0),
            sequence_number: Some(0),
        }),
    });

    harness.handle_event(Event {
        id: "msg-code".into(),
        event_seq: 1,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: "fn hello() {\n    println!(\"Hello, world!\");\n}\n```".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(0),
            sequence_number: Some(1),
        }),
    });

    harness.handle_event(Event {
        id: "msg-code".into(),
        event_seq: 2,
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "Here's a simple function:\n\n```rust\nfn hello() {\n    println!(\"Hello, world!\");\n}\n```".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(0),
            sequence_number: Some(2),
        }),
    });

    let output = render_chat_widget_to_vt100(&mut harness, 80, 24);
    insta::assert_snapshot!("multiline_formatting", output);
}

#[test]
#[ignore = "repro for bottom-line clipping"]
fn clip_repro_autofollow_wrap() {
    let mut harness = ChatWidgetHarness::new();

    let mut frames: Vec<String> = Vec::new();

    // Initial frame before streaming begins
    frames.extend(render_chat_widget_frames_to_vt100(&mut harness, &[(80, 6)]));

    let base_order = |seq: u64| OrderMeta {
        request_ordinal: 1,
        output_index: Some(0),
        sequence_number: Some(seq),
    };

    let stream_id = "answer-1".to_string();

    let deltas = [
        "Here is a summary of the wrap behavior: the viewport should stay locked near the bottom even when lines wrap across the width of the terminal.",
        " Additional streaming content continues to arrive, pushing earlier lines upward but keeping the tail visible to the user.",
        " Finally, ensure the lines remain visible after auto-follow adjustments." ,
    ];

    for (idx, delta) in deltas.iter().enumerate() {
        harness.handle_event(Event {
            id: stream_id.clone(),
            event_seq: idx as u64,
            msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
                delta: delta.to_string(),
            }),
            order: Some(base_order(idx as u64)),
        });
        frames.extend(render_chat_widget_frames_to_vt100(&mut harness, &[(80, 6)]));
    }

    // Final assistant message closes the stream
    let final_message = deltas.join("");
    harness.handle_event(Event {
        id: stream_id.clone(),
        event_seq: deltas.len() as u64,
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: final_message.clone(),
        }),
        order: Some(base_order(deltas.len() as u64)),
    });
    let records = code_tui::test_helpers::history_records(&mut harness);
    assert!(!records.is_empty(), "history should contain records for clip repro");
    frames.extend(render_chat_widget_frames_to_vt100(&mut harness, &[(80, 6)]));

    let combined = frames
        .iter()
        .enumerate()
        .map(|(idx, frame)| format!("--- frame {} ---\n{}", idx, frame))
        .collect::<Vec<_>>()
        .join("\n");

    let last_frame = frames.last().expect("last frame available");
    insta::assert_snapshot!("clip_repro_autofollow_wrap", combined);

    assert!(
        last_frame.contains("tail visible to the user")
            || last_frame.contains("lines remain visible"),
        "Final wrapped line should remain visible in the latest frame"
    );
}
