//! VT100-backed snapshot tests for ChatWidget.
//!
//! These tests render `ChatWidget` into a `VT100Backend` terminal at a fixed
//! size and snapshot the screen contents using `insta`. The harness ensures
//! deterministic output (e.g. it fixes the greeting hour) so diffs stay stable.

#![cfg(test)]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use code_core::protocol::{
    AgentMessageDeltaEvent, AgentMessageEvent, Event, EventMsg, OrderMeta,
};
use code_tui::test_helpers::{render_chat_widget_to_vt100, ChatWidgetHarness};

#[test]
fn baseline_empty_chat() {
    let mut harness = ChatWidgetHarness::new();
    code_tui::test_helpers::set_standard_terminal_mode(&mut harness, false);

    let output = render_chat_widget_to_vt100(&mut harness, 80, 24);
    insta::assert_snapshot!("empty_chat", output);
}

#[test]
fn baseline_simple_conversation() {
    let mut harness = ChatWidgetHarness::new();

    harness.push_user_prompt("Can you help me understand the available commands?");

    // Assistant greeting
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

    // Assistant continues with another message.
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
