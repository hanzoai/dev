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

use code_core::parse_command::parse_command;
use code_core::protocol::{
    AgentMessageDeltaEvent, AgentMessageEvent, AgentReasoningDeltaEvent, AgentReasoningEvent,
    Event, EventMsg, ExecCommandBeginEvent, ExecCommandEndEvent, OrderMeta,
};
use code_tui::test_helpers::{
    render_chat_widget_frames_to_vt100, render_chat_widget_to_vt100, ChatWidgetHarness,
};
use std::{path::PathBuf, time::Duration};

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
#[ignore = "repro: streaming answer fails to auto-follow, clipping tail"]
fn clip_repro_autofollow_wrap() {
    let mut harness = ChatWidgetHarness::new();

    // Seed history with a mix of user input, background notices, reasoning, and prior answers to
    // better match the conditions where the clipping bug manifests in the app.
    harness.push_user_prompt("Can you review the last run and summarize the findings?");
    harness.push_background_event("✅ Connected to Chrome via CDP");

    let reasoning_id = "reason-1".to_string();
    let reasoning_order = |seq: u64| OrderMeta {
        request_ordinal: 1,
        output_index: Some(0),
        sequence_number: Some(seq),
    };
    harness.handle_event(Event {
        id: reasoning_id.clone(),
        event_seq: 0,
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "Considering recent edits and repo state... ".into(),
        }),
        order: Some(reasoning_order(0)),
    });
    harness.handle_event(Event {
        id: reasoning_id.clone(),
        event_seq: 1,
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "Evaluating plan viability.".into(),
        }),
        order: Some(reasoning_order(1)),
    });
    harness.handle_event(Event {
        id: reasoning_id.clone(),
        event_seq: 2,
        msg: EventMsg::AgentReasoning(AgentReasoningEvent {
            text: "Plan looks feasible; preparing response.".into(),
        }),
        order: Some(reasoning_order(2)),
    });

    // Older assistant response to push the scrollback.
    harness.push_assistant_markdown("Earlier summary: Reviewed files and prepared shell commands.");

    harness.push_background_event("ℹ️ Auto Drive queued additional tasks");

    let mut frames: Vec<String> = Vec::new();
    let viewport = (90, 8);

    // Ensure seeded events are reflected in the widget before capturing frames.
    let _ = code_tui::test_helpers::history_records(&mut harness);

    // Initial frame before streaming begins
    frames.extend(render_chat_widget_frames_to_vt100(&mut harness, &[viewport]));

    let base_order = |seq: u64| OrderMeta {
        request_ordinal: 1,
        output_index: Some(0),
        sequence_number: Some(seq),
    };

    let stream_id = "answer-1".to_string();

    let tail_marker = "TAIL: stay visible";
    let deltas: Vec<String> = vec![
        "Here is a summary of the wrap behavior:\n\n- The viewport should stay locked near the bottom.\n- Lines that wrap must remain visible even as the buffer grows.".into(),
        "\nStreaming content continues to arrive, pushing earlier lines upward but keeping the tail visible to the user.\n1. Capture each delta.\n2. Commit the render.\n3. Maintain auto-follow.".into(),
        "\nFinally, ensure the lines remain visible after auto-follow adjustments by re-evaluating the scroll state and redrawing the composer footer.".into(),
        format!("\n{}", tail_marker),
    ];

    for (idx, delta) in deltas.iter().enumerate() {
        harness.handle_event(Event {
            id: stream_id.clone(),
            event_seq: idx as u64,
            msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent { delta: delta.clone() }),
            order: Some(base_order(idx as u64)),
        });
        frames.extend(render_chat_widget_frames_to_vt100(&mut harness, &[viewport]));
    }

    // Final assistant message closes the stream
    // Simulate the user scrolling slightly above the bottom just before the final answer
    // arrives. The auto-follow logic should pull us back down when the assistant
    // completes; the regression previously clipped the final wrapped lines instead.
    code_tui::test_helpers::force_scroll_offset(&mut harness, 6);

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
    let tail_in_history = records.iter().any(|record| matches!(
        record,
        code_core::history::state::HistoryRecord::AssistantMessage(state)
            if state.markdown.contains(tail_marker)
    ));
    assert!(tail_in_history, "final assistant message should include the tail marker");

    let scroll_offset_after_flush = code_tui::test_helpers::scroll_offset(&harness);
    assert!(
        scroll_offset_after_flush > 0,
        "auto-follow failed: expected a non-zero scroll offset to reproduce clipping"
    );
    frames.extend(render_chat_widget_frames_to_vt100(&mut harness, &[viewport]));

    let combined = frames
        .iter()
        .enumerate()
        .map(|(idx, frame)| format!("--- frame {} ---\n{}", idx, frame))
        .collect::<Vec<_>>()
        .join("\n");

    let last_frame = frames.last().expect("last frame available");
    insta::assert_snapshot!("clip_repro_autofollow_wrap", combined);

    assert!(
        last_frame.contains(tail_marker),
        "Tail marker should remain visible at the bottom of the viewport"
    );
}

#[test]
#[ignore = "repro: assistant tail spacing missing after explore & reasoning stack"]
fn repro_explore_reasoning_tail_spacing() {
    let mut harness = ChatWidgetHarness::new();

    code_tui::test_helpers::set_standard_terminal_mode(&mut harness, false);
    harness.push_user_prompt("› What does this repo do?");

    let mut seq_counter = 1_u64;
    let mut next_seq = || {
        let current = seq_counter;
        seq_counter = seq_counter.saturating_add(1);
        current
    };
    let order_for = |seq: u64| OrderMeta {
        request_ordinal: 1,
        output_index: Some(0),
        sequence_number: Some(seq),
    };

    let reasoning_id = "reason-explore".to_string();
    let seq = next_seq();
    harness.handle_event(Event {
        id: reasoning_id.clone(),
        event_seq: seq,
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "Inspecting repository structure".into(),
        }),
        order: Some(order_for(seq)),
    });

    let mut send_explore = |call_id: &str, command: &str| {
        let cmd: Vec<String> = vec![
            "bash".into(),
            "-lc".into(),
            command.to_string(),
        ];
        let parsed = parse_command(&cmd);
        let begin = ExecCommandBeginEvent {
            call_id: call_id.to_string(),
            command: cmd.clone(),
            cwd: PathBuf::from("."),
            parsed_cmd: parsed,
        };
        let begin_seq = next_seq();
        harness.handle_event(Event {
            id: call_id.to_string(),
            event_seq: begin_seq,
            msg: EventMsg::ExecCommandBegin(begin),
            order: Some(order_for(begin_seq)),
        });

        let end = ExecCommandEndEvent {
            call_id: call_id.to_string(),
            stdout: String::new(),
            stderr: String::new(),
            exit_code: 0,
            duration: Duration::from_millis(120),
        };
        let end_seq = next_seq();
        harness.handle_event(Event {
            id: call_id.to_string(),
            event_seq: end_seq,
            msg: EventMsg::ExecCommandEnd(end),
            order: Some(order_for(end_seq)),
        });
    };

    send_explore("explore-list", "ls ./");
    send_explore("explore-read", "sed -n '1,160p' README.md");

    let seq = next_seq();
    harness.handle_event(Event {
        id: reasoning_id.clone(),
        event_seq: seq,
        msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent {
            delta: "Summarizing repository components".into(),
        }),
        order: Some(order_for(seq)),
    });
    let seq = next_seq();
    harness.handle_event(Event {
        id: reasoning_id.clone(),
        event_seq: seq,
        msg: EventMsg::AgentReasoning(AgentReasoningEvent {
            text: "Summarizing repository components".into(),
        }),
        order: Some(order_for(seq)),
    });

    let assistant_body = indoc::indoc! {
        "
        • Overview
          - Provides the “Code” terminal app: a fast, local coding agent forked from openai/codex, adding browser control, multi-agent planning/execution, theming, and granular
            reasoning toggles (/chrome, /plan, /solve, /code, /themes).
          - Lets you run it directly (npx -y @just-every/code) or install globally; it authenticates via ChatGPT sign-in or API keys and can orchestrate external CLIs for Claude,
            Gemini, and Qwen.

        Repository Layout
          - codex-cli/: JavaScript/TypeScript CLI that implements the command dispatcher, command palette, sandbox modes, and integrations with external agents.
          - code-rs/: Writable Rust workspace powering core execution flow, event streaming, TUI rendering, and backend services (mirrors upstream codex-rs but is the editable copy).
          - docs/ + screenshots: product overview, UI previews, logo assets.
          - Scripts like ./build-fast.sh: single required validation pipeline ensuring the repo builds and the TUI/core stay warning-free.
          - Additional support packages (sdk/, prompts/, Formula/, homebrew-tap/) deliver SDK bindings, system prompts, and packaging metadata.

        In short, the repo maintains the full stack—CLI frontend, Rust backend, docs, and tooling—for the Code developer assistant.
        "
    }
    .trim()
    .to_string();

    let final_message = format!("{}\n", assistant_body);

    let answer_id = "answer-explore".to_string();
    let seq = next_seq();
    harness.handle_event(Event {
        id: answer_id.clone(),
        event_seq: seq,
        msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent {
            delta: format!("{}\n", assistant_body),
        }),
        order: Some(order_for(seq)),
    });
    let seq = next_seq();
    harness.handle_event(Event {
        id: answer_id.clone(),
        event_seq: seq,
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: final_message.clone(),
        }),
        order: Some(order_for(seq)),
    });

    let records = code_tui::test_helpers::history_records(&mut harness);
    assert!(
        records.iter().any(|record| matches!(
            record,
            code_core::history::state::HistoryRecord::AssistantMessage(state)
                if state.markdown.trim_end().ends_with("the Code developer assistant.")
        )),
        "final assistant markdown should contain assistant summary"
    );

    code_tui::test_helpers::force_scroll_offset(&mut harness, 6);

    let viewport = (90, 10);
    let frame = render_chat_widget_to_vt100(&mut harness, viewport.0, viewport.1);
    assert!(
        frame.contains("Additional support packages"),
        "expected assistant body to be visible near the viewport bottom"
    );
    assert!(
        !frame.contains("the Code developer assistant."),
        "final assistant sentence should be clipped from the viewport"
    );

    harness.push_user_prompt("› This will be cut off");
    let after_user_frame = render_chat_widget_to_vt100(&mut harness, viewport.0, viewport.1);
    assert!(
        !after_user_frame.contains("This will be cut off"),
        "user prompt should appear at bottom, but it is missing due to clipping"
    );
}
