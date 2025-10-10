//! VT100-backed snapshot tests for ChatWidget.
//!
//! These tests render `ChatWidget` into a `VT100Backend` terminal at a fixed
//! size and snapshot the screen contents using `insta`. The harness ensures
//! deterministic output (e.g. it fixes the greeting hour) so diffs stay stable.

#![cfg(test)]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use code_core::protocol::{
    AgentInfo,
    AgentMessageDeltaEvent,
    AgentMessageEvent,
    AgentStatusUpdateEvent,
    BackgroundEventEvent,
    BrowserScreenshotUpdateEvent,
    CustomToolCallBeginEvent,
    CustomToolCallEndEvent,
    Event,
    EventMsg,
    OrderMeta,
    WebSearchBeginEvent,
    WebSearchCompleteEvent,
};
use code_tui::test_helpers::{
    force_scroll_offset as harness_force_scroll_offset,
    layout_metrics as harness_layout_metrics,
    render_chat_widget_to_vt100,
    ChatWidgetHarness,
};
use serde_json::json;
use std::path::PathBuf;
use std::time::Duration;

fn normalize_output(text: String) -> String {
    text
        .replace('✧', "✶")
        .replace('◇', "✶")
        .replace('✦', "✶")
        .replace('◆', "✶")
        .replace('✨', "✶")
}

fn is_history_header(line: &str) -> bool {
    let trimmed = line.trim_start();
    matches!(trimmed.chars().next(), Some(ch) if matches!(ch, '›' | '•' | '⋮' | '⚙' | '✔' | '✖' | '✶'))
}

fn count_collapsed_boundaries(output: &str) -> usize {
    let mut collapsed = 0usize;
    let mut saw_header = false;
    let mut blank_since_last_header = false;

    for line in output.lines() {
        if line.trim_end().is_empty() {
            if saw_header {
                blank_since_last_header = true;
            }
            continue;
        }

        if is_history_header(line) {
            if saw_header && !blank_since_last_header {
                collapsed = collapsed.saturating_add(1);
            }
            saw_header = true;
            blank_since_last_header = false;
        }
    }

    collapsed
}

fn push_ordered_event(
    harness: &mut ChatWidgetHarness,
    event_seq: &mut u64,
    order_seq: &mut u64,
    msg: EventMsg,
) {
    let seq = *event_seq;
    let ord = *order_seq;
    let event = Event {
        id: "turn-1".into(),
        event_seq: seq,
        msg,
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(0),
            sequence_number: Some(ord),
        }),
    };
    harness.handle_event(event);
    *event_seq = seq.saturating_add(1);
    *order_seq = ord.saturating_add(1);
}

fn push_unordered_event(
    harness: &mut ChatWidgetHarness,
    event_seq: &mut u64,
    msg: EventMsg,
) {
    let seq = *event_seq;
    harness.handle_event(Event {
        id: format!("unordered-{seq}"),
        event_seq: seq,
        msg,
        order: None,
    });
    *event_seq = seq.saturating_add(1);
}

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
fn scroll_spacing_remains_when_scrolled_up() {
    let mut harness = ChatWidgetHarness::new();

    harness.push_user_prompt("First user message about scrolling behaviour.");
    harness.push_assistant_markdown("Assistant reply number one with enough text to wrap the layout and ensure spacing stays visible while at the bottom of the viewport.");
    harness.push_user_prompt("Second user follow-up that also contributes to the total height so we can scroll.");
    harness.push_assistant_markdown("Assistant reply number two with multiple paragraphs.\n\nHere is another paragraph to expand height.\n\nYet another paragraph for good measure.");
    harness.push_user_prompt("Third user prompt to push history further.");
    harness.push_assistant_markdown("Assistant reply number three, still going strong.\n\n- Bullet one\n- Bullet two\n- Bullet three");
    harness.push_user_prompt("Fourth user prompt to guarantee overflow beyond the viewport height.");
    harness.push_assistant_markdown("Assistant reply number four with extra padding to pad out the history list even more.\n\nFinal paragraph to top it off.");

    let _bottom = normalize_output(render_chat_widget_to_vt100(&mut harness, 80, 24));
    let metrics = harness_layout_metrics(&harness);
    assert!(
        metrics.last_max_scroll > 0,
        "scenario must overflow the history viewport to exercise scrolling"
    );

    let offset = metrics.last_max_scroll.min(5).max(1);
    harness_force_scroll_offset(&mut harness, offset);
    let scrolled = normalize_output(render_chat_widget_to_vt100(&mut harness, 80, 24));

    let collapsed_boundaries = count_collapsed_boundaries(&scrolled);
    assert_eq!(
        0,
        collapsed_boundaries,
        "Spacing collapsed unexpectedly when scrolled; investigate history layout spacing"
    );

    insta::assert_snapshot!(
        "scroll_spacing_scrolled_intact",
        scrolled
    );
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
fn tool_activity_showcase() {
    let mut harness = ChatWidgetHarness::new();

    harness.push_user_prompt("Can you gather details from the latest docs update?");

    let mut event_seq = 0u64;
    let mut order_seq = 0u64;

    // Completed web search call
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::WebSearchBegin(WebSearchBeginEvent {
            call_id: "search-complete".into(),
            query: Some("ratatui widget patterns".into()),
        }),
    );
    harness.override_running_tool_elapsed("search-complete", Duration::from_millis(0));
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::WebSearchComplete(WebSearchCompleteEvent {
            call_id: "search-complete".into(),
            query: Some("ratatui widget patterns".into()),
        }),
    );

    // Running web search call
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::WebSearchBegin(WebSearchBeginEvent {
            call_id: "search-running".into(),
            query: Some("async rust tui example".into()),
        }),
    );
    harness.override_running_tool_elapsed("search-running", Duration::from_secs(75));

    // Browser tool: completed click
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "browser-finished".into(),
            tool_name: "browser_click".into(),
            parameters: Some(json!({
                "type": "double",
                "x": 512,
                "y": 284,
                "selector": "#login-button"
            })),
        }),
    );
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
            call_id: "browser-finished".into(),
            tool_name: "browser_click".into(),
            parameters: Some(json!({
                "type": "double",
                "x": 512,
                "y": 284,
                "selector": "#login-button"
            })),
            duration: Duration::from_secs(8),
            result: Ok("{\n  \"status\": \"ok\",\n  \"notes\": \"Login button clicked\"\n}".into()),
        }),
    );

    // Browser tool: active scroll
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "browser-running".into(),
            tool_name: "browser_scroll".into(),
            parameters: Some(json!({
                "dx": 0,
                "dy": 640,
                "speed": "smooth"
            })),
        }),
    );
    harness.override_running_tool_elapsed("browser-running", Duration::from_secs(95));

    // Agent tool: completed run
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "agent-done".into(),
            tool_name: "agent".into(),
            parameters: Some(json!({
                "action": "create",
                "create": {
                    "name": "qa-bot",
                    "task": "Run targeted regression suite",
                    "plan": ["Init workspace", "Nextest smoke", "Summarize"]
                }
            })),
        }),
    );
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
            call_id: "agent-done".into(),
            tool_name: "agent".into(),
            parameters: Some(json!({
                "action": "create",
                "create": {
                    "name": "qa-bot",
                    "task": "Run targeted regression suite",
                    "plan": ["Init workspace", "Nextest smoke", "Summarize"]
                }
            })),
            duration: Duration::from_secs(94),
            result: Ok("Regression sweep complete\n- 58 tests passed\n- 0 failures".into()),
        }),
    );

    // Agent tool: active wait
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "agent-pending".into(),
            tool_name: "agent".into(),
            parameters: Some(json!({
                "action": "wait",
                "wait": {
                    "agent_id": "deploy-helper",
                    "timeout_seconds": 600
                }
            })),
        }),
    );
    harness.override_running_tool_elapsed("agent-pending", Duration::from_secs(185));

    let output = render_chat_widget_to_vt100(&mut harness, 80, 40);
    insta::assert_snapshot!("tool_activity_showcase", output);
}

#[test]
fn browser_session_grouped_desired_layout() {
    let mut harness = ChatWidgetHarness::new();
    harness.push_user_prompt("Open docs and find login button");

    let mut event_seq = 0u64;
    let mut order_seq = 0u64;

    // Browser open -> sets initial URL
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "browser-session".into(),
            tool_name: "browser_open".into(),
            parameters: Some(json!({
                "url": "https://example.com/docs"
            })),
        }),
    );
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
            call_id: "browser-session".into(),
            tool_name: "browser_open".into(),
            parameters: Some(json!({
                "url": "https://example.com/docs"
            })),
            duration: Duration::from_secs(5),
            result: Ok("{\n  \"status\": \"ok\"\n}".into()),
        }),
    );

    // Additional browser interactions
    let actions = [
        (
            "browser_click",
            json!({
                "selector": "#sign-in",
                "description": "Sign in button"
            }),
            Duration::from_secs(13),
        ),
        (
            "browser_scroll",
            json!({
                "dx": 0,
                "dy": 640
            }),
            Duration::from_secs(14),
        ),
        (
            "browser_type",
            json!({
                "text": "docs search"
            }),
            Duration::from_secs(33),
        ),
    ];

    for (tool, params, dur) in actions {
        push_ordered_event(
            &mut harness,
            &mut event_seq,
            &mut order_seq,
            EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
                call_id: format!("browser-session-{tool}"),
                tool_name: tool.into(),
                parameters: Some(params.clone()),
            }),
        );
        push_ordered_event(
            &mut harness,
            &mut event_seq,
            &mut order_seq,
            EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
                call_id: format!("browser-session-{tool}"),
                tool_name: tool.into(),
                parameters: Some(params),
                duration: dur,
                result: Ok("{\n  \"status\": \"ok\"\n}".into()),
            }),
        );
    }

    // Capture console warning (represented as background event)
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::BackgroundEvent(BackgroundEventEvent {
            message: "cdp warning: Refused to load script from cdn.example.com".into(),
        }),
    );

    // Screenshot update for active tab
    harness.handle_event(Event {
        id: "browser-shot".into(),
        event_seq,
        msg: EventMsg::BrowserScreenshotUpdate(BrowserScreenshotUpdateEvent {
            screenshot_path: PathBuf::from("/tmp/browser_session.png"),
            url: "https://example.com/docs".into(),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(1),
            sequence_number: Some(order_seq),
        }),
    });

    let output = render_chat_widget_to_vt100(&mut harness, 80, 32);
    insta::assert_snapshot!("browser_session_grouped_desired_layout", output);
}

#[test]
fn browser_session_grouped_with_unordered_actions() {
    let mut harness = ChatWidgetHarness::new();
    harness.push_user_prompt("Handle captcha gracefully");

    let mut event_seq = 0u64;
    let mut order_seq = 0u64;

    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "browser-session-open".into(),
            tool_name: "browser_open".into(),
            parameters: Some(json!({ "url": "https://example.com" })),
        }),
    );
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
            call_id: "browser-session-open".into(),
            tool_name: "browser_open".into(),
            parameters: Some(json!({ "url": "https://example.com" })),
            duration: Duration::from_secs(3),
            result: Ok("{ \"status\": \"ok\" }".into()),
        }),
    );

    push_unordered_event(
        &mut harness,
        &mut event_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "browser-session-type".into(),
            tool_name: "browser_type".into(),
            parameters: Some(json!({ "text": "pizza" })),
        }),
    );
    push_unordered_event(
        &mut harness,
        &mut event_seq,
        EventMsg::BackgroundEvent(BackgroundEventEvent {
            message: "Encountering captcha block".into(),
        }),
    );
    push_unordered_event(
        &mut harness,
        &mut event_seq,
        EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
            call_id: "browser-session-type".into(),
            tool_name: "browser_type".into(),
            parameters: Some(json!({ "text": "pizza" })),
            duration: Duration::from_secs(2),
            result: Ok("{ \"status\": \"typed\" }".into()),
        }),
    );

    push_unordered_event(
        &mut harness,
        &mut event_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "browser-session-key".into(),
            tool_name: "browser_key".into(),
            parameters: Some(json!({ "key": "Enter" })),
        }),
    );
    push_unordered_event(
        &mut harness,
        &mut event_seq,
        EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
            call_id: "browser-session-key".into(),
            tool_name: "browser_key".into(),
            parameters: Some(json!({ "key": "Enter" })),
            duration: Duration::from_secs(1),
            result: Ok("{ \"status\": \"ok\" }".into()),
        }),
    );

    let output = render_chat_widget_to_vt100(&mut harness, 80, 32);
    let output = normalize_output(output);
    insta::assert_snapshot!(
        "browser_session_grouped_with_unordered_actions",
        output
    );
}

#[test]
fn agent_run_grouped_desired_layout() {
    let mut harness = ChatWidgetHarness::new();
    harness.push_user_prompt("Kick off QA bot regression run");

    let mut event_seq = 0u64;
    let mut order_seq = 0u64;

    // Agent run begins
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "agent-run".into(),
            tool_name: "agent".into(),
            parameters: Some(json!({
                "action": "create",
                "create": {
                    "name": "qa-bot",
                    "task": "Run targeted regression suite",
                    "plan": ["Init workspace", "Nextest smoke", "Summarize"]
                }
            })),
        }),
    );

    // Status update with multiple agents
    harness.handle_event(Event {
        id: "agent-status".into(),
        event_seq,
        msg: EventMsg::AgentStatusUpdate(AgentStatusUpdateEvent {
            agents: vec![
                AgentInfo {
                    id: "qa-bot".into(),
                    name: "QA Bot".into(),
                    status: "running tests".into(),
                    batch_id: None,
                    model: None,
                    last_progress: None,
                    result: None,
                    error: None,
                    elapsed_ms: Some(29_000),
                    token_count: Some(12_400),
                },
                AgentInfo {
                    id: "doc-writer".into(),
                    name: "Doc Writer".into(),
                    status: "planning".into(),
                    batch_id: None,
                    model: None,
                    last_progress: None,
                    result: None,
                    error: None,
                    elapsed_ms: Some(4_500),
                    token_count: None,
                },
            ],
            context: Some("regression sweep".into()),
            task: Some("Ship bugfix patch".into()),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(1),
            sequence_number: Some(order_seq),
        }),
    });
    event_seq += 1;
    order_seq += 1;

    // Agent result
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
            call_id: "agent-run".into(),
            tool_name: "agent".into(),
            parameters: Some(json!({
                "action": "create",
                "create": {
                    "name": "qa-bot",
                    "task": "Run targeted regression suite",
                    "plan": ["Init workspace", "Nextest smoke", "Summarize"]
                }
            })),
            duration: Duration::from_secs(94),
            result: Ok("Regression sweep complete\n- 58 tests passed\n- 0 failures".into()),
        }),
    );

    let output = render_chat_widget_to_vt100(&mut harness, 80, 32);
    let output = normalize_output(output);
    insta::assert_snapshot!(
        "agent_run_grouped_desired_layout",
        &output,
        @"agent_run_grouped_desired_layout"
    );
}

#[test]
fn agent_run_grouped_plain_tool_name() {
    let mut harness = ChatWidgetHarness::new();
    harness.push_user_prompt("Kick off QA bot regression run");

    let mut event_seq = 0u64;
    let mut order_seq = 0u64;

    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "agent-run-plain".into(),
            tool_name: "agent".into(),
            parameters: Some(json!({
                "action": "create",
                "create": {
                    "name": "qa-bot",
                    "task": "Run targeted regression suite",
                    "plan": ["Init workspace", "Nextest smoke", "Summarize"]
                }
            })),
        }),
    );

    harness.handle_event(Event {
        id: "agent-status".into(),
        event_seq,
        msg: EventMsg::AgentStatusUpdate(AgentStatusUpdateEvent {
            agents: vec![
                AgentInfo {
                    id: "qa-bot".into(),
                    name: "QA Bot".into(),
                    status: "running".into(),
                    batch_id: Some("batch-001".into()),
                    model: Some("claude".into()),
                    last_progress: Some("Executing smoke tests".into()),
                    result: None,
                    error: None,
                    elapsed_ms: Some(18_750),
                    token_count: Some(8_900),
                },
            ],
            context: Some("regression sweep".into()),
            task: Some("Ship bugfix patch".into()),
        }),
        order: Some(OrderMeta {
            request_ordinal: 1,
            output_index: Some(1),
            sequence_number: Some(order_seq),
        }),
    });
    event_seq += 1;
    order_seq += 1;

    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallEnd(CustomToolCallEndEvent {
            call_id: "agent-run-plain".into(),
            tool_name: "agent".into(),
            parameters: Some(json!({
                "action": "create",
                "create": {
                    "name": "qa-bot",
                    "task": "Run targeted regression suite",
                    "plan": ["Init workspace", "Nextest smoke", "Summarize"]
                }
            })),
            duration: Duration::from_secs(104),
            result: Ok("Regression sweep complete\n- 58 tests passed\n- 0 failures".into()),
        }),
    );

    let output = render_chat_widget_to_vt100(&mut harness, 80, 32);
    let output = normalize_output(output);
    insta::assert_snapshot!("agent_run_grouped_plain_tool_name", output);
}

#[test]
fn plan_agent_keeps_single_aggregate_block() {
    let mut harness = ChatWidgetHarness::new();
    harness.push_user_prompt("/plan deduplicate agent aggregates");

    let mut event_seq = 0u64;
    let mut order_seq = 0u64;

    // Planner agent begins with an ordered event so the tracker stores a request-scoped key.
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "plan-call".into(),
            tool_name: "agent".into(),
            parameters: Some(json!({
                "action": "create",
                "create": {
                    "name": "planner",
                    "task": "Draft implementation plan",
                    "plan": ["Outline work", "Validate approach", "Summarize"]
                }
            })),
        }),
    );

    // Status update arrives without ordering metadata; this rewrites the tracker key
    // to the batch form while leaving agent_run_by_order pointing at the old key.
    harness.handle_event(Event {
        id: "agent-status".into(),
        event_seq,
        msg: EventMsg::AgentStatusUpdate(AgentStatusUpdateEvent {
            agents: vec![AgentInfo {
                id: "planner".into(),
                name: "Planner".into(),
                status: "running".into(),
                batch_id: Some("batch-plan".into()),
                model: Some("gpt-4o".into()),
                last_progress: Some("refining steps".into()),
                result: None,
                error: None,
                elapsed_ms: Some(12_300),
                token_count: Some(6_100),
            }],
            context: Some("/plan coordination".into()),
            task: Some("Draft implementation plan".into()),
        }),
        order: None,
    });
    event_seq += 1;

    // A follow-up agent action arrives with ordering metadata. Because the status update above
    // rewired the tracker to a batch-scoped key without updating the order map, this ordered
    // begin cannot find the existing tracker and inserts a second aggregate block.
    push_ordered_event(
        &mut harness,
        &mut event_seq,
        &mut order_seq,
        EventMsg::CustomToolCallBegin(CustomToolCallBeginEvent {
            call_id: "plan-result".into(),
            tool_name: "agent_result".into(),
            parameters: Some(json!({
                "action": "result"
            })),
        }),
    );

    let output = render_chat_widget_to_vt100(&mut harness, 80, 40);
    let agent_blocks = harness.count_agent_run_cells();
    assert_eq!(agent_blocks, 1, "expected a single aggregate agent block, saw {}\n{}", agent_blocks, output);
}
