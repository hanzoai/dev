use super::{tool_cards, ChatWidget, OrderKey};
use super::tool_cards::ToolCardSlot;
use crate::history_cell::BrowserSessionCell;
use code_core::protocol::OrderMeta;
use serde_json::Value;
use std::path::PathBuf;
use std::time::Duration;

pub(super) struct BrowserSessionTracker {
    pub slot: ToolCardSlot,
    pub cell: BrowserSessionCell,
    pub elapsed: Duration,
}

impl BrowserSessionTracker {
    fn new(order_key: OrderKey) -> Self {
        Self {
            slot: ToolCardSlot::new(order_key),
            cell: BrowserSessionCell::new(),
            elapsed: Duration::default(),
        }
    }
}

pub(super) fn handle_custom_tool_begin(
    chat: &mut ChatWidget<'_>,
    order: Option<&OrderMeta>,
    call_id: &str,
    tool_name: &str,
    params: Option<Value>,
) -> bool {
    if !tool_name.starts_with("browser_") || tool_name == "browser_fetch" {
        return false;
    }

    let (order_key, ordinal) = order_key_and_ordinal(chat, order);
    let key = select_session_key(chat, order, call_id, tool_name);
    let mut tracker = chat
        .tools_state
        .browser_sessions
        .remove(&key)
        .unwrap_or_else(|| BrowserSessionTracker::new(order_key));
    tracker.slot.set_order_key(order_key);

    if let Some(Value::Object(json)) = params.as_ref() {
        if tool_name == "browser_open" {
            if let Some(url) = json.get("url").and_then(|v| v.as_str()) {
                tracker.cell.set_url(url.to_string());
            }
        }
    }

    tool_cards::assign_tool_card_key(&mut tracker.slot, &mut tracker.cell, Some(key.clone()));
    tool_cards::ensure_tool_card::<BrowserSessionCell>(chat, &mut tracker.slot, &tracker.cell);

    chat
        .tools_state
        .browser_session_by_call
        .insert(call_id.to_string(), key.clone());
    if let Some(ord) = ordinal {
        chat
            .tools_state
            .browser_session_by_order
            .insert(ord, key.clone());
    }
    chat.tools_state.browser_last_key = Some(key.clone());

    chat
        .tools_state
        .browser_sessions
        .insert(key, tracker);

    true
}

pub(super) fn handle_custom_tool_end(
    chat: &mut ChatWidget<'_>,
    order: Option<&OrderMeta>,
    call_id: &str,
    tool_name: &str,
    params: Option<Value>,
    duration: Duration,
    result: &Result<String, String>,
) -> bool {
    if !tool_name.starts_with("browser_") || tool_name == "browser_fetch" {
        return false;
    }

    let key = chat
        .tools_state
        .browser_session_by_call
        .remove(call_id)
        .or_else(|| order.and_then(|meta| chat.tools_state.browser_session_by_order.get(&meta.request_ordinal).cloned()))
        .unwrap_or_else(|| browser_key(order, call_id));

    let mut tracker = match chat.tools_state.browser_sessions.remove(&key) {
        Some(tracker) => tracker,
        None => return false,
    };

    let params_to_use = params.as_ref();
    if tool_name == "browser_open" {
        if let Some(Value::Object(json)) = params_to_use {
            if let Some(url) = json.get("url").and_then(|v| v.as_str()) {
                tracker.cell.set_url(url.to_string());
            }
        }
    }

    let summary = summarize_action(tool_name, params_to_use, result);
    let timestamp = tracker.elapsed;
    tracker
        .cell
        .record_action(timestamp, duration, summary);
    tracker.elapsed = tracker.elapsed.saturating_add(duration);

    tool_cards::assign_tool_card_key(&mut tracker.slot, &mut tracker.cell, Some(key.clone()));
    tool_cards::replace_tool_card::<BrowserSessionCell>(chat, &mut tracker.slot, &tracker.cell);

    if let Some(ord) = order.map(|m| m.request_ordinal) {
        chat
            .tools_state
            .browser_session_by_order
            .insert(ord, key.clone());
    }
    chat
        .tools_state
        .browser_sessions
        .insert(key, tracker);

    true
}

pub(super) fn handle_background_event(
    chat: &mut ChatWidget<'_>,
    order: Option<&OrderMeta>,
    message: &str,
) -> bool {
    if chat.tools_state.browser_sessions.is_empty() {
        return false;
    }

    let key = key_from_order_or_last(chat, order);
    let Some(key) = key else { return false; };

    let mut tracker = match chat.tools_state.browser_sessions.remove(&key) {
        Some(tracker) => tracker,
        None => return false,
    };

    let console_line = if message.starts_with("⚠️") {
        message.to_string()
    } else {
        format!("⚠️  {}", message)
    };
    tracker.cell.add_console_message(console_line);

    tool_cards::assign_tool_card_key(&mut tracker.slot, &mut tracker.cell, Some(key.clone()));
    tool_cards::replace_tool_card::<BrowserSessionCell>(chat, &mut tracker.slot, &tracker.cell);

    chat
        .tools_state
        .browser_sessions
        .insert(key, tracker);

    true
}

pub(super) fn handle_screenshot_update(
    chat: &mut ChatWidget<'_>,
    order: Option<&OrderMeta>,
    screenshot_path: &PathBuf,
    url: &str,
) -> bool {
    if chat.tools_state.browser_sessions.is_empty() {
        return false;
    }

    let key = key_from_order_or_last(chat, order);
    let Some(key) = key else { return false; };

    let mut tracker = match chat.tools_state.browser_sessions.remove(&key) {
        Some(tracker) => tracker,
        None => return false,
    };

    tracker.cell.set_url(url.to_string());
    tracker.cell.set_screenshot(screenshot_path.clone());

    tool_cards::assign_tool_card_key(&mut tracker.slot, &mut tracker.cell, Some(key.clone()));
    tool_cards::replace_tool_card::<BrowserSessionCell>(chat, &mut tracker.slot, &tracker.cell);

    chat
        .tools_state
        .browser_sessions
        .insert(key, tracker);

    true
}

fn order_key_and_ordinal(chat: &mut ChatWidget<'_>, order: Option<&OrderMeta>) -> (OrderKey, Option<u64>) {
    match order {
        Some(meta) => (chat.provider_order_key_from_order_meta(meta), Some(meta.request_ordinal)),
        None => (chat.next_internal_key(), None),
    }
}

fn browser_key(order: Option<&OrderMeta>, call_id: &str) -> String {
    if let Some(meta) = order {
        format!("req:{}:{}", meta.request_ordinal, call_id)
    } else {
        format!("call:{}", call_id)
    }
}

fn select_session_key(
    chat: &mut ChatWidget<'_>,
    order: Option<&OrderMeta>,
    call_id: &str,
    tool_name: &str,
) -> String {
    if let Some(meta) = order {
        if let Some(existing) = chat
            .tools_state
            .browser_session_by_order
            .get(&meta.request_ordinal)
            .cloned()
        {
            if chat.tools_state.browser_sessions.contains_key(&existing) {
                return existing;
            }
        }
        if let Some(last) = chat.tools_state.browser_last_key.clone() {
            if chat.tools_state.browser_sessions.contains_key(&last) {
                return last;
            }
        }
    }

    let mut key = browser_key(order, call_id);

    if order.is_none() && tool_name != "browser_open" {
        if let Some(last) = chat.tools_state.browser_last_key.clone() {
            if chat.tools_state.browser_sessions.contains_key(&last) {
                key = last;
            }
        }
    }

    key
}

fn key_from_order_or_last(
    chat: &mut ChatWidget<'_>,
    order: Option<&OrderMeta>,
) -> Option<String> {
    if let Some(meta) = order {
        if let Some(key) = chat
            .tools_state
            .browser_session_by_order
            .get(&meta.request_ordinal)
            .cloned()
        {
            return Some(key);
        }
    }
    chat.tools_state.browser_last_key.clone()
}

fn summarize_action(
    tool_name: &str,
    params: Option<&Value>,
    result: &Result<String, String>,
) -> String {
    match tool_name {
        "browser_open" => params
            .and_then(|value| value.get("url"))
            .and_then(|value| value.as_str())
            .map(|url| format!("Open URL {}", url))
            .unwrap_or_else(|| "Open page".to_string()),
        "browser_click" => {
            let description = params
                .and_then(|value| value.get("description"))
                .and_then(|value| value.as_str())
                .map(|s| s.to_string());
            let selector = params
                .and_then(|value| value.get("selector"))
                .and_then(|value| value.as_str())
                .map(|s| s.to_string());
            match (description, selector) {
                (Some(desc), Some(sel)) => format!("Click {} ({})", desc, sel),
                (Some(desc), None) => format!("Click {}", desc),
                (None, Some(sel)) => format!("Click {}", sel),
                _ => "Click".to_string(),
            }
        }
        "browser_scroll" => {
            let dx = params
                .and_then(|value| value.get("dx"))
                .and_then(|value| value.as_i64())
                .unwrap_or(0);
            let dy = params
                .and_then(|value| value.get("dy"))
                .and_then(|value| value.as_i64())
                .unwrap_or(0);
            if dx == 0 {
                format!("Scroll by dy={}", dy)
            } else {
                format!("Scroll by dx={} dy={}", dx, dy)
            }
        }
        "browser_type" => {
            let text = params
                .and_then(|value| value.get("text"))
                .and_then(|value| value.as_str())
                .unwrap_or("");
            let open = '\u{201c}';
            let close = '\u{201d}';
            format!("Type {}{}{}", open, text, close)
        }
        "browser_close" => match result {
            Ok(_) => "Close browser".to_string(),
            Err(err) => format!("Close browser (error: {}...)", truncate(err, 24)),
        },
        other => other.replace('_', " "),
    }
}

fn truncate(input: &str, max: usize) -> String {
    if input.chars().count() <= max {
        input.to_string()
    } else {
        let truncated: String = input.chars().take(max).collect();
        format!("{}…", truncated)
    }
}
