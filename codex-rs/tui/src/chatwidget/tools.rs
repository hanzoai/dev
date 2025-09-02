//! Tool (Web Search, MCP) lifecycle handlers extracted from ChatWidget.

use super::ChatWidget;
use crate::history_cell;
use codex_core::protocol::{McpToolCallBeginEvent, McpToolCallEndEvent};

pub(super) fn web_search_begin(chat: &mut ChatWidget<'_>, call_id: String, query: Option<String>) {
    for cell in &chat.history_cells { cell.trigger_fade(); }
    chat.finalize_active_stream();
    chat.flush_interrupt_queue();

    let cell = history_cell::new_running_web_search(query.clone());
    chat.history_push(cell);
    if let Some(last_idx) = chat.history_cells.len().checked_sub(1) {
        chat.tools_state.running_web_search.insert(call_id, (last_idx, query));
    }
    chat.bottom_pane.update_status_text("Searched".to_string());
    chat.mark_needs_redraw();
}

pub(super) fn web_search_complete(chat: &mut ChatWidget<'_>, call_id: String, query: Option<String>) {
    if let Some((idx, maybe_query)) = chat.tools_state.running_web_search.remove(&call_id) {
        let mut target_idx = None;
        if idx < chat.history_cells.len() {
            let is_ws = chat.history_cells[idx]
                .as_any()
                .downcast_ref::<history_cell::RunningToolCallCell>()
                .is_some_and(|rt| rt.has_title("Web Search..."));
            if is_ws { target_idx = Some(idx); }
        }
        if target_idx.is_none() {
            for i in (0..chat.history_cells.len()).rev() {
                if let Some(rt) = chat.history_cells[i]
                    .as_any()
                    .downcast_ref::<history_cell::RunningToolCallCell>()
                {
                    if rt.has_title("Web Search...") {
                        target_idx = Some(i);
                        break;
                    }
                }
            }
        }
        if let Some(i) = target_idx {
            if let Some(rt) = chat.history_cells[i]
                .as_any()
                .downcast_ref::<history_cell::RunningToolCallCell>()
            {
                let final_query = query.or(maybe_query);
                let completed = rt.finalize_web_search(true, final_query);
                chat.history_replace_at(i, Box::new(completed));

                // Merge adjacent Web Search blocks with same header
                if i > 0 {
                    let new_lines = chat.history_cells[i].display_lines();
                    let new_header = new_lines.first().and_then(|l| l.spans.get(0)).map(|s| s.content.clone().to_string()).unwrap_or_default();
                    let prev_lines = chat.history_cells[i - 1].display_lines();
                    let prev_header = prev_lines.first().and_then(|l| l.spans.get(0)).map(|s| s.content.clone().to_string()).unwrap_or_default();
                    if !new_header.is_empty() && new_header == prev_header {
                        let mut combined = prev_lines.clone();
                        while combined.last().map(|l| crate::render::line_utils::is_blank_line_trim(l)).unwrap_or(false) { combined.pop(); }
                        let mut body: Vec<ratatui::text::Line<'static>> = new_lines.into_iter().skip(1).collect();
                        while body.first().map(|l| crate::render::line_utils::is_blank_line_trim(l)).unwrap_or(false) { body.remove(0); }
                        while body.last().map(|l| crate::render::line_utils::is_blank_line_trim(l)).unwrap_or(false) { body.pop(); }
                        if let Some(first_line) = body.first_mut() {
                            if let Some(first_span) = first_line.spans.get_mut(0) {
                                if first_span.content == "  └ " || first_span.content == "└ " {
                                    first_span.content = "  ".into();
                                }
                            }
                        }
                        combined.extend(body);
                        chat.history_replace_at(i - 1, Box::new(history_cell::PlainHistoryCell { lines: combined, kind: history_cell::HistoryCellType::Plain }));
                        chat.history_remove_at(i);
                    }
                }
                // history_replace_at already invalidates and redraws
            }
        }
    }
    chat.bottom_pane.update_status_text("responding".to_string());
    chat.maybe_hide_spinner();
}

pub(super) fn mcp_begin(chat: &mut ChatWidget<'_>, ev: McpToolCallBeginEvent) {
    for cell in &chat.history_cells { cell.trigger_fade(); }
    let McpToolCallBeginEvent { call_id, invocation } = ev;
    let cell = history_cell::new_running_mcp_tool_call(invocation);
    chat.history_push(cell);
    if let Some(last_idx) = chat.history_cells.len().checked_sub(1) {
        chat.tools_state.running_custom_tools.insert(call_id, last_idx);
    }
}

pub(super) fn mcp_end(chat: &mut ChatWidget<'_>, ev: McpToolCallEndEvent) {
    let McpToolCallEndEvent { call_id, duration, invocation, result } = ev;
    let success = !result.as_ref().map(|r| r.is_error.unwrap_or(false)).unwrap_or(false);
    let completed = history_cell::new_completed_mcp_tool_call(80, invocation, duration, success, result);
    if let Some(idx) = chat.tools_state.running_custom_tools.remove(&call_id) {
        if idx < chat.history_cells.len() {
            chat.history_replace_at(idx, completed);
            return;
        }
    }
    chat.history_push(completed);
}
