use super::{tool_cards, ChatWidget, OrderKey, ToolCallId};
use super::tool_cards::ToolCardSlot;
use crate::history_cell::{WebSearchSessionCell, WebSearchStatus};
use std::time::Instant;
use std::mem;

pub(super) struct WebSearchTracker {
    pub slot: ToolCardSlot,
    pub cell: WebSearchSessionCell,
    pub call_id: String,
    pub started_at: Instant,
}

impl WebSearchTracker {
    fn new(order_key: OrderKey, call_id: String) -> Self {
        Self {
            slot: ToolCardSlot::new(order_key),
            cell: WebSearchSessionCell::new(),
            call_id,
            started_at: Instant::now(),
        }
    }

    fn card_key(&self) -> String {
        format!("web_search:{}", self.call_id)
    }

    fn assign_key(&mut self) {
        let key = self.card_key();
        self.cell.set_signature(Some(key.clone()));
        tool_cards::assign_tool_card_key(&mut self.slot, &mut self.cell, Some(key.clone()));
        self.slot.set_signature(Some(key));
    }

    fn insert(&mut self, chat: &mut ChatWidget<'_>) {
        self.assign_key();
        tool_cards::ensure_tool_card::<WebSearchSessionCell>(chat, &mut self.slot, &self.cell);
    }

    fn replace(&mut self, chat: &mut ChatWidget<'_>) {
        tool_cards::replace_tool_card::<WebSearchSessionCell>(chat, &mut self.slot, &self.cell);
    }
}

pub(super) fn handle_begin(
    chat: &mut ChatWidget<'_>,
    order_key: OrderKey,
    call_id: String,
    query: Option<String>,
) {
    let mut tracker = WebSearchTracker::new(order_key, call_id.clone());
    tracker.cell.set_query(query.clone());
    if let Some(q) = query {
        tracker
            .cell
            .push_info(format!("Searching for \"{}\"", q));
    } else {
        tracker.cell.ensure_started_message();
    }
    tracker.insert(chat);

    chat
        .tools_state
        .web_search_sessions
        .insert(ToolCallId(call_id), tracker);
    chat.bottom_pane.update_status_text("Search".to_string());
    chat.request_redraw();
}

pub(super) fn handle_complete(
    chat: &mut ChatWidget<'_>,
    order_key: OrderKey,
    call_id: String,
    query: Option<String>,
) {
    let map_key = ToolCallId(call_id.clone());
    let tracker_opt = chat.tools_state.web_search_sessions.remove(&map_key);

    if let Some(mut tracker) = tracker_opt {
        tracker.slot.set_order_key(order_key);
        tracker.cell.set_query(query.clone());
        let duration = tracker.started_at.elapsed();
        tracker.cell.push_success("Results ready");
        tracker.cell.set_duration(Some(duration));
        tracker.cell.set_status(WebSearchStatus::Completed);
        tracker.replace(chat);
    } else {
        let mut tracker = WebSearchTracker::new(order_key, call_id.clone());
        tracker.cell.set_query(query.clone());
        if let Some(q) = query {
            tracker
                .cell
                .push_info(format!("Searched for \"{}\"", q));
        }
        tracker.cell.push_success("Results ready");
        tracker.cell.set_status(WebSearchStatus::Completed);
        tracker.insert(chat);
    }

    chat.bottom_pane.update_status_text("responding".to_string());
    chat.maybe_hide_spinner();
}

pub(super) fn finalize_all_failed(chat: &mut ChatWidget<'_>, message: &str) {
    if chat.tools_state.web_search_sessions.is_empty() {
        return;
    }
    let mut trackers = mem::take(&mut chat.tools_state.web_search_sessions);
    for (_, mut tracker) in trackers.drain() {
        tracker.slot.set_order_key(chat.next_internal_key());
        tracker.cell.push_error(message.to_string());
        tracker.cell.set_status(WebSearchStatus::Failed);
        tracker.replace(chat);
    }
}

pub(super) fn finalize_all_completed(chat: &mut ChatWidget<'_>, message: &str) {
    if chat.tools_state.web_search_sessions.is_empty() {
        return;
    }
    let mut trackers = mem::take(&mut chat.tools_state.web_search_sessions);
    for (_, mut tracker) in trackers.drain() {
        tracker.slot.set_order_key(chat.next_internal_key());
        tracker.cell.push_success(message.to_string());
        tracker.cell.set_status(WebSearchStatus::Completed);
        tracker.replace(chat);
    }
}
