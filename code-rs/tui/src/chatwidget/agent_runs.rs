use super::{ChatWidget, OrderKey};
use crate::history::state::HistoryId;
use crate::history_cell::AgentRunCell;
use code_core::protocol::{AgentStatusUpdateEvent, OrderMeta};
use serde_json::Value;
use std::collections::HashSet;
use std::time::Duration;

const AGENT_TOOL_NAMES: &[&str] = &[
    "agent",
    "agent_run",
    "agent_result",
    "agent_wait",
    "agent_cancel",
    "agent_check",
    "agent_list",
];

pub(super) fn is_agent_tool(tool_name: &str) -> bool {
    AGENT_TOOL_NAMES
        .iter()
        .copied()
        .any(|name| name.eq_ignore_ascii_case(tool_name))
}

fn is_primary_run_tool(tool_name: &str) -> bool {
    matches!(tool_name, "agent" | "agent_run")
}

fn format_elapsed_short(duration: Duration) -> String {
    let secs = duration.as_secs();
    let minutes = secs / 60;
    let seconds = secs % 60;
    if minutes > 0 {
        format!("{}m{:02}s", minutes, seconds)
    } else {
        format!("{:02}s", seconds)
    }
}

fn begin_action_for(tool_name: &str, metadata: &InvocationMetadata) -> Option<String> {
    let label = metadata
        .label
        .clone()
        .or_else(|| metadata.agent_ids.clone().into_iter().next())
        .unwrap_or_else(|| "agent".to_string());
    match tool_name {
        "agent" | "agent_run" => Some(format!("Started agent run for {}", label)),
        "agent_wait" => metadata
            .batch_id
            .as_ref()
            .map(|batch| format!("Waiting for agents in batch {}", batch)),
        "agent_result" => Some(format!("Requested results for {}", label)),
        "agent_cancel" => Some(format!("Cancelling agent batch for {}", label)),
        "agent_check" => Some(format!("Checking agent status for {}", label)),
        "agent_list" => Some("Listing available agents".to_string()),
        _ => None,
    }
}

fn end_action_for(
    tool_name: &str,
    duration: Duration,
    success: bool,
    message: Option<&str>,
) -> Option<String> {
    let elapsed = format_elapsed_short(duration);
    match tool_name {
        "agent" | "agent_run" => {
            if success {
                Some(format!("Agent run completed in {}", elapsed))
            } else {
                let detail = message.unwrap_or("unknown error");
                Some(format!("Agent run failed in {} — {}", elapsed, detail))
            }
        }
        "agent_wait" => {
            if success {
                Some(format!("Finished waiting in {}", elapsed))
            } else {
                let detail = message.unwrap_or("wait failed");
                Some(format!("Wait failed in {} — {}", elapsed, detail))
            }
        }
        "agent_result" => {
            if success {
                Some(format!("Fetched agent results in {}", elapsed))
            } else {
                let detail = message.unwrap_or("result error");
                Some(format!("Result fetch failed in {} — {}", elapsed, detail))
            }
        }
        "agent_cancel" => Some(format!("Cancel request completed in {}", elapsed)),
        "agent_check" => Some(format!("Status check finished in {}", elapsed)),
        "agent_list" => Some("Listed agents".to_string()),
        _ => None,
    }
}

pub(super) struct AgentRunTracker {
    pub order_key: OrderKey,
    pub cell_index: Option<usize>,
    pub history_id: Option<HistoryId>,
    pub cell: AgentRunCell,
    pub batch_id: Option<String>,
    agent_ids: HashSet<String>,
    task: Option<String>,
    has_custom_name: bool,
    call_ids: HashSet<String>,
}

impl AgentRunTracker {
    pub fn new(order_key: OrderKey) -> Self {
        Self {
            order_key,
            cell_index: None,
            history_id: None,
            cell: AgentRunCell::new("(pending)".to_string()),
            batch_id: None,
            agent_ids: HashSet::new(),
            task: None,
            has_custom_name: false,
            call_ids: HashSet::new(),
        }
    }

    fn merge_agent_ids<I>(&mut self, ids: I)
    where
        I: IntoIterator<Item = String>,
    {
        for id in ids {
            self.agent_ids.insert(id);
        }
    }

    fn set_task(&mut self, task: Option<String>) {
        if let Some(value) = task {
            self.task = Some(value);
        }
        self.cell.set_task(self.task.clone());
    }

    fn set_agent_name(&mut self, name: Option<String>, override_existing: bool) {
        if let Some(name) = name {
            if override_existing || !self.has_custom_name {
                self.cell.set_agent_name(name);
                self.has_custom_name = true;
            }
        }
    }
}

#[derive(Default)]
struct InvocationMetadata {
    batch_id: Option<String>,
    agent_ids: Vec<String>,
    task: Option<String>,
    plan: Vec<String>,
    label: Option<String>,
}

impl InvocationMetadata {
    fn from(tool_name: &str, params: Option<&Value>) -> Self {
        let mut meta = InvocationMetadata::default();
        if let Some(Value::Object(map)) = params {
            if let Some(batch) = map.get("batch_id").and_then(|v| v.as_str()) {
                meta.batch_id = Some(batch.to_string());
            }
            if let Some(agent_id) = map.get("agent_id").and_then(|v| v.as_str()) {
                meta.agent_ids.push(agent_id.to_string());
            }
            if let Some(agent_name) = map.get("agent_name").and_then(|v| v.as_str()) {
                meta.label = Some(agent_name.to_string());
            }
            if let Some(task) = map.get("task").and_then(|v| v.as_str()) {
                meta.task = Some(task.to_string());
            }
            if let Some(plan) = map.get("plan").and_then(|v| v.as_array()) {
                meta.plan = plan
                    .iter()
                    .filter_map(|step| step.as_str().map(|s| s.to_string()))
                    .collect();
            }
            if let Some(models) = map.get("models").and_then(|v| v.as_array()) {
                for model in models {
                    if let Some(name) = model.as_str() {
                        meta.agent_ids.push(name.to_string());
                    }
                }
            }
            if let Some(agents) = map.get("agents").and_then(|v| v.as_array()) {
                for entry in agents {
                    if let Some(obj) = entry.as_object() {
                        if let Some(id) = obj.get("id").and_then(|v| v.as_str()) {
                            meta.agent_ids.push(id.to_string());
                        }
                        if meta.label.is_none() {
                            if let Some(name) = obj.get("name").and_then(|v| v.as_str()) {
                                meta.label = Some(name.to_string());
                            }
                        }
                    }
                }
            }
        }
        if meta.label.is_none() {
            if let Some(first) = meta.agent_ids.first() {
                meta.label = Some(first.clone());
            }
        }
        meta.agent_ids = dedup(meta.agent_ids);
        if meta.plan.is_empty() && is_primary_run_tool(tool_name) {
            // Leave plan empty; the UI will render a placeholder.
        }
        meta
    }
}

pub(super) fn handle_custom_tool_begin(
    chat: &mut ChatWidget<'_>,
    order: Option<&OrderMeta>,
    call_id: &str,
    tool_name: &str,
    params: Option<Value>,
) -> bool {
    if !is_agent_tool(tool_name) {
        return false;
    }

    let metadata = InvocationMetadata::from(tool_name, params.as_ref());
    let (order_key, ordinal) = order_key_and_ordinal(chat, order);
    let mut key = agent_key(order, call_id, tool_name, &metadata);

    let mut tracker = chat
        .tools_state
        .agent_runs
        .remove(&key)
        .unwrap_or_else(|| AgentRunTracker::new(order_key));
    tracker.order_key = order_key;

    ensure_agent_cell(chat, &mut tracker);

    if let Some(batch) = metadata.batch_id.clone() {
        tracker.batch_id.get_or_insert(batch);
    }
    tracker.merge_agent_ids(metadata.agent_ids.clone());

    tracker.set_agent_name(metadata.label.clone(), true);
    if !metadata.plan.is_empty() {
        tracker.cell.set_plan(metadata.plan.clone());
    }
    tracker.set_task(metadata.task.clone());

    if let Some(action) = begin_action_for(tool_name, &metadata) {
        tracker.cell.record_action(action);
    }

    replace_agent_cell(chat, &mut tracker);

    key = update_mappings(
        chat,
        key,
        order,
        Some(call_id),
        ordinal,
        tool_name,
        &mut tracker,
    );
    chat.tools_state.agent_last_key = Some(key.clone());
    chat.tools_state.agent_runs.insert(key, tracker);

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
    if !is_agent_tool(tool_name) {
        return false;
    }

    let metadata = InvocationMetadata::from(tool_name, params.as_ref());
    let ordinal = order.map(|m| m.request_ordinal);
    let mut key = lookup_key(chat, order, call_id)
        .unwrap_or_else(|| agent_key(order, call_id, tool_name, &metadata));

    let mut tracker = match chat.tools_state.agent_runs.remove(&key) {
        Some(existing) => existing,
        None => return false,
    };

    ensure_agent_cell(chat, &mut tracker);

    if let Some(batch) = metadata.batch_id.clone() {
        tracker.batch_id.get_or_insert(batch);
    }
    tracker.merge_agent_ids(metadata.agent_ids.clone());

    tracker.set_agent_name(metadata.label.clone(), true);
    if !metadata.plan.is_empty() {
        tracker.cell.set_plan(metadata.plan.clone());
    }
    tracker.set_task(metadata.task.clone());

    tracker.cell.set_duration(Some(duration));
    match result {
        Ok(text) => {
            let lines = lines_from(text);
            if !lines.is_empty() {
                tracker.cell.set_latest_result(lines);
            }
            tracker.cell.set_status_label("Completed");
            tracker.cell.mark_completed();
            if let Some(action) = end_action_for(tool_name, duration, true, Some(text.as_str())) {
                tracker.cell.record_action(action);
            }
        }
        Err(err) => {
            tracker.cell.set_latest_result(vec![err.clone()]);
            tracker.cell.mark_failed();
            if let Some(action) = end_action_for(tool_name, duration, false, Some(err.as_str())) {
                tracker.cell.record_action(action);
            }
        }
    }

    replace_agent_cell(chat, &mut tracker);

    key = update_mappings(
        chat,
        key,
        order,
        Some(call_id),
        ordinal,
        tool_name,
        &mut tracker,
    );
    chat.tools_state.agent_last_key = Some(key.clone());
    chat.tools_state.agent_runs.insert(key, tracker);

    true
}

pub(super) fn handle_status_update(chat: &mut ChatWidget<'_>, event: &AgentStatusUpdateEvent) {
    if chat.tools_state.agent_runs.is_empty() {
        return;
    }

    let mut candidate_keys: Vec<String> = Vec::new();
    for agent in &event.agents {
        if let Some(batch_id) = agent.batch_id.as_ref() {
            if let Some(key) = chat.tools_state.agent_run_by_batch.get(batch_id) {
                candidate_keys.push(key.clone());
            }
        }
        if let Some(key) = chat.tools_state.agent_run_by_agent.get(&agent.id) {
            candidate_keys.push(key.clone());
        }
    }

    if candidate_keys.is_empty() {
        if let Some(task) = event.task.as_ref() {
            if let Some((key, _)) = chat
                .tools_state
                .agent_runs
                .iter()
                .find(|(_, tracker)| tracker.task.as_ref() == Some(task))
            {
                candidate_keys.push(key.clone());
            }
        }
    }

    if candidate_keys.is_empty() {
        if let Some(last) = chat.tools_state.agent_last_key.clone() {
            candidate_keys.push(last);
        }
    }

    candidate_keys = dedup(candidate_keys);

    for key in candidate_keys {
        let mut tracker = match chat.tools_state.agent_runs.remove(&key) {
            Some(existing) => existing,
            None => continue,
        };

        let mut current_key = key;

        ensure_agent_cell(chat, &mut tracker);

        if let Some(task) = event.task.clone() {
            tracker.set_task(Some(task));
        } else {
            tracker.set_task(tracker.task.clone());
        }

        let mut rows: Vec<(String, String)> = Vec::new();
        let mut status_collect = StatusSummary::default();
        let mut latest_lines: Option<Vec<String>> = None;

        for agent in &event.agents {
            let mut status_text = agent.status.clone();
            if let Some(progress) = agent.last_progress.as_ref() {
                if !progress.trim().is_empty() {
                    status_text = format!("{} — {}", status_text, progress);
                }
            }
            if let Some(result) = agent.result.as_ref() {
                if !result.trim().is_empty() {
                    status_text = format!("{} (result)", status_text);
                }
            }
            if let Some(error) = agent.error.as_ref() {
                if !error.trim().is_empty() {
                    status_text = format!("{} (error)", status_text);
                }
            }

            rows.push((agent.name.clone(), status_text.clone()));
            tracker.agent_ids.insert(agent.id.clone());
            if let Some(batch_id) = agent.batch_id.as_ref() {
                tracker.batch_id.get_or_insert(batch_id.clone());
            }

            let phase = classify_status(&agent.status, agent.result.is_some(), agent.error.is_some());
            status_collect.observe(phase);

            if let Some(result) = agent.result.as_ref() {
                latest_lines = Some(lines_from(result));
            } else if let Some(error) = agent.error.as_ref() {
                latest_lines = Some(lines_from(error));
            }
            tracker.set_agent_name(Some(agent.name.clone()), false);
        }

        if let Some(lines) = latest_lines {
            if !lines.is_empty() {
                tracker.cell.set_latest_result(lines);
            }
        }

        tracker.cell.set_status_rows(rows.clone());
        status_collect.apply(&mut tracker.cell);

        if !rows.is_empty() {
            let summary = rows
                .iter()
                .map(|(name, status)| format!("{}: {}", name, status))
                .collect::<Vec<_>>()
                .join("; ");
            tracker
                .cell
                .record_action(format!("Status update — {}", summary));
        }

        replace_agent_cell(chat, &mut tracker);
        current_key = update_mappings(
            chat,
            current_key,
            None,
            None,
            None,
            "agent_status",
            &mut tracker,
        );
        chat.tools_state.agent_last_key = Some(current_key.clone());
        chat.tools_state.agent_runs.insert(current_key, tracker);
    }
}

fn ensure_agent_cell(chat: &mut ChatWidget<'_>, tracker: &mut AgentRunTracker) -> Option<usize> {
    if let Some(id) = tracker.history_id {
        if let Some(idx) = chat.cell_index_for_history_id(id) {
            tracker.cell_index = Some(idx);
            return Some(idx);
        }
    }

    if let Some(idx) = tracker
        .cell_index
        .and_then(|idx| if idx < chat.history_cells.len() { Some(idx) } else { None })
    {
        return Some(idx);
    }

    let idx = chat.history_insert_with_key_global(Box::new(tracker.cell.clone()), tracker.order_key);
    tracker.cell_index = Some(idx);
    tracker.history_id = chat.history_cell_ids.get(idx).and_then(|slot| *slot);
    Some(idx)
}

fn replace_agent_cell(chat: &mut ChatWidget<'_>, tracker: &mut AgentRunTracker) {
    if let Some(idx) = ensure_agent_cell(chat, tracker) {
        chat.history_replace_at(idx, Box::new(tracker.cell.clone()));
        if let Some(id) = chat.history_cell_ids.get(idx).and_then(|slot| *slot) {
            tracker.history_id = Some(id);
        }
    }
}

fn order_key_and_ordinal(chat: &mut ChatWidget<'_>, order: Option<&OrderMeta>) -> (OrderKey, Option<u64>) {
    match order {
        Some(meta) => (chat.provider_order_key_from_order_meta(meta), Some(meta.request_ordinal)),
        None => (chat.next_internal_key(), None),
    }
}

fn agent_key(
    order: Option<&OrderMeta>,
    call_id: &str,
    tool_name: &str,
    metadata: &InvocationMetadata,
) -> String {
    if let Some(batch) = metadata.batch_id.as_ref() {
        return format!("batch:{}", batch);
    }
    if is_primary_run_tool(tool_name) {
        if let Some(meta) = order {
            return format!("req:{}:agent-run", meta.request_ordinal);
        }
    }
    if let Some(first) = metadata.agent_ids.first() {
        return format!("agent:{}", first);
    }
    if let Some(meta) = order {
        return format!("req:{}:{}", meta.request_ordinal, call_id);
    }
    format!("call:{}", call_id)
}

fn lookup_key(chat: &mut ChatWidget<'_>, order: Option<&OrderMeta>, call_id: &str) -> Option<String> {
    chat
        .tools_state
        .agent_run_by_call
        .remove(call_id)
        .or_else(|| order.and_then(|meta| chat.tools_state.agent_run_by_order.get(&meta.request_ordinal).cloned()))
        .or_else(|| chat.tools_state.agent_last_key.clone())
}

fn update_mappings(
    chat: &mut ChatWidget<'_>,
    mut key: String,
    order: Option<&OrderMeta>,
    call_id: Option<&str>,
    ordinal: Option<u64>,
    tool_name: &str,
    tracker: &mut AgentRunTracker,
) -> String {
    let original_key = key.clone();

    if let Some(batch) = tracker.batch_id.as_ref() {
        let batch_key = format!("batch:{}", batch);
        if batch_key != key {
            key = batch_key;
        }
    }

    if is_primary_run_tool(tool_name) {
        if let Some(ord) = ordinal {
            let ord_key = format!("req:{}:agent-run", ord);
            if ord_key != key {
                key = ord_key;
            }
        }
    }

    if let Some(cid) = call_id {
        tracker.call_ids.insert(cid.to_string());
        chat
            .tools_state
            .agent_run_by_call
            .insert(cid.to_string(), key.clone());
    }
    if let Some(meta) = order {
        chat
            .tools_state
            .agent_run_by_order
            .insert(meta.request_ordinal, key.clone());
    }
    if let Some(batch) = tracker.batch_id.as_ref() {
        chat
            .tools_state
            .agent_run_by_batch
            .insert(batch.clone(), key.clone());
    }
    for agent_id in &tracker.agent_ids {
        chat
            .tools_state
            .agent_run_by_agent
            .insert(agent_id.clone(), key.clone());
    }

    if key != original_key {
        for cid in &tracker.call_ids {
            chat
                .tools_state
                .agent_run_by_call
                .insert(cid.clone(), key.clone());
        }
    }

    key
}

#[derive(Default)]
struct StatusSummary {
    any_failed: bool,
    any_cancelled: bool,
    any_running: bool,
    any_pending: bool,
    total: usize,
    completed: usize,
}

impl StatusSummary {
    fn observe(&mut self, phase: AgentPhase) {
        self.total += 1;
        match phase {
            AgentPhase::Failed => {
                self.any_failed = true;
            }
            AgentPhase::Cancelled => {
                self.any_cancelled = true;
            }
            AgentPhase::Running => {
                self.any_running = true;
            }
            AgentPhase::Pending => {
                self.any_pending = true;
            }
            AgentPhase::Completed => {
                self.completed += 1;
            }
        }
    }

    fn apply(self, cell: &mut AgentRunCell) {
        if self.any_failed {
            cell.mark_failed();
            return;
        }
        if self.any_cancelled {
            cell.set_status_label("Cancelled");
            cell.mark_completed();
            return;
        }
        if self.total > 0 && self.completed == self.total {
            cell.set_status_label("Completed");
            cell.mark_completed();
            return;
        }
        if self.any_running {
            cell.set_status_label("Running");
            return;
        }
        if self.any_pending {
            cell.set_status_label("Pending");
            return;
        }
        cell.set_status_label("Running");
    }
}

#[derive(Clone, Copy)]
enum AgentPhase {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

fn classify_status(status: &str, has_result: bool, has_error: bool) -> AgentPhase {
    if has_error {
        return AgentPhase::Failed;
    }
    if has_result {
        return AgentPhase::Completed;
    }
    let token = status
        .split_whitespace()
        .next()
        .unwrap_or(status)
        .to_ascii_lowercase();
    match token.as_str() {
        "failed" | "error" | "errored" => AgentPhase::Failed,
        "cancelled" | "canceled" => AgentPhase::Cancelled,
        "completed" | "complete" | "done" | "success" | "succeeded" => AgentPhase::Completed,
        "pending" | "queued" | "waiting" | "starting" => AgentPhase::Pending,
        _ => AgentPhase::Running,
    }
}

fn lines_from(input: &str) -> Vec<String> {
    input.lines().map(|line| line.to_string()).collect()
}

fn dedup(mut values: Vec<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    values.retain(|value| seen.insert(value.clone()));
    values
}
