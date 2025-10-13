use super::{tool_cards, ChatWidget, OrderKey};
use super::tool_cards::ToolCardSlot;
use crate::history_cell::{
    AgentDetail,
    AgentRunCell,
    AgentStatusKind,
    AgentStatusPreview,
    PlainHistoryCell,
    StepProgress,
    plain_message_state_from_paragraphs,
};
use crate::history::state::{PlainMessageKind, PlainMessageRole};
use code_core::protocol::{AgentStatusUpdateEvent, OrderMeta};
use serde_json::Value;
use std::collections::{HashMap, HashSet};
use std::time::{Duration, Instant};

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
    let action = metadata.action.as_deref().unwrap_or_else(|| match tool_name {
        "agent" | "agent_run" => "create",
        "agent_wait" => "wait",
        "agent_result" => "result",
        "agent_cancel" => "cancel",
        "agent_check" => "status",
        "agent_list" => "list",
        other => other,
    });

    match action {
        "create" => Some(format!("Started agent run for {}", label)),
        "wait" => metadata
            .batch_id
            .as_ref()
            .map(|batch| format!("Waiting for agents in batch {}", batch)),
        "result" => Some(format!("Requested results for {}", label)),
        "cancel" => Some(format!("Cancelling agent batch for {}", label)),
        "status" => Some(format!("Checking agent status for {}", label)),
        "list" => Some("Listing available agents".to_string()),
        _ => None,
    }
}

fn end_action_for(
    tool_name: &str,
    metadata: &InvocationMetadata,
    duration: Duration,
    success: bool,
    message: Option<&str>,
) -> Option<String> {
    let elapsed = format_elapsed_short(duration);
    let action = metadata.action.as_deref().unwrap_or_else(|| match tool_name {
        "agent" | "agent_run" => "create",
        "agent_wait" => "wait",
        "agent_result" => "result",
        "agent_cancel" => "cancel",
        "agent_check" => "status",
        "agent_list" => "list",
        other => other,
    });

    match action {
        "create" => {
            if success {
                Some(format!("Agent run completed in {}", elapsed))
            } else {
                let detail = message.unwrap_or("unknown error");
                Some(format!("Agent run failed in {} — {}", elapsed, detail))
            }
        }
        "wait" => {
            if success {
                Some(format!("Finished waiting in {}", elapsed))
            } else {
                let detail = message.unwrap_or("wait failed");
                Some(format!("Wait failed in {} — {}", elapsed, detail))
            }
        }
        "result" => {
            if success {
                Some(format!("Fetched agent results in {}", elapsed))
            } else {
                let detail = message.unwrap_or("result error");
                Some(format!("Result fetch failed in {} — {}", elapsed, detail))
            }
        }
        "cancel" => Some(format!("Cancel request completed in {}", elapsed)),
        "status" => Some(format!("Status check finished in {}", elapsed)),
        "list" => Some("Listed agents".to_string()),
        _ => None,
    }
}

pub(super) struct AgentRunTracker {
    pub slot: ToolCardSlot,
    pub cell: AgentRunCell,
    pub batch_id: Option<String>,
    pub batch_label: Option<String>,
    agent_ids: HashSet<String>,
    task: Option<String>,
    context: Option<String>,
    has_custom_name: bool,
    call_ids: HashSet<String>,
    agent_started_at: HashMap<String, Instant>,
    agent_elapsed: HashMap<String, Duration>,
    agent_token_counts: HashMap<String, u64>,
    anchor_inserted: bool,
}

impl AgentRunTracker {
    pub fn new(order_key: OrderKey) -> Self {
        Self {
            slot: ToolCardSlot::new(order_key),
            cell: AgentRunCell::new("(pending)".to_string()),
            batch_id: None,
            batch_label: None,
            agent_ids: HashSet::new(),
            task: None,
            context: None,
            has_custom_name: false,
            call_ids: HashSet::new(),
            agent_started_at: HashMap::new(),
            agent_elapsed: HashMap::new(),
            agent_token_counts: HashMap::new(),
            anchor_inserted: false,
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

    fn set_context(&mut self, context: Option<String>) {
        if let Some(value) = context {
            self.context = Some(value);
        }
        self.cell.set_context(self.context.clone());
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

fn insert_agent_anchor(chat: &mut ChatWidget<'_>, order_key: OrderKey, tracker: &AgentRunTracker) {
    let message = agent_anchor_text(tracker);
    let state = plain_message_state_from_paragraphs(
        PlainMessageKind::Plain,
        PlainMessageRole::System,
        [message],
    );
    let cell = PlainHistoryCell::from_state(state);
    let _ = chat.history_insert_with_key_global(Box::new(cell), order_key);
}

fn agent_anchor_text(tracker: &AgentRunTracker) -> String {
    if let Some(label) = tracker.cell.summary_label() {
        if !label.is_empty() {
            return format!(
                "Agent batch \"{}\" started here; latest status is shown below.",
                label
            );
        }
    }
    if let Some(batch) = tracker.batch_id.as_ref() {
        if !batch.is_empty() {
            return format!(
                "Agent batch {} started here; latest status is shown below.",
                batch
            );
        }
    }
    "Agent activity started here; latest status is shown below.".to_string()
}

#[derive(Default)]
struct InvocationMetadata {
    batch_id: Option<String>,
    agent_ids: Vec<String>,
    task: Option<String>,
    plan: Vec<String>,
    label: Option<String>,
    action: Option<String>,
    context: Option<String>,
}

impl InvocationMetadata {
    fn from(tool_name: &str, params: Option<&Value>) -> Self {
        let mut meta = InvocationMetadata::default();
        if let Some(Value::Object(map)) = params {
            if let Some(action) = map.get("action").and_then(|v| v.as_str()) {
                meta.action = Some(action.to_string());
            }
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
            if let Some(context) = map.get("context").and_then(|v| v.as_str()) {
                meta.context = Some(context.to_string());
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
            if let Some(create) = map.get("create").and_then(|v| v.as_object()) {
                if meta.task.is_none() {
                    if let Some(task) = create.get("task").and_then(|v| v.as_str()) {
                        meta.task = Some(task.to_string());
                    }
                }
                if let Some(name) = create.get("name").and_then(|v| v.as_str()) {
                    meta.label = Some(name.to_string());
                }
                if meta.context.is_none() {
                    if let Some(context) = create.get("context").and_then(|v| v.as_str()) {
                        meta.context = Some(context.to_string());
                    }
                }
                if meta.plan.is_empty() {
                    if let Some(plan) = create.get("plan").and_then(|v| v.as_array()) {
                        meta.plan = plan
                            .iter()
                            .filter_map(|step| step.as_str().map(|s| s.to_string()))
                            .collect();
                    }
                }
                if let Some(models) = create.get("models").and_then(|v| v.as_array()) {
                    for model in models {
                        if let Some(name) = model.as_str() {
                            meta.agent_ids.push(name.to_string());
                        }
                    }
                }
            }
            if let Some(wait) = map.get("wait").and_then(|v| v.as_object()) {
                if meta.batch_id.is_none() {
                    if let Some(batch) = wait.get("batch_id").and_then(|v| v.as_str()) {
                        meta.batch_id = Some(batch.to_string());
                    }
                }
                if let Some(agent_id) = wait.get("agent_id").and_then(|v| v.as_str()) {
                    meta.agent_ids.push(agent_id.to_string());
                }
            }
            if let Some(status) = map.get("status").and_then(|v| v.as_object()) {
                if let Some(agent_id) = status.get("agent_id").and_then(|v| v.as_str()) {
                    meta.agent_ids.push(agent_id.to_string());
                }
            }
            if let Some(result) = map.get("result").and_then(|v| v.as_object()) {
                if let Some(agent_id) = result.get("agent_id").and_then(|v| v.as_str()) {
                    meta.agent_ids.push(agent_id.to_string());
                }
            }
            if let Some(cancel) = map.get("cancel").and_then(|v| v.as_object()) {
                if meta.batch_id.is_none() {
                    if let Some(batch) = cancel.get("batch_id").and_then(|v| v.as_str()) {
                        meta.batch_id = Some(batch.to_string());
                    }
                }
                if let Some(agent_id) = cancel.get("agent_id").and_then(|v| v.as_str()) {
                    meta.agent_ids.push(agent_id.to_string());
                }
            }
            if let Some(list) = map.get("list").and_then(|v| v.as_object()) {
                if meta.batch_id.is_none() {
                    if let Some(batch) = list.get("batch_id").and_then(|v| v.as_str()) {
                        meta.batch_id = Some(batch.to_string());
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

    let mut reuse_key: Option<String> = None;

    if let Some(ord) = ordinal {
        if let Some(existing) = chat
            .tools_state
            .agent_run_by_order
            .get(&ord)
            .cloned()
        {
            reuse_key = Some(existing);
        }
    }

    if reuse_key.is_none() {
        if let Some(batch) = metadata.batch_id.as_ref() {
            if let Some(existing) = chat
                .tools_state
                .agent_run_by_batch
                .get(batch)
                .cloned()
            {
                reuse_key = Some(existing);
            }
        }
    }

    if reuse_key.is_none() {
        for agent_id in &metadata.agent_ids {
            if let Some(existing) = chat
                .tools_state
                .agent_run_by_agent
                .get(agent_id)
                .cloned()
            {
                reuse_key = Some(existing);
                break;
            }
        }
    }

    if reuse_key.is_none()
        && metadata.batch_id.is_none()
        && metadata.agent_ids.is_empty()
        && ordinal.is_none()
    {
        reuse_key = chat.tools_state.agent_last_key.clone();
    }

    let mut key = reuse_key.unwrap_or_else(|| agent_key(order, call_id, tool_name, &metadata));

    let mut tracker = match chat.tools_state.agent_runs.remove(&key) {
        Some(existing) => existing,
        None => {
            key = agent_key(order, call_id, tool_name, &metadata);
            chat
                .tools_state
                .agent_runs
                .remove(&key)
                .unwrap_or_else(|| AgentRunTracker::new(order_key))
        }
    };
    tracker.slot.set_order_key(order_key);

    if let Some(batch) = metadata.batch_id.clone() {
        tracker.batch_id.get_or_insert(batch);
    }

    let label_opt = metadata.label.as_ref().map(|value| value.to_string());
    if let Some(label) = label_opt.as_ref() {
        tracker.batch_label = Some(label.clone());
    }

    tracker.merge_agent_ids(metadata.agent_ids.clone());

    tracker.set_agent_name(label_opt, true);

    let header_label = tracker
        .batch_label
        .as_ref()
        .map(|value| value.clone())
        .or_else(|| tracker.batch_id.clone());
    tracker.cell.set_batch_label(header_label);
    if !metadata.plan.is_empty() {
        tracker.cell.set_plan(metadata.plan.clone());
    }
    tracker.set_context(metadata.context.clone());
    tracker.set_task(metadata.task.clone());

    if tracker.slot.has_order_change() && !tracker.anchor_inserted {
        if let Some(previous) = tracker.slot.last_inserted_order() {
            insert_agent_anchor(chat, previous, &tracker);
            tracker.anchor_inserted = true;
        }
    }

    if let Some(action) = begin_action_for(tool_name, &metadata) {
        tracker.cell.record_action(action);
    }

    key = update_mappings(
        chat,
        key,
        order,
        Some(call_id),
        ordinal,
        tool_name,
        &mut tracker,
    );
    tool_cards::assign_tool_card_key(&mut tracker.slot, &mut tracker.cell, Some(key.clone()));
    let header_label = tracker
        .batch_label
        .as_ref()
        .map(|value| value.clone())
        .or_else(|| tracker.batch_id.clone());
    tracker.cell.set_batch_label(header_label);
    tool_cards::replace_tool_card::<AgentRunCell>(chat, &mut tracker.slot, &tracker.cell);
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
    let order_key = order
        .map(|meta| chat.provider_order_key_from_order_meta(meta))
        .unwrap_or_else(|| chat.next_internal_key());
    let ordinal = order.map(|m| m.request_ordinal);
    let mut key = lookup_key(chat, order, call_id)
        .unwrap_or_else(|| agent_key(order, call_id, tool_name, &metadata));

    let mut tracker = match chat.tools_state.agent_runs.remove(&key) {
        Some(existing) => existing,
        None => return false,
    };

    tracker.slot.set_order_key(order_key);

    if let Some(batch) = metadata.batch_id.clone() {
        tracker.batch_id.get_or_insert(batch);
    }

    let label_opt = metadata.label.as_ref().map(|value| value.to_string());
    if let Some(label) = label_opt.as_ref() {
        tracker.batch_label = Some(label.clone());
    }

    tracker.merge_agent_ids(metadata.agent_ids.clone());

    tracker.set_agent_name(label_opt, true);
    if !metadata.plan.is_empty() {
        tracker.cell.set_plan(metadata.plan.clone());
    }
    tracker.set_context(metadata.context.clone());
    tracker.set_task(metadata.task.clone());

    if tracker.slot.has_order_change() && !tracker.anchor_inserted {
        if let Some(previous) = tracker.slot.last_inserted_order() {
            insert_agent_anchor(chat, previous, &tracker);
            tracker.anchor_inserted = true;
        }
    }

    tracker.cell.set_duration(Some(duration));
    match result {
        Ok(text) => {
            let lines = lines_from(text);
            if !lines.is_empty() {
                tracker.cell.set_latest_result(lines);
            }
            tracker.cell.set_status_label("Completed");
            tracker.cell.mark_completed();
            if let Some(action) = end_action_for(tool_name, &metadata, duration, true, Some(text.as_str())) {
                tracker.cell.record_action(action);
            }
        }
        Err(err) => {
            tracker.cell.set_latest_result(vec![err.clone()]);
            tracker.cell.mark_failed();
            if let Some(action) = end_action_for(tool_name, &metadata, duration, false, Some(err.as_str())) {
                tracker.cell.record_action(action);
            }
        }
    }

    key = update_mappings(
        chat,
        key,
        order,
        Some(call_id),
        ordinal,
        tool_name,
        &mut tracker,
    );
    tool_cards::assign_tool_card_key(&mut tracker.slot, &mut tracker.cell, Some(key.clone()));
    tool_cards::replace_tool_card::<AgentRunCell>(chat, &mut tracker.slot, &tracker.cell);
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

        let order_key = chat.next_internal_key();
        tracker.slot.set_order_key(order_key);

        if let Some(context) = event.context.clone() {
            tracker.set_context(Some(context));
        } else {
            tracker.set_context(tracker.context.clone());
        }

        if let Some(task) = event.task.clone() {
            tracker.set_task(Some(task));
        } else {
            tracker.set_task(tracker.task.clone());
        }

        let mut previews: Vec<AgentStatusPreview> = Vec::new();
        let mut status_collect = StatusSummary::default();
        let mut summary_lines: Option<Vec<String>> = None;

        for agent in &event.agents {
            tracker.agent_ids.insert(agent.id.clone());
            if let Some(batch_id) = agent.batch_id.as_ref() {
                tracker.batch_id.get_or_insert(batch_id.clone());
            }

            let phase = classify_status(&agent.status, agent.result.is_some(), agent.error.is_some());

            let mut details: Vec<AgentDetail> = Vec::new();

            if let Some(result) = agent.result.as_ref() {
                let mut lines = lines_from(result);
                if lines.is_empty() {
                    lines.push(result.clone());
                }
                let mut collected: Vec<String> = Vec::new();
                for line in lines {
                    if !line.trim().is_empty() {
                        collected.push(line.clone());
                        details.push(AgentDetail::Result(line));
                    }
                }
                if !collected.is_empty() {
                    summary_lines = Some(collected);
                }
            }

            if details.is_empty() {
                if let Some(error) = agent.error.as_ref() {
                    let mut lines = lines_from(error);
                    if lines.is_empty() {
                        lines.push(error.clone());
                    }
                    let mut collected: Vec<String> = Vec::new();
                    for line in lines {
                        if !line.trim().is_empty() {
                            collected.push(line.clone());
                            details.push(AgentDetail::Error(line));
                        }
                    }
                    if !collected.is_empty() {
                        summary_lines = Some(collected);
                    }
                }
            }

            let step_progress = agent
                .last_progress
                .as_deref()
                .and_then(parse_progress);

            if details.is_empty() {
                if let Some(progress) = agent.last_progress.as_ref() {
                    let mut lines = lines_from(progress);
                    if lines.is_empty() {
                        lines.push(progress.clone());
                    }
                    for line in lines {
                        if !line.trim().is_empty() {
                            details.push(AgentDetail::Progress(line));
                        }
                    }
                }
            }

            if details.is_empty() {
                details.push(AgentDetail::Info(agent.status.clone()));
            }

            let last_update = details
                .last()
                .map(|detail| match detail {
                    AgentDetail::Progress(text)
                    | AgentDetail::Result(text)
                    | AgentDetail::Error(text)
                    | AgentDetail::Info(text) => text.clone(),
                });

            let elapsed = compute_agent_elapsed(
                &mut tracker,
                agent.id.as_str(),
                agent.elapsed_ms,
                phase,
            );
            let elapsed_updated_at = elapsed.map(|_| Instant::now());
            let token_count = resolve_agent_token_count(
                &mut tracker,
                agent.id.as_str(),
                agent.token_count,
                &details,
            );

            let preview = AgentStatusPreview {
                id: agent.id.clone(),
                name: agent.name.clone(),
                status: agent.status.clone(),
                model: agent.model.clone(),
                details,
                status_kind: phase_to_status_kind(phase),
                step_progress,
                elapsed,
                token_count,
                last_update,
                elapsed_updated_at,
            };
            previews.push(preview);

            status_collect.observe(phase);

            tracker.set_agent_name(Some(agent.name.clone()), false);
        }

        tracker.cell.set_agent_overview(previews.clone());
        let header_label = tracker
            .batch_label
            .as_ref()
            .map(|value| value.clone())
            .or_else(|| tracker.batch_id.clone());
        tracker.cell.set_batch_label(header_label);
        status_collect.apply(&mut tracker.cell);

        if let Some(lines) = summary_lines {
            tracker.cell.set_latest_result(lines);
        } else {
            tracker.cell.set_latest_result(Vec::new());
        }

        if tracker.slot.has_order_change() && !tracker.anchor_inserted {
            if let Some(previous) = tracker.slot.last_inserted_order() {
                insert_agent_anchor(chat, previous, &tracker);
                tracker.anchor_inserted = true;
            }
        }

        if !previews.is_empty() {
            let summary = previews
                .iter()
                .map(|preview| format!("{}: {}", preview.name, preview.status))
                .collect::<Vec<_>>()
                .join("; ");
            tracker
                .cell
                .record_action(format!("Status update — {}", summary));
        }

        current_key = update_mappings(
            chat,
            current_key,
            None,
            None,
            None,
            "agent_status",
            &mut tracker,
        );
        tool_cards::assign_tool_card_key(&mut tracker.slot, &mut tracker.cell, Some(current_key.clone()));
        tool_cards::replace_tool_card::<AgentRunCell>(chat, &mut tracker.slot, &tracker.cell);
        chat.tools_state.agent_last_key = Some(current_key.clone());
        chat.tools_state.agent_runs.insert(current_key, tracker);
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
        for stored in chat.tools_state.agent_run_by_order.values_mut() {
            if *stored == original_key {
                *stored = key.clone();
            }
        }
        if let Some(batch) = tracker.batch_id.as_ref() {
            if let Some(stored) = chat.tools_state.agent_run_by_batch.get_mut(batch) {
                if *stored == original_key {
                    *stored = key.clone();
                }
            }
        }
        for agent_id in &tracker.agent_ids {
            if let Some(stored) = chat.tools_state.agent_run_by_agent.get_mut(agent_id) {
                if *stored == original_key {
                    *stored = key.clone();
                }
            }
        }
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

fn phase_to_status_kind(phase: AgentPhase) -> AgentStatusKind {
    match phase {
        AgentPhase::Completed => AgentStatusKind::Completed,
        AgentPhase::Failed => AgentStatusKind::Failed,
        AgentPhase::Cancelled => AgentStatusKind::Cancelled,
        AgentPhase::Pending => AgentStatusKind::Pending,
        AgentPhase::Running => AgentStatusKind::Running,
    }
}

fn compute_agent_elapsed(
    tracker: &mut AgentRunTracker,
    agent_id: &str,
    elapsed_ms: Option<u64>,
    phase: AgentPhase,
) -> Option<Duration> {
    if let Some(ms) = elapsed_ms {
        let duration = Duration::from_millis(ms);
        tracker
            .agent_elapsed
            .insert(agent_id.to_string(), duration);
        if matches!(phase, AgentPhase::Completed | AgentPhase::Failed | AgentPhase::Cancelled) {
            tracker.agent_started_at.remove(agent_id);
        }
        return Some(duration);
    }

    let start_entry = tracker
        .agent_started_at
        .entry(agent_id.to_string())
        .or_insert_with(Instant::now);
    let duration = start_entry.elapsed();

    let entry = tracker
        .agent_elapsed
        .entry(agent_id.to_string())
        .or_insert(duration);
    if duration > *entry {
        *entry = duration;
    }

    if matches!(phase, AgentPhase::Completed | AgentPhase::Failed | AgentPhase::Cancelled) {
        tracker.agent_started_at.remove(agent_id);
    }

    tracker.agent_elapsed.get(agent_id).copied()
}

fn resolve_agent_token_count(
    tracker: &mut AgentRunTracker,
    agent_id: &str,
    explicit: Option<u64>,
    details: &[AgentDetail],
) -> Option<u64> {
    if let Some(value) = explicit {
        tracker
            .agent_token_counts
            .insert(agent_id.to_string(), value);
        return Some(value);
    }

    let inferred = details.iter().rev().find_map(|detail| match detail {
        AgentDetail::Progress(text)
        | AgentDetail::Result(text)
        | AgentDetail::Error(text)
        | AgentDetail::Info(text) => extract_token_count_from_text(text),
    });

    if let Some(value) = inferred {
        tracker
            .agent_token_counts
            .insert(agent_id.to_string(), value);
        return Some(value);
    }

    tracker.agent_token_counts.get(agent_id).copied()
}

fn extract_token_count_from_text(text: &str) -> Option<u64> {
    let lower = text.to_ascii_lowercase();
    if !lower.contains("token") && !lower.contains("tok") {
        return None;
    }

    let mut candidate = None;
    let mut fragment = String::new();

    for ch in text.chars() {
        if ch.is_ascii_digit()
            || matches!(ch, '.' | ',' | '_' | 'k' | 'K' | 'm' | 'M')
        {
            fragment.push(ch);
        } else {
            if let Some(value) = parse_token_fragment(&fragment) {
                candidate = Some(value);
            }
            fragment.clear();
        }
    }

    if let Some(value) = parse_token_fragment(&fragment) {
        candidate = Some(value);
    }

    candidate
}

fn parse_token_fragment(fragment: &str) -> Option<u64> {
    let trimmed = fragment.trim();
    if trimmed.is_empty() {
        return None;
    }

    let mut multiplier = 1f64;
    let mut base = trimmed;
    if let Some(last) = trimmed.chars().last() {
        match last {
            'k' | 'K' => {
                multiplier = 1_000f64;
                base = trimmed[..trimmed.len().saturating_sub(1)].trim();
            }
            'm' | 'M' => {
                multiplier = 1_000_000f64;
                base = trimmed[..trimmed.len().saturating_sub(1)].trim();
            }
            _ => {}
        }
    }

    let normalized = base.replace(',', "").replace('_', "");
    if normalized.is_empty() {
        return None;
    }

    if normalized.chars().all(|c| c.is_ascii_digit()) {
        let value: u64 = normalized.parse().ok()?;
        let computed = (value as f64 * multiplier).round();
        if computed > 0.0 {
            return Some(computed as u64);
        }
        return None;
    }

    if normalized.contains('.') {
        let value: f64 = normalized.parse().ok()?;
        let computed = (value * multiplier).round();
        if computed > 0.0 {
            return Some(computed as u64);
        }
        return None;
    }

    None
}

fn parse_progress(progress: &str) -> Option<StepProgress> {
    for token in progress.split_whitespace() {
        if let Some((done, total)) = token.split_once('/') {
            let completed = done.trim().parse::<u32>().ok()?;
            let total = total.trim().parse::<u32>().ok()?;
            if total > 0 {
                return Some(StepProgress { completed: completed.min(total), total });
            }
        }
    }
    None
}

fn lines_from(input: &str) -> Vec<String> {
    input.lines().map(|line| line.to_string()).collect()
}

fn dedup(mut values: Vec<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    values.retain(|value| seen.insert(value.clone()));
    values
}
