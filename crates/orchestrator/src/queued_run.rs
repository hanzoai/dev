//! `QueuedRunDecider.ts`: pure validation and reordering decisions for queued runs.

use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct QueuedRunEntry {
    pub id: String,
    pub queue_position: Option<i64>,
    pub is_automatic_completion: bool,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReorderInput {
    pub queued_runs: Vec<QueuedRunEntry>,
    pub run_id: String,
    pub before_run_id: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct QueuePositionUpdate {
    pub run_id: String,
    pub queue_position: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(untagged)]
pub enum ReorderDecision {
    Err {
        ok: bool,
        error: String,
    },
    Ok {
        ok: bool,
        updates: Vec<QueuePositionUpdate>,
    },
}

pub fn decide_reorder(input: &ReorderInput) -> ReorderDecision {
    let Some(moving) = input.queued_runs.iter().find(|run| run.id == input.run_id) else {
        return ReorderDecision::Err {
            ok: false,
            error: format!("Run {} is not queued.", input.run_id),
        };
    };

    if moving.is_automatic_completion {
        return ReorderDecision::Err {
            ok: false,
            error: "Automatic completion deliveries cannot be reordered.".to_string(),
        };
    }

    let automatic_runs: Vec<&QueuedRunEntry> = input
        .queued_runs
        .iter()
        .filter(|run| run.is_automatic_completion)
        .collect();

    if let Some(before_run_id) = input.before_run_id.as_deref()
        && automatic_runs.iter().any(|run| run.id == before_run_id)
    {
        return ReorderDecision::Err {
            ok: false,
            error: "Queued messages cannot be reordered ahead of automatic completion delivery."
                .to_string(),
        };
    }

    let without_moving: Vec<&QueuedRunEntry> = input
        .queued_runs
        .iter()
        .filter(|run| !run.is_automatic_completion && run.id != input.run_id)
        .collect();

    let before_index = match input.before_run_id.as_deref() {
        None => without_moving.len(),
        Some(before_run_id) => {
            match without_moving.iter().position(|run| run.id == before_run_id) {
                Some(idx) => idx,
                None => {
                    return ReorderDecision::Err {
                        ok: false,
                        error: format!("Queue target {before_run_id} is not queued."),
                    };
                }
            }
        }
    };

    let mut reordered: Vec<&QueuedRunEntry> = Vec::with_capacity(input.queued_runs.len());
    reordered.extend(automatic_runs);
    reordered.extend(&without_moving[..before_index]);
    reordered.push(moving);
    reordered.extend(&without_moving[before_index..]);

    let updates = reordered
        .into_iter()
        .enumerate()
        .filter_map(|(index, run)| {
            let queue_position = index + 1;
            if run.queue_position == Some(queue_position as i64) {
                None
            } else {
                Some(QueuePositionUpdate {
                    run_id: run.id.clone(),
                    queue_position,
                })
            }
        })
        .collect();

    ReorderDecision::Ok { ok: true, updates }
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct QueuedRunStatusEntry {
    pub id: String,
    pub status: String,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CancelInput {
    pub run_id: String,
    pub run: Option<QueuedRunStatusEntry>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EditInput {
    pub run_id: String,
    pub text: String,
    pub run: Option<QueuedRunStatusEntry>,
    pub has_message: bool,
    pub is_automatic_completion: bool,
    pub is_guest: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(untagged)]
pub enum ValidationDecision {
    Err { ok: bool, error: String },
    Ok { ok: bool },
}

pub fn decide_cancel(input: &CancelInput) -> ValidationDecision {
    match input.run.as_ref() {
        Some(run) if run.status == "queued" => ValidationDecision::Ok { ok: true },
        _ => ValidationDecision::Err {
            ok: false,
            error: format!("Run {} is not queued.", input.run_id),
        },
    }
}

pub fn decide_edit(input: &EditInput) -> ValidationDecision {
    if input.text.trim().is_empty() {
        return ValidationDecision::Err {
            ok: false,
            error: format!("Queued run {} cannot be edited to an empty message.", input.run_id),
        };
    }
    let Some(run) = input.run.as_ref().filter(|run| run.status == "queued") else {
        return ValidationDecision::Err {
            ok: false,
            error: format!("Run {} is not queued.", input.run_id),
        };
    };
    if !input.has_message {
        return ValidationDecision::Err {
            ok: false,
            error: format!("Queued run {} has no user message.", run.id),
        };
    }
    if input.is_automatic_completion {
        return ValidationDecision::Err {
            ok: false,
            error: "Automatic completion deliveries cannot be edited.".to_string(),
        };
    }
    if input.is_guest {
        return ValidationDecision::Err {
            ok: false,
            error: "A guest's message cannot be edited. Cancel it instead.".to_string(),
        };
    }
    ValidationDecision::Ok { ok: true }
}

pub fn apply(name: &str, input: &RawValue) -> Option<Result<String, serde_json::Error>> {
    match name {
        "queuedRunReorder" => Some(
            serde_json::from_str::<ReorderInput>(input.get())
                .and_then(|v| serde_json::to_string(&decide_reorder(&v))),
        ),
        "queuedRunCancel" => Some(
            serde_json::from_str::<CancelInput>(input.get())
                .and_then(|v| serde_json::to_string(&decide_cancel(&v))),
        ),
        "queuedRunEdit" => Some(
            serde_json::from_str::<EditInput>(input.get())
                .and_then(|v| serde_json::to_string(&decide_edit(&v))),
        ),
        _ => None,
    }
}
