//! `DispatchModeLimit.ts`: the mode limits a dispatch is rechecked against.

use serde::{Deserialize, Serialize};
use serde_json::value::RawValue;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum RuntimeMode {
    ApprovalRequired,
    AutoAcceptEdits,
    Auto,
    FullAccess,
}

/// `plan` ranks below `default`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum InteractionMode {
    Plan,
    Default,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DispatchModes {
    pub runtime_mode: RuntimeMode,
    pub interaction_mode: InteractionMode,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RuntimeModeCeiling {
    pub organization: String,
    pub max_runtime_mode: RuntimeMode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Exceeded {
    Runtime,
    Interaction,
}

pub fn exceeded_dispatch_mode_limit(limit: DispatchModes, modes: DispatchModes) -> Option<Exceeded> {
    if modes.runtime_mode > limit.runtime_mode {
        Some(Exceeded::Runtime)
    } else if modes.interaction_mode > limit.interaction_mode {
        Some(Exceeded::Interaction)
    } else {
        None
    }
}

/// A caller's limit, lowered to the ceiling when the caller may otherwise go above it.
pub fn narrow_to_ceiling(
    limit: Option<DispatchModes>,
    ceiling: Option<&RuntimeModeCeiling>,
) -> Option<DispatchModes> {
    match (limit, ceiling) {
        (Some(l), Some(c)) if l.runtime_mode > c.max_runtime_mode => Some(DispatchModes {
            runtime_mode: c.max_runtime_mode,
            ..l
        }),
        (limit, _) => limit,
    }
}

/// The fields of a domain event the ceiling check reads.
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Event {
    #[serde(rename = "type")]
    pub kind: String,
    pub thread_id: String,
    pub payload: Option<EventPayload>,
}

#[derive(Clone, Copy, Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EventPayload {
    pub runtime_mode: Option<RuntimeMode>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Refused {
    Mode,
    Turn,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CeilingRefusal {
    pub thread_id: String,
    pub runtime_mode: RuntimeMode,
    pub refused: Refused,
}

fn starts_run(kind: &str) -> bool {
    kind == "run.created" || kind == "run-attempt.created"
}

/// What a planned command would do above the ceiling, judged by its events. `committed` holds the
/// committed mode of each thread a planned run belongs to; later entries win, as in a Map.
pub fn runtime_mode_ceiling_refusal(
    max: RuntimeMode,
    events: &[Event],
    committed: &[(String, RuntimeMode)],
) -> Option<CeilingRefusal> {
    let mut planned: Vec<(String, RuntimeMode)> = Vec::new();
    let set = |planned: &mut Vec<(String, RuntimeMode)>, id: &str, mode: RuntimeMode| {
        match planned.iter_mut().find(|(t, _)| t == id) {
            Some(slot) => slot.1 = mode,
            None => planned.push((id.to_owned(), mode)),
        }
    };
    for (id, mode) in committed {
        set(&mut planned, id, *mode);
    }
    for event in events {
        if event.kind == "thread.created" || event.kind == "thread.runtime-mode-updated" {
            let Some(mode) = event.payload.and_then(|p| p.runtime_mode) else {
                continue;
            };
            set(&mut planned, &event.thread_id, mode);
            if mode > max {
                return Some(CeilingRefusal {
                    thread_id: event.thread_id.clone(),
                    runtime_mode: mode,
                    refused: Refused::Mode,
                });
            }
        } else if starts_run(&event.kind) {
            let mode = planned.iter().find(|(t, _)| *t == event.thread_id).map(|p| p.1);
            if let Some(mode) = mode.filter(|m| *m > max) {
                return Some(CeilingRefusal {
                    thread_id: event.thread_id.clone(),
                    runtime_mode: mode,
                    refused: Refused::Turn,
                });
            }
        }
    }
    None
}

/// Threads whose committed mode `runtime_mode_ceiling_refusal` needs: those a planned run
/// belongs to, in first-seen order.
pub fn threads_starting_work(events: &[Event]) -> Vec<String> {
    let mut created: Vec<&str> = Vec::new();
    let mut starting: Vec<String> = Vec::new();
    for event in events {
        if event.kind == "thread.created" {
            created.push(&event.thread_id);
        } else if starts_run(&event.kind)
            && !created.contains(&event.thread_id.as_str())
            && !starting.contains(&event.thread_id)
        {
            starting.push(event.thread_id.clone());
        }
    }
    starting
}

#[derive(Deserialize)]
struct ExceededIn {
    limit: DispatchModes,
    modes: DispatchModes,
}
#[derive(Deserialize)]
struct NarrowIn {
    limit: Option<DispatchModes>,
    ceiling: Option<RuntimeModeCeiling>,
}
#[derive(Deserialize)]
struct EventsIn {
    events: Vec<Event>,
}
#[derive(Deserialize)]
struct RefusalIn {
    max: RuntimeMode,
    events: Vec<Event>,
    #[serde(rename = "runtimeModes")]
    runtime_modes: Vec<(String, RuntimeMode)>,
}

pub fn apply(name: &str, input: &RawValue) -> Option<Result<String, serde_json::Error>> {
    let json = input.get();
    let out = match name {
        "exceeded" => serde_json::from_str::<ExceededIn>(json)
            .and_then(|i| serde_json::to_string(&exceeded_dispatch_mode_limit(i.limit, i.modes))),
        "narrow" => serde_json::from_str::<NarrowIn>(json)
            .and_then(|i| serde_json::to_string(&narrow_to_ceiling(i.limit, i.ceiling.as_ref()))),
        "threadsStartingWork" => serde_json::from_str::<EventsIn>(json)
            .and_then(|i| serde_json::to_string(&threads_starting_work(&i.events))),
        "ceilingRefusal" => serde_json::from_str::<RefusalIn>(json).and_then(|i| {
            serde_json::to_string(&runtime_mode_ceiling_refusal(i.max, &i.events, &i.runtime_modes))
        }),
        _ => return None,
    };
    Some(out)
}
