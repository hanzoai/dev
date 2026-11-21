//! `modelSwitch.ts`: the model or effort change a new run brings.

use serde::Deserialize;
use serde_json::value::RawValue;
use std::collections::BTreeSet;

#[derive(Deserialize)]
struct Selection {
    #[serde(rename = "instanceId")]
    instance_id: String,
    model: String,
    #[serde(default)]
    options: Vec<ModelOption>,
}

#[derive(Deserialize)]
struct ModelOption {
    id: String,
    value: serde_json::Value,
}

impl Selection {
    /// Options as (id, value spelled as a string), sorted: equality only needs one consistent order.
    fn canonical(&self) -> Vec<(&str, String)> {
        let mut options: Vec<(&str, String)> = self
            .options
            .iter()
            .map(|o| {
                let value = match &o.value {
                    serde_json::Value::String(s) => s.clone(),
                    other => other.to_string(),
                };
                (o.id.as_str(), value)
            })
            .collect();
        options.sort();
        options
    }

    fn equals(&self, other: &Self) -> bool {
        self.instance_id == other.instance_id
            && self.model == other.model
            && self.canonical() == other.canonical()
    }
}

#[derive(Deserialize)]
struct Run {
    ordinal: i64,
    status: String,
    #[serde(rename = "completedAt")]
    completed_at: Option<f64>,
    handoff: bool,
    #[serde(rename = "userMessageId")]
    user_message_id: String,
    #[serde(rename = "modelSelection")]
    model_selection: Box<RawValue>,
}

#[derive(Deserialize)]
struct Item {
    #[serde(rename = "messageId")]
    message_id: String,
    #[serde(rename = "inputIntent")]
    input_intent: String,
    status: String,
    text: String,
    attachments: usize,
}

#[derive(Deserialize)]
struct Input {
    runs: Vec<Run>,
    items: Vec<Item>,
    to: Box<RawValue>,
    baseline: Option<Box<RawValue>>,
}

/// JS `String.prototype.trim` whitespace, which is not Rust's.
fn js_trim(s: &str) -> &str {
    s.trim_matches(|c: char| {
        matches!(c, '\u{FEFF}')
            || (c.is_whitespace() && c != '\u{85}')
    })
}

fn is_shown(item: &Item) -> bool {
    let compaction = js_trim(&item.text).to_lowercase() == "/compact" && item.attachments == 0;
    !(item.input_intent == "queued_turn" && item.status == "pending")
        && !item.message_id.starts_with("async-answer:")
        && !compaction
}

fn is_candidate(run: &Run) -> bool {
    run.status != "rolled_back" && run.status != "queued"
}

/// Held queues can execute out of ordinal order; unfinished runs count as latest.
fn ran_after(run: &Run, other: &Run) -> bool {
    let end = |r: &Run| r.completed_at.unwrap_or(f64::INFINITY);
    if end(run) == end(other) {
        run.ordinal > other.ordinal
    } else {
        end(run) > end(other)
    }
}

pub fn apply(input: &RawValue) -> Result<String, serde_json::Error> {
    let input: Input = serde_json::from_str(input.get())?;
    let shown: BTreeSet<&str> = input
        .items
        .iter()
        .filter(|i| is_shown(i))
        .map(|i| i.message_id.as_str())
        .collect();
    let mut previous: Option<&Run> = None;
    for run in &input.runs {
        let anchors = is_candidate(run) && (shown.contains(run.user_message_id.as_str()) || run.handoff);
        if !anchors {
            continue;
        }
        if previous.is_none_or(|p| ran_after(run, p)) {
            previous = Some(run);
        }
    }
    let from: Option<&RawValue> = match (previous, &input.baseline) {
        (Some(p), _) => Some(&p.model_selection),
        (None, Some(b)) => Some(b),
        (None, None) => None,
    };
    let to: Selection = serde_json::from_str(input.to.get())?;
    Ok(match from {
        Some(raw) if !serde_json::from_str::<Selection>(raw.get())?.equals(&to) => {
            format!(r#"{{"modelSwitch":{{"from":{},"to":{}}}}}"#, raw.get(), input.to.get())
        }
        _ => "{}".to_owned(),
    })
}
