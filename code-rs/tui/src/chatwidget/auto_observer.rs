use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::Arc;

use anyhow::{anyhow, Context, Result};
use code_core::{
    error::CodexErr,
    ModelClient,
    OpenAiTool,
    Prompt,
    ResponseEvent,
    TextFormat,
};
use code_core::model_family::{derive_default_model_family, find_family_for_model};
use code_protocol::models::{ContentItem, ResponseItem};
use futures::StreamExt;
use serde::Deserialize;
use serde_json::{self, json, Value};
use tracing::{debug, error, warn};

use crate::app_event::AutoObserverTelemetry;
use crate::app_event;

type ObserverMode = app_event::ObserverMode;
use crate::chatwidget::AutoObserverStatus;
use crate::thread_spawner;

use super::auto_coordinator::{
    extract_first_json_object,
    make_message,
    AutoCoordinatorCommand,
    MODEL_SLUG,
};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub(super) struct CrossCheckTurnSnapshot {
    pub cli_prompt: Option<String>,
    pub cli_context: Option<String>,
    pub progress_summary: Option<String>,
}

#[derive(Debug)]
pub(super) struct AutoObserverHandle {
    pub tx: Sender<AutoObserverCommand>,
    cadence: u32,
}

impl AutoObserverHandle {
    pub fn cadence(&self) -> u32 {
        self.cadence.max(1)
    }
}

#[derive(Debug)]
pub(super) enum AutoObserverCommand {
    Bootstrap {
        goal_text: String,
        environment_details: String,
    },
    Trigger(ObserverTrigger),
    BeginCrossCheck {
        conversation: Vec<ResponseItem>,
        _from_index: usize,
        forced: bool,
        summary: Option<String>,
        focus: Option<String>,
    },
    Stop,
}

#[derive(Clone, Copy, Debug)]
enum ObserverToolPolicy {
    ReadOnly,
    Limited,
    FullAudit,
}

impl ObserverToolPolicy {
    fn for_mode(mode: ObserverMode) -> Self {
        match mode {
            ObserverMode::Bootstrap => ObserverToolPolicy::ReadOnly,
            ObserverMode::Cadence => ObserverToolPolicy::Limited,
            ObserverMode::CrossCheck => ObserverToolPolicy::FullAudit,
        }
    }

    fn tools(self) -> Vec<OpenAiTool> {
        match self {
            ObserverToolPolicy::ReadOnly => vec![OpenAiTool::WebSearch(Default::default())],
            ObserverToolPolicy::Limited => vec![OpenAiTool::WebSearch(Default::default())],
            ObserverToolPolicy::FullAudit => vec![
                OpenAiTool::LocalShell {},
                OpenAiTool::WebSearch(Default::default()),
            ],
        }
    }
}

pub(super) fn observer_tools_for_mode(mode: ObserverMode) -> Vec<OpenAiTool> {
    ObserverToolPolicy::for_mode(mode).tools()
}

#[derive(Debug, Clone)]
pub(super) enum ObserverReason {
    Cadence,
    CrossCheck {
        forced: bool,
        summary: Option<String>,
        focus: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub(super) struct ObserverTrigger {
    pub conversation: Vec<ResponseItem>,
    pub goal_text: String,
    pub environment_details: String,
    pub reason: ObserverReason,
    pub turn_snapshot: Option<CrossCheckTurnSnapshot>,
    pub tools: Vec<OpenAiTool>,
}

#[derive(Debug, Clone)]
pub(super) struct ObserverEvaluation {
    pub status: AutoObserverStatus,
    pub replace_message: Option<String>,
    pub additional_instructions: Option<String>,
    pub raw_output: String,
    pub parsed_response: Value,
}

#[derive(Debug, Clone)]
pub(super) struct ObserverOutcome {
    pub mode: ObserverMode,
    pub status: AutoObserverStatus,
    pub replace_message: Option<String>,
    pub additional_instructions: Option<String>,
    pub telemetry: AutoObserverTelemetry,
    pub reason: ObserverReason,
    pub conversation: Vec<ResponseItem>,
    pub turn_snapshot: Option<CrossCheckTurnSnapshot>,
    pub raw_output: Option<String>,
    pub parsed_response: Option<Value>,
}

const OBSERVER_SCHEMA_NAME: &str = "auto_coordinator_observer";

pub(super) fn start_auto_observer(
    client: Arc<ModelClient>,
    cadence: u32,
    coordinator_tx: Sender<AutoCoordinatorCommand>,
) -> Result<AutoObserverHandle> {
    let (tx, rx) = mpsc::channel();
    let thread_tx = tx.clone();

    if thread_spawner::spawn_lightweight("auto-observer", move || {
        if let Err(err) = run_observer_loop(client, rx, coordinator_tx) {
            error!("auto observer loop error: {err:#}");
        }
    })
    .is_none()
    {
        error!("auto observer spawn rejected: background thread limit reached");
        return Err(anyhow!("auto observer worker unavailable"));
    }

    Ok(AutoObserverHandle {
        tx: thread_tx,
        cadence,
    })
}

#[allow(dead_code)]
pub(super) fn run_observer_once(
    runtime: &tokio::runtime::Runtime,
    client: Arc<ModelClient>,
    trigger: ObserverTrigger,
) -> Result<ObserverEvaluation> {
    let (tx, _rx) = mpsc::channel();
    evaluate_observer(runtime, client, trigger, tx, ObserverMode::Cadence)
}

fn build_bootstrap_trigger(goal_text: &str, environment_details: &str) -> ObserverTrigger {
    let prompt = format!(
        "You are the QA observer. Before automation begins, inspect the repository and outline how to validate completion of the primary goal.\n\nPrimary goal:\n{goal}\n\nEnvironment:\n{environment}\n\nReturn a concise readiness summary and any immediate risks to monitor.",
        goal = goal_text.trim(),
        environment = environment_details.trim()
    );

    let mut conversation = Vec::new();
    conversation.push(make_message("developer", prompt));

    ObserverTrigger {
        conversation,
        goal_text: goal_text.to_string(),
        environment_details: environment_details.to_string(),
        reason: ObserverReason::Cadence,
        turn_snapshot: None,
        tools: observer_tools_for_mode(ObserverMode::Bootstrap),
    }
}

fn run_observer_loop(
    client: Arc<ModelClient>,
    rx: Receiver<AutoObserverCommand>,
    coordinator_tx: Sender<AutoCoordinatorCommand>,
) -> Result<()> {
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("creating runtime for auto observer")?;

    let mut telemetry = AutoObserverTelemetry {
        trigger_count: 0,
        last_status: AutoObserverStatus::Ok,
        last_intervention: None,
    };

    while let Ok(cmd) = rx.recv() {
        match cmd {
            AutoObserverCommand::Bootstrap {
                goal_text,
                environment_details,
            } => {
                let trigger = build_bootstrap_trigger(&goal_text, &environment_details);
                telemetry.trigger_count += 1;
                match evaluate_observer(
                    &runtime,
                    client.clone(),
                    trigger.clone(),
                    coordinator_tx.clone(),
                    ObserverMode::Bootstrap,
                ) {
                    Ok(eval) => {
                        let ObserverEvaluation {
                            status,
                            replace_message,
                            additional_instructions,
                            raw_output,
                            parsed_response,
                        } = eval;

                        telemetry.last_status = status;
                        telemetry.last_intervention = summarize_intervention(
                            replace_message.as_deref(),
                            additional_instructions.as_deref(),
                        );

                        let outcome = ObserverOutcome {
                            mode: ObserverMode::Bootstrap,
                            status,
                            replace_message,
                            additional_instructions,
                            telemetry: telemetry.clone(),
                            reason: ObserverReason::Cadence,
                            conversation: trigger.conversation.clone(),
                            turn_snapshot: None,
                            raw_output: Some(raw_output),
                            parsed_response: Some(parsed_response),
                        };

                        if coordinator_tx
                            .send(AutoCoordinatorCommand::ObserverResult(outcome))
                            .is_err()
                        {
                            break;
                        }
                    }
                    Err(err) => {
                        warn!("auto observer bootstrap error: {err:#}");
                        telemetry.last_status = AutoObserverStatus::Ok;
                        telemetry.last_intervention = Some(format!("error: {err}"));
                        let outcome = ObserverOutcome {
                            mode: ObserverMode::Bootstrap,
                            status: AutoObserverStatus::Ok,
                            replace_message: None,
                            additional_instructions: None,
                            telemetry: telemetry.clone(),
                            reason: ObserverReason::Cadence,
                            conversation: Vec::new(),
                            turn_snapshot: None,
                            raw_output: None,
                            parsed_response: None,
                        };
                        if coordinator_tx
                            .send(AutoCoordinatorCommand::ObserverResult(outcome))
                            .is_err()
                        {
                            break;
                        }
                    }
                }
            }
            AutoObserverCommand::Trigger(trigger) => {
                telemetry.trigger_count += 1;
                let mode = match trigger.reason {
                    ObserverReason::CrossCheck { .. } => ObserverMode::CrossCheck,
                    _ => ObserverMode::Cadence,
                };
                match evaluate_observer(
                    &runtime,
                    client.clone(),
                    trigger.clone(),
                    coordinator_tx.clone(),
                    mode,
                ) {
                    Ok(eval) => {
                        let ObserverEvaluation {
                            status,
                            replace_message,
                            additional_instructions,
                            raw_output,
                            parsed_response,
                        } = eval;

                        telemetry.last_status = status;
                        telemetry.last_intervention = summarize_intervention(
                            replace_message.as_deref(),
                            additional_instructions.as_deref(),
                        );

                        let outcome = ObserverOutcome {
                            mode,
                            status,
                            replace_message,
                            additional_instructions,
                            telemetry: telemetry.clone(),
                            reason: trigger.reason.clone(),
                            conversation: trigger.conversation.clone(),
                            turn_snapshot: trigger.turn_snapshot.clone(),
                            raw_output: Some(raw_output),
                            parsed_response: Some(parsed_response),
                        };

                        if coordinator_tx
                            .send(AutoCoordinatorCommand::ObserverResult(outcome))
                            .is_err()
                        {
                            break;
                        }
                    }
                    Err(err) => {
                        warn!("auto observer evaluation error: {err:#}");
                        telemetry.last_status = AutoObserverStatus::Ok;
                        telemetry.last_intervention = Some(format!("error: {err}"));
                        let outcome = ObserverOutcome {
                            mode,
                            status: AutoObserverStatus::Ok,
                            replace_message: None,
                            additional_instructions: None,
                            telemetry: telemetry.clone(),
                            reason: trigger.reason,
                            conversation: Vec::new(),
                            turn_snapshot: None,
                            raw_output: None,
                            parsed_response: None,
                        };
                        if coordinator_tx
                            .send(AutoCoordinatorCommand::ObserverResult(outcome))
                            .is_err()
                        {
                            break;
                        }
                    }
                }
            }
            AutoObserverCommand::BeginCrossCheck {
                conversation,
                _from_index: _,
                forced,
                summary,
                focus,
            } => {
                let summary_clone = summary.clone();
                let focus_clone = focus.clone();
                let trigger = ObserverTrigger {
                    conversation,
                    goal_text: String::new(),
                    environment_details: String::new(),
                    reason: ObserverReason::CrossCheck {
                        forced,
                        summary,
                        focus,
                    },
                    turn_snapshot: None,
                    tools: observer_tools_for_mode(ObserverMode::CrossCheck),
                };
                telemetry.trigger_count += 1;
                match evaluate_observer(
                    &runtime,
                    client.clone(),
                    trigger.clone(),
                    coordinator_tx.clone(),
                    ObserverMode::CrossCheck,
                ) {
                    Ok(eval) => {
                        let ObserverEvaluation {
                            status,
                            replace_message,
                            additional_instructions,
                            raw_output,
                            parsed_response,
                        } = eval;

                        telemetry.last_status = status;
                        telemetry.last_intervention = summarize_intervention(
                            replace_message.as_deref(),
                            additional_instructions.as_deref(),
                        );

                        let outcome = ObserverOutcome {
                            mode: ObserverMode::CrossCheck,
                            status,
                            replace_message,
                            additional_instructions,
                            telemetry: telemetry.clone(),
                            reason: trigger.reason,
                            conversation: trigger.conversation,
                            turn_snapshot: None,
                            raw_output: Some(raw_output),
                            parsed_response: Some(parsed_response),
                        };

                        if coordinator_tx
                            .send(AutoCoordinatorCommand::ObserverResult(outcome))
                            .is_err()
                        {
                            break;
                        }
                    }
                    Err(err) => {
                        warn!("auto observer cross-check error: {err:#}");
                        telemetry.last_status = AutoObserverStatus::Failing;
                        telemetry.last_intervention = Some(format!("error: {err}"));
                        let outcome = ObserverOutcome {
                            mode: ObserverMode::CrossCheck,
                            status: AutoObserverStatus::Failing,
                            replace_message: None,
                            additional_instructions: Some(format!(
                                "Cross-check failed due to observer error: {err}"
                            )),
                            telemetry: telemetry.clone(),
                            reason: ObserverReason::CrossCheck {
                                forced,
                                summary: summary_clone,
                                focus: focus_clone,
                            },
                            conversation: Vec::new(),
                            turn_snapshot: None,
                            raw_output: None,
                            parsed_response: None,
                        };
                        if coordinator_tx
                            .send(AutoCoordinatorCommand::ObserverResult(outcome))
                            .is_err()
                        {
                            break;
                        }
                    }
                }
            }
            AutoObserverCommand::Stop => break,
        }
    }

    Ok(())
}

fn evaluate_observer(
    runtime: &tokio::runtime::Runtime,
    client: Arc<ModelClient>,
    trigger: ObserverTrigger,
    coordinator_tx: Sender<AutoCoordinatorCommand>,
    mode: ObserverMode,
) -> Result<ObserverEvaluation> {
    let preferred_slug = match trigger.reason {
        ObserverReason::CrossCheck { .. } => "gpt-5",
        _ => MODEL_SLUG,
    };
    let mut prompt = build_observer_prompt(&trigger, preferred_slug);
    let log_tag = match mode {
        ObserverMode::Bootstrap => "auto/observer/bootstrap",
        ObserverMode::Cadence => "auto/observer/cadence",
        ObserverMode::CrossCheck => "auto/observer/cross_check",
    };
    prompt.set_log_tag(log_tag);
    match run_observer_prompt(runtime, client.clone(), prompt.clone(), coordinator_tx.clone(), mode)
    {
        Ok(result) => Ok(result),
        Err(err) => {
            let fallback_slug = client.default_model_slug().to_string();
            if should_retry_with_default_model(&err) && fallback_slug != preferred_slug {
                debug!(
                    preferred = %preferred_slug,
                    fallback = %fallback_slug,
                    "auto observer falling back to configured model after invalid model error"
                );
                let mut fallback_prompt = build_observer_prompt(&trigger, &fallback_slug);
                fallback_prompt.set_log_tag(log_tag);
                let original_error = err.to_string();
                return run_observer_prompt(
                    runtime,
                    client,
                    fallback_prompt,
                    coordinator_tx,
                    mode,
                )
                .map_err(|fallback_err| {
                    fallback_err.context(format!(
                        "observer fallback with model '{}' failed after original error: {}",
                        fallback_slug, original_error
                    ))
                });
            }
            Err(err)
        }
    }
}

fn run_observer_prompt(
    runtime: &tokio::runtime::Runtime,
    client: Arc<ModelClient>,
    prompt: Prompt,
    coordinator_tx: Sender<AutoCoordinatorCommand>,
    mode: ObserverMode,
) -> Result<ObserverEvaluation> {
    let raw = runtime.block_on(async {
        request_observer_response(client.clone(), &prompt, mode, coordinator_tx).await
    })?;

    let (response, value) = parse_observer_response(&raw)?;

    let status = match response.status.as_str() {
        "ok" => AutoObserverStatus::Ok,
        "failing" => AutoObserverStatus::Failing,
        other => return Err(anyhow!("unexpected status '{other}'")),
    };

    let trimmed_replace_message = response
        .replace_message
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());
    let trimmed_additional_instructions = response
        .additional_instructions
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());

    if matches!(status, AutoObserverStatus::Failing)
        && trimmed_replace_message.is_none()
        && trimmed_additional_instructions.is_none()
    {
        warn!("observer returned failing status without guidance");
    }

    let (replace_message, additional_instructions) = partition_observer_guidance(
        status,
        trimmed_replace_message,
        trimmed_additional_instructions,
    );

    debug!(
        "[Auto observer] status={status:?} replace={} instructions={}",
        replace_message.is_some(),
        additional_instructions.is_some()
    );

    Ok(ObserverEvaluation {
        status,
        replace_message,
        additional_instructions,
        raw_output: raw,
        parsed_response: value,
    })
}

fn partition_observer_guidance(
    status: AutoObserverStatus,
    replace_message: Option<String>,
    additional_instructions: Option<String>,
) -> (Option<String>, Option<String>) {
    if matches!(status, AutoObserverStatus::Failing) {
        (replace_message, additional_instructions)
    } else {
        (None, additional_instructions)
    }
}

fn build_observer_prompt(trigger: &ObserverTrigger, model_slug: &str) -> Prompt {
    let mut prompt = Prompt::default();
    prompt.store = true;

    let instructions = build_observer_instructions(&trigger.environment_details, trigger.reason.clone());
    prompt.input.push(make_message("developer", instructions));
    let goal = format!("Primary Goal\n{}", trigger.goal_text);
    prompt.input.push(make_message("developer", goal));
    prompt.input.extend(trigger.conversation.clone());
    prompt.set_tools(trigger.tools.clone());

    let schema = build_observer_schema();
    prompt.text_format = Some(TextFormat {
        r#type: "json_schema".to_string(),
        name: Some(OBSERVER_SCHEMA_NAME.to_string()),
        strict: Some(true),
        schema: Some(schema),
    });
    prompt.model_override = Some(model_slug.to_string());
    let family = find_family_for_model(model_slug)
        .unwrap_or_else(|| derive_default_model_family(model_slug));
    prompt.model_family_override = Some(family);
    prompt
}

fn should_retry_with_default_model(err: &anyhow::Error) -> bool {
    err.chain().any(|cause| {
        if let Some(code_err) = cause.downcast_ref::<CodexErr>() {
            if let CodexErr::UnexpectedStatus(err) = code_err {
                if !err.status.is_client_error() {
                    return false;
                }
                let body_lower = err.body.to_lowercase();
                return body_lower.contains("invalid model")
                    || body_lower.contains("unknown model")
                    || body_lower.contains("model_not_found")
                    || body_lower.contains("model does not exist");
            }
        }
        false
    })
}

async fn request_observer_response(
    client: Arc<ModelClient>,
    prompt: &Prompt,
    mode: ObserverMode,
    coordinator_tx: Sender<AutoCoordinatorCommand>,
) -> Result<String> {
    let mut stream = client.stream(prompt).await?;
    let mut out = String::new();
    let mut last_summary_index: Option<u32> = None;
    while let Some(ev) = stream.next().await {
        match ev {
            Ok(ResponseEvent::OutputTextDelta { delta, .. }) => out.push_str(&delta),
            Ok(ResponseEvent::OutputItemDone { item, .. }) => {
                if let ResponseItem::Message { content, .. } = item {
                    for c in content {
                        if let ContentItem::OutputText { text } = c {
                            out.push_str(&text);
                        }
                    }
                }
            }
            Ok(ResponseEvent::ReasoningSummaryDelta {
                delta,
                summary_index,
                ..
            }) => {
                last_summary_index = summary_index;
                let cleaned = strip_reasoning_prefix(&delta);
                if !cleaned.trim().is_empty() {
                    let _ = coordinator_tx.send(AutoCoordinatorCommand::ObserverThinking {
                        mode,
                        delta: cleaned.to_string(),
                        summary_index,
                    });
                }
            }
            Ok(ResponseEvent::ReasoningContentDelta { delta, .. }) => {
                let cleaned = strip_reasoning_prefix(&delta);
                if !cleaned.trim().is_empty() {
                    let _ = coordinator_tx.send(AutoCoordinatorCommand::ObserverThinking {
                        mode,
                        delta: cleaned.to_string(),
                        summary_index: last_summary_index,
                    });
                }
            }
            Ok(ResponseEvent::Completed { .. }) => break,
            Err(err) => return Err(anyhow!("observer stream error: {err}")),
            _ => {}
        }
    }
    Ok(out)
}

fn strip_reasoning_prefix(input: &str) -> &str {
    const PREFIXES: [&str; 2] = ["Observer:", "Coordinator:"];
    for prefix in PREFIXES {
        if let Some(head) = input.get(..prefix.len()) {
            if head.eq_ignore_ascii_case(prefix) {
                let rest = input.get(prefix.len()..).unwrap_or_default();
                return rest.strip_prefix(' ').unwrap_or(rest);
            }
        }
    }
    input
}

#[derive(Debug, Deserialize)]
struct ObserverResponse {
    status: String,
    #[serde(default)]
    replace_message: Option<String>,
    #[serde(default)]
    additional_instructions: Option<String>,
}

fn parse_observer_response(raw: &str) -> Result<(ObserverResponse, Value)> {
    let value: Value = match serde_json::from_str(raw) {
        Ok(v) => v,
        Err(_) => {
            let Some(blob) = extract_first_json_object(raw) else {
                return Err(anyhow!("observer response was not valid JSON"));
            };
            serde_json::from_str(&blob).context("parsing JSON from observer output")?
        }
    };
    let response: ObserverResponse = serde_json::from_value(value.clone())
        .context("decoding observer response")?;
    Ok((response, value))
}

fn build_observer_instructions(environment_details: &str, reason: ObserverReason) -> String {
    let body = match reason {
        ObserverReason::Cadence => "You are observing a AI Coordinator trying to drive a CLI towards a Primary Goal (shown below).\nPlease critically observe the conversation between the Coordinator and the CLI. Detect either of these issues;\n- Stuck in a loop\n- Not working towards primary goal\nGenerate a response based on this information;\n`status`: one of 'ok' or 'failing' - most of the time it will be 'ok', but use 'failing' when intervention absolutely is needed. When using 'failing' please provide one or both fields below to correct the problem;\n`replace_message`: A message to replace the last Coordinator message\n`additional_instructions`: Instructions to give to the Coordinator for future runs\n**Warning**\nYou almost always want to use `status`: \"ok\". You are a last resort. Avoid setting `status`: \"failing\" for minor issues as it will disrupt the progress of the task.".to_string(),
        ObserverReason::CrossCheck { forced, ref summary, ref focus } => {
            let mut lines = Vec::new();
            lines.push("You are a senior QA reviewer performing an end-to-end cross-check of the Auto Drive run.".to_string());
            lines.push("Confirm the Primary Goal is fully satisfied with explicit evidence. Assume nothing; require proof.".to_string());
            lines.push("Use the available tools (shell, browser inspection, web search, agents) to run commands, inspect artifacts, and gather concrete evidence.".to_string());

            if let Some(text) = summary {
                if !text.trim().is_empty() {
                    lines.push(format!("Cross-check emphasis: {}", text.trim()));
                }
            }
            if let Some(text) = focus {
                if !text.trim().is_empty() {
                    lines.push(format!("Probe these risks or flows: {}", text.trim()));
                }
            }

            if forced {
                lines.push("This cross-check gates completion. Default to `status`: 'failing' unless the goal is unquestionably complete.".to_string());
            } else {
                lines.push("Be strict. Favor `status`: 'failing' whenever evidence is missing, ambiguous, or work appears partial.".to_string());
            }

            lines.push("Response contract (JSON):".to_string());
            lines.push("- `status`: 'ok' only when the goal is entirely complete and verified; otherwise 'failing'.".to_string());
            lines.push("- `additional_instructions`: concise, actionable developer steps to close the gaps you found.".to_string());
            lines.push("- `replace_message`: only when the last coordinator message must be replaced immediately.".to_string());

            lines.join("\n")
        }
    };
    format!("{body}\nEnvironment:\n{environment_details}")
}

fn build_observer_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "status": {
                "type": "string",
                "enum": ["ok", "failing"],
            },
            "replace_message": {
                "type": ["string", "null"],
                "minLength": 1,
            },
            "additional_instructions": {
                "type": ["string", "null"],
                "minLength": 1,
            }
        },
        "required": ["status", "replace_message", "additional_instructions"],
        "additionalProperties": false
    })
}

pub(super) fn summarize_intervention(
    replace_message: Option<&str>,
    additional_instructions: Option<&str>,
) -> Option<String> {
    let source = replace_message.or(additional_instructions)?;
    let trimmed = source.trim();
    if trimmed.is_empty() {
        return None;
    }
    const MAX_LEN: usize = 160;
    if trimmed.len() > MAX_LEN {
        let mut out = trimmed.chars().take(MAX_LEN).collect::<String>();
        out.push('…');
        Some(out)
    } else {
        Some(trimmed.to_string())
    }
}

// Helper so observer can append the coordinator's latest prompt.
pub(super) fn build_observer_conversation(
    conversation: Vec<ResponseItem>,
    coordinator_prompt: Option<&str>,
) -> Vec<ResponseItem> {
    let mut filtered: Vec<ResponseItem> = Vec::new();
    let mut prefer_assistant_prompt = true;

    for item in conversation {
        match item {
            ResponseItem::Message { id, role, content } => {
                if id.as_deref() == Some("auto-drive-reasoning") {
                    continue;
                }

                if role == "assistant" {
                    let mut new_content: Vec<ContentItem> = Vec::new();
                    for entry in content {
                        match entry {
                            ContentItem::InputText { text } => {
                                let already_prefixed = text.trim_start().starts_with("Coordinator:");
                                if !already_prefixed {
                                    prefer_assistant_prompt = false;
                                }
                                let prefixed = if already_prefixed {
                                    text
                                } else {
                                    format!("Coordinator: {text}")
                                };
                                new_content.push(ContentItem::InputText { text: prefixed });
                            }
                            ContentItem::OutputText { text } => {
                                let already_prefixed = text.trim_start().starts_with("Coordinator:");
                                if !already_prefixed {
                                    prefer_assistant_prompt = false;
                                }
                                let prefixed = if already_prefixed {
                                    text
                                } else {
                                    format!("Coordinator: {text}")
                                };
                                new_content.push(ContentItem::OutputText { text: prefixed });
                            }
                            other => new_content.push(other),
                        }
                    }
                    filtered.push(ResponseItem::Message {
                        id: None,
                        role,
                        content: new_content,
                    });
                } else {
                    filtered.push(ResponseItem::Message { id, role, content });
                }
            }
            ResponseItem::Reasoning { .. } => {
                // Observer should not inspect reasoning blocks.
                continue;
            }
            other => filtered.push(other),
        }
    }

    if let Some(prompt) = coordinator_prompt.and_then(|p| {
        let trimmed = p.trim();
        (!trimmed.is_empty()).then_some(trimmed)
    }) {
        let text = if prompt.trim_start().starts_with("Coordinator:") {
            prompt.to_string()
        } else {
            format!("Coordinator: {prompt}")
        };
        let append_as_assistant = prefer_assistant_prompt
            && filtered
                .last()
                .map(|item| match item {
                    ResponseItem::Message { role, content, .. } if role == "assistant" => content
                        .iter()
                        .all(|chunk| match chunk {
                        ContentItem::InputText { text }
                        | ContentItem::OutputText { text } => {
                            text.trim_start().starts_with("Coordinator:")
                        }
                        _ => false,
                    }),
                _ => false,
            })
            .unwrap_or(false);

        if append_as_assistant {
            filtered.push(ResponseItem::Message {
                id: None,
                role: "assistant".to_string(),
                content: vec![ContentItem::OutputText { text }],
            });
        } else {
            filtered.push(ResponseItem::Message {
                id: None,
                role: "user".to_string(),
                content: vec![ContentItem::InputText { text }],
            });
        }
    }

    filtered
}
