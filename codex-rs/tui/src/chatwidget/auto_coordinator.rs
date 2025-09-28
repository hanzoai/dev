use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::{Arc, Mutex};

use anyhow::{anyhow, Context, Result};
use codex_core::config::Config;
use codex_core::config::ConfigOverrides;
use codex_core::config_types::ReasoningEffort;
use codex_core::debug_logger::DebugLogger;
use codex_core::model_family::{find_family_for_model, derive_default_model_family};
use codex_core::protocol::SandboxPolicy;
use codex_core::{AuthManager, ModelClient, Prompt, ResponseEvent, TextFormat};
use codex_protocol::models::{ContentItem, ResponseItem};
use futures::StreamExt;
use serde::Deserialize;
use serde_json::{self, json, Value};
use tracing::debug;
use uuid::Uuid;

use crate::app_event::{AppEvent, AutoCoordinatorStatus};
use crate::app_event_sender::AppEventSender;

const MODEL_SLUG: &str = "gpt-5";
const SCHEMA_NAME: &str = "auto_coordinator_flow";

#[derive(Debug, Clone)]
pub(super) struct AutoCoordinatorHandle {
    pub tx: Sender<AutoCoordinatorCommand>,
}

#[derive(Debug)]
pub(super) enum AutoCoordinatorCommand {
    UpdateConversation(Vec<ResponseItem>),
    Stop,
}

#[derive(Debug, Deserialize)]
struct CoordinatorDecision {
    finish_status: String,
    thoughts: String,
    #[serde(default)]
    prompt: Option<String>,
}

pub(super) fn start_auto_coordinator(
    app_event_tx: AppEventSender,
    goal_text: String,
    conversation: Vec<ResponseItem>,
    debug_enabled: bool,
) -> Result<AutoCoordinatorHandle> {
    let (cmd_tx, cmd_rx) = mpsc::channel();
    let thread_tx = cmd_tx.clone();

    std::thread::spawn(move || {
        if let Err(err) = run_auto_loop(
            app_event_tx,
            goal_text,
            conversation,
            cmd_rx,
            debug_enabled,
        ) {
            tracing::error!("auto coordinator loop error: {err:#}");
        }
    });

    Ok(AutoCoordinatorHandle { tx: thread_tx })
}

fn run_auto_loop(
    app_event_tx: AppEventSender,
    goal_text: String,
    initial_conversation: Vec<ResponseItem>,
    cmd_rx: Receiver<AutoCoordinatorCommand>,
    debug_enabled: bool,
) -> Result<()> {
    let cfg = Config::load_with_cli_overrides(vec![], ConfigOverrides::default())
        .context("loading config")?;
    let preferred_auth = if cfg.using_chatgpt_auth {
        codex_protocol::mcp_protocol::AuthMode::ChatGPT
    } else {
        codex_protocol::mcp_protocol::AuthMode::ApiKey
    };
    let auth_mgr = AuthManager::shared(
        cfg.codex_home.clone(),
        preferred_auth,
        cfg.responses_originator_header.clone(),
    );
    let client = ModelClient::new(
        Arc::new(cfg.clone()),
        Some(auth_mgr),
        cfg.model_provider.clone(),
        ReasoningEffort::Medium,
        cfg.model_reasoning_summary,
        cfg.model_text_verbosity,
        Uuid::new_v4(),
        Arc::new(Mutex::new(
            DebugLogger::new(debug_enabled)
                .unwrap_or_else(|_| DebugLogger::new(false).expect("debug logger")),
        )),
    );

    let developer_intro = build_developer_message(&goal_text, matches!(
        cfg.sandbox_policy,
        SandboxPolicy::DangerFullAccess
    ));
    let schema = build_schema();
    let platform = std::env::consts::OS;
    debug!("[Auto coordinator] starting: goal={goal_text} platform={platform}");

    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("creating runtime for auto coordinator")?;

    let mut pending_conversation = Some(initial_conversation);
    let mut stopped = false;

    loop {
        if stopped {
            break;
        }

        if let Some(conv) = pending_conversation.take() {
            match request_coordinator_decision(
                &runtime,
                &client,
                &developer_intro,
                &schema,
                conv,
                &app_event_tx,
            ) {
                Ok((status, thoughts, prompt_opt)) => {
                    let event = AppEvent::AutoCoordinatorDecision {
                        status,
                        thoughts,
                        prompt: prompt_opt,
                    };
                    app_event_tx.send(event);
                    if !matches!(status, AutoCoordinatorStatus::Continue) {
                        stopped = true;
                        continue;
                    }
                }
                Err(err) => {
                    let event = AppEvent::AutoCoordinatorDecision {
                        status: AutoCoordinatorStatus::Failed,
                        thoughts: format!("Coordinator error: {err}"),
                        prompt: None,
                    };
                    app_event_tx.send(event);
                    stopped = true;
                    continue;
                }
            }
        }

        match cmd_rx.recv() {
            Ok(AutoCoordinatorCommand::UpdateConversation(conv)) => {
                pending_conversation = Some(conv);
            }
            Ok(AutoCoordinatorCommand::Stop) | Err(_) => {
                stopped = true;
            }
        }
    }

    Ok(())
}

fn build_developer_message(goal_text: &str, full_access: bool) -> String {
    let sandbox = if full_access { "full access" } else { "limited sandbox" };
    format!(
        "You are coordinating prompts sent to a running Code CLI process. You should act like a human maintainer of the project would act.\n\n**Primary Goal**\n{goal_text}\n\n**Rules**\n- `finish_status`: one of `continue`, `finish_success`, or `finish_failed`.\n  * Use `continue` when another prompt is reasonable. Always prefer this option.\n  * Use `finish_success` when the goal has been completed in it's entirety and absolutely no work remains.\n  * Use `finish_failed` when the goal absolutely can not be satisfied. This should almost never be used. Try other approaches and gather more information if there is no clear path forward.\n- `thoughts`: short status (<= 160 characters) describing you thought process around what the next prompt should be\n- `prompt`: the exact prompt to provide to the Code CLI process. You will receive the response the CLI provides.\n- First plan, then execute. Allow the CLI to plan for you. You should get it to do the thinking for you.\n- Don't repeat yourself. You will see past prompts and outputs. Always push the project forward.\n- Often a simple 'Please continue' or 'Work on feature A next' or 'What do you think is the best approach?' is sufficient. Your job is to keep things running in an appropriate direction. The CLI does all the actual work and thinking. You do not need to know much about the project or codebase, allow the CLI to do all this for you. You are focused on overall direction not implementation details.\n- Only stop when no other options remain. A human is observing your work and will step in if they want it to go in a different direction. You should not ask them for assistance - you should use your judgement to move on the most likely path forward. The human may override your message send to the CLI if they choose to go in another direction. This allows you to just guess the best path, knowing an overseer will step in if needed.\n\nEnvironment: {sandbox}"
    )
}

fn build_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "finish_status": {
                "type": "string",
                "enum": ["continue", "finish_success", "finish_failed"],
                "description": "Decision on how to proceed"
            },
            "thoughts": { "type": "string", "minLength": 1, "maxLength": 160 },
            "prompt": {
                "type": ["string", "null"],
                "minLength": 1,
                "description": "Prompt to send to Code CLI when finish_status is 'continue'"
            }
        },
        "required": ["finish_status", "thoughts", "prompt"],
        "additionalProperties": false
    })
}

fn request_coordinator_decision(
    runtime: &tokio::runtime::Runtime,
    client: &ModelClient,
    developer_intro: &str,
    schema: &Value,
    mut conversation: Vec<ResponseItem>,
    app_event_tx: &AppEventSender,
) -> Result<(AutoCoordinatorStatus, String, Option<String>)> {
    let mut prompt = Prompt::default();
    prompt.store = true;
    prompt.input.push(make_message("developer", developer_intro.to_string()));
    prompt.input.append(&mut conversation);
    prompt.text_format = Some(TextFormat {
        r#type: "json_schema".to_string(),
        name: Some(SCHEMA_NAME.to_string()),
        strict: Some(true),
        schema: Some(schema.clone()),
    });
    prompt.model_override = Some(MODEL_SLUG.to_string());
    let family = find_family_for_model(MODEL_SLUG)
        .unwrap_or_else(|| derive_default_model_family(MODEL_SLUG));
    prompt.model_family_override = Some(family);

    let raw = request_decision(runtime, client, &prompt, app_event_tx)?;
    let (decision, value) = parse_decision(&raw)?;
    debug!("[Auto coordinator] model decision: {:?}", value);

    let status = match decision.finish_status.as_str() {
        "continue" => AutoCoordinatorStatus::Continue,
        "finish_success" => AutoCoordinatorStatus::Success,
        "finish_failed" => AutoCoordinatorStatus::Failed,
        other => {
            return Err(anyhow!("unexpected finish_status '{other}'"));
        }
    };

    let prompt_opt = match status {
        AutoCoordinatorStatus::Continue => {
            let prompt_text = decision
                .prompt
                .as_ref()
                .map(|s| s.trim())
                .filter(|s| !s.is_empty())
                .ok_or_else(|| anyhow!("model response missing prompt for continue"))?;
            let cleaned = strip_role_prefix(prompt_text);
            Some(cleaned.to_string())
        }
        _ => None,
    };

    Ok((status, decision.thoughts.trim().to_string(), prompt_opt))
}

fn request_decision(
    runtime: &tokio::runtime::Runtime,
    client: &ModelClient,
    prompt: &Prompt,
    app_event_tx: &AppEventSender,
) -> Result<String> {
    let tx = app_event_tx.clone();
    runtime.block_on(async move {
        let mut stream = client.stream(prompt).await?;
        let mut out = String::new();
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
                Ok(ResponseEvent::ReasoningSummaryDelta { delta, .. })
                | Ok(ResponseEvent::ReasoningContentDelta { delta, .. }) => {
                    let message = strip_role_prefix(&delta).to_string();
                    tx.send(AppEvent::AutoCoordinatorThinking { delta: message });
                }
                Ok(ResponseEvent::Completed { .. }) => break,
                Err(err) => return Err(anyhow!("model stream error: {err}")),
                _ => {}
            }
        }
        Ok(out)
    })
}

fn parse_decision(raw: &str) -> Result<(CoordinatorDecision, Value)> {
    let value: Value = match serde_json::from_str(raw) {
        Ok(v) => v,
        Err(_) => {
            let Some(json_blob) = extract_first_json_object(raw) else {
                return Err(anyhow!("model response was not valid JSON"));
            };
            serde_json::from_str(&json_blob).context("parsing JSON from model output")?
        }
    };
    let decision: CoordinatorDecision = serde_json::from_value(value.clone())
        .context("decoding coordinator decision")?;
    Ok((decision, value))
}

fn extract_first_json_object(input: &str) -> Option<String> {
    let mut depth = 0usize;
    let mut in_str = false;
    let mut escape = false;
    let mut start: Option<usize> = None;
    for (idx, ch) in input.char_indices() {
        if in_str {
            if escape {
                escape = false;
                continue;
            }
            match ch {
                '"' => in_str = false,
                '\\' => escape = true,
                _ => {}
            }
            continue;
        }
        match ch {
            '"' => in_str = true,
            '{' => {
                if depth == 0 {
                    start = Some(idx);
                }
                depth += 1;
            }
            '}' => {
                if depth == 0 {
                    continue;
                }
                depth -= 1;
                if depth == 0 {
                    let Some(s) = start else { return None; };
                    return Some(input[s..=idx].to_string());
                }
            }
            _ => {}
        }
    }
    None
}

fn make_message(role: &str, text: String) -> ResponseItem {
    let content = if role.eq_ignore_ascii_case("assistant") {
        ContentItem::OutputText { text }
    } else {
        ContentItem::InputText { text }
    };

    ResponseItem::Message {
        id: None,
        role: role.to_string(),
        content: vec![content],
    }
}

fn strip_role_prefix(input: &str) -> &str {
    let trimmed = input.trim_start();
    const PREFIXES: [&str; 2] = ["Coordinator:", "CLI:"];
    for prefix in PREFIXES {
        if let Some(head) = trimmed.get(..prefix.len()) {
            if head.eq_ignore_ascii_case(prefix) {
                if let Some(rest) = trimmed.get(prefix.len()..) {
                    return rest;
                }
            }
        }
    }
    trimmed
}
