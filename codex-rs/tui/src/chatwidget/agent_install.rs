use std::sync::mpsc::Receiver;
use std::sync::{Arc, Mutex};

use anyhow::{anyhow, Context, Result};
use codex_core::config::Config;
use codex_core::config::ConfigOverrides;
use codex_core::config_types::ReasoningEffort;
use codex_core::debug_logger::DebugLogger;
use codex_core::protocol::SandboxPolicy;
use codex_core::{AuthManager, ModelClient, Prompt, ResponseEvent, TextFormat};
use codex_protocol::models::{ContentItem, ResponseItem};
use futures::StreamExt;
use serde::Deserialize;
use serde_json::{self, json, Value};
use tracing::debug;
use uuid::Uuid;

use crate::app_event::{AppEvent, TerminalAfter, TerminalRunController, TerminalRunEvent};
use crate::app_event_sender::AppEventSender;

const MAX_OUTPUT_CHARS: usize = 8_000;
const MAX_STEPS: usize = 6;

#[derive(Debug, Deserialize)]
struct InstallDecision {
    finish_status: String,
    message: String,
    #[serde(default)]
    command: Option<String>,
}

pub(super) fn start_agent_install_session(
    app_event_tx: AppEventSender,
    terminal_id: u64,
    agent_name: String,
    default_command: String,
    cwd: Option<String>,
    controller: TerminalRunController,
    mut controller_rx: Receiver<TerminalRunEvent>,
    selected_index: usize,
    debug_enabled: bool,
) {
    std::thread::spawn(move || {
        let runtime = match tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
        {
            Ok(rt) => rt,
            Err(err) => {
                let msg = format!("Failed to start install helper runtime: {err}");
                app_event_tx.send(AppEvent::TerminalChunk {
                    id: terminal_id,
                    chunk: format!("{msg}\n").into_bytes(),
                    _is_stderr: true,
                });
                app_event_tx.send(AppEvent::TerminalUpdateMessage {
                    id: terminal_id,
                    message: msg,
                });
                return;
            }
        };

        if let Err(err) = run_install_loop(
            &runtime,
            &app_event_tx,
            terminal_id,
            &agent_name,
            &default_command,
            cwd.as_deref(),
            controller,
            &mut controller_rx,
            selected_index,
            debug_enabled,
        ) {
            let msg = if debug_enabled {
                format!("Install helper error: {err:#}")
            } else {
                format!("Install helper error: {err}")
            };
            app_event_tx.send(AppEvent::TerminalChunk {
                id: terminal_id,
                chunk: format!("{msg}\n").into_bytes(),
                _is_stderr: true,
            });
            app_event_tx.send(AppEvent::TerminalUpdateMessage {
                id: terminal_id,
                message: msg,
            });
        }
    });
}

fn run_install_loop(
    runtime: &tokio::runtime::Runtime,
    app_event_tx: &AppEventSender,
    terminal_id: u64,
    agent_name: &str,
    default_command: &str,
    cwd: Option<&str>,
    controller: TerminalRunController,
    controller_rx: &mut Receiver<TerminalRunEvent>,
    selected_index: usize,
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
        ReasoningEffort::Low,
        cfg.model_reasoning_summary,
        cfg.model_text_verbosity,
        Uuid::new_v4(),
        Arc::new(Mutex::new(
            DebugLogger::new(debug_enabled)
                .unwrap_or_else(|_| DebugLogger::new(false).expect("debug logger")),
        )),
    );

    let platform = std::env::consts::OS;
    let sandbox = if matches!(cfg.sandbox_policy, SandboxPolicy::DangerFullAccess) {
        "full access"
    } else {
        "limited sandbox"
    };
    let cwd_text = cwd.unwrap_or("unknown");

    if debug_enabled {
        debug!(
            target: "agent_install",
            "Starting guided install session: agent={agent_name} default_command={default_command} platform={platform} sandbox={sandbox} cwd={cwd}",
            agent_name = agent_name,
            default_command = default_command,
            platform = platform,
            sandbox = sandbox,
            cwd = cwd_text,
        );
    }

    let developer = format!(
        "You are coordinating shell commands to install the agent named \"{agent_name}\".\n\nRules:\n- `finish_status`: one of `continue`, `finish_success`, or `finish_failed`.\n  * Use `continue` when another shell command is required.\n  * Use `finish_success` when installation completed successfully.\n  * Use `finish_failed` when installation cannot continue or needs manual intervention.\n- `message`: short status (<= 160 characters) describing what happened or what to do next.\n- `command`: exact shell command to run next. Supply a single non-interactive command when `finish_status` is `continue`; set to null otherwise.\n- The provided command will be executed and its output returned to you. Prefer non-destructive diagnostics (search, list, install alternative package) when handling errors.\n- Always inspect the latest command output before choosing the next action. Suggest follow-up steps (e.g. alternate packages, additional instructions) when a command fails.\n- Respect the detected platform: use Homebrew on macOS, apt/dnf/pacman on Linux, winget/choco/powershell on Windows.",
    );

    let schema = json!({
        "type": "object",
        "properties": {
            "finish_status": {
                "type": "string",
                "enum": ["continue", "finish_success", "finish_failed"],
                "description": "Use 'continue' to supply another command, 'finish_success' when installation completed, or 'finish_failed' when installation cannot proceed."
            },
            "message": { "type": "string", "minLength": 1, "maxLength": 160 },
            "command": {
                "type": ["string", "null"],
                "minLength": 1,
                "description": "Shell command to execute next. Must be null when finish_status is not 'continue'.",
            }
        },
        "required": ["finish_status", "message", "command"],
        "additionalProperties": false
    });

    let developer_msg = make_message("developer", developer);
    let mut conversation: Vec<ResponseItem> = Vec::new();
    let initial_user = format!(
        "Install target: {agent_name}.\nPlatform: {platform}.\nSandbox: {sandbox}.\nWorking directory: {cwd_text}.\nSuggested starting command: {default_command}.\nPlease propose the first command to run.",
    );
    conversation.push(make_message("user", initial_user));

    let mut steps = 0usize;
    loop {
        steps += 1;
        if steps > MAX_STEPS {
            return Err(anyhow!("hit step limit without completing install"));
        }

        if debug_enabled {
            debug!(target: "agent_install", step = steps, "Requesting next install command");
        }
        app_event_tx.send(AppEvent::TerminalUpdateMessage {
            id: terminal_id,
            message: format!("Planning step {steps}…"),
        });

        let mut prompt = Prompt::default();
        prompt.input.push(developer_msg.clone());
        prompt.input.extend(conversation.clone());
        prompt.store = true;
        prompt.text_format = Some(TextFormat {
            r#type: "json_schema".to_string(),
            name: Some("agent_install_flow".to_string()),
            strict: Some(true),
            schema: Some(schema.clone()),
        });

        let raw = request_decision(runtime, &client, &prompt).context("model stream failed")?;
        let (decision, raw_value) = parse_decision(&raw)?;
        if debug_enabled {
            debug!(
                target: "agent_install",
                step = steps,
                "Model decision: message={message:?} command={command:?} raw={raw:?}",
                message = decision.message,
                command = decision.command.as_deref().unwrap_or("<none>"),
                raw = &raw_value,
            );
        }
        conversation.push(make_message("assistant", raw.clone()));

        app_event_tx.send_background_event_late(format!(
            "Install helper: {}",
            decision.message
        ));
        app_event_tx.send(AppEvent::TerminalUpdateMessage {
            id: terminal_id,
            message: decision.message.clone(),
        });

        let finish_status = decision.finish_status.as_str();
        match finish_status {
            "continue" => {
                let command = decision
                    .command
                    .as_deref()
                    .map(str::trim)
                    .filter(|c| !c.is_empty())
                    .ok_or_else(|| anyhow!("model response missing command for next step"))?;

                app_event_tx.send(AppEvent::TerminalChunk {
                    id: terminal_id,
                    chunk: format!("$ {}\n", command).into_bytes(),
                    _is_stderr: false,
                });
                app_event_tx.send(AppEvent::TerminalRunCommand {
                    id: terminal_id,
                    command: wrap_command(command),
                    command_display: command.to_string(),
                    controller: Some(controller.clone()),
                });

                let Some((output, exit_code)) = collect_command_output(controller_rx)
                    .context("collecting command output")?
                else {
                    if debug_enabled {
                        debug!(target: "agent_install", "Command collection cancelled by user");
                    }
                    app_event_tx.send_background_event_late("Install cancelled by user".to_string());
                    break;
                };
                if debug_enabled {
                    debug!(
                        target: "agent_install",
                        "Command finished: command={command} exit_code={exit_code:?}"
                    );
                }

                let truncated = tail_chars(&output, MAX_OUTPUT_CHARS);
                let summary = format!(
                    "Command: {command}\nExit code: {}\nOutput (last {} chars):\n{}",
                    exit_code
                        .map(|c| c.to_string())
                        .unwrap_or_else(|| "unknown".to_string()),
                    truncated.chars().count(),
                    truncated
                );
                conversation.push(make_message("user", summary));

                let status = if let Some(code) = exit_code {
                    format!("Command exited {code}. Analyzing…")
                } else {
                    "Command ended (exit code unknown). Analyzing…".to_string()
                };
                app_event_tx.send(AppEvent::TerminalUpdateMessage {
                    id: terminal_id,
                    message: status,
                });
            }
            "finish_success" => {
                if decision
                    .command
                    .as_deref()
                    .map(str::trim)
                    .filter(|c| !c.is_empty())
                    .is_some()
                {
                    return Err(anyhow!("finish_success must set command to null"));
                }
                app_event_tx.send_background_event_late(format!(
                    "✅ Agent {agent_name} install: {}",
                    decision.message
                ));
                app_event_tx.send(AppEvent::TerminalForceClose { id: terminal_id });
                app_event_tx.send(AppEvent::TerminalAfter(
                    TerminalAfter::RefreshAgentsAndClose { selected_index },
                ));
                break;
            }
            "finish_failed" => {
                if decision
                    .command
                    .as_deref()
                    .map(str::trim)
                    .filter(|c| !c.is_empty())
                    .is_some()
                {
                    return Err(anyhow!("finish_failed must set command to null"));
                }
                app_event_tx.send_background_event_late(format!(
                    "❌ Agent {agent_name} install failed: {}",
                    decision.message
                ));
                // keep terminal open for inspection
                break;
            }
            other => {
                return Err(anyhow!("unexpected finish_status '{other}'"));
            }
        }
    }

    Ok(())
}

fn request_decision(
    runtime: &tokio::runtime::Runtime,
    client: &ModelClient,
    prompt: &Prompt,
) -> Result<String> {
    runtime.block_on(async {
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
                Ok(ResponseEvent::Completed { .. }) => break,
                Err(err) => return Err(anyhow!("model stream error: {err}")),
                _ => {}
            }
        }
        Ok(out)
    })
}

fn parse_decision(raw: &str) -> Result<(InstallDecision, Value)> {
    let value: Value = match serde_json::from_str(raw) {
        Ok(v) => v,
        Err(_) => {
            let Some(json_blob) = extract_first_json_object(raw) else {
                return Err(anyhow!("model response was not valid JSON"));
            };
            serde_json::from_str(&json_blob).context("parsing JSON from model output")?
        }
    };
    let decision: InstallDecision = serde_json::from_value(value.clone())
        .context("decoding install decision")?;
    Ok((decision, value))
}

fn collect_command_output(
    controller_rx: &Receiver<TerminalRunEvent>,
) -> Result<Option<(String, Option<i32>)>> {
    let mut buf: Vec<u8> = Vec::new();
    let exit_code = loop {
        match controller_rx.recv() {
            Ok(TerminalRunEvent::Chunk { data, _is_stderr: _ }) => buf.extend_from_slice(&data),
            Ok(TerminalRunEvent::Exit { exit_code, _duration: _ }) => break exit_code,
            Err(_) => return Ok(None),
        }
    };
    let text = String::from_utf8_lossy(&buf).to_string();
    Ok(Some((text, exit_code)))
}

fn wrap_command(raw: &str) -> Vec<String> {
    if raw.is_empty() {
        return Vec::new();
    }
    if cfg!(target_os = "windows") {
        vec![
            "powershell.exe".to_string(),
            "-NoProfile".to_string(),
            "-ExecutionPolicy".to_string(),
            "Bypass".to_string(),
            "-Command".to_string(),
            raw.to_string(),
        ]
    } else {
        vec!["/bin/bash".to_string(), "-lc".to_string(), raw.to_string()]
    }
}

fn tail_chars(text: &str, max_chars: usize) -> String {
    let total = text.chars().count();
    if total <= max_chars {
        return text.to_string();
    }
    let mut idx = text.len();
    let mut count = 0usize;
    for (i, _) in text.char_indices().rev() {
        count += 1;
        if count >= max_chars {
            idx = i;
            break;
        }
    }
    text[idx..].to_string()
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
