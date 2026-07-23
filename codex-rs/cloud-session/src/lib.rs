//! Best-effort live streaming of a `hanzo code` run to the Hanzo Cloud session
//! registry, so the run shows up in the hanzo.bot playground (`/sessions/:id`).
//!
//! Shared by every frontend (`exec`, `tui`): each taps the same
//! `ServerNotification` stream and mirrors it to the cloud. The type is
//! decoupled from any concrete `Config` — it authenticates through the
//! [`AuthManagerConfig`] trait and takes run metadata as a plain [`SessionMeta`]
//! value, so both the exec and interactive paths reuse one implementation.
//!
//! Entirely additive and defensive. Registration and every event emit are
//! fire-and-forget: if the user is signed out, offline, or the registry errors,
//! streaming silently disables and the run itself is never affected. Opt out
//! with `HANZO_SESSION_TRACKING=0`.
//!
//! Event kinds and payloads follow the playground render contract
//! (`describeEvent`): `message`, `tool-call`, `log`, `status`, `task`,
//! `file_update`. File changes carry ONLY metadata (path + line counts) — never
//! the diff text — so nothing sensitive rides the stream.

use std::path::PathBuf;

use codex_agents_client::HttpClient;
use codex_agents_client::SessionRegister;
use codex_app_server_protocol::CollabAgentTool;
use codex_app_server_protocol::ServerNotification;
use codex_app_server_protocol::ThreadItem;
use codex_app_server_protocol::TokenUsageBreakdown;
use codex_app_server_protocol::UserInput;
use codex_git_utils::get_git_repo_root;
use codex_login::AuthManager;
use codex_login::AuthManagerConfig;
use serde::Serialize;
use serde_json::Value;
use serde_json::json;

const DEFAULT_BASE_URL: &str = "https://api.hanzo.ai";

/// Cap any single text/output field so an event payload stays well under the
/// registry's 64 KiB body limit even when several fields are populated.
const MAX_FIELD_CHARS: usize = 8_000;

/// The run details a session is registered with — supplied by the caller from
/// whichever `Config` it holds, so this crate stays free of any config type.
pub struct SessionMeta {
    pub model: String,
    pub cwd: PathBuf,
    pub provider: String,
}

/// A registered cloud session that mirrors this run's events. Cheaply cloned
/// internals (`reqwest::Client` + `Arc` auth) let each emit run on its own
/// detached task without blocking the run loop.
pub struct CloudSession {
    client: HttpClient,
    id: String,
}

impl CloudSession {
    /// Register a session for this run, or return `None` if tracking is opted
    /// out, the user is signed out, the base URL is untrusted, or the registry
    /// is unreachable. Never returns an error to the caller — a failure to
    /// track must not surface in a normal run.
    pub async fn start(config: &impl AuthManagerConfig, meta: SessionMeta) -> Option<Self> {
        if std::env::var("HANZO_SESSION_TRACKING").ok().as_deref() == Some("0") {
            return None;
        }

        let base_url =
            std::env::var("CODEX_AGENTS_BASE_URL").unwrap_or_else(|_| DEFAULT_BASE_URL.to_string());
        // The signed-in bearer JWT is attached to every request, so the base URL
        // is a trust boundary: only ever send the token to HTTPS *.hanzo.ai (or
        // loopback for local testing). Mirrors `validate_base_url` in the CLI.
        if !base_url_is_trusted(&base_url) {
            return None;
        }

        let auth_manager =
            AuthManager::shared_from_config(config, /*enable_codex_api_key_env*/ false).await;
        // Signed out → nothing to track; stay silent.
        let auth = auth_manager.auth().await?;
        let auth_provider = codex_model_provider::auth_provider_from_auth(&auth);

        let client = HttpClient::new(base_url)
            .ok()?
            .with_auth_provider(auth_provider);

        let repo = get_git_repo_root(&meta.cwd)
            .and_then(|root| root.file_name().map(|n| n.to_string_lossy().into_owned()))
            .unwrap_or_default();
        let title = if repo.is_empty() {
            meta.model.clone()
        } else {
            format!("{repo} · {}", meta.model)
        };

        let reg = SessionRegister {
            agent: meta.model,
            title,
            host: hostname(),
            cwd: meta.cwd.to_string_lossy().into_owned(),
            repo,
            target: String::new(),
            provider: meta.provider,
            account: String::new(),
        };

        let session = client.register_session(&reg).await.ok()?;
        eprintln!(
            "\u{2197} tracking this session live at https://hanzo.bot/sessions/{}",
            session.id
        );
        Some(Self {
            client,
            id: session.id,
        })
    }

    /// Map a server notification to zero or more cloud events and emit each on
    /// its own detached task. Never blocks the run loop and never fails a run.
    pub fn observe(&self, notification: &ServerNotification) {
        for (kind, payload) in map_events(notification) {
            let client = self.client.clone();
            let id = self.id.clone();
            tokio::spawn(async move {
                let _ = client.append_session_event(&id, kind, payload).await;
            });
        }
    }

    /// Mark the run terminal (`done` | `error`). Best-effort.
    pub async fn finish(self, status: &str) {
        let _ = self.client.patch_session_status(&self.id, status).await;
    }
}

/// Restrict the registry base URL to HTTPS `hanzo.ai` (and subdomains), or a
/// loopback host for local testing. A plain-string check avoids pulling in the
/// `url` crate for a purely advisory gate.
fn base_url_is_trusted(base_url: &str) -> bool {
    if base_url.starts_with("http://127.0.0.1")
        || base_url.starts_with("http://localhost")
        || base_url.starts_with("http://[::1]")
    {
        return true;
    }
    let Some(rest) = base_url.strip_prefix("https://") else {
        return false;
    };
    let host = rest.split(['/', ':']).next().unwrap_or("");
    host == "hanzo.ai" || host.ends_with(".hanzo.ai")
}

fn hostname() -> String {
    std::env::var("HOSTNAME")
        .or_else(|_| std::env::var("HOST"))
        .unwrap_or_default()
}

/// The consumer-contract mapping: a server notification → cloud event(s).
fn map_events(notification: &ServerNotification) -> Vec<(&'static str, Value)> {
    use ServerNotification as N;
    match notification {
        N::ItemCompleted(completed) => map_item(&completed.item),
        N::TurnStarted(_) => vec![("status", json!({ "status": "running" }))],
        N::TurnCompleted(_) => vec![("status", json!({ "status": "idle" }))],
        N::TurnPlanUpdated(plan) => vec![(
            "task",
            json!({
                "tasks": plan
                    .plan
                    .iter()
                    .map(|step| json!({
                        "text": clip(&step.step),
                        "status": serde_json::to_value(step.status).unwrap_or(Value::Null),
                    }))
                    .collect::<Vec<_>>()
            }),
        )],
        N::ThreadTokenUsageUpdated(usage) => match token_total(&usage.token_usage.total) {
            Some(tokens) => vec![(
                "context",
                json!({
                    "tokens": tokens,
                    "contextWindow": usage.token_usage.model_context_window,
                }),
            )],
            None => Vec::new(),
        },
        N::Error(err) => vec![(
            "status",
            json!({ "status": "error", "error": clip(&err.error.message) }),
        )],
        _ => Vec::new(),
    }
}

fn map_item(item: &ThreadItem) -> Vec<(&'static str, Value)> {
    use ThreadItem as I;
    match item {
        I::UserMessage { content, .. } => {
            let text = user_text(content);
            if text.is_empty() {
                Vec::new()
            } else {
                vec![("message", json!({ "role": "user", "text": clip(&text) }))]
            }
        }
        I::AgentMessage { text, .. } => {
            vec![(
                "message",
                json!({ "role": "assistant", "text": clip(text) }),
            )]
        }
        I::CollabAgentToolCall {
            tool,
            receiver_thread_ids,
            ..
        } if matches!(tool, CollabAgentTool::SpawnAgent) => {
            let agent = receiver_thread_ids
                .first()
                .cloned()
                .unwrap_or_else(|| "sub-agent".to_string());
            vec![("spawn", json!({ "agent": agent }))]
        }
        I::Reasoning {
            summary, content, ..
        } => {
            let text = if summary.is_empty() {
                content.join("\n")
            } else {
                summary.join("\n")
            };
            if text.is_empty() {
                Vec::new()
            } else {
                vec![("log", json!({ "type": "reasoning", "text": clip(&text) }))]
            }
        }
        I::CommandExecution {
            command,
            aggregated_output,
            exit_code,
            ..
        } => {
            let mut events = vec![(
                "tool-call",
                json!({ "name": "shell", "input": { "command": clip(command) } }),
            )];
            if let Some(output) = aggregated_output {
                events.push((
                    "log",
                    json!({
                        "type": "tool-result",
                        "output": clip(output),
                        "exitCode": exit_code,
                    }),
                ));
            }
            events
        }
        I::McpToolCall {
            server,
            tool,
            arguments,
            ..
        } => vec![(
            "tool-call",
            json!({ "name": format!("{server}.{tool}"), "input": arguments }),
        )],
        I::FileChange { changes, .. } => changes
            .iter()
            .map(|change| {
                let (additions, deletions) = diff_counts(&change.diff);
                (
                    "file_update",
                    json!({
                        "path": change.path,
                        "change": kind_label(change.kind.clone()),
                        "additions": additions,
                        "deletions": deletions,
                    }),
                )
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Concatenate the text parts of a user message; image parts are dropped.
fn user_text(content: &[UserInput]) -> String {
    content
        .iter()
        .filter_map(|part| match part {
            UserInput::Text { text, .. } => Some(text.as_str()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// A single total-token count from a usage breakdown: an explicit total when the
/// wire provides one, else input + output. Field-name tolerant so it survives
/// schema tweaks without breaking the stream.
fn token_total(breakdown: &TokenUsageBreakdown) -> Option<u64> {
    let obj = serde_json::to_value(breakdown).ok()?;
    let obj = obj.as_object()?;
    if let Some(total) = obj.get("total_tokens").and_then(|v| v.as_u64()) {
        return Some(total);
    }
    let input = obj
        .get("input_tokens")
        .and_then(|v| v.as_u64())
        .unwrap_or(0);
    let output = obj
        .get("output_tokens")
        .and_then(|v| v.as_u64())
        .unwrap_or(0);
    match input + output {
        0 => None,
        sum => Some(sum),
    }
}

/// Truncate to a character budget, appending an ellipsis when clipped.
fn clip(s: &str) -> String {
    if s.chars().count() <= MAX_FIELD_CHARS {
        return s.to_string();
    }
    let mut clipped: String = s.chars().take(MAX_FIELD_CHARS).collect();
    clipped.push('\u{2026}');
    clipped
}

/// Count added/removed lines in a unified diff — a metadata summary that lets
/// the stream describe a change without ever carrying the diff body.
fn diff_counts(diff: &str) -> (usize, usize) {
    let mut additions = 0;
    let mut deletions = 0;
    for line in diff.lines() {
        if line.starts_with("+++") || line.starts_with("---") {
            continue;
        }
        match line.as_bytes().first() {
            Some(b'+') => additions += 1,
            Some(b'-') => deletions += 1,
            _ => {}
        }
    }
    (additions, deletions)
}

/// A short label for a serde-tagged enum (e.g. `PatchChangeKind`): the string
/// itself when it serializes as one, or its `type` tag when it's an object.
fn kind_label<T: Serialize>(value: T) -> String {
    match serde_json::to_value(value) {
        Ok(Value::String(s)) => s,
        Ok(Value::Object(map)) => map
            .get("type")
            .and_then(|t| t.as_str())
            .unwrap_or("change")
            .to_string(),
        _ => "change".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use codex_app_server_protocol::ErrorNotification;
    use codex_app_server_protocol::FileUpdateChange;
    use codex_app_server_protocol::PatchApplyStatus;
    use codex_app_server_protocol::PatchChangeKind;
    use codex_app_server_protocol::TurnError;
    use codex_app_server_protocol::UserInput;

    #[test]
    fn trusted_base_urls_are_hanzo_https_or_loopback() {
        assert!(base_url_is_trusted("https://api.hanzo.ai"));
        assert!(base_url_is_trusted("https://cloud.hanzo.ai/v1"));
        assert!(base_url_is_trusted("http://127.0.0.1:8000"));
        assert!(base_url_is_trusted("http://localhost:8080"));
        // Untrusted: plain-HTTP prod, look-alike hosts, and token-stealing suffixes.
        assert!(!base_url_is_trusted("http://api.hanzo.ai"));
        assert!(!base_url_is_trusted("https://hanzo.ai.evil.com"));
        assert!(!base_url_is_trusted("https://evil.com/api.hanzo.ai"));
        assert!(!base_url_is_trusted("https://nothanzo.ai"));
    }

    #[test]
    fn agent_message_maps_to_message_event() {
        let item = ThreadItem::AgentMessage {
            id: "i1".to_string(),
            text: "done".to_string(),
            phase: None,
            memory_citation: None,
        };
        let events = map_item(&item);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].0, "message");
        assert_eq!(events[0].1["role"], "assistant");
        assert_eq!(events[0].1["text"], "done");
    }

    #[test]
    fn file_change_emits_metadata_only_never_the_diff_body() {
        let secret = "const API_KEY: &str = \"sk-topsecret\";";
        let diff = format!("--- a/src/x.rs\n+++ b/src/x.rs\n+{secret}\n-old_line\n");
        let item = ThreadItem::FileChange {
            id: "i2".to_string(),
            changes: vec![FileUpdateChange {
                path: "src/x.rs".to_string(),
                kind: PatchChangeKind::Add,
                diff,
            }],
            status: PatchApplyStatus::Completed,
        };
        let events = map_item(&item);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].0, "file_update");
        let payload = &events[0].1;
        assert_eq!(payload["path"], "src/x.rs");
        assert_eq!(payload["change"], "add");
        assert_eq!(payload["additions"], 1);
        assert_eq!(payload["deletions"], 1);
        // The safety-critical invariant: the diff body (and any secret in it)
        // must NEVER appear in the streamed payload.
        let serialized = payload.to_string();
        assert!(!serialized.contains("sk-topsecret"));
        assert!(!serialized.contains("old_line"));
    }

    #[test]
    fn user_message_maps_to_user_role() {
        let item = ThreadItem::UserMessage {
            id: "u1".to_string(),
            client_id: None,
            content: vec![UserInput::Text {
                text: "fix the bug".to_string(),
                text_elements: vec![],
            }],
        };
        let events = map_item(&item);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].0, "message");
        assert_eq!(events[0].1["role"], "user");
        assert_eq!(events[0].1["text"], "fix the bug");
    }

    #[test]
    fn user_text_joins_text_parts() {
        let content = vec![
            UserInput::Text {
                text: "a".to_string(),
                text_elements: vec![],
            },
            UserInput::Text {
                text: "b".to_string(),
                text_elements: vec![],
            },
        ];
        assert_eq!(user_text(&content), "a\nb");
    }

    #[test]
    fn error_notification_maps_to_error_status() {
        let notification = ServerNotification::Error(ErrorNotification {
            error: TurnError {
                message: "boom".to_string(),
                codex_error_info: None,
                additional_details: None,
            },
            will_retry: false,
            thread_id: "t".to_string(),
            turn_id: "u".to_string(),
        });
        let events = map_events(&notification);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].0, "status");
        assert_eq!(events[0].1["status"], "error");
        assert_eq!(events[0].1["error"], "boom");
    }

    #[test]
    fn diff_counts_ignores_headers() {
        let (add, del) = diff_counts("--- a\n+++ b\n+one\n+two\n-gone\n context\n");
        assert_eq!((add, del), (2, 1));
    }

    #[test]
    fn clip_truncates_oversized_fields() {
        let big = "x".repeat(MAX_FIELD_CHARS + 100);
        let clipped = clip(&big);
        assert!(clipped.chars().count() <= MAX_FIELD_CHARS + 1);
        assert!(clipped.ends_with('\u{2026}'));
        assert_eq!(clip("short"), "short");
    }
}
