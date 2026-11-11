//! Collect and share a redacted session + diagnostic report with Hanzo.
//!
//! When a coding session stops early, stalls, or hits a tool/auth failure,
//! `dev report` (aliases: `dev share`, `dev bug`, `dev feedback`) and
//! `dev doctor --share` package:
//! 1. Redacted `dev doctor --json` diagnostics
//! 2. Toolchain co-installation status (`dev`, `hanzo-mcp`, `hanzo`, MCP config)
//! 3. Latest (or requested `--session`) rollout summary and redacted JSONL tail
//! 4. Kai decision and ledger traces (`~/.hanzo/dev/kai/decisions.jsonl`, `ledger.jsonl`)
//!
//! The bundle is written locally to `~/.hanzo/dev/reports/report-<id>.json` and
//! uploaded to Hanzo's feedback intake (`https://api.hanzo.ai/v1/event`) unless
//! `--local-only` is passed, printing a shareable Report ID.

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;
use std::time::SystemTime;

use anyhow::Context;
use anyhow::Result;
use chrono::Utc;
use clap::Args;
use codex_arg0::Arg0DispatchPaths;
use codex_utils_cli::CliConfigOverrides;
use serde::Serialize;
use serde_json::Value;
use serde_json::json;

const MAX_ROLLOUT_TAIL_BYTES: usize = 180 * 1024;
const MAX_KAI_LINES: usize = 60;
const UPLOAD_TIMEOUT: Duration = Duration::from_secs(20);

#[derive(Debug, Clone, Args)]
pub struct ReportArgs {
    /// Optional note describing what went wrong (e.g. "coding stopped mid-turn on enso").
    #[arg(value_name = "NOTE", trailing_var_arg = true)]
    pub note: Vec<String>,

    /// Specific session ID or rollout filename substring to attach (defaults to most recent).
    #[arg(long, short = 's', value_name = "SESSION_ID")]
    pub session: Option<String>,

    /// Save the diagnostic bundle locally without uploading to Hanzo.
    #[arg(long, visible_alias = "no-upload", default_value_t = false)]
    pub local_only: bool,

    /// Emit the report summary and bundle metadata as JSON.
    #[arg(long, default_value_t = false)]
    pub json: bool,
}

impl ReportArgs {
    pub fn from_doctor() -> Self {
        Self {
            note: vec!["Shared via `dev doctor --share`".to_string()],
            session: None,
            local_only: false,
            json: false,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolchainStatus {
    pub dev_version: String,
    pub dev_path: Option<String>,
    pub hanzo_mcp_installed: bool,
    pub hanzo_mcp_path: Option<String>,
    pub hanzo_mcp_version: Option<String>,
    pub hanzo_mcp_configured: bool,
    pub hanzo_cli_installed: bool,
    pub hanzo_cli_path: Option<String>,
    pub hanzo_cli_version: Option<String>,
    pub default_skills_present: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SessionDiagnostic {
    pub session_id: String,
    pub rollout_file: String,
    pub modified_at: Option<String>,
    pub line_count: usize,
    pub turn_started_count: usize,
    pub turn_completed_count: usize,
    pub tool_call_count: usize,
    pub last_event_type: Option<String>,
    pub last_turn_status: Option<String>,
    pub last_assistant_message: Option<String>,
    pub premature_stop_suspected: bool,
    pub premature_stop_reason: Option<String>,
    pub rollout_tail: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DiagnosticBundle {
    pub report_id: String,
    pub created_at: String,
    pub note: String,
    pub os: String,
    pub arch: String,
    pub toolchain: ToolchainStatus,
    pub session: Option<SessionDiagnostic>,
    pub kai_decisions_tail: Vec<Value>,
    pub kai_ledger_tail: Vec<Value>,
    pub doctor: Value,
}

pub async fn run(
    args: ReportArgs,
    overrides: CliConfigOverrides,
    interactive: &codex_tui::Cli,
    paths: &Arg0DispatchPaths,
) -> Result<()> {
    let home = hanzo_config::home();
    let report_id = uuid::Uuid::new_v4().to_string();
    let created_at = Utc::now().to_rfc3339();
    let note = args.note.join(" ").trim().to_string();

    let (doctor_json_str, _doctor_failed) =
        codex_cli::doctor::collect_doctor_report_json(overrides, interactive, paths).await?;
    let doctor: Value = serde_json::from_str(&doctor_json_str).unwrap_or_else(|_| json!({}));

    let toolchain = inspect_toolchain(&home);
    let session = inspect_session(&home, args.session.as_deref());
    let kai_decisions_tail = read_jsonl_tail(&home.join("kai/decisions.jsonl"), MAX_KAI_LINES);
    let kai_ledger_tail = read_jsonl_tail(&home.join("kai/ledger.jsonl"), MAX_KAI_LINES);

    let bundle = DiagnosticBundle {
        report_id: report_id.clone(),
        created_at: created_at.clone(),
        note: note.clone(),
        os: std::env::consts::OS.to_string(),
        arch: std::env::consts::ARCH.to_string(),
        toolchain,
        session,
        kai_decisions_tail,
        kai_ledger_tail,
        doctor,
    };

    let reports_dir = home.join("reports");
    fs::create_dir_all(&reports_dir)
        .with_context(|| format!("failed to create {}", reports_dir.display()))?;
    let local_path = reports_dir.join(format!("report-{report_id}.json"));
    let bundle_bytes = serde_json::to_vec_pretty(&bundle)?;
    fs::write(&local_path, &bundle_bytes)
        .with_context(|| format!("failed to write {}", local_path.display()))?;

    let (uploaded, upload_error) = if args.local_only {
        (false, None)
    } else {
        match upload_bundle(&bundle, &bundle_bytes).await {
            Ok(()) => (true, None),
            Err(err) => (false, Some(format!("{err:#}"))),
        }
    };

    if args.json {
        let summary = json!({
            "reportId": bundle.report_id,
            "createdAt": bundle.created_at,
            "uploaded": uploaded,
            "uploadError": upload_error,
            "localPath": local_path.display().to_string(),
            "sessionId": bundle.session.as_ref().map(|s| s.session_id.clone()),
            "prematureStopSuspected": bundle.session.as_ref().is_some_and(|s| s.premature_stop_suspected),
            "prematureStopReason": bundle.session.as_ref().and_then(|s| s.premature_stop_reason.clone()),
            "toolchain": bundle.toolchain,
        });
        println!("{}", serde_json::to_string_pretty(&summary)?);
        return Ok(());
    }

    println!();
    println!("Hanzo Diagnostic & Session Report");
    println!("────────────────────────────────────────────────────────");
    println!("  Report ID   : {report_id}");
    if let Some(session) = &bundle.session {
        println!("  Session ID  : {}", session.session_id);
        println!(
            "  Turns/Tools : {} started, {} completed, {} tool calls",
            session.turn_started_count, session.turn_completed_count, session.tool_call_count
        );
        if let Some(status) = &session.last_turn_status {
            println!("  Last Status : {status}");
        }
        if session.premature_stop_suspected
            && let Some(reason) = &session.premature_stop_reason
        {
            println!("  Stop Signal : {reason}");
        }
    } else {
        println!("  Session ID  : (no rollout found in {})", home.join("sessions").display());
    }
    println!(
        "  Toolchain   : dev {} | hanzo-mcp: {} | hanzo: {}",
        bundle.toolchain.dev_version,
        if bundle.toolchain.hanzo_mcp_installed {
            bundle
                .toolchain
                .hanzo_mcp_version
                .as_deref()
                .unwrap_or("installed")
        } else {
            "missing (run `curl -fsSL https://hanzo.sh | sh -s -- dev mcp`)"
        },
        if bundle.toolchain.hanzo_cli_installed {
            bundle
                .toolchain
                .hanzo_cli_version
                .as_deref()
                .unwrap_or("installed")
        } else {
            "not installed"
        }
    );
    println!(
        "  Kai Traces  : {} decisions, {} ledger entries attached",
        bundle.kai_decisions_tail.len(),
        bundle.kai_ledger_tail.len()
    );
    println!("  Local Bundle: {}", local_path.display());
    if uploaded {
        println!("  Shared      : Uploaded to Hanzo support (share Report ID `{report_id}`)");
    } else if args.local_only {
        println!("  Shared      : Saved locally (--local-only)");
    } else if let Some(err) = upload_error {
        println!("  Shared      : Upload skipped ({err}); share Local Bundle file directly");
    }
    println!();

    Ok(())
}

fn inspect_toolchain(home: &Path) -> ToolchainStatus {
    let dev_path = std::env::current_exe()
        .ok()
        .map(|p| redact_home(home, &p.display().to_string()));
    let mcp_path_buf = which::which("hanzo-mcp").ok();
    let hanzo_mcp_installed = mcp_path_buf.is_some();
    let hanzo_mcp_version = mcp_path_buf
        .as_deref()
        .and_then(|bin| command_version(bin, &["--version"]));
    let hanzo_mcp_path = mcp_path_buf.map(|p| redact_home(home, &p.display().to_string()));

    let cli_path_buf = which::which("hanzo").ok();
    let hanzo_cli_installed = cli_path_buf.is_some();
    let hanzo_cli_version = cli_path_buf
        .as_deref()
        .and_then(|bin| command_version(bin, &["--version"]));
    let hanzo_cli_path = cli_path_buf.map(|p| redact_home(home, &p.display().to_string()));

    let hanzo_mcp_configured = fs::read_to_string(home.join("config.toml"))
        .ok()
        .and_then(|raw| raw.parse::<toml::Value>().ok())
        .and_then(|val| {
            val.get("mcp_servers")
                .and_then(|m| m.get("hanzo"))
                .and_then(|h| h.get("command"))
                .and_then(toml::Value::as_str)
                .map(|cmd| !cmd.is_empty())
        })
        .unwrap_or(false);

    let mut default_skills_present = Vec::new();
    for (name, _) in hanzo_config::DEFAULT_SKILLS {
        if home.join("skills").join(name).join("SKILL.md").is_file() {
            default_skills_present.push((*name).to_string());
        }
    }

    ToolchainStatus {
        dev_version: hanzo::version::get().to_string(),
        dev_path,
        hanzo_mcp_installed,
        hanzo_mcp_path,
        hanzo_mcp_version,
        hanzo_mcp_configured,
        hanzo_cli_installed,
        hanzo_cli_path,
        hanzo_cli_version,
        default_skills_present,
    }
}

fn command_version(bin: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new(bin).args(args).output().ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8_lossy(&output.stdout);
    let first = text.lines().next()?.trim();
    (!first.is_empty()).then(|| first.to_string())
}

fn inspect_session(home: &Path, filter: Option<&str>) -> Option<SessionDiagnostic> {
    let sessions_dir = home.join("sessions");
    let mut candidates = Vec::new();
    collect_jsonl_files(&sessions_dir, &mut candidates);
    if let Some(needle) = filter {
        candidates.retain(|(path, _)| {
            path.file_name()
                .and_then(|n| n.to_str())
                .is_some_and(|name| name.contains(needle))
        });
    }
    candidates.sort_by(|a, b| b.1.cmp(&a.1));
    let (rollout_path, mtime) = candidates.into_iter().next()?;
    let raw = fs::read_to_string(&rollout_path).ok()?;
    let lines: Vec<&str> = raw.lines().collect();

    let mut session_id = rollout_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown")
        .to_string();
    let mut turn_started_count = 0usize;
    let mut turn_completed_count = 0usize;
    let mut tool_call_count = 0usize;
    let mut last_event_type = None;
    let mut last_turn_status = None;
    let mut last_assistant_message: Option<String> = None;
    let mut last_turn_had_tool_after_assistant = false;

    for line in &lines {
        let Ok(val) = serde_json::from_str::<Value>(line) else {
            continue;
        };
        if let Some(ty) = val.get("type").and_then(Value::as_str) {
            last_event_type = Some(ty.to_string());
            match ty {
                "session_meta" => {
                    if let Some(id) = val
                        .get("payload")
                        .and_then(|p| p.get("id"))
                        .and_then(Value::as_str)
                    {
                        session_id = id.to_string();
                    }
                }
                "event_msg" => {
                    if let Some(ev_ty) = val
                        .get("payload")
                        .and_then(|p| p.get("type"))
                        .and_then(Value::as_str)
                    {
                        last_event_type = Some(format!("event_msg:{ev_ty}"));
                        match ev_ty {
                            "task_started" | "turn_started" => {
                                turn_started_count += 1;
                                last_turn_status = Some("in_progress".to_string());
                            }
                            "task_complete" | "turn_complete" => {
                                turn_completed_count += 1;
                                last_turn_status = Some("completed".to_string());
                            }
                            "turn_aborted" | "error" | "stream_error" => {
                                last_turn_status = Some(ev_ty.to_string());
                            }
                            "agent_message" => {
                                if let Some(msg) = val
                                    .get("payload")
                                    .and_then(|p| p.get("message"))
                                    .and_then(Value::as_str)
                                    && !msg.trim().is_empty()
                                {
                                    last_assistant_message = Some(truncate_chars(msg.trim(), 600));
                                    last_turn_had_tool_after_assistant = false;
                                }
                            }
                            _ => {}
                        }
                    }
                }
                "response_item" => {
                    if let Some(item_ty) = val
                        .get("payload")
                        .and_then(|p| p.get("type"))
                        .and_then(Value::as_str)
                    {
                        match item_ty {
                            "function_call" | "custom_tool_call" | "local_shell_call" => {
                                tool_call_count += 1;
                                last_turn_had_tool_after_assistant = true;
                            }
                            "message" => {
                                if val
                                    .get("payload")
                                    .and_then(|p| p.get("role"))
                                    .and_then(Value::as_str)
                                    == Some("assistant")
                                    && let Some(content) = val
                                        .get("payload")
                                        .and_then(|p| p.get("content"))
                                        .and_then(Value::as_array)
                                {
                                    for item in content {
                                        if let Some(text) =
                                            item.get("text").and_then(Value::as_str)
                                            && !text.trim().is_empty()
                                        {
                                            last_assistant_message =
                                                Some(truncate_chars(text.trim(), 600));
                                            last_turn_had_tool_after_assistant = false;
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
    }

    let mut premature_stop_suspected = false;
    let mut premature_stop_reason = None;
    if turn_started_count > turn_completed_count {
        premature_stop_suspected = true;
        premature_stop_reason = Some(format!(
            "turn started ({turn_started_count}) without matching turn completion ({turn_completed_count}); last status: {}",
            last_turn_status.as_deref().unwrap_or("unknown")
        ));
    } else if !last_turn_had_tool_after_assistant
        && let Some(msg) = &last_assistant_message
        && looks_like_dangling_preamble(msg)
    {
        premature_stop_suspected = true;
        premature_stop_reason = Some(format!(
            "turn ended on a tool-call announcement preamble without emitting a tool call: {:?}",
            truncate_chars(msg, 160)
        ));
    }

    let mut rollout_tail = Vec::new();
    let mut bytes_used = 0usize;
    for line in lines.iter().rev() {
        let redacted = redact_secrets(&redact_home(home, line));
        if bytes_used + redacted.len() > MAX_ROLLOUT_TAIL_BYTES && !rollout_tail.is_empty() {
            break;
        }
        bytes_used += redacted.len();
        rollout_tail.push(redacted);
    }
    rollout_tail.reverse();

    let modified_at = mtime
        .duration_since(SystemTime::UNIX_EPOCH)
        .ok()
        .and_then(|d| chrono::DateTime::from_timestamp(d.as_secs() as i64, 0))
        .map(|dt| dt.to_rfc3339());

    Some(SessionDiagnostic {
        session_id,
        rollout_file: redact_home(home, &rollout_path.display().to_string()),
        modified_at,
        line_count: lines.len(),
        turn_started_count,
        turn_completed_count,
        tool_call_count,
        last_event_type,
        last_turn_status,
        last_assistant_message,
        premature_stop_suspected,
        premature_stop_reason,
        rollout_tail,
    })
}

fn collect_jsonl_files(dir: &Path, out: &mut Vec<(PathBuf, SystemTime)>) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_jsonl_files(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("jsonl") {
            let mtime = entry
                .metadata()
                .and_then(|m| m.modified())
                .unwrap_or(SystemTime::UNIX_EPOCH);
            out.push((path, mtime));
        }
    }
}

fn read_jsonl_tail(path: &Path, max_lines: usize) -> Vec<Value> {
    let Ok(raw) = fs::read_to_string(path) else {
        return Vec::new();
    };
    let lines: Vec<&str> = raw.lines().collect();
    let start = lines.len().saturating_sub(max_lines);
    lines[start..]
        .iter()
        .filter_map(|line| serde_json::from_str::<Value>(&redact_secrets(line)).ok())
        .collect()
}

fn looks_like_dangling_preamble(text: &str) -> bool {
    let t = text.trim();
    if t.is_empty() || t.len() > 600 || t.contains("```") || t.ends_with('?') {
        return false;
    }
    let last_line = t
        .lines()
        .rev()
        .map(str::trim)
        .find(|l| !l.is_empty())
        .unwrap_or("");
    let lower = last_line.to_ascii_lowercase();
    if last_line.ends_with(':') {
        return true;
    }
    const PREFIXES: &[&str] = &[
        "let me ",
        "now let me ",
        "let's ",
        "now let's ",
        "i'll ",
        "now i'll ",
        "i will ",
        "now i will ",
        "next, i'll ",
        "first, let me ",
    ];
    const VERBS: &[&str] = &[
        "look at",
        "check",
        "read",
        "search",
        "inspect",
        "open",
        "examine",
        "run",
        "grep",
        "find",
        "view",
        "fetch",
        "list",
        "explore",
        "verify",
        "test",
    ];
    PREFIXES.iter().any(|p| lower.contains(p)) && VERBS.iter().any(|v| lower.contains(v))
}

fn redact_home(home: &Path, text: &str) -> String {
    let mut out = text.replace(&home.display().to_string(), "<DEV_HOME>");
    if let Some(user_home) = std::env::var_os("HOME").or_else(|| std::env::var_os("USERPROFILE")) {
        let user_str = PathBuf::from(user_home).display().to_string();
        if !user_str.is_empty() {
            out = out.replace(&user_str, "<HOME>");
        }
    }
    out
}

fn redact_secrets(text: &str) -> String {
    let Ok(re) = regex_lite::Regex::new(
        r"(Bearer\s+[A-Za-z0-9._\-]+|\b(?:hk|sk|hz)-[A-Za-z0-9._\-]{8,}|\beyJ[A-Za-z0-9_\-]+\.[A-Za-z0-9_\-]+\.[A-Za-z0-9_\-]+)",
    ) else {
        return text.to_string();
    };
    re.replace_all(text, "<REDACTED_CREDENTIAL>").into_owned()
}

fn truncate_chars(s: &str, max: usize) -> String {
    if s.chars().count() <= max {
        s.to_string()
    } else {
        let head: String = s.chars().take(max).collect();
        format!("{head}…")
    }
}

async fn upload_bundle(bundle: &DiagnosticBundle, bundle_bytes: &[u8]) -> Result<()> {
    let endpoint = std::env::var("HANZO_REPORT_ENDPOINT")
        .ok()
        .filter(|s| !s.trim().is_empty())
        .unwrap_or_else(|| hanzo::FEEDBACK_ENDPOINT.to_string());
    let key = hanzo::FEEDBACK_KEY;
    let session_id = bundle
        .session
        .as_ref()
        .map_or_else(|| bundle.report_id.clone(), |s| s.session_id.clone());
    let title = if bundle.note.is_empty() {
        format!("[Bug Report]: Hanzo Dev session {session_id}")
    } else {
        format!("[Bug Report]: {}", truncate_chars(&bundle.note, 120))
    };

    let event_payload = json!({
        "messageId": bundle.report_id,
        "type": "error",
        "event": "feedback",
        "distinctId": session_id,
        "personId": "",
        "sessionId": session_id,
        "traceId": bundle.report_id,
        "product": "dev",
        "service": "dev",
        "site": "dev/bug",
        "path": "/feedback/bug",
        "level": "error",
        "release": bundle.toolchain.dev_version,
        "error": {
            "type": title,
            "message": if bundle.note.is_empty() {
                bundle.session.as_ref().and_then(|s| s.premature_stop_reason.clone()).unwrap_or_else(|| "Diagnostic report shared via `dev report`".to_string())
            } else {
                bundle.note.clone()
            },
            "handled": true,
        },
        "properties": {
            "report_id": bundle.report_id,
            "thread_id": session_id,
            "cli_version": bundle.toolchain.dev_version,
            "hanzo_mcp_installed": bundle.toolchain.hanzo_mcp_installed.to_string(),
            "hanzo_mcp_configured": bundle.toolchain.hanzo_mcp_configured.to_string(),
            "premature_stop_suspected": bundle.session.as_ref().is_some_and(|s| s.premature_stop_suspected).to_string(),
            "last_turn_status": bundle.session.as_ref().and_then(|s| s.last_turn_status.clone()).unwrap_or_default(),
        }
    });

    let client = reqwest::Client::builder()
        .timeout(UPLOAD_TIMEOUT)
        .build()
        .context("failed to build HTTP client")?;

    let resp = client
        .post(&endpoint)
        .header("Authorization", format!("Bearer {key}"))
        .json(&event_payload)
        .send()
        .await
        .context("failed to send report event")?;
    anyhow::ensure!(
        resp.status().is_success(),
        "report endpoint returned HTTP {}",
        resp.status()
    );

    let body_text = String::from_utf8_lossy(bundle_bytes);
    let attachment_payload = json!({
        "type": "log",
        "event": "feedback.attachment",
        "kind": "dev-diagnostic-report.json",
        "distinctId": session_id,
        "personId": "",
        "sessionId": session_id,
        "traceId": bundle.report_id,
        "product": "dev",
        "service": "dev",
        "site": "dev/bug",
        "path": "/feedback/bug",
        "level": "info",
        "release": bundle.toolchain.dev_version,
        "log": {
            "body": body_text,
            "attributes": {
                "filename": "dev-diagnostic-report.json",
                "contentType": "application/json",
                "encoding": "utf-8",
                "part": 1,
                "parts": 1,
                "bytes": bundle_bytes.len(),
            }
        }
    });

    let att_resp = client
        .post(&endpoint)
        .header("Authorization", format!("Bearer {key}"))
        .json(&attachment_payload)
        .send()
        .await
        .context("failed to upload diagnostic bundle attachment")?;
    anyhow::ensure!(
        att_resp.status().is_success(),
        "attachment endpoint returned HTTP {}",
        att_resp.status()
    );

    // Best-effort sync to Hanzo CRM (`crm.hanzo.ai` -> Leads / Forms -> Feedback & Bugs)
    let crm_endpoint = std::env::var("HANZO_CRM_FEEDBACK_ENDPOINT")
        .ok()
        .filter(|s| !s.trim().is_empty())
        .unwrap_or_else(|| "https://api.hanzo.ai/v1/marketing/leads".to_string());
    let reporter_email = std::env::var("HANZO_USER_EMAIL")
        .ok()
        .filter(|s| s.contains('@'))
        .unwrap_or_else(|| {
            let short: String = session_id.chars().filter(|c| c.is_ascii_alphanumeric()).take(12).collect();
            format!("dev-report-{}@report.hanzo.ai", if short.is_empty() { "anon" } else { &short })
        });
    let summary_text = if !bundle.note.is_empty() {
        bundle.note.clone()
    } else if let Some(reason) = bundle.session.as_ref().and_then(|s| s.premature_stop_reason.clone()) {
        reason
    } else {
        format!("Diagnostic bundle {} submitted via `dev report`", bundle.report_id)
    };
    let crm_payload = json!({
        "email": reporter_email,
        "name": format!("Hanzo Dev CLI ({})", bundle.toolchain.dev_version),
        "program": "feedback",
        "source": "hanzo-dev/report",
        "need": truncate_chars(&summary_text, 3800),
        "answers": [
            { "question": "Report ID", "answer": bundle.report_id },
            { "question": "Session ID", "answer": session_id },
            { "question": "CLI & MCP Toolchain", "answer": format!(
                "dev={} | hanzo-mcp={} (configured={}) | os={}/{}",
                bundle.toolchain.dev_version,
                bundle.toolchain.hanzo_mcp_installed,
                bundle.toolchain.hanzo_mcp_configured,
                bundle.os,
                bundle.arch
            ) },
            { "question": "Premature Stop Analysis", "answer": format!(
                "suspected={} | reason={}",
                bundle.session.as_ref().is_some_and(|s| s.premature_stop_suspected),
                bundle.session.as_ref().and_then(|s| s.premature_stop_reason.clone()).unwrap_or_else(|| "none".to_string())
            ) },
            { "question": "Kai Decision Ledger Tail", "answer": truncate_chars(
                &serde_json::to_string(&bundle.kai_ledger_tail).unwrap_or_default(),
                1800
            ) }
        ]
    });
    let _ = client
        .post(&crm_endpoint)
        .header("X-Idempotency-Key", &bundle.report_id)
        .json(&crm_payload)
        .send()
        .await;

    Ok(())
}
