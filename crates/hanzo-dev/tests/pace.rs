//! A provider's rate limit slows a run down and never ends it.
//!
//! A 429 or an overload is waited out (its `Retry-After`, or full-jitter backoff) for as long
//! as the turn lives, past any retry budget, and `dev exec --json` names the wait
//! `turn.waiting`, never `error`. Rate-limit headers that say the budget is spent hold the
//! next request until the window resets, so no 429 happens at all.

use std::process::Output;
use std::process::Stdio;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;
use std::time::Instant;

use serde_json::Value;
use serde_json::json;
use wiremock::Mock;
use wiremock::MockServer;
use wiremock::Request;
use wiremock::Respond;
use wiremock::ResponseTemplate;
use wiremock::matchers::method;
use wiremock::matchers::path;

fn sse(events: &[Value]) -> String {
    events
        .iter()
        .map(|event| {
            format!(
                "event: {}\ndata: {event}\n\n",
                event["type"].as_str().unwrap_or_default()
            )
        })
        .collect()
}

fn completed(id: &str) -> Value {
    json!({
        "type": "response.completed",
        "response": {"id": id, "usage": {"input_tokens": 1, "input_tokens_details": null,
            "output_tokens": 1, "output_tokens_details": null, "total_tokens": 2}}
    })
}

fn answer(text: &str) -> String {
    sse(&[
        json!({"type": "response.created", "response": {"id": "r"}}),
        json!({"type": "response.output_item.done", "item": {"type": "message", "role": "assistant",
            "id": "m", "content": [{"type": "output_text", "text": text}]}}),
        completed("r"),
    ])
}

fn command() -> String {
    let arguments = json!({"cmd": "printf paced"}).to_string();
    sse(&[
        json!({"type": "response.created", "response": {"id": "c"}}),
        json!({"type": "response.output_item.done", "item": {"type": "function_call",
            "call_id": "call", "name": "exec_command", "arguments": arguments}}),
        completed("c"),
    ])
}

fn ok(body: String) -> ResponseTemplate {
    ResponseTemplate::new(200)
        .insert_header("content-type", "text/event-stream")
        .set_body_string(body)
}

/// Answers the n-th request (from 0) with `script[n]`, the last one thereafter, and keeps
/// when each request arrived.
#[derive(Clone)]
struct Script {
    script: Arc<Vec<ResponseTemplate>>,
    arrivals: Arc<Mutex<Vec<Instant>>>,
}

impl Script {
    fn new(script: Vec<ResponseTemplate>) -> Self {
        Self {
            script: Arc::new(script),
            arrivals: Arc::default(),
        }
    }

    fn arrivals(&self) -> Vec<Instant> {
        self.arrivals.lock().expect("arrivals").clone()
    }
}

impl Respond for Script {
    fn respond(&self, _: &Request) -> ResponseTemplate {
        let mut arrivals = self.arrivals.lock().expect("arrivals");
        arrivals.push(Instant::now());
        let at = (arrivals.len() - 1).min(self.script.len() - 1);
        self.script[at].clone()
    }
}

/// Runs one `dev exec --json` task against `server` and returns what it printed.
async fn run(server: &MockServer, provider: &str) -> (Output, Vec<Value>) {
    let home = tempfile::tempdir().expect("a home");
    let work = tempfile::tempdir().expect("a project");
    std::fs::write(
        home.path().join("config.toml"),
        format!(
            "model_provider = \"mock\"\nmodel = \"zen6\"\n\n[model_providers.mock]\nname = \"Mock\"\nbase_url = \"{}/v1\"\nwire_api = \"responses\"\n{provider}",
            server.uri()
        ),
    )
    .expect("the profile is writable");
    let run = tokio::process::Command::new(env!("CARGO_BIN_EXE_dev"))
        .args([
            "exec",
            "--json",
            "--skip-git-repo-check",
            "--dangerously-bypass-approvals-and-sandbox",
            "-C",
        ])
        .arg(work.path())
        .arg("go")
        .env("DEV_HOME", home.path())
        .stdin(Stdio::null())
        .output();
    let output = tokio::time::timeout(Duration::from_secs(120), run)
        .await
        .expect("the run finishes")
        .expect("dev runs");
    let events = String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter_map(|line| serde_json::from_str(line).ok())
        .collect();
    (output, events)
}

fn kinds(events: &[Value]) -> Vec<&str> {
    events
        .iter()
        .filter_map(|event| event["type"].as_str())
        .collect()
}

/// The run ended well, said its answer once, and named no error at any point.
fn finished(output: &Output, events: &[Value], text: &str) {
    let kinds = kinds(events);
    assert!(
        output.status.success(),
        "dev exec failed: {kinds:?}\nstderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !kinds
            .iter()
            .any(|kind| matches!(*kind, "error" | "turn.failed")),
        "a wait surfaced as an error: {events:?}"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("Too Many Requests") && !stdout.contains("status 429"),
        "the status code reached the output: {events:?}"
    );
    let answers: Vec<_> = events
        .iter()
        .filter(|event| {
            event["type"] == "item.completed" && event["item"]["type"] == "agent_message"
        })
        .map(|event| event["item"]["text"].as_str().unwrap_or_default())
        .collect();
    assert_eq!(answers, [text], "{events:?}");
}

fn waits(events: &[Value]) -> Vec<&str> {
    events
        .iter()
        .filter(|event| event["type"] == "turn.waiting")
        .filter_map(|event| event["message"].as_str())
        .collect()
}

fn too_many(retry_after: Option<&str>) -> ResponseTemplate {
    let response = ResponseTemplate::new(429)
        .set_body_json(json!({"error": {"type": "requests", "message": "Rate limit reached"}}));
    match retry_after {
        Some(seconds) => response.insert_header("retry-after", seconds),
        None => response,
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_429_waits_for_its_retry_after_and_then_finishes() {
    let server = MockServer::start().await;
    let script = Script::new(vec![too_many(Some("2")), ok(answer("done"))]);
    Mock::given(method("POST"))
        .and(path("/v1/responses"))
        .respond_with(script.clone())
        .mount(&server)
        .await;

    let (output, events) = run(&server, "").await;
    finished(&output, &events, "done");
    assert_eq!(waits(&events), ["waiting for rate limit, retrying in 2s"]);
    let arrivals = script.arrivals();
    assert_eq!(arrivals.len(), 2, "one 429, one answer");
    let waited = arrivals[1] - arrivals[0];
    assert!(
        waited >= Duration::from_millis(1900),
        "retried after {waited:?}, not 2s"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn rate_limits_and_overloads_are_retried_past_every_retry_budget() {
    let server = MockServer::start().await;
    let overloaded = ResponseTemplate::new(503)
        .set_body_json(json!({"error": {"code": "server_is_overloaded", "message": "busy"}}));
    let script = Script::new(vec![
        too_many(None),
        overloaded,
        too_many(None),
        too_many(None),
        ok(answer("done")),
    ]);
    Mock::given(method("POST"))
        .and(path("/v1/responses"))
        .respond_with(script.clone())
        .mount(&server)
        .await;

    // No retry budget at all: any refusal would end the turn if a budget applied.
    let (output, events) = run(&server, "request_max_retries = 0\nstream_max_retries = 0\n").await;
    finished(&output, &events, "done");
    assert_eq!(script.arrivals().len(), 5);
    let waits = waits(&events);
    assert_eq!(waits.len(), 4, "{waits:?}");
    assert!(
        waits[1].starts_with("waiting for server capacity, retrying in "),
        "{waits:?}"
    );
    for wait in [waits[0], waits[2], waits[3]] {
        assert!(
            wait.starts_with("waiting for rate limit, retrying in "),
            "{waits:?}"
        );
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_spent_budget_holds_the_next_request_until_it_resets() {
    let server = MockServer::start().await;
    let spent = ok(command())
        .insert_header("x-ratelimit-limit-requests", "40")
        .insert_header("x-ratelimit-remaining-requests", "0")
        .insert_header("x-ratelimit-reset-requests", "3s");
    let script = Script::new(vec![spent, ok(answer("done"))]);
    Mock::given(method("POST"))
        .and(path("/v1/responses"))
        .respond_with(script.clone())
        .mount(&server)
        .await;

    let (output, events) = run(&server, "").await;
    finished(&output, &events, "done");
    assert!(
        waits(&events).is_empty(),
        "pacing is not a retry: {events:?}"
    );
    let arrivals = script.arrivals();
    assert_eq!(arrivals.len(), 2, "the command's output went back once");
    let held = arrivals[1] - arrivals[0];
    assert!(
        held >= Duration::from_millis(2800),
        "sent after {held:?}, before the reset"
    );
}
