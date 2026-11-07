//! A Hanzo model's reasoning effort: `auto` unless set, and `auto` is no effort on the wire,
//! so api.hanzo.ai's router decides. A setting stored under a retired name (`none`, which the
//! /model picker once wrote as every Hanzo model's default) moves to `instant`.

use std::process::Stdio;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

use codex_protocol::openai_models::ReasoningEffort;
use serde_json::Value;
use serde_json::json;
use wiremock::Mock;
use wiremock::MockServer;
use wiremock::Request;
use wiremock::Respond;
use wiremock::ResponseTemplate;
use wiremock::matchers::method;
use wiremock::matchers::path;

#[test]
fn every_hanzo_model_starts_at_auto_and_offers_the_table_in_order() {
    let catalog =
        codex_models_manager::bundled_models_response().expect("the bundled catalog parses");
    let auto: ReasoningEffort = hanzo::effort::DEFAULT.parse().expect("auto parses");
    let table: Vec<String> = hanzo::effort::EFFORTS
        .iter()
        .map(|(name, _)| name.to_string())
        .collect();
    for slug in [hanzo::DEFAULT_MODEL, "enso-auto", "enso-pro", "zen6"] {
        let model = catalog
            .models
            .iter()
            .find(|model| model.slug == slug)
            .unwrap_or_else(|| panic!("{slug} is in the catalog"));
        assert_eq!(model.default_reasoning_level, Some(auto.clone()), "{slug}");
        let offered: Vec<String> = model
            .supported_reasoning_levels
            .iter()
            .map(|preset| preset.effort.to_string())
            .collect();
        assert_eq!(offered, table, "{slug}");
    }
}

/// Records each request's `reasoning` and answers with one message.
#[derive(Clone, Default)]
struct Engine {
    reasoning: Arc<Mutex<Vec<Value>>>,
}

impl Respond for Engine {
    fn respond(&self, request: &Request) -> ResponseTemplate {
        let body: Value = serde_json::from_slice(&request.body).unwrap_or_default();
        self.reasoning
            .lock()
            .expect("reasoning")
            .push(body["reasoning"].clone());
        let events = [
            json!({"type": "response.created", "response": {"id": "r"}}),
            json!({"type": "response.output_item.done", "item": {"type": "message", "role": "assistant",
                "id": "m", "content": [{"type": "output_text", "text": "done"}]}}),
            json!({"type": "response.completed", "response": {"id": "r", "usage": {"input_tokens": 1,
                "input_tokens_details": null, "output_tokens": 1, "output_tokens_details": null, "total_tokens": 2}}}),
        ];
        let body: String = events
            .iter()
            .map(|event| {
                format!(
                    "event: {}\ndata: {event}\n\n",
                    event["type"].as_str().unwrap_or_default()
                )
            })
            .collect();
        ResponseTemplate::new(200)
            .insert_header("content-type", "text/event-stream")
            .set_body_string(body)
    }
}

/// One `dev exec` task on enso-auto with `stored` at the top of the profile; returns the
/// reasoning each request carried and the profile as the run left it.
async fn run(stored: &str, args: &[&str]) -> (Vec<Value>, String) {
    let server = MockServer::start().await;
    let engine = Engine::default();
    Mock::given(method("POST"))
        .and(path("/v1/responses"))
        .respond_with(engine.clone())
        .mount(&server)
        .await;
    let home = tempfile::tempdir().expect("a home");
    let work = tempfile::tempdir().expect("a project");
    let config = home.path().join("config.toml");
    std::fs::write(
        &config,
        format!(
            "{stored}model_provider = \"mock\"\nmodel = \"enso-auto\"\n\n[model_providers.mock]\nname = \"Mock\"\nbase_url = \"{}/v1\"\nwire_api = \"responses\"\n",
            server.uri()
        ),
    )
    .expect("the profile is writable");
    let run = tokio::process::Command::new(env!("CARGO_BIN_EXE_dev"))
        .args(["exec", "--skip-git-repo-check"])
        .args(args)
        .arg("-C")
        .arg(work.path())
        .arg("go")
        .env("DEV_HOME", home.path())
        .stdin(Stdio::null())
        .output();
    let output = tokio::time::timeout(Duration::from_secs(120), run)
        .await
        .expect("the run finishes")
        .expect("dev runs");
    assert!(
        output.status.success(),
        "dev exec failed\nstderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let reasoning = engine.reasoning.lock().expect("reasoning").clone();
    assert!(!reasoning.is_empty(), "the model was asked");
    (
        reasoning,
        std::fs::read_to_string(config).expect("the profile"),
    )
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn an_unset_effort_is_auto_and_sends_none_so_the_router_decides() {
    let (reasoning, _) = run("", &[]).await;
    for asked in &reasoning {
        assert!(
            asked.get("effort").is_none(),
            "auto sent an effort: {asked}"
        );
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_stored_none_moves_to_instant_and_is_sent_as_instant() {
    let (reasoning, profile) = run("model_reasoning_effort = \"none\"\n", &[]).await;
    assert!(
        profile.contains("model_reasoning_effort = \"instant\""),
        "{profile}"
    );
    for asked in &reasoning {
        assert_eq!(asked["effort"], "instant", "{asked}");
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn an_explicit_effort_is_sent_as_written() {
    let (reasoning, _) = run("", &["-c", "model_reasoning_effort=\"high\""]).await;
    for asked in &reasoning {
        assert_eq!(asked["effort"], "high", "{asked}");
    }
}
