//! Automatic approval review and memory run on models the provider serves.
//!
//! Upstream's own ids for these jobs are OpenAI's, and the bundled catalog keeps
//! OpenAI's entries, unlisted, in every provider's catalog. A reviewer that took
//! "in the catalog" for "served" asked api.hanzo.ai and the LAN engines for a
//! model neither has.

use std::process::Stdio;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

use codex_model_provider::create_model_provider;
use codex_model_provider_info::ModelProviderInfo;
use codex_protocol::openai_models::ModelVisibility;
use serde_json::Value;
use serde_json::json;
use wiremock::Mock;
use wiremock::MockServer;
use wiremock::Request;
use wiremock::Respond;
use wiremock::ResponseTemplate;
use wiremock::matchers::method;
use wiremock::matchers::path;

fn provider(table: &str) -> ModelProviderInfo {
    toml::from_str(table).expect("a provider table parses")
}

fn hanzo() -> ModelProviderInfo {
    let config: toml::Value =
        toml::from_str(hanzo_config::DEFAULT_CONFIG).expect("the default config parses");
    config["model_providers"]["hanzo"]
        .clone()
        .try_into()
        .expect("the default config's provider parses")
}

#[test]
fn hanzo_names_a_model_it_serves_for_review_and_memory() {
    let hanzo = create_model_provider(hanzo(), None);
    for model in [
        hanzo.approval_review_preferred_model(),
        hanzo.memory_extraction_preferred_model(),
        hanzo.memory_consolidation_preferred_model(),
    ] {
        assert_eq!(model, hanzo_config::DEFAULT_MODEL);
        let listed = codex_models_manager::bundled_models_response()
            .expect("the bundled catalog parses")
            .models
            .into_iter()
            .any(|known| known.slug == model && matches!(known.visibility, ModelVisibility::List));
        assert!(
            listed,
            "{model} is listed in the catalog api.hanzo.ai serves"
        );
    }
}

#[test]
fn only_hanzo_takes_the_hanzo_choice() {
    let openai = create_model_provider(ModelProviderInfo::create_openai_provider(None), None);
    let lan = create_model_provider(
        provider(
            "name = \"LAN\"\nbase_url = \"http://dgx.local:1235/v1\"\nwire_api = \"responses\"",
        ),
        None,
    );
    for other in [openai, lan] {
        assert_ne!(
            other.approval_review_preferred_model(),
            hanzo_config::DEFAULT_MODEL
        );
        assert_ne!(
            other.memory_extraction_preferred_model(),
            hanzo_config::DEFAULT_MODEL
        );
        assert_ne!(
            other.memory_consolidation_preferred_model(),
            hanzo_config::DEFAULT_MODEL
        );
    }
}

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

fn created(id: &str) -> Value {
    json!({"type": "response.created", "response": {"id": id}})
}

fn completed(id: &str) -> Value {
    json!({
        "type": "response.completed",
        "response": {
            "id": id,
            "usage": {"input_tokens": 0, "input_tokens_details": null, "output_tokens": 0,
                      "output_tokens_details": null, "total_tokens": 0}
        }
    })
}

fn message(id: &str, text: &str) -> Value {
    json!({
        "type": "response.output_item.done",
        "item": {"type": "message", "role": "assistant", "id": id,
                 "content": [{"type": "output_text", "text": text}]}
    })
}

/// A Responses engine that asks once for an escalated command, allows it when
/// asked to review, and finishes once it sees the command's output.
#[derive(Clone, Default)]
struct Engine {
    reviews: Arc<Mutex<Vec<String>>>,
    outputs: Arc<Mutex<Vec<String>>>,
}

impl Respond for Engine {
    fn respond(&self, request: &Request) -> ResponseTemplate {
        let body: Value = serde_json::from_slice(&request.body).unwrap_or_default();
        let reviewer = body["client_metadata"]["x-openai-subagent"] == "guardian"
            || request
                .headers
                .get("x-openai-subagent")
                .and_then(|value| value.to_str().ok())
                == Some("guardian");
        let outputs: Vec<String> = body["input"]
            .as_array()
            .into_iter()
            .flatten()
            .filter(|item| item["type"] == "function_call_output")
            .map(|item| item["output"].to_string())
            .collect();
        let events = if reviewer {
            let model = body["model"].as_str().unwrap_or_default().to_string();
            self.reviews.lock().expect("reviews").push(model);
            vec![
                created("review"),
                message("assessment", r#"{"outcome":"allow"}"#),
                completed("review"),
            ]
        } else if !outputs.is_empty() {
            self.outputs.lock().expect("outputs").extend(outputs);
            vec![created("done"), message("reply", "done"), completed("done")]
        } else {
            let arguments = json!({
                "cmd": "printf reviewed",
                "sandbox_permissions": "require_escalated",
                "justification": "Run the step the user asked for.",
            });
            vec![
                created("act"),
                json!({
                    "type": "response.output_item.done",
                    "item": {"type": "function_call", "call_id": "act", "name": "exec_command",
                             "arguments": arguments.to_string()}
                }),
                completed("act"),
            ]
        };
        ResponseTemplate::new(200)
            .insert_header("content-type", "text/event-stream")
            .set_body_string(sse(&events))
    }
}

/// A provider that is neither OpenAI nor Hanzo and has no catalog of its own (a
/// LAN engine named with `-c model_provider=`) is reviewed by the model it is
/// already running, never by an id it was not asked to serve.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn a_provider_off_hanzo_reviews_on_the_model_it_runs() {
    let server = MockServer::start().await;
    let engine = Engine::default();
    Mock::given(method("POST"))
        .and(path("/v1/responses"))
        .respond_with(engine.clone())
        .mount(&server)
        .await;

    let home = tempfile::tempdir().expect("a home");
    let work = tempfile::tempdir().expect("a project");
    std::fs::write(
        home.path().join("config.toml"),
        format!(
            "model_provider = \"lan\"\n\n[model_providers.lan]\nname = \"LAN\"\nbase_url = \"{}/v1\"\nwire_api = \"responses\"\n",
            server.uri()
        ),
    )
    .expect("the profile is writable");

    let run = tokio::process::Command::new(env!("CARGO_BIN_EXE_dev"))
        .args([
            "exec",
            "--skip-git-repo-check",
            "--approve-for-me",
            "-m",
            "zen6",
            "-C",
        ])
        .arg(work.path())
        .arg("run the protected step")
        .env("DEV_HOME", home.path())
        .stdin(Stdio::null())
        .output();
    let output = tokio::time::timeout(Duration::from_secs(180), run)
        .await
        .expect("the run finishes")
        .expect("dev runs");
    let reviews = engine.reviews.lock().expect("reviews").clone();
    let outputs = engine.outputs.lock().expect("outputs").clone();
    assert!(
        output.status.success(),
        "dev exec failed\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(!reviews.is_empty(), "the escalated command was reviewed");
    assert!(
        reviews.iter().all(|model| model == "zen6"),
        "reviewed on {reviews:?}"
    );
    assert!(
        outputs.iter().any(|output| output.contains("reviewed")),
        "the allowed command ran: {outputs:?}"
    );
}
