//! For tests: Kai serving the Decisions API by a rule, Zen serving chat completions by a rule,
//! a thread that records what Kai did to it, and the trace's lines.

use crate::Settings;
use crate::agent::Act;
use crate::program::Kind;
use crate::program::Question;
use crate::program::argmax;
use hanzo_loop::Answer;
use indexmap::IndexMap;
use serde_json::Map;
use serde_json::Value;
use serde_json::json;
use std::path::Path;
use std::sync::Mutex;
use wiremock::Mock;
use wiremock::MockServer;
use wiremock::ResponseTemplate;
use wiremock::matchers::method;
use wiremock::matchers::path;

/// The calibration the shipped agent programs pin: an answer under another runs in shadow.
pub const CALIBRATION: &str = "cal_6dbb072f8109a222";

/// Kai answering each question by `rule(question id, question, state)` under `calibration`.
pub async fn kai_with(
    calibration: &str,
    rule: impl Fn(&str, &Question, &Value) -> Vec<f64> + Send + Sync + 'static,
) -> MockServer {
    let server = MockServer::start().await;
    let calibration = calibration.to_string();
    Mock::given(method("POST"))
        .and(path("/v1/decisions"))
        .respond_with(move |request: &wiremock::Request| {
            let body: Value = serde_json::from_slice(&request.body).unwrap();
            let questions: IndexMap<String, Question> =
                serde_json::from_value(body["questions"].clone()).unwrap();
            let answers: Map<String, Value> = questions
                .iter()
                .map(|(id, q)| (id.clone(), answer(q, &rule(id, q, &body["state"]))))
                .collect();
            let model = body["model"].as_str().unwrap_or_default();
            ResponseTemplate::new(200).set_body_json(json!({
                "id": "dec_test",
                "model": model,
                "provider": "Hanzo",
                "answers": answers,
                "usage": {"input_tokens": 1, "output_tokens": 0},
                "routing": {
                    "backend": "kai",
                    "checkpoint": format!("{model}@test"),
                    "calibration": calibration,
                    "reason": "test",
                },
                "state_hash": "sha256:00",
                "latency_ms": 1.0,
            }))
        })
        .mount(&server)
        .await;
    server
}

/// Kai answering by `rule` under the calibration the agent programs pin.
pub async fn kai(
    rule: impl Fn(&str, &Question, &Value) -> Vec<f64> + Send + Sync + 'static,
) -> MockServer {
    kai_with(CALIBRATION, rule).await
}

/// A distribution in the Decisions answer shape.
fn answer(q: &Question, p: &[f64]) -> Value {
    let keys = q.keys();
    let (best, _) = argmax(p);
    let probabilities: Map<String, Value> = keys
        .iter()
        .zip(p)
        .map(|(k, p)| (k.clone(), json!(p)))
        .collect();
    match q.kind {
        Kind::Noul => json!({"type": "noul", "noul": p[1]}),
        Kind::Choice => {
            json!({"type": "choice", "choice": keys[best], "probabilities": probabilities})
        }
        Kind::Score => {
            let score: f64 = p.iter().enumerate().map(|(i, p)| i as f64 * p).sum();
            json!({"type": "score", "score": score, "probabilities": probabilities})
        }
    }
}

/// `kai.toml` as `text`, pointed at `server`.
pub fn settings(server: &MockServer, text: &str, home: &Path) -> Settings {
    let text = format!("url = \"{}/v1\"\n{text}", server.uri());
    Settings::parse(&text, home).unwrap()
}

/// How many decisions requests `server` has received.
pub async fn asked(server: &MockServer) -> usize {
    server
        .received_requests()
        .await
        .unwrap_or_default()
        .iter()
        .filter(|r| r.url.path() == "/v1/decisions")
        .count()
}

/// Probabilities putting `p` on `label` of `q`'s options and the rest evenly.
pub fn on(q: &Question, label: &str, p: f64) -> Vec<f64> {
    let keys = q.keys();
    let rest = (1.0 - p) / (keys.len() as f64 - 1.0).max(1.0);
    keys.iter()
        .map(|k| if k == label { p } else { rest })
        .collect()
}

/// Zen answering chat completions by `rule(inputs)`, the inputs being the JSON a flow sends.
pub async fn zen(rule: impl Fn(&Value) -> String + Send + Sync + 'static) -> MockServer {
    let server = MockServer::start().await;
    Mock::given(method("POST"))
        .and(path("/v1/chat/completions"))
        .respond_with(move |request: &wiremock::Request| {
            let content = inputs(request);
            ResponseTemplate::new(200).set_body_json(json!({
                "choices": [{"message": {"role": "assistant", "content": rule(&content)}}],
            }))
        })
        .mount(&server)
        .await;
    server
}

/// The inputs of a chat completion a flow sent.
pub fn inputs(request: &wiremock::Request) -> Value {
    let body: Value = serde_json::from_slice(&request.body).unwrap();
    let text = body["messages"][1]["content"].as_str().unwrap_or_default();
    let json = text
        .split_once("Inputs, as JSON:\n")
        .map_or("null", |(_, j)| j);
    serde_json::from_str(json).unwrap()
}

/// What Kai did to a thread.
#[derive(Default)]
pub struct Thread {
    pub warned: Mutex<Vec<String>>,
    pub steered: Mutex<Vec<String>>,
    pub resumed: Mutex<Vec<String>>,
}

impl Act for Thread {
    fn warn(&self, _turn: &str, message: String) {
        self.warned.lock().unwrap().push(message);
    }

    fn steer(&self, text: String) -> Answer<'_, ()> {
        self.steered.lock().unwrap().push(text);
        Box::pin(async {})
    }

    fn resume(&self, text: String) -> Answer<'_, bool> {
        self.resumed.lock().unwrap().push(text);
        Box::pin(async { true })
    }
}

/// The trace's lines, once there are `n`, waiting at most five seconds for them.
pub fn lines(path: &Path, n: usize) -> Vec<Value> {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    loop {
        let lines: Vec<Value> = std::fs::read_to_string(path)
            .unwrap_or_default()
            .lines()
            .map(|l| serde_json::from_str(l).unwrap())
            .collect();
        if lines.len() >= n || std::time::Instant::now() > deadline {
            return lines;
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
}
