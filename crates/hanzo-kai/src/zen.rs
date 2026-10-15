//! Zen through an OpenAI-compatible endpoint: the only model calls a flow makes, and only to
//! write code or text. The endpoint is Hanzo's (`https://api.hanzo.ai/v1`, signed with the
//! credential Dev already uses) or any server that speaks `/chat/completions`, a local Zen 6
//! among them; the credential is sent to Hanzo's alone.
//!
//! A call streams, so the time to the first token and the time to the last are both known, and
//! each paid call is a line in the spend file with the model Hanzo says served it.
//!
//! Every answered call is kept under `calls`, named by the SHA-256 of its endpoint, model and
//! canonical request body, with the request, the response and the timings. A request that was
//! answered before is answered from there, makes no call and costs nothing, and its [`Generation`]
//! says `cached`: its times are those of the lookup, not of the model.

use hanzo_config::kai::Generate;
use serde_json::Value;
use serde_json::json;
use sha2::Digest;
use sha2::Sha256;
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

/// Tokens an answer may be given room for.
const TOKENS: u32 = 512;
/// Tokens an answer that reasons first may be given room for.
const THOUGHT: u32 = 3072;
/// The longest one call may take, by effort: an answer that has not come by then is not coming.
const LOW: Duration = Duration::from_secs(60);
const FULL: Duration = Duration::from_secs(120);
/// What an unpriced call is counted at, dollars per million tokens: above any model this flow
/// calls (`openai/gpt-6-sol`'s listed rate), so the budget is a ceiling on the spend and not a
/// guess at it.
const RATE: (f64, f64) = (2.11, 10.55);
/// Tries on a server that says it is busy.
const TRIES: u32 = 3;

/// How much the model reasons before it answers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Effort {
    /// None: the answer is the edit and nothing else.
    Low,
    /// The model's own reasoning first.
    Full,
}

/// One answer and what it took.
#[derive(Debug, Clone, PartialEq)]
pub struct Generation {
    pub text: String,
    /// Request sent to the first token, milliseconds.
    pub first: f64,
    /// Request sent to the last token, milliseconds; retries included.
    pub total: f64,
    pub prompt: u64,
    pub completion: u64,
    /// The model the server says answered.
    pub served: String,
    /// The model the request named.
    pub model: String,
    /// Answered from a call kept earlier: nothing was sent, nothing was spent.
    pub cached: bool,
}

/// `v` as JSON with every object's keys in order, so equal requests spell alike.
pub fn canonical(v: &Value) -> String {
    match v {
        Value::Object(map) => {
            let mut keys: Vec<&String> = map.keys().collect();
            keys.sort();
            let parts: Vec<String> = keys
                .into_iter()
                .map(|k| format!("{}:{}", Value::String(k.clone()), canonical(&map[k])))
                .collect();
            format!("{{{}}}", parts.join(","))
        }
        Value::Array(items) => format!("[{}]", items.iter().map(canonical).collect::<Vec<_>>().join(",")),
        other => other.to_string(),
    }
}

/// The name a call is kept under: the SHA-256 of its endpoint, model and canonical request body.
pub fn key(endpoint: &str, model: &str, body: &Value) -> String {
    let mut hash = Sha256::new();
    for part in [endpoint, model, &canonical(body)] {
        hash.update(part.len().to_le_bytes());
        hash.update(part.as_bytes());
    }
    hash.finalize().iter().map(|b| format!("{b:02x}")).collect()
}

/// Zen over `POST {url}/chat/completions` as `model`.
pub struct Zen {
    url: String,
    model: String,
    escalate: Option<String>,
    key: Option<String>,
    cloud: bool,
    spend: Option<(PathBuf, f64)>,
    calls: Option<PathBuf>,
    client: reqwest::Client,
    runtime: tokio::runtime::Handle,
}

impl Zen {
    /// Zen as `generate` names it, signed with `key` when its URL is Hanzo's. Call from within
    /// the async runtime; generation then blocks a thread off it.
    pub fn new(generate: &Generate, key: Option<String>) -> Zen {
        let cloud = hanzo_config::is_hanzo(Some(&generate.url));
        Zen {
            url: generate.url.trim_end_matches('/').to_string(),
            model: generate.model.clone(),
            escalate: generate.escalate.clone(),
            key: key.filter(|_| cloud),
            cloud,
            spend: cloud
                .then(|| (generate.spend.clone(), generate.budget.unwrap_or(f64::INFINITY))),
            calls: generate.calls.clone(),
            client: reqwest::Client::builder()
                .connect_timeout(Duration::from_secs(5))
                .timeout(Duration::from_secs(600))
                .build()
                .unwrap_or_default(),
            runtime: tokio::runtime::Handle::current(),
        }
    }

    /// The model that writes the first attempts.
    pub fn model(&self) -> &str {
        &self.model
    }

    /// The model an escalated attempt is written by, when there is one.
    pub fn escalation(&self) -> Option<&str> {
        self.escalate.as_deref()
    }

    /// Dollars the spend file holds so far.
    fn spent(path: &PathBuf) -> f64 {
        std::fs::read_to_string(path)
            .unwrap_or_default()
            .lines()
            .filter_map(|l| serde_json::from_str::<Value>(l).ok()?["cost"].as_f64())
            .sum()
    }

    fn record(&self, g: &Generation, headers: &Value) {
        let Some((path, _)) = &self.spend else {
            return;
        };
        let cost = (g.prompt as f64 * RATE.0 + g.completion as f64 * RATE.1) / 1e6;
        let line = json!({
            "time": crate::trace::now(),
            "model": g.model,
            "served": g.served,
            "prompt_tokens": g.prompt,
            "completion_tokens": g.completion,
            "ms": g.total,
            "cost": cost,
            "cost_basis": format!("ceiling at ${}/M in, ${}/M out", RATE.0, RATE.1),
            "headers": headers,
        });
        if let Some(dir) = path.parent() {
            let _ = std::fs::create_dir_all(dir);
        }
        if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(path) {
            let _ = writeln!(f, "{line}");
        }
    }

    /// The request `model` is asked for `user` under `system`.
    fn body(&self, model: &str, system: &str, user: &str, effort: Effort) -> Value {
        let mut body = json!({
            "model": model,
            "temperature": 0,
            "max_tokens": if effort == Effort::Full { THOUGHT } else { TOKENS },
            "stream": true,
            "stream_options": {"include_usage": true},
            "messages": [
                {"role": "system", "content": system},
                {"role": "user", "content": user},
            ],
        });
        // At low effort the answer is the edit and nothing else: no reasoning is bought, and the
        // reply is one JSON object.
        if effort == Effort::Low {
            body["response_format"] = json!({"type": "json_object"});
        }
        match (self.cloud, effort) {
            (true, Effort::Low) => body["reasoning_effort"] = json!("none"),
            (true, Effort::Full) => body["reasoning_effort"] = json!("medium"),
            (false, Effort::Low) => body["chat_template_kwargs"] = json!({"enable_thinking": false}),
            (false, Effort::Full) => body["chat_template_kwargs"] = json!({"enable_thinking": true}),
        }
        body
    }

    async fn once(&self, model: &str, body: &Value, effort: Effort) -> Result<(Generation, Value), String> {
        let mut request = self
            .client
            .post(format!("{}/chat/completions", self.url))
            .timeout(if effort == Effort::Full { FULL } else { LOW })
            .json(body);
        if let Some(key) = &self.key {
            request = request.bearer_auth(key);
        }
        let begun = Instant::now();
        let mut response = request.send().await.map_err(|e| format!("zen: {}", e.without_url()))?;
        let status = response.status();
        let headers = json!({
            "served": response.headers().get("x-hanzo-served").and_then(|v| v.to_str().ok()),
            "lane": response.headers().get("x-hanzo-lane").and_then(|v| v.to_str().ok()),
            "paid_by": response.headers().get("x-hanzo-paid-by").and_then(|v| v.to_str().ok()),
        });
        if !status.is_success() {
            let body = response.text().await.unwrap_or_default();
            let why: Value = serde_json::from_str(&body).unwrap_or_default();
            let msg = why["error"]["message"]
                .as_str()
                .or_else(|| why["error"].as_str())
                .map(str::to_string)
                .unwrap_or_else(|| crate::decide::cut(&body, 200));
            return Err(format!("zen: {status}: {msg}"));
        }
        let (mut pending, mut answer, mut first) = (String::new(), String::new(), None);
        let (mut prompt_tokens, mut completion_tokens, mut served) = (0, 0, String::new());
        while let Some(chunk) = response.chunk().await.map_err(|e| format!("zen: {}", e.without_url()))? {
            pending.push_str(&String::from_utf8_lossy(&chunk));
            while let Some(at) = pending.find('\n') {
                let line: String = pending.drain(..=at).collect();
                let Some(data) = line.trim().strip_prefix("data:") else {
                    continue;
                };
                let data = data.trim();
                if data == "[DONE]" || data.is_empty() {
                    continue;
                }
                let Ok(event) = serde_json::from_str::<Value>(data) else {
                    continue;
                };
                let delta = &event["choices"][0]["delta"];
                let thought = delta["reasoning_content"].as_str().or_else(|| delta["reasoning"].as_str());
                if thought.is_some_and(|t| !t.is_empty()) {
                    first.get_or_insert_with(|| begun.elapsed().as_secs_f64() * 1000.0);
                }
                if let Some(piece) = delta["content"].as_str()
                    && !piece.is_empty()
                {
                    first.get_or_insert_with(|| begun.elapsed().as_secs_f64() * 1000.0);
                    answer.push_str(piece);
                }
                if let Some(u) = event.get("usage").filter(|u| u.is_object()) {
                    prompt_tokens = u["prompt_tokens"].as_u64().unwrap_or(prompt_tokens);
                    completion_tokens = u["completion_tokens"].as_u64().unwrap_or(completion_tokens);
                }
                if let Some(m) = event["model"].as_str() {
                    served = m.to_string();
                }
            }
        }
        let total = begun.elapsed().as_secs_f64() * 1000.0;
        if answer.is_empty() {
            return Err("zen: an answer without text".into());
        }
        if let Some(s) = headers["served"].as_str() {
            served = s.to_string();
        }
        Ok((
            Generation {
                text: answer,
                first: first.unwrap_or(total),
                total,
                prompt: prompt_tokens,
                completion: completion_tokens,
                served,
                model: model.to_string(),
                cached: false,
            },
            headers,
        ))
    }

    /// The call kept for `name`, when there is one.
    fn kept(&self, name: &str) -> Option<Generation> {
        let dir = self.calls.as_ref()?;
        let doc: Value = serde_json::from_str(&std::fs::read_to_string(dir.join(format!("{name}.json"))).ok()?).ok()?;
        let r = &doc["response"];
        Some(Generation {
            text: r["text"].as_str().filter(|t| !t.is_empty())?.to_string(),
            first: 0.0,
            total: 0.0,
            prompt: r["prompt_tokens"].as_u64()?,
            completion: r["completion_tokens"].as_u64()?,
            served: r["served"].as_str()?.to_string(),
            model: doc["model"].as_str()?.to_string(),
            cached: true,
        })
    }

    /// Keeps an answered call: its request, its response and its timings.
    fn keep(&self, name: &str, body: &Value, g: &Generation, headers: &Value) {
        let Some(dir) = &self.calls else {
            return;
        };
        let doc = json!({
            "key": name,
            "endpoint": self.url,
            "model": g.model,
            "request": body,
            "response": {"text": g.text, "prompt_tokens": g.prompt, "completion_tokens": g.completion, "served": g.served},
            "timings": {"first_ms": g.first, "total_ms": g.total},
            "headers": headers,
            "time": crate::trace::now(),
        });
        let _ = std::fs::create_dir_all(dir);
        let tmp = dir.join(format!("{name}.tmp{}", std::process::id()));
        if std::fs::write(&tmp, doc.to_string()).is_ok() {
            let _ = std::fs::rename(&tmp, dir.join(format!("{name}.json")));
        }
    }

    async fn complete(&self, model: &str, system: &str, user: &str, effort: Effort) -> Result<Generation, String> {
        let body = self.body(model, system, user, effort);
        let name = key(&self.url, model, &body);
        let looked = Instant::now();
        if let Some(mut g) = self.kept(&name) {
            let took = looked.elapsed().as_secs_f64() * 1000.0;
            (g.first, g.total) = (took, took);
            return Ok(g);
        }
        if let Some((path, budget)) = &self.spend
            && Self::spent(path) >= *budget
        {
            return Err(format!("zen: the budget of ${budget:.2} is spent ({})", path.display()));
        }
        let begun = Instant::now();
        let mut last = String::new();
        for attempt in 0..TRIES {
            match self.once(model, &body, effort).await {
                Ok((mut g, headers)) => {
                    // A retry's wait is part of the time the answer took.
                    g.total = begun.elapsed().as_secs_f64() * 1000.0;
                    self.record(&g, &headers);
                    self.keep(&name, &body, &g, &headers);
                    return Ok(g);
                }
                Err(e) if e.contains("503") || e.contains("502") || e.contains("429") => {
                    last = e;
                    tokio::time::sleep(Duration::from_millis(500 * u64::from(attempt + 1))).await;
                }
                Err(e) => return Err(e),
            }
        }
        Err(last)
    }

    /// `model`'s answer to `user` under `system`. Blocks: call it off the async runtime.
    pub fn generate(&self, model: &str, system: &str, user: &str, effort: Effort) -> Result<Generation, String> {
        self.runtime.block_on(self.complete(model, system, user, effort))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing;
    use pretty_assertions::assert_eq;
    use std::path::Path;

    #[test]
    fn a_calls_name_is_the_hash_of_endpoint_model_and_body_whatever_the_key_order() {
        let a = json!({"model": "m", "messages": [{"role": "user", "content": "x"}], "temperature": 0});
        let b = json!({"temperature": 0, "messages": [{"content": "x", "role": "user"}], "model": "m"});
        assert_eq!(canonical(&a), canonical(&b));
        let name = key("https://api.hanzo.ai/v1", "m", &a);
        assert_eq!(name.len(), 64);
        assert_eq!(name, key("https://api.hanzo.ai/v1", "m", &b));
        assert!(name != key("https://api.hanzo.ai/v1", "n", &a), "the model");
        assert!(name != key("http://127.0.0.1/v1", "m", &a), "the endpoint");
        assert!(name != key("https://api.hanzo.ai/v1", "m", &json!({"model": "m", "temperature": 1})), "the body");
        // The parts cannot run together: moving a character across a boundary moves the name.
        assert!(key("ab", "c", &a) != key("a", "bc", &a));
    }

    fn zen_at(server: &wiremock::MockServer, calls: &Path, spend: &Path) -> Zen {
        // A server that is not Hanzo's: no credential, and no spend file.
        let generate = Generate {
            url: format!("{}/v1", server.uri()),
            model: "base".into(),
            spend: spend.to_path_buf(),
            budget: None,
            escalate: Some("strong".into()),
            calls: Some(calls.to_path_buf()),
        };
        Zen::new(&generate, None)
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn a_repeated_request_is_answered_from_the_kept_call_and_says_so() {
        let server = testing::zen(|_| "{\"edits\":[]}".to_string()).await;
        let home = tempfile::tempdir().unwrap();
        let (calls, spend) = (home.path().join("calls"), home.path().join("spend.jsonl"));
        let zen = zen_at(&server, &calls, &spend);
        let first = tokio::task::block_in_place(|| zen.generate("base", "sys", "user", Effort::Low)).unwrap();
        assert!(!first.cached);
        assert_eq!((first.prompt, first.completion, first.model.as_str()), (100, 20, "base"));
        let again = tokio::task::block_in_place(|| zen.generate("base", "sys", "user", Effort::Low)).unwrap();
        assert!(again.cached);
        assert_eq!((again.text.as_str(), again.prompt, again.completion, again.served.as_str()), (first.text.as_str(), 100, 20, "zen-test"));
        assert!(again.total < 1000.0, "the lookup's time, not the model's: {}", again.total);
        assert_eq!(server.received_requests().await.unwrap().len(), 1, "no second call was sent");
        // Another model, effort or text is another call.
        tokio::task::block_in_place(|| zen.generate("strong", "sys", "user", Effort::Low)).unwrap();
        tokio::task::block_in_place(|| zen.generate("base", "sys", "user", Effort::Full)).unwrap();
        tokio::task::block_in_place(|| zen.generate("base", "sys", "other", Effort::Low)).unwrap();
        assert_eq!(server.received_requests().await.unwrap().len(), 4);
        // The file holds the request, the response and the timings.
        let name = key(&zen.url, "base", &zen.body("base", "sys", "user", Effort::Low));
        let doc: Value = serde_json::from_str(&std::fs::read_to_string(calls.join(format!("{name}.json"))).unwrap()).unwrap();
        assert_eq!(doc["request"]["model"], "base");
        assert_eq!(doc["response"]["text"], "{\"edits\":[]}");
        assert!(doc["timings"]["total_ms"].as_f64().unwrap() > 0.0);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn a_failed_call_is_not_kept_and_a_damaged_one_is_asked_again() {
        let server = testing::zen(|_| "{}".to_string()).await;
        let home = tempfile::tempdir().unwrap();
        let (calls, spend) = (home.path().join("calls"), home.path().join("spend.jsonl"));
        let zen = zen_at(&server, &calls, &spend);
        let body = zen.body("base", "s", "u", Effort::Low);
        let name = key(&zen.url, "base", &body);
        std::fs::create_dir_all(&calls).unwrap();
        std::fs::write(calls.join(format!("{name}.json")), "not json").unwrap();
        let g = tokio::task::block_in_place(|| zen.generate("base", "s", "u", Effort::Low)).unwrap();
        assert!(!g.cached, "a file that does not read is a miss");
        assert!(zen.kept(&name).is_some(), "and is written again");
    }
}
