//! Zen through an OpenAI-compatible endpoint: the only model calls a flow makes, and only to
//! write code or text. The endpoint is Hanzo's (`https://api.hanzo.ai/v1`, signed with the
//! credential Dev already uses) or any server that speaks `/chat/completions`, a local Zen 6
//! among them; the credential is sent to Hanzo's alone.
//!
//! A call streams, so the time to the first token and the time to the last are both known, and
//! each paid call is a line in the spend file with the model Hanzo says served it.
//!
//! Every call that reached the model is kept under `calls`, named by the SHA-256 of its endpoint,
//! model and canonical request body, with the request, the response (its finish reason and the
//! usage the server reported, reasoning tokens apart) and the timings. A request that was
//! answered before is answered from there, makes no call and costs nothing, and its [`Generation`]
//! says `cached`: its times are those of the lookup, not of the model. A call that came back
//! without an answer is kept with its error and is asked again; its tokens are spent and counted.
//!
//! An edit is asked for as a JSON schema (`response_format`, strict) where the server is Hanzo's,
//! which constrains the decoding: the answer cannot be prose, nor a string with a bare quote in it.
//!
//! A call may be begun before the loop needs it ([`Zen::start`]) and waited for later. Dropping
//! the [`Pending`] cancels it: the stream is closed, so the server stops writing and billing it,
//! and the spend file gets a line for what it was sent (`cancelled`, its prompt counted from the
//! request's characters at the ceiling rate, as every unpriced call is).

use hanzo_config::kai::Generate;
use serde_json::Value;
use serde_json::json;
use sha2::Digest;
use sha2::Sha256;
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

/// Tokens an answer that does not reason may be given room for: the edit, and a generator's
/// thinking aloud before it (700 to 900 tokens, where 512 cut the JSON off).
const TOKENS: u32 = 2048;
/// Tokens an answer that reasons first may be given room for: the reasoning and the answer share
/// this one cap. Measured on `openai/gpt-6-sol` over ten failing tests, a cap of 3,072 at medium
/// effort lost five answers of ten to reasoning alone (finish length, no text); the reasoning ran
/// 221 to 3,106 tokens at medium and 58 to 1,001 at low.
const THOUGHT: u32 = 16384;
/// The longest one call may take, by effort: an answer that has not come by then is not coming.
const LOW: Duration = Duration::from_secs(60);
const FULL: Duration = Duration::from_secs(240);
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
    /// Request sent to the last token of the answer's text, milliseconds.
    pub last: f64,
    /// Request sent to the end of the stream, milliseconds; retries included.
    pub total: f64,
    pub prompt: u64,
    pub completion: u64,
    /// The model the server says answered.
    pub served: String,
    /// Why the server stopped: `stop`, or `length` when the tokens ran out.
    pub finish: String,
    /// Tokens of `completion` the model spent reasoning, when the server says.
    pub reasoning: u64,
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

/// The edits an answer holds, as a strict JSON schema: what [`crate::edit::parse`] reads.
fn schema() -> Value {
    json!({
        "type": "json_schema",
        "json_schema": {
            "name": "edits",
            "strict": true,
            "schema": {
                "type": "object",
                "additionalProperties": false,
                "required": ["edits"],
                "properties": {
                    "edits": {
                        "type": "array",
                        "minItems": 1,
                        "items": {
                            "type": "object",
                            "additionalProperties": false,
                            "required": ["file", "line", "old", "new"],
                            "properties": {
                                "file": {"type": "string"},
                                "line": {"type": "integer"},
                                "old": {"type": "string"},
                                "new": {"type": "string"}
                            }
                        }
                    }
                }
            }
        }
    })
}

/// Zen over `POST {url}/chat/completions` as `model`.
#[derive(Clone)]
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
        let cloud = hanzo::is_hanzo(Some(&generate.url));
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

    /// The model an escalated attempt that has no edit falls back to: the generator, and never an
    /// Anthropic model (the rule `hanzo-config` holds `generate.escalate` to).
    pub fn fallback(&self) -> Option<&str> {
        let lower = self.model.to_lowercase();
        (!lower.contains("anthropic") && !lower.contains("claude")).then_some(self.model.as_str())
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
        Self::append(path, &line);
    }

    /// The line a call cancelled before its end leaves in the spend file: the prompt it was sent,
    /// counted at three characters a token (more tokens than text takes, so the budget stays a
    /// ceiling), and the pieces it had streamed, a token each.
    fn record_cut(&self, model: &str, body: &Value, streamed: u64, ms: f64) {
        let Some((path, _)) = &self.spend else {
            return;
        };
        let chars: usize = body["messages"]
            .as_array()
            .map(|m| m.iter().filter_map(|x| x["content"].as_str()).map(str::len).sum())
            .unwrap_or_default();
        let prompt = chars.div_ceil(3) as u64;
        let line = json!({
            "time": crate::trace::now(),
            "model": model,
            "cancelled": true,
            "prompt_tokens": prompt,
            "completion_tokens": streamed,
            "ms": ms,
            "cost": (prompt as f64 * RATE.0 + streamed as f64 * RATE.1) / 1e6,
            "cost_basis": format!("estimated: the prompt at 3 characters a token, a token a piece streamed, at ${}/M in, ${}/M out", RATE.0, RATE.1),
        });
        Self::append(path, &line);
    }

    fn append(path: &PathBuf, line: &Value) {
        if let Some(dir) = path.parent() {
            let _ = std::fs::create_dir_all(dir);
        }
        if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open(path) {
            let _ = writeln!(f, "{line}");
        }
    }

    /// The request `model` is asked for `user` under `system`.
    fn body(&self, model: &str, system: &str, user: &str, effort: Effort) -> Value {
        let max_tokens = if effort == Effort::Full { THOUGHT } else { TOKENS };
        let mut body = json!({
            "model": model,
            "temperature": 0,
            "max_tokens": max_tokens,
            "max_completion_tokens": max_tokens,
            "stream": true,
            "stream_options": {"include_usage": true},
            "messages": [
                {"role": "system", "content": system},
                {"role": "user", "content": user},
            ],
        });
        // The answer is the edit and nothing else. Hanzo's API constrains the decoding to the
        // schema; a server of another kind is asked for a JSON object.
        if self.cloud {
            body["response_format"] = schema();
        } else if effort == Effort::Low {
            body["response_format"] = json!({"type": "json_object"});
        }
        match (self.cloud, effort) {
            (true, Effort::Low) => body["reasoning_effort"] = json!("none"),
            (true, Effort::Full) => body["reasoning_effort"] = json!("low"),
            (false, Effort::Low) => body["chat_template_kwargs"] = json!({"enable_thinking": false}),
            (false, Effort::Full) => body["chat_template_kwargs"] = json!({"enable_thinking": true}),
        }
        body
    }

    /// One request. A reply that is not an error is a [`Generation`] whether or not it holds text:
    /// its tokens were spent either way.
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
        // Until the stream has ended, a drop of this future is a cancelled call: it leaves its
        // line in the spend file.
        let mut cut = Cut { zen: self, model, body, streamed: 0, begun, armed: true };
        let mut response = match request.send().await {
            Ok(r) => r,
            Err(e) => {
                cut.armed = false;
                return Err(format!("zen: {}", e.without_url()));
            }
        };
        let status = response.status();
        let headers = json!({
            "served": response.headers().get("x-hanzo-served").and_then(|v| v.to_str().ok()),
            "lane": response.headers().get("x-hanzo-lane").and_then(|v| v.to_str().ok()),
            "paid_by": response.headers().get("x-hanzo-paid-by").and_then(|v| v.to_str().ok()),
        });
        if !status.is_success() {
            // A request the server refused wrote nothing.
            cut.armed = false;
            let body = response.text().await.unwrap_or_default();
            let why: Value = serde_json::from_str(&body).unwrap_or_default();
            let msg = why["error"]["message"]
                .as_str()
                .or_else(|| why["error"].as_str())
                .map(str::to_string)
                .unwrap_or_else(|| crate::decide::cut(&body, 200));
            return Err(format!("zen: {status}: {msg}"));
        }
        let (mut pending, mut answer, mut first, mut last) = (String::new(), String::new(), None, 0.0);
        let (mut prompt_tokens, mut completion_tokens, mut reasoning_tokens) = (0, 0, 0);
        let (mut served, mut finish) = (String::new(), String::new());
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
                    cut.streamed += 1;
                }
                if let Some(piece) = delta["content"].as_str()
                    && !piece.is_empty()
                {
                    first.get_or_insert_with(|| begun.elapsed().as_secs_f64() * 1000.0);
                    last = begun.elapsed().as_secs_f64() * 1000.0;
                    cut.streamed += 1;
                    answer.push_str(piece);
                }
                if let Some(why) = event["choices"][0]["finish_reason"].as_str() {
                    finish = why.to_string();
                }
                if let Some(u) = event.get("usage").filter(|u| u.is_object()) {
                    prompt_tokens = u["prompt_tokens"].as_u64().unwrap_or(prompt_tokens);
                    completion_tokens = u["completion_tokens"].as_u64().unwrap_or(completion_tokens);
                    reasoning_tokens = u["completion_tokens_details"]["reasoning_tokens"].as_u64().unwrap_or(reasoning_tokens);
                }
                if let Some(m) = event["model"].as_str() {
                    served = m.to_string();
                }
            }
        }
        let total = begun.elapsed().as_secs_f64() * 1000.0;
        cut.armed = false;
        if let Some(s) = headers["served"].as_str() {
            served = s.to_string();
        }
        Ok((
            Generation {
                text: answer,
                first: first.unwrap_or(total),
                last: if last > 0.0 { last } else { total },
                total,
                prompt: prompt_tokens,
                completion: completion_tokens,
                served,
                finish,
                reasoning: reasoning_tokens,
                model: model.to_string(),
                cached: false,
            },
            headers,
        ))
    }

    /// The call kept for `name`, when there is one and it answered.
    fn kept(&self, name: &str) -> Option<Generation> {
        let dir = self.calls.as_ref()?;
        let doc: Value = serde_json::from_str(&std::fs::read_to_string(dir.join(format!("{name}.json"))).ok()?).ok()?;
        let r = &doc["response"];
        Some(Generation {
            text: r["text"].as_str().filter(|t| !t.is_empty() && doc["error"].is_null())?.to_string(),
            first: 0.0,
            last: 0.0,
            total: 0.0,
            prompt: r["prompt_tokens"].as_u64()?,
            completion: r["completion_tokens"].as_u64()?,
            served: r["served"].as_str()?.to_string(),
            finish: r["finish_reason"].as_str().unwrap_or_default().to_string(),
            reasoning: r["reasoning_tokens"].as_u64().unwrap_or_default(),
            model: doc["model"].as_str()?.to_string(),
            cached: true,
        })
    }

    /// Keeps a call that reached the model: its request, its response and its timings, and the
    /// error when it had no answer.
    fn keep(&self, name: &str, body: &Value, g: &Generation, headers: &Value, error: Option<&str>) {
        let Some(dir) = &self.calls else {
            return;
        };
        let doc = json!({
            "key": name,
            "endpoint": self.url,
            "model": g.model,
            "request": body,
            "response": {
                "text": g.text,
                "prompt_tokens": g.prompt,
                "completion_tokens": g.completion,
                "reasoning_tokens": g.reasoning,
                "finish_reason": g.finish,
                "served": g.served,
            },
            "error": error,
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
            (g.first, g.last, g.total) = (took, took, took);
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
                    if g.finish == "error" {
                        // The server ended the answer with an error: what it wrote is not an answer.
                        let why = format!("zen: the server ended the answer with an error after {} completion tokens", g.completion);
                        self.keep(&name, &body, &g, &headers, Some(&why));
                        last = why;
                        tokio::time::sleep(Duration::from_millis(500 * u64::from(attempt + 1))).await;
                        continue;
                    }
                    if g.text.is_empty() {
                        let why = format!(
                            "zen: an answer without text (finish {}, {} of {} completion tokens reasoning)",
                            if g.finish.is_empty() { "none" } else { &g.finish },
                            g.reasoning,
                            g.completion
                        );
                        self.keep(&name, &body, &g, &headers, Some(&why));
                        return Err(why);
                    }
                    self.keep(&name, &body, &g, &headers, None);
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

    /// [`Zen::generate`] begun now and waited for later: the call runs on the runtime while the
    /// caller does other work. Dropping the [`Pending`] cancels it.
    pub fn start(&self, model: &str, system: &str, user: &str, effort: Effort) -> Pending {
        let zen = self.clone();
        let (m, s, u) = (model.to_string(), system.to_string(), user.to_string());
        Pending {
            handle: Some(self.runtime.spawn(async move { zen.complete(&m, &s, &u, effort).await })),
            runtime: self.runtime.clone(),
            begun: Instant::now(),
        }
    }
}

/// A call begun with [`Zen::start`].
pub struct Pending {
    handle: Option<tokio::task::JoinHandle<Result<Generation, String>>>,
    runtime: tokio::runtime::Handle,
    begun: Instant,
}

impl Pending {
    /// Whether the answer is in.
    pub fn done(&self) -> bool {
        self.handle.as_ref().is_none_or(tokio::task::JoinHandle::is_finished)
    }

    /// Milliseconds since the call was begun.
    pub fn age(&self) -> f64 {
        self.begun.elapsed().as_secs_f64() * 1000.0
    }

    /// The answer, waiting for it if it is not in. Blocks: call it off the async runtime.
    pub fn wait(mut self) -> Result<Generation, String> {
        let handle = self.handle.take().ok_or("zen: the call was already waited for")?;
        self.runtime.block_on(handle).map_err(|e| format!("zen: the call did not finish: {e}"))?
    }

    /// Cancels the call and returns once it has stopped, its line in the spend file written;
    /// `true` when it had not finished. Blocks: call it off the async runtime.
    pub fn cancel(mut self) -> bool {
        let Some(handle) = self.handle.take() else {
            return false;
        };
        let running = !handle.is_finished();
        handle.abort();
        let _ = self.runtime.block_on(handle);
        running
    }
}

impl Drop for Pending {
    fn drop(&mut self) {
        if let Some(handle) = self.handle.take() {
            handle.abort();
        }
    }
}

/// While armed, a call that is dropped before its stream ends: its drop records the call as
/// cancelled.
struct Cut<'a> {
    zen: &'a Zen,
    model: &'a str,
    body: &'a Value,
    /// Pieces of text the stream had written.
    streamed: u64,
    begun: Instant,
    armed: bool,
}

impl Drop for Cut<'_> {
    fn drop(&mut self) {
        if self.armed {
            self.zen.record_cut(self.model, self.body, self.streamed, self.begun.elapsed().as_secs_f64() * 1000.0);
        }
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

    /// A server answering one streamed completion by `reply(request body)`: its content, finish reason and usage.
    async fn server(reply: impl Fn(&Value) -> (String, &'static str, Value) + Send + Sync + 'static) -> wiremock::MockServer {
        use wiremock::matchers::{method, path};
        let server = wiremock::MockServer::start().await;
        wiremock::Mock::given(method("POST"))
            .and(path("/v1/chat/completions"))
            .respond_with(move |request: &wiremock::Request| {
                let body: Value = serde_json::from_slice(&request.body).unwrap();
                let (content, finish, usage) = reply(&body);
                let first = json!({"model": "m", "choices": [{"delta": {"content": content}, "finish_reason": null}]});
                let last = json!({"model": "m", "choices": [{"delta": {}, "finish_reason": finish}]});
                let usage = json!({"model": "m", "choices": [], "usage": usage});
                wiremock::ResponseTemplate::new(200)
                    .insert_header("content-type", "text/event-stream")
                    .set_body_string(format!("data: {first}\n\ndata: {last}\n\ndata: {usage}\n\ndata: [DONE]\n\n"))
            })
            .mount(&server)
            .await;
        server
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn an_answer_the_tokens_ran_out_before_is_an_error_that_is_kept_with_its_usage_and_asked_again() {
        let usage = json!({"prompt_tokens": 70, "completion_tokens": 3072, "completion_tokens_details": {"reasoning_tokens": 3072}});
        let server = server(move |_| (String::new(), "length", usage.clone())).await;
        let home = tempfile::tempdir().unwrap();
        let (calls, spend) = (home.path().join("calls"), home.path().join("spend.jsonl"));
        let zen = zen_at(&server, &calls, &spend);
        for _ in 0..2 {
            let why = tokio::task::block_in_place(|| zen.generate("base", "sys", "user", Effort::Full)).unwrap_err();
            assert_eq!(why, "zen: an answer without text (finish length, 3072 of 3072 completion tokens reasoning)");
        }
        assert_eq!(server.received_requests().await.unwrap().len(), 2, "a call without an answer is asked again");
        let name = key(&zen.url, "base", &zen.body("base", "sys", "user", Effort::Full));
        let doc: Value = serde_json::from_str(&std::fs::read_to_string(calls.join(format!("{name}.json"))).unwrap()).unwrap();
        assert_eq!(doc["response"]["finish_reason"], "length");
        assert_eq!(doc["response"]["reasoning_tokens"], 3072);
        assert_eq!(doc["request"]["messages"][1]["content"], "user", "the request is kept");
        assert!(doc["error"].as_str().unwrap().contains("without text"));
        assert!(zen.kept(&name).is_none());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn a_call_the_server_ended_with_an_error_is_asked_again_and_a_good_one_is_kept_with_what_it_cost() {
        let usage = json!({"prompt_tokens": 10, "completion_tokens": 40, "completion_tokens_details": {"reasoning_tokens": 25}});
        let n = std::sync::atomic::AtomicUsize::new(0);
        let server = server(move |_| match n.fetch_add(1, std::sync::atomic::Ordering::SeqCst) {
            0 => ("{\"edits\":[] ruin".to_string(), "error", usage.clone()),
            _ => ("{\"edits\":[]}".to_string(), "stop", usage.clone()),
        })
        .await;
        let home = tempfile::tempdir().unwrap();
        let (calls, spend) = (home.path().join("calls"), home.path().join("spend.jsonl"));
        let zen = zen_at(&server, &calls, &spend);
        let g = tokio::task::block_in_place(|| zen.generate("base", "sys", "user", Effort::Low)).unwrap();
        assert_eq!((g.text.as_str(), g.finish.as_str(), g.reasoning, g.completion), ("{\"edits\":[]}", "stop", 25, 40));
        assert_eq!(server.received_requests().await.unwrap().len(), 2);
        let again = tokio::task::block_in_place(|| zen.generate("base", "sys", "user", Effort::Low)).unwrap();
        assert!(again.cached && (again.finish.as_str(), again.reasoning) == ("stop", 25));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn at_hanzos_api_an_edit_is_asked_for_as_a_schema_with_room_for_the_reasoning_and_the_answer() {
        let generate = Generate {
            url: "https://api.hanzo.ai/v1".into(),
            model: "enso-flash".into(),
            spend: std::path::PathBuf::from("/nonexistent/spend.jsonl"),
            budget: None,
            escalate: Some("openai/gpt-6-sol".into()),
            calls: None,
        };
        let zen = Zen::new(&generate, Some("key".into()));
        let low = zen.body("enso-flash", "s", "u", Effort::Low);
        let full = zen.body("openai/gpt-6-sol", "s", "u", Effort::Full);
        assert_eq!((low["reasoning_effort"].as_str(), low["max_tokens"].as_u64()), (Some("none"), Some(u64::from(TOKENS))));
        assert_eq!((full["reasoning_effort"].as_str(), full["max_tokens"].as_u64()), (Some("low"), Some(u64::from(THOUGHT))));
        assert_eq!(full["max_completion_tokens"], full["max_tokens"]);
        assert!(full["max_tokens"].as_u64().unwrap() > 3072, "the answer cannot be starved by a cap of 3,072");
        for body in [&low, &full] {
            let format = &body["response_format"];
            assert_eq!((format["type"].as_str(), format["json_schema"]["strict"].as_bool()), (Some("json_schema"), Some(true)));
            let edits = &format["json_schema"]["schema"]["properties"]["edits"];
            assert_eq!(edits["minItems"], 1);
            assert_eq!(edits["items"]["required"], json!(["file", "line", "old", "new"]));
        }
        // Another server is asked for a JSON object at low effort and for nothing at full.
        let local = Zen::new(&Generate { url: "http://127.0.0.1:1/v1".into(), ..generate }, None);
        assert_eq!(local.body("m", "s", "u", Effort::Low)["response_format"]["type"], "json_object");
        assert!(local.body("m", "s", "u", Effort::Full).get("response_format").is_none());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn the_fallback_is_the_generator_and_never_an_anthropic_model() {
        let at = |model: &str| {
            Zen::new(
                &Generate { url: "http://127.0.0.1:1/v1".into(), model: model.into(), spend: PathBuf::from("/nonexistent"), budget: None, escalate: None, calls: None },
                None,
            )
        };
        assert_eq!(at("enso-flash").fallback(), Some("enso-flash"));
        for model in ["claude-sonnet-5-5", "anthropic/claude-haiku-5.5", "Anthropic/x"] {
            assert_eq!(at(model).fallback(), None, "{model}");
        }
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn a_call_begun_ahead_answers_when_waited_for_as_the_same_call_would() {
        let server = testing::zen(|_| "{\"edits\":[]}".to_string()).await;
        let home = tempfile::tempdir().unwrap();
        let (calls, spend) = (home.path().join("calls"), home.path().join("spend.jsonl"));
        let zen = zen_at(&server, &calls, &spend);
        let pending = zen.start("base", "sys", "user", Effort::Low);
        let g = tokio::task::block_in_place(|| pending.wait()).unwrap();
        assert!(!g.cached && g.text == "{\"edits\":[]}" && g.first <= g.last && g.last <= g.total, "{g:?}");
        // It was kept as any call is: the same request again is answered from there.
        let again = tokio::task::block_in_place(|| zen.generate("base", "sys", "user", Effort::Low)).unwrap();
        assert!(again.cached);
        assert_eq!(server.received_requests().await.unwrap().len(), 1);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn a_cancelled_call_stops_and_leaves_its_line_in_the_spend_file() {
        use wiremock::matchers::{method, path};
        let server = wiremock::MockServer::start().await;
        wiremock::Mock::given(method("POST"))
            .and(path("/v1/chat/completions"))
            .respond_with(wiremock::ResponseTemplate::new(200).set_delay(Duration::from_secs(30)))
            .mount(&server)
            .await;
        let home = tempfile::tempdir().unwrap();
        let (calls, spend) = (home.path().join("calls"), home.path().join("spend.jsonl"));
        let mut zen = zen_at(&server, &calls, &spend);
        // Paid, as every call to Hanzo's API is.
        zen.spend = Some((spend.clone(), f64::INFINITY));
        let pending = zen.start("base", "sys", "user", Effort::Low);
        tokio::time::sleep(Duration::from_millis(300)).await;
        assert!(!pending.done());
        let begun = Instant::now();
        assert!(tokio::task::block_in_place(|| pending.cancel()), "it was running");
        assert!(begun.elapsed() < Duration::from_secs(5), "a cancel does not wait for the answer");
        assert_eq!(server.received_requests().await.unwrap().len(), 1);
        let lines: Vec<Value> = std::fs::read_to_string(&spend).unwrap().lines().map(|l| serde_json::from_str(l).unwrap()).collect();
        assert_eq!(lines.len(), 1);
        assert_eq!((lines[0]["cancelled"].as_bool(), lines[0]["model"].as_str()), (Some(true), Some("base")));
        // "sys" and "user": seven characters, three tokens at three characters a token; nothing streamed.
        assert_eq!((lines[0]["prompt_tokens"].as_u64(), lines[0]["completion_tokens"].as_u64()), (Some(3), Some(0)));
        assert!(lines[0]["cost"].as_f64().unwrap() > 0.0);
        assert!(!calls.exists() || std::fs::read_dir(&calls).unwrap().next().is_none(), "a cancelled call is not kept");
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
