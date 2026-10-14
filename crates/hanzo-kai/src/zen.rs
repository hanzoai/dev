//! Zen through an OpenAI-compatible endpoint: the only model calls a flow makes, and only to
//! write code or text. The endpoint is Hanzo's (`https://api.hanzo.ai/v1`, signed with the
//! credential Dev already uses) or any server that speaks `/chat/completions`, a local Zen 6
//! among them; the credential is sent to Hanzo's alone.
//!
//! A call streams, so the time to the first token and the time to the last are both known, and
//! each paid call is a line in the spend file with the model Hanzo says served it.

use hanzo_config::kai::Generate;
use serde_json::Value;
use serde_json::json;
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;

/// Tokens an answer may be given room for.
const TOKENS: u32 = 512;
/// Tokens an answer that reasons first may be given room for.
const THOUGHT: u32 = 6144;
/// What an unpriced call is counted at, dollars per million tokens: above any Zen model's rate,
/// so the budget is a ceiling on the spend and not a guess at it.
const RATE: (f64, f64) = (2.0, 10.0);
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
}

/// Zen over `POST {url}/chat/completions` as `model`.
pub struct Zen {
    url: String,
    model: String,
    key: Option<String>,
    cloud: bool,
    spend: Option<(PathBuf, f64)>,
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
            key: key.filter(|_| cloud),
            cloud,
            spend: cloud
                .then(|| (generate.spend.clone(), generate.budget.unwrap_or(f64::INFINITY))),
            client: reqwest::Client::builder()
                .connect_timeout(Duration::from_secs(5))
                .timeout(Duration::from_secs(600))
                .build()
                .unwrap_or_default(),
            runtime: tokio::runtime::Handle::current(),
        }
    }

    pub fn model(&self) -> &str {
        &self.model
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
            "model": self.model,
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

    async fn once(&self, system: &str, user: &str, effort: Effort) -> Result<(Generation, Value), String> {
        let mut body = json!({
            "model": self.model,
            "temperature": 0,
            "max_tokens": if effort == Effort::Full { THOUGHT } else { TOKENS },
            "stream": true,
            "stream_options": {"include_usage": true},
            "messages": [
                {"role": "system", "content": system},
                {"role": "user", "content": user},
            ],
        });
        // At low effort the answer is the edit and nothing else: no reasoning is bought.
        match (self.cloud, effort) {
            (true, Effort::Low) => body["reasoning_effort"] = json!("none"),
            (true, Effort::Full) => body["reasoning_effort"] = json!("medium"),
            (false, Effort::Low) => body["chat_template_kwargs"] = json!({"enable_thinking": false}),
            (false, Effort::Full) => body["chat_template_kwargs"] = json!({"enable_thinking": true}),
        }
        let mut request = self
            .client
            .post(format!("{}/chat/completions", self.url))
            .json(&body);
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
            },
            headers,
        ))
    }

    async fn complete(&self, system: &str, user: &str, effort: Effort) -> Result<Generation, String> {
        if let Some((path, budget)) = &self.spend
            && Self::spent(path) >= *budget
        {
            return Err(format!("zen: the budget of ${budget:.2} is spent ({})", path.display()));
        }
        let begun = Instant::now();
        let mut last = String::new();
        for attempt in 0..TRIES {
            match self.once(system, user, effort).await {
                Ok((mut g, headers)) => {
                    // A retry's wait is part of the time the answer took.
                    g.total = begun.elapsed().as_secs_f64() * 1000.0;
                    self.record(&g, &headers);
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

    /// Zen's answer to `user` under `system`. Blocks: call it off the async runtime.
    pub fn generate(&self, system: &str, user: &str, effort: Effort) -> Result<Generation, String> {
        self.runtime.block_on(self.complete(system, user, effort))
    }
}
