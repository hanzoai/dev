//! Zen through the Hanzo API: the only model calls a flow makes, and only to write code or
//! text.

use program::Revision;
use serde_json::Value;
use serde_json::json;
use std::time::Duration;

const SYSTEM: &str = "You are Zen, writing code for Hanzo Dev. Answer with exactly what the \
request asks for and nothing else. When it asks for a change, answer with one unified diff \
against the repository root that `git apply` accepts, in a single ```diff block.";

/// Zen over `POST /v1/chat/completions`.
pub struct Zen {
    base: String,
    key: Option<String>,
    model: String,
    client: reqwest::Client,
    runtime: tokio::runtime::Handle,
}

impl Zen {
    /// Zen at `base` (`https://api.hanzo.ai/v1`) as `model`, signed with `key`. Call from
    /// within the async runtime; generation then blocks a thread off it.
    pub fn new(base: &str, key: Option<String>, model: &str) -> Zen {
        Zen {
            base: base.trim_end_matches('/').to_string(),
            key,
            model: model.to_string(),
            client: reqwest::Client::builder()
                .timeout(Duration::from_secs(600))
                .build()
                .unwrap_or_default(),
            runtime: tokio::runtime::Handle::current(),
        }
    }

    async fn complete(&self, prompt: &str, context: &Value) -> Result<String, String> {
        let body = json!({
            "model": self.model,
            "temperature": 0,
            "messages": [
                {"role": "system", "content": SYSTEM},
                {"role": "user", "content": format!(
                    "{prompt}\n\nInputs, as JSON:\n{}",
                    serde_json::to_string_pretty(context).unwrap_or_default()
                )},
            ],
        });
        let mut request = self
            .client
            .post(format!("{}/chat/completions", self.base))
            .json(&body);
        if let Some(key) = &self.key {
            request = request.bearer_auth(key);
        }
        let response = request.send().await.map_err(|e| format!("zen: {e}"))?;
        let status = response.status();
        let answer: Value = response.json().await.map_err(|e| format!("zen: {e}"))?;
        if !status.is_success() {
            let why = answer["error"]["message"]
                .as_str()
                .unwrap_or("no reason given");
            return Err(format!("zen: {status}: {why}"));
        }
        answer["choices"][0]["message"]["content"]
            .as_str()
            .map(str::to_string)
            .ok_or_else(|| "zen: an answer without text".to_string())
    }
}

impl program::Zen for Zen {
    fn generate(&self, prompt: &str, context: &Value) -> program::Result<String> {
        self.runtime
            .block_on(self.complete(prompt, context))
            .map_err(program::Error::Backend)
    }

    fn revision(&self) -> Revision {
        Revision {
            model: self.model.clone(),
            calibration: None,
        }
    }
}
