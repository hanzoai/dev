//! Chat completion interface - matches Python's client.chat

use crate::{Result, HanzoError, client::Hanzo, types::{Message, Response, ChatRequest}};
use futures::StreamExt;
use serde_json::error::Error as JsonError;

/// Chat interface - matches Python's client.chat
#[derive(Clone)]
pub struct Chat {
    client: Hanzo,
}

impl Chat {
    pub(crate) fn new(client: Hanzo) -> Self {
        Self { client }
    }

    /// Create chat completion - matches Python: client.chat.create(messages=[...])
    pub async fn create(&self, messages: Vec<Message>) -> Result<Response> {
        self.create_with_model(&self.client.inner().default_model, messages).await
    }

    /// Create with specific model - matches Python: client.chat.create(model="gpt-5", messages=[...])
    pub async fn create_with_model(&self, model: &str, messages: Vec<Message>) -> Result<Response> {
        let request = ChatRequest {
            model: model.to_string(),
            messages,
            temperature: None,
            max_tokens: None,
            stream: false,
        };

        self.send_request(request).await
    }

    /// Stream chat completion - matches Python: client.chat.create(messages=[...], stream=True)
    pub async fn stream(&self, messages: Vec<Message>) -> Result<impl futures::Stream<Item = Result<StreamEvent>>> {
        self.stream_with_model(&self.client.inner().default_model, messages).await
    }

    /// Stream with specific model
    pub async fn stream_with_model(
        &self,
        model: &str,
        messages: Vec<Message>,
    ) -> Result<impl futures::Stream<Item = Result<StreamEvent>>> {
        let request = ChatRequest {
            model: model.to_string(),
            messages,
            temperature: None,
            max_tokens: None,
            stream: true,
        };

        self.send_stream_request(request).await
    }

    async fn send_request(&self, request: ChatRequest) -> Result<Response> {
        let url = format!("{}/chat/completions", self.client.inner().base_url);

        let req = self.client.inner().http_client
            .post(&url)
            .json(&request)
            .build()
            .map_err(|e| HanzoError::InvalidRequest(e.to_string()))?;

        let resp = self.client.request(req).await?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await.unwrap_or_default();
            return Err(match status.as_u16() {
                401 => HanzoError::AuthenticationError(text),
                429 => HanzoError::RateLimitError(text),
                _ => HanzoError::APIError(format!("{}: {}", status, text)),
            });
        }

        resp.json::<Response>().await
            .map_err(|e| HanzoError::APIError(format!("JSON parse error: {}", e)))
    }

    async fn send_stream_request(
        &self,
        request: ChatRequest,
    ) -> Result<impl futures::Stream<Item = Result<StreamEvent>>> {
        let url = format!("{}/chat/completions", self.client.inner().base_url);

        let req = self.client.inner().http_client
            .post(&url)
            .json(&request)
            .build()
            .map_err(|e| HanzoError::InvalidRequest(e.to_string()))?;

        let resp = self.client.request(req).await?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp.text().await.unwrap_or_default();
            return Err(match status.as_u16() {
                401 => HanzoError::AuthenticationError(text),
                429 => HanzoError::RateLimitError(text),
                _ => HanzoError::APIError(format!("{}: {}", status, text)),
            });
        }

        // Parse SSE stream
        let stream = resp.bytes_stream()
            .map(move |chunk| {
                match chunk {
                    Ok(bytes) => {
                        // Parse SSE events from bytes
                        parse_sse_chunk(&bytes)
                    }
                    Err(e) => vec![Err(HanzoError::NetworkError(e.to_string()))],
                }
            })
            .flat_map(futures::stream::iter);

        Ok(stream)
    }
}

/// Stream event for chat completions
#[derive(Debug, Clone)]
pub struct StreamEvent {
    pub delta: Option<String>,
    pub finish_reason: Option<String>,
}

fn parse_sse_chunk(bytes: &[u8]) -> Vec<Result<StreamEvent>> {
    let text = match std::str::from_utf8(bytes) {
        Ok(t) => t,
        Err(e) => return vec![Err(HanzoError::APIError(format!("UTF-8 error: {}", e)))],
    };

    let mut events = Vec::new();
    for line in text.lines() {
        if line.starts_with("data: ") {
            let data = &line[6..];
            if data == "[DONE]" {
                events.push(Ok(StreamEvent {
                    delta: None,
                    finish_reason: Some("stop".to_string()),
                }));
            } else if let Ok(json) = serde_json::from_str::<serde_json::Value>(data) {
                if let Some(delta) = json["choices"][0]["delta"]["content"].as_str() {
                    events.push(Ok(StreamEvent {
                        delta: Some(delta.to_string()),
                        finish_reason: None,
                    }));
                }
            }
        }
    }

    events
}