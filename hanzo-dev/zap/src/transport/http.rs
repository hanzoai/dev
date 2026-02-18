//! HTTP/SSE transport for ZAP protocol.
//!
//! Provides ZAP connectivity over HTTP for environments where raw TCP
//! is not available (browsers, serverless, proxied infrastructure).
//!
//! Protocol:
//! - **Send**: `POST {base_url}/zap` with ZAP JSON envelope body
//! - **Recv**: Response body (unary) or SSE stream (streaming)
//! - **WebSocket**: `{base_url}/zap/ws` for persistent bidirectional connection
//!
//! This is the DEFAULT transport for Hanzo services at `api.hanzo.ai`.
//! Falls back gracefully: ZAP → HTTP/JSON → MCP.

use crate::error::Error;
use crate::error::Result;
use crate::message::MessageType;
use reqwest::Client as HttpClient;
use serde::Deserialize;
use serde::Serialize;
use std::collections::VecDeque;
use tokio::sync::Mutex;
use tracing::debug;
use tracing::trace;

/// ZAP-over-HTTP JSON envelope (matches server-side zapRequest).
#[derive(Debug, Clone, Serialize)]
pub struct ZapEnvelope {
    pub method: String,
    pub id: String,
    pub params: serde_json::Value,
}

/// ZAP-over-HTTP JSON response (matches server-side zapResponse).
#[derive(Debug, Clone, Deserialize)]
pub struct ZapResponseEnvelope {
    pub id: String,
    pub result: Option<serde_json::Value>,
    pub error: Option<ZapErrorEnvelope>,
}

/// ZAP error in response envelope.
#[derive(Debug, Clone, Deserialize)]
pub struct ZapErrorEnvelope {
    pub code: i32,
    pub message: String,
}

/// HTTP/SSE transport for ZAP connections.
///
/// Uses HTTP POST for request/response and supports SSE for streaming.
/// This transport works through standard HTTP infrastructure (load balancers,
/// CDNs, API gateways) without requiring raw TCP access.
pub struct HttpTransport {
    base_url: String,
    client: HttpClient,
    auth_token: Option<String>,
    recv_queue: Mutex<VecDeque<Vec<u8>>>,
    request_counter: std::sync::atomic::AtomicU64,
}

impl HttpTransport {
    /// Connect to a ZAP HTTP endpoint.
    ///
    /// # Arguments
    /// * `base_url` - Base URL (e.g., "https://api.hanzo.ai")
    /// * `auth_token` - Optional bearer token for authentication
    pub async fn connect(base_url: &str, auth_token: Option<String>) -> Result<Self> {
        let base_url = base_url.trim_end_matches('/').to_string();

        debug!("connecting to ZAP HTTP endpoint: {}", base_url);

        let client = HttpClient::builder()
            .timeout(std::time::Duration::from_secs(120))
            .build()
            .map_err(|e| Error::Connection(format!("HTTP client build failed: {}", e)))?;

        Ok(Self {
            base_url,
            client,
            auth_token,
            recv_queue: Mutex::new(VecDeque::new()),
            request_counter: std::sync::atomic::AtomicU64::new(1),
        })
    }

    /// Generate a unique request ID.
    fn next_id(&self) -> String {
        let id = self
            .request_counter
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        format!("zap-{}", id)
    }

    /// Send a ZAP method call and receive the response.
    ///
    /// This is the primary API for unary (non-streaming) calls.
    pub async fn call(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<serde_json::Value> {
        let id = self.next_id();
        let envelope = ZapEnvelope {
            method: method.to_string(),
            id: id.clone(),
            params,
        };

        trace!("ZAP HTTP call: {} (id={})", method, id);

        let url = format!("{}/zap", self.base_url);
        let mut req = self.client.post(&url).json(&envelope);

        if let Some(ref token) = self.auth_token {
            req = req.header("Authorization", format!("Bearer {}", token));
        }

        let resp = req
            .send()
            .await
            .map_err(|e| Error::Connection(format!("HTTP request failed: {}", e)))?;

        if !resp.status().is_success() {
            let status = resp.status().as_u16();
            let body = resp.text().await.unwrap_or_default();
            return Err(Error::Protocol(format!(
                "HTTP {}: {}",
                status,
                body.chars().take(500).collect::<String>()
            )));
        }

        let zap_resp: ZapResponseEnvelope = resp
            .json()
            .await
            .map_err(|e| Error::Protocol(format!("invalid ZAP response: {}", e)))?;

        if let Some(err) = zap_resp.error {
            return Err(Error::Protocol(format!(
                "ZAP error {}: {}",
                err.code, err.message
            )));
        }

        Ok(zap_resp.result.unwrap_or(serde_json::Value::Null))
    }

    /// Send a ZAP method call with SSE streaming response.
    ///
    /// Returns an async stream of SSE events for streaming methods
    /// like chat.completions with stream=true.
    pub async fn call_stream(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<SseStream> {
        let id = self.next_id();
        let envelope = ZapEnvelope {
            method: method.to_string(),
            id: id.clone(),
            params,
        };

        trace!("ZAP HTTP stream call: {} (id={})", method, id);

        let url = format!("{}/zap", self.base_url);
        let mut req = self
            .client
            .post(&url)
            .json(&envelope)
            .header("Accept", "text/event-stream");

        if let Some(ref token) = self.auth_token {
            req = req.header("Authorization", format!("Bearer {}", token));
        }

        let resp = req
            .send()
            .await
            .map_err(|e| Error::Connection(format!("HTTP stream request failed: {}", e)))?;

        if !resp.status().is_success() {
            let status = resp.status().as_u16();
            let body = resp.text().await.unwrap_or_default();
            return Err(Error::Protocol(format!("HTTP {}: {}", status, body)));
        }

        Ok(SseStream {
            response: resp,
            buffer: String::new(),
        })
    }

    /// Send a raw ZAP wire message (for low-level protocol compatibility).
    ///
    /// Encodes the message type and payload into the HTTP body.
    pub async fn send(&self, msg_type: MessageType, payload: &[u8]) -> Result<()> {
        // For HTTP transport, we encode wire messages as JSON
        let msg = serde_json::json!({
            "type": msg_type as u8,
            "payload": base64_encode(payload),
        });

        let url = format!("{}/zap", self.base_url);
        let mut req = self.client.post(&url).json(&msg);

        if let Some(ref token) = self.auth_token {
            req = req.header("Authorization", format!("Bearer {}", token));
        }

        let resp = req
            .send()
            .await
            .map_err(|e| Error::Connection(format!("send failed: {}", e)))?;

        if !resp.status().is_success() {
            return Err(Error::Protocol(format!("HTTP {}", resp.status())));
        }

        // Queue response for recv()
        let body = resp
            .bytes()
            .await
            .map_err(|e| Error::Connection(format!("read response failed: {}", e)))?;

        let mut queue = self.recv_queue.lock().await;
        queue.push_back(body.to_vec());

        Ok(())
    }

    /// Receive a queued response message.
    pub async fn recv(&self) -> Result<Vec<u8>> {
        let mut queue = self.recv_queue.lock().await;
        queue
            .pop_front()
            .ok_or_else(|| Error::Protocol("no pending response".to_string()))
    }

    /// Get the base URL.
    pub fn base_url(&self) -> &str {
        &self.base_url
    }
}

/// SSE stream reader for streaming ZAP responses.
pub struct SseStream {
    response: reqwest::Response,
    buffer: String,
}

impl SseStream {
    /// Read the next SSE event from the stream.
    ///
    /// Returns `None` when the stream is complete.
    pub async fn next_event(&mut self) -> Result<Option<SseEvent>> {
        use tokio::io::AsyncBufReadExt;

        loop {
            let chunk = self
                .response
                .chunk()
                .await
                .map_err(|e| Error::Connection(format!("stream read failed: {}", e)))?;

            let chunk = match chunk {
                Some(c) => c,
                None => return Ok(None), // Stream ended
            };

            let text = String::from_utf8_lossy(&chunk);
            self.buffer.push_str(&text);

            // Try to parse complete SSE events from buffer
            if let Some(event) = self.try_parse_event() {
                return Ok(Some(event));
            }
        }
    }

    fn try_parse_event(&mut self) -> Option<SseEvent> {
        // SSE events are separated by double newlines
        if let Some(pos) = self.buffer.find("\n\n") {
            let raw = self.buffer[..pos].to_string();
            self.buffer = self.buffer[pos + 2..].to_string();

            let mut event_type = String::new();
            let mut data = String::new();

            for line in raw.lines() {
                if let Some(rest) = line.strip_prefix("event: ") {
                    event_type = rest.to_string();
                } else if let Some(rest) = line.strip_prefix("data: ") {
                    if !data.is_empty() {
                        data.push('\n');
                    }
                    data.push_str(rest);
                }
            }

            if !data.is_empty() {
                return Some(SseEvent {
                    event: if event_type.is_empty() {
                        None
                    } else {
                        Some(event_type)
                    },
                    data,
                });
            }
        }
        None
    }
}

/// A parsed SSE event.
#[derive(Debug, Clone)]
pub struct SseEvent {
    /// Event type (from `event:` field), if present.
    pub event: Option<String>,
    /// Event data (from `data:` field).
    pub data: String,
}

impl SseEvent {
    /// Parse the data field as JSON.
    pub fn json<T: serde::de::DeserializeOwned>(&self) -> Result<T> {
        serde_json::from_str(&self.data)
            .map_err(|e| Error::Protocol(format!("invalid SSE JSON: {}", e)))
    }

    /// Check if this is a `[DONE]` sentinel.
    pub fn is_done(&self) -> bool {
        self.data.trim() == "[DONE]"
    }
}

/// Base64 encode bytes (for wire-level message transport over HTTP).
fn base64_encode(data: &[u8]) -> String {
    use base64::Engine;
    base64::engine::general_purpose::STANDARD.encode(data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_envelope_serialize() {
        let env = ZapEnvelope {
            method: "billing.getBalance".to_string(),
            id: "req-1".to_string(),
            params: serde_json::json!({"user": "hanzo/alice", "currency": "usd"}),
        };
        let json = serde_json::to_string(&env).expect("serialize");
        assert!(json.contains("billing.getBalance"));
        assert!(json.contains("hanzo/alice"));
    }

    #[test]
    fn test_response_deserialize() {
        let json = r#"{"id":"req-1","result":{"balance":500,"currency":"usd"}}"#;
        let resp: ZapResponseEnvelope = serde_json::from_str(json).expect("deserialize");
        assert_eq!(resp.id, "req-1");
        assert!(resp.result.is_some());
        assert!(resp.error.is_none());
    }

    #[test]
    fn test_error_response_deserialize() {
        let json = r#"{"id":"req-2","error":{"code":-32601,"message":"method not found"}}"#;
        let resp: ZapResponseEnvelope = serde_json::from_str(json).expect("deserialize");
        assert_eq!(resp.id, "req-2");
        assert!(resp.error.is_some());
        assert_eq!(resp.error.as_ref().expect("err").code, -32601);
    }

    #[test]
    fn test_sse_event_done() {
        let event = SseEvent {
            event: None,
            data: "[DONE]".to_string(),
        };
        assert!(event.is_done());

        let event2 = SseEvent {
            event: Some("content_block_delta".to_string()),
            data: r#"{"text":"hello"}"#.to_string(),
        };
        assert!(!event2.is_done());
        assert_eq!(event2.event.as_deref(), Some("content_block_delta"));
    }
}
