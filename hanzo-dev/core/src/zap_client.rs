//! ZAP cloud service client for the dev CLI.
//!
//! Connects to `zap.hanzo.ai:9651` via TLS 1.3, performs the luxfi/zap
//! handshake, and sends MsgType 100 cloud service requests (chat.completions).
//!
//! This is the real native ZAP binary transport — no HTTP, no fallback.

use crate::client_common::ResponseEvent;
use crate::client_common::ResponseStream;
use crate::error::CodeErr;
use crate::error::Result;
use crate::protocol::TokenUsage;
use crate::zap_wire::*;
use hanzo_protocol::models::ContentItem;
use hanzo_protocol::models::ResponseItem;
use serde::Deserialize;
use std::sync::Arc;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use tokio::io::AsyncWriteExt;
use tokio::net::TcpStream;
use tokio::sync::mpsc;
use tracing::debug;
use tracing::warn;

const DEFAULT_ZAP_ENDPOINT: &str = "zap.hanzo.ai:9651";
const CLIENT_NODE_ID: &str = "dev-client";

// ── OpenAI-compatible response types ────────────────────────────────────

#[derive(Debug, Deserialize)]
struct ChatCompletionResponse {
    id: String,
    model: Option<String>,
    choices: Vec<ChatChoice>,
    usage: Option<ChatUsage>,
}

#[derive(Debug, Deserialize)]
struct ChatChoice {
    index: Option<u32>,
    message: ChatMessage,
    finish_reason: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ChatMessage {
    role: Option<String>,
    content: Option<String>,
    tool_calls: Option<Vec<serde_json::Value>>,
}

#[derive(Debug, Deserialize)]
struct ChatUsage {
    prompt_tokens: Option<u32>,
    completion_tokens: Option<u32>,
    total_tokens: Option<u32>,
}

// ── ZAP Cloud Client ────────────────────────────────────────────────────

/// A client that speaks the luxfi/zap binary wire protocol over TLS.
pub(crate) struct ZapCloudClient {
    stream: tokio_rustls::client::TlsStream<TcpStream>,
    req_id: AtomicU32,
    #[allow(dead_code)]
    peer_id: String,
}

impl ZapCloudClient {
    /// Connect to the ZAP endpoint via TLS 1.3, perform handshake.
    pub async fn connect(endpoint: Option<&str>) -> Result<Self> {
        let addr = endpoint.unwrap_or(DEFAULT_ZAP_ENDPOINT);

        // Parse host from "host:port"
        let host = addr
            .split(':')
            .next()
            .unwrap_or("zap.hanzo.ai")
            .to_string();

        // TLS configuration
        let mut root_store = rustls::RootCertStore::empty();
        root_store.extend(webpki_roots::TLS_SERVER_ROOTS.iter().cloned());

        let config = rustls::ClientConfig::builder()
            .with_root_certificates(root_store)
            .with_no_client_auth();

        let connector = tokio_rustls::TlsConnector::from(Arc::new(config));
        let server_name = rustls::pki_types::ServerName::try_from(host.clone())
            .map_err(|e| CodeErr::ServerError(format!("ZAP: invalid server name '{host}': {e}")))?;

        let tcp = TcpStream::connect(addr)
            .await
            .map_err(|e| CodeErr::ServerError(format!("ZAP: TCP connect to {addr} failed: {e}")))?;
        tcp.set_nodelay(true).ok();

        let mut tls = connector
            .connect(server_name, tcp)
            .await
            .map_err(|e| CodeErr::ServerError(format!("ZAP: TLS handshake with {addr} failed: {e}")))?;

        debug!("ZAP: TLS 1.3 connected to {}", addr);

        // Send handshake
        let handshake_bytes = build_handshake(CLIENT_NODE_ID);
        write_frame(&mut tls, &handshake_bytes)
            .await
            .map_err(|e| CodeErr::ServerError(format!("ZAP: handshake send failed: {e}")))?;

        // Read handshake response
        let resp_data = read_frame(&mut tls)
            .await
            .map_err(|e| CodeErr::ServerError(format!("ZAP: handshake read failed: {e}")))?;

        let resp_msg = Message::parse(resp_data)
            .map_err(|e| CodeErr::ServerError(format!("ZAP: handshake parse failed: {e}")))?;

        let peer_id = parse_handshake(&resp_msg);
        debug!("ZAP: handshake complete, peer={}", peer_id);

        Ok(Self {
            stream: tls,
            req_id: AtomicU32::new(0),
            peer_id,
        })
    }

    /// Send a MsgType 100 Call request and receive the response.
    async fn call(
        &mut self,
        method: &str,
        auth: &str,
        body: &[u8],
    ) -> Result<(u32, Vec<u8>, String)> {
        let req_id = self.req_id.fetch_add(1, Ordering::Relaxed) + 1;

        // Build the ZAP message
        let msg_bytes = build_cloud_request(method, auth, body);

        // Wrap with 8-byte Call correlation header
        let mut wrapped = Vec::with_capacity(8 + msg_bytes.len());
        wrapped.extend_from_slice(&req_id.to_le_bytes());
        wrapped.extend_from_slice(&REQ_FLAG_REQ.to_le_bytes());
        wrapped.extend_from_slice(&msg_bytes);

        // Send
        write_frame(&mut self.stream, &wrapped)
            .await
            .map_err(|e| CodeErr::ServerError(format!("ZAP: send failed: {e}")))?;

        // Read response — loop until we match our reqID
        loop {
            let data = read_frame(&mut self.stream)
                .await
                .map_err(|e| CodeErr::ServerError(format!("ZAP: recv failed: {e}")))?;

            if data.len() < 8 {
                continue;
            }

            let resp_flag = u32::from_le_bytes([data[4], data[5], data[6], data[7]]);
            if resp_flag != REQ_FLAG_RESP {
                continue;
            }

            let resp_id = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
            if resp_id != req_id {
                continue;
            }

            // Parse the ZAP message (skip 8-byte Call header)
            let msg = Message::parse(data[8..].to_vec())
                .map_err(|e| CodeErr::ServerError(format!("ZAP: response parse failed: {e}")))?;

            return Ok(parse_cloud_response(&msg));
        }
    }

    /// Gracefully shut down the TLS connection.
    pub async fn close(mut self) {
        self.stream.shutdown().await.ok();
    }
}

// ── Public API: stream_zap ──────────────────────────────────────────────

/// Execute a chat completion via native ZAP binary transport.
///
/// Connects to the ZAP endpoint, sends a MsgType 100 request with
/// method="chat.completions", parses the response, and emits
/// ResponseEvents through the returned stream.
pub(crate) async fn stream_zap(
    model: &str,
    messages_json: serde_json::Value,
    auth_token: &str,
    endpoint: Option<&str>,
    tools: Option<&serde_json::Value>,
) -> Result<ResponseStream> {
    // Build the chat completion request body
    let mut request_body = serde_json::json!({
        "model": model,
        "messages": messages_json,
    });
    if let Some(tools_val) = tools {
        request_body["tools"] = tools_val.clone();
    }

    let body_bytes = serde_json::to_vec(&request_body).map_err(|e| {
        CodeErr::ServerError(format!("ZAP: failed to serialize request: {e}"))
    })?;

    let auth = if auth_token.starts_with("Bearer ") {
        auth_token.to_string()
    } else {
        format!("Bearer {}", auth_token)
    };

    // Connect and call
    let mut client = ZapCloudClient::connect(endpoint).await?;

    let (status, resp_body, error_msg) = client
        .call("chat.completions", &auth, &body_bytes)
        .await?;

    client.close().await;

    // Handle error responses
    if status != 200 {
        let err = if !error_msg.is_empty() {
            error_msg
        } else if !resp_body.is_empty() {
            String::from_utf8_lossy(&resp_body).into_owned()
        } else {
            format!("ZAP cloud returned status {status}")
        };
        return Err(CodeErr::ServerError(format!("ZAP: {err}")));
    }

    // Parse the chat completion response
    let completion: ChatCompletionResponse = serde_json::from_slice(&resp_body).map_err(|e| {
        warn!(
            "ZAP: failed to parse response body: {} — raw: {}",
            e,
            String::from_utf8_lossy(&resp_body)
        );
        CodeErr::ServerError(format!("ZAP: invalid response JSON: {e}"))
    })?;

    // Convert to ResponseEvents and send through channel
    let (tx, rx) = mpsc::channel::<Result<ResponseEvent>>(16);

    tokio::spawn(async move {
        // Created event
        if tx.send(Ok(ResponseEvent::Created)).await.is_err() {
            return;
        }

        // Emit each choice as an OutputItemDone
        for choice in &completion.choices {
            if let Some(content) = &choice.message.content {
                let item = ResponseItem::Message {
                    id: Some(completion.id.clone()),
                    role: "assistant".to_string(),
                    content: vec![ContentItem::OutputText {
                        text: content.clone(),
                    }],
                };
                if tx
                    .send(Ok(ResponseEvent::OutputItemDone {
                        item,
                        sequence_number: None,
                        output_index: choice.index,
                    }))
                    .await
                    .is_err()
                {
                    return;
                }
            }
        }

        // Completed event with token usage
        let token_usage = completion.usage.map(|u| TokenUsage {
            input_tokens: u64::from(u.prompt_tokens.unwrap_or(0)),
            output_tokens: u64::from(u.completion_tokens.unwrap_or(0)),
            total_tokens: u64::from(u.total_tokens.unwrap_or(0)),
            cached_input_tokens: 0,
            reasoning_output_tokens: 0,
        });

        let _ = tx
            .send(Ok(ResponseEvent::Completed {
                response_id: completion.id.clone(),
                token_usage,
            }))
            .await;
    });

    Ok(ResponseStream { rx_event: rx })
}
