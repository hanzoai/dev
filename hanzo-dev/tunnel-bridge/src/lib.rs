//! # hanzo-tunnel-bridge
//!
//! Bridge between `hanzo-tunnel` (cloud commands/events) and the dev app-server
//! (JSON-RPC protocol). Translates cloud commands into app-server requests and
//! app-server events into cloud events.
//!
//! This crate is the thin adapter that makes dev remotely controllable from
//! `app.hanzo.bot` via the shared tunnel.
//!
//! Requires the `hanzo-tunnel` optional dependency to be enabled for full
//! functionality. Without it, only the translation helpers are available.

#[cfg(feature = "tunnel")]
use hanzo_tunnel::protocol::{CommandPayload, EventPayload, Frame};
use serde_json::Value;
#[cfg(feature = "tunnel")]
use std::sync::Arc;
#[cfg(feature = "tunnel")]
use tokio::sync::{Mutex, mpsc};
use tracing::debug;
#[cfg(feature = "tunnel")]
use tracing::warn;

/// A JSON-RPC request to send to the app-server.
#[derive(Debug, Clone)]
pub struct JsonRpcRequest {
    pub method: String,
    pub params: Value,
}

/// Maps cloud command methods to app-server JSON-RPC methods.
///
/// Cloud commands are high-level ("chat.send", "exec.approve"), while
/// app-server uses specific JSON-RPC methods ("sendUserMessage",
/// "execCommandApproval", etc.).
#[cfg(feature = "tunnel")]
pub fn cloud_command_to_jsonrpc(
    cmd: &CommandPayload,
    conversation_id: &str,
) -> Option<JsonRpcRequest> {
    match cmd.method.as_str() {
        "chat.send" => {
            let message = cmd.params.get("message")?.as_str()?;
            Some(JsonRpcRequest {
                method: "sendUserMessage".into(),
                params: serde_json::json!({
                    "conversationId": conversation_id,
                    "items": [{
                        "type": "text",
                        "data": message,
                    }]
                }),
            })
        }
        "exec.approve" => {
            let call_id = cmd.params.get("call_id")?.as_str()?;
            let command = cmd.params.get("command").cloned().unwrap_or(Value::Array(vec![]));
            let cwd = cmd
                .params
                .get("cwd")
                .and_then(|v| v.as_str())
                .unwrap_or(".");
            Some(JsonRpcRequest {
                method: "execCommandApproval".into(),
                params: serde_json::json!({
                    "conversationId": conversation_id,
                    "callId": call_id,
                    "command": command,
                    "cwd": cwd,
                }),
            })
        }
        "patch.approve" => {
            let call_id = cmd.params.get("call_id")?.as_str()?;
            Some(JsonRpcRequest {
                method: "applyPatchApproval".into(),
                params: serde_json::json!({
                    "conversationId": conversation_id,
                    "callId": call_id,
                }),
            })
        }
        "exec.deny" | "patch.deny" => {
            // Denials are handled by the app-server as abort
            Some(JsonRpcRequest {
                method: "abortTurn".into(),
                params: serde_json::json!({
                    "conversationId": conversation_id,
                }),
            })
        }
        _ => {
            warn!(method = %cmd.method, "unknown cloud command, passing through");
            Some(JsonRpcRequest {
                method: cmd.method.clone(),
                params: cmd.params.clone(),
            })
        }
    }
}

/// Maps app-server notification methods to cloud event names.
///
/// App-server notifications use `codex/event/<EventType>` naming.
/// Cloud events use shorter `<category>.<action>` naming.
///
/// Returns the event name and the params as a tuple (without wrapping in a Frame)
/// so this helper is usable regardless of tunnel availability.
pub fn notification_event_name(method: &str) -> &str {
    match method {
        "codex/event/agentMessage" => "chat.message",
        "codex/event/agentMessageDelta" => "chat.delta",
        "codex/event/agentReasoning" => "chat.reasoning",
        "codex/event/agentReasoningDelta" => "chat.reasoning_delta",
        "codex/event/taskStarted" => "turn.started",
        "codex/event/taskComplete" => "turn.completed",
        "codex/event/turnAborted" => "turn.aborted",
        "codex/event/execCommandBegin" => "exec.begin",
        "codex/event/execCommandOutputDelta" => "exec.output",
        "codex/event/execCommandEnd" => "exec.end",
        "codex/event/execApprovalRequest" => "exec.approval_request",
        "codex/event/applyPatchApprovalRequest" => "patch.approval_request",
        "codex/event/patchApplyBegin" => "patch.begin",
        "codex/event/patchApplyEnd" => "patch.end",
        "codex/event/sessionConfigured" => "session.configured",
        "codex/event/tokenCount" => "session.token_count",
        _ => {
            debug!(method, "unmapped notification, forwarding raw");
            method
        }
    }
}

/// Maps app-server notification methods to cloud event frames.
#[cfg(feature = "tunnel")]
pub fn notification_to_cloud_event(method: &str, params: &Value) -> Option<Frame> {
    let event_name = notification_event_name(method);
    Some(Frame::Event(EventPayload {
        event: event_name.into(),
        data: params.clone(),
    }))
}

/// Bridge state — tracks the active conversation and pending requests.
#[cfg(feature = "tunnel")]
pub struct TunnelBridge {
    /// The active conversation ID (set after newConversation).
    conversation_id: Arc<Mutex<Option<String>>>,
    /// Sender for outgoing tunnel frames (events to cloud).
    tunnel_tx: mpsc::Sender<Frame>,
    /// Next JSON-RPC request ID.
    next_id: Arc<std::sync::atomic::AtomicU64>,
}

#[cfg(feature = "tunnel")]
impl TunnelBridge {
    /// Create a new bridge.
    ///
    /// `tunnel_tx` sends frames to the cloud via the tunnel connection.
    pub fn new(tunnel_tx: mpsc::Sender<Frame>) -> Self {
        Self {
            conversation_id: Arc::new(Mutex::new(None)),
            tunnel_tx,
            next_id: Arc::new(std::sync::atomic::AtomicU64::new(1)),
        }
    }

    /// Set the active conversation ID.
    pub async fn set_conversation_id(&self, id: String) {
        *self.conversation_id.lock().await = Some(id);
    }

    /// Get the active conversation ID.
    pub async fn conversation_id(&self) -> Option<String> {
        self.conversation_id.lock().await.clone()
    }

    /// Allocate the next JSON-RPC request ID.
    pub fn next_request_id(&self) -> u64 {
        self.next_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    }

    /// Handle a cloud command: translate it and return the JSON-RPC request
    /// to send to the app-server.
    pub async fn handle_cloud_command(
        &self,
        cmd: CommandPayload,
    ) -> Result<Option<JsonRpcRequest>, hanzo_tunnel::TunnelError> {
        let conv_id = self
            .conversation_id
            .lock()
            .await
            .clone()
            .unwrap_or_default();

        let rpc = cloud_command_to_jsonrpc(&cmd, &conv_id);

        if rpc.is_none() {
            // Respond with error to cloud.
            self.tunnel_tx
                .send(Frame::Response(hanzo_tunnel::protocol::ResponsePayload {
                    id: cmd.id,
                    ok: false,
                    data: None,
                    error: Some(format!("unknown method: {}", cmd.method)),
                }))
                .await
                .map_err(|_| hanzo_tunnel::TunnelError::ChannelClosed)?;
        }

        Ok(rpc)
    }

    /// Handle an app-server notification: translate it and forward to the cloud.
    pub async fn handle_app_notification(
        &self,
        method: &str,
        params: &Value,
    ) -> Result<(), hanzo_tunnel::TunnelError> {
        if let Some(frame) = notification_to_cloud_event(method, params) {
            self.tunnel_tx
                .send(frame)
                .await
                .map_err(|_| hanzo_tunnel::TunnelError::ChannelClosed)?;
        }
        Ok(())
    }

    /// Send an acknowledgment back to the cloud for a command.
    pub async fn ack_command(
        &self,
        command_id: String,
        data: Option<Value>,
    ) -> Result<(), hanzo_tunnel::TunnelError> {
        self.tunnel_tx
            .send(Frame::Response(hanzo_tunnel::protocol::ResponsePayload {
                id: command_id,
                ok: true,
                data,
                error: None,
            }))
            .await
            .map_err(|_| hanzo_tunnel::TunnelError::ChannelClosed)
    }
}

#[cfg(all(test, feature = "tunnel"))]
mod tests {
    use super::*;

    #[test]
    fn chat_send_translation() {
        let cmd = CommandPayload {
            id: "req-1".into(),
            method: "chat.send".into(),
            params: serde_json::json!({"message": "fix the bug"}),
        };

        let rpc = cloud_command_to_jsonrpc(&cmd, "conv-123").unwrap();
        assert_eq!(rpc.method, "sendUserMessage");
        assert_eq!(rpc.params["conversationId"], "conv-123");
        assert_eq!(rpc.params["items"][0]["data"], "fix the bug");
    }

    #[test]
    fn exec_approve_translation() {
        let cmd = CommandPayload {
            id: "req-2".into(),
            method: "exec.approve".into(),
            params: serde_json::json!({
                "call_id": "call-1",
                "command": ["cargo", "test"],
                "cwd": "/tmp"
            }),
        };

        let rpc = cloud_command_to_jsonrpc(&cmd, "conv-123").unwrap();
        assert_eq!(rpc.method, "execCommandApproval");
        assert_eq!(rpc.params["callId"], "call-1");
    }

    #[test]
    fn notification_mapping() {
        let params = serde_json::json!({
            "msg": {"type": "agentMessageDelta", "delta": "hello"},
            "event_seq": 1
        });

        let frame =
            notification_to_cloud_event("codex/event/agentMessageDelta", &params).unwrap();
        match frame {
            Frame::Event(e) => {
                assert_eq!(e.event, "chat.delta");
                assert_eq!(e.data["msg"]["delta"], "hello");
            }
            _ => panic!("expected Event frame"),
        }
    }

    #[test]
    fn approval_request_mapping() {
        let params = serde_json::json!({
            "msg": {
                "type": "execApprovalRequest",
                "call_id": "c1",
                "command": ["rm", "-rf", "/tmp/test"]
            }
        });

        let frame =
            notification_to_cloud_event("codex/event/execApprovalRequest", &params).unwrap();
        match frame {
            Frame::Event(e) => {
                assert_eq!(e.event, "exec.approval_request");
            }
            _ => panic!("expected Event frame"),
        }
    }

    #[test]
    fn turn_lifecycle_events() {
        let started = notification_to_cloud_event(
            "codex/event/taskStarted",
            &serde_json::json!({}),
        )
        .unwrap();
        assert!(matches!(started, Frame::Event(EventPayload { event, .. }) if event == "turn.started"));

        let completed = notification_to_cloud_event(
            "codex/event/taskComplete",
            &serde_json::json!({"last_agent_message": "Done!"}),
        )
        .unwrap();
        assert!(matches!(completed, Frame::Event(EventPayload { event, .. }) if event == "turn.completed"));
    }

    #[test]
    fn unknown_command_passes_through() {
        let cmd = CommandPayload {
            id: "req-3".into(),
            method: "custom.action".into(),
            params: serde_json::json!({"foo": "bar"}),
        };

        let rpc = cloud_command_to_jsonrpc(&cmd, "conv-123").unwrap();
        assert_eq!(rpc.method, "custom.action");
    }

    #[test]
    fn deny_maps_to_abort() {
        let cmd = CommandPayload {
            id: "req-4".into(),
            method: "exec.deny".into(),
            params: serde_json::json!({}),
        };

        let rpc = cloud_command_to_jsonrpc(&cmd, "conv-123").unwrap();
        assert_eq!(rpc.method, "abortTurn");
    }

    #[tokio::test]
    async fn bridge_conversation_id() {
        let (tx, _rx) = mpsc::channel(16);
        let bridge = TunnelBridge::new(tx);
        assert!(bridge.conversation_id().await.is_none());

        bridge.set_conversation_id("conv-abc".into()).await;
        assert_eq!(bridge.conversation_id().await.unwrap(), "conv-abc");
    }

    #[tokio::test]
    async fn bridge_ack_command() {
        let (tx, mut rx) = mpsc::channel(16);
        let bridge = TunnelBridge::new(tx);

        bridge
            .ack_command("req-1".into(), Some(serde_json::json!({"ok": true})))
            .await
            .unwrap();

        let frame = rx.recv().await.unwrap();
        match frame {
            Frame::Response(r) => {
                assert!(r.ok);
                assert_eq!(r.id, "req-1");
            }
            _ => panic!("expected Response"),
        }
    }

    #[test]
    fn next_request_id_increments() {
        let (tx, _rx) = mpsc::channel(1);
        let bridge = TunnelBridge::new(tx);
        assert_eq!(bridge.next_request_id(), 1);
        assert_eq!(bridge.next_request_id(), 2);
        assert_eq!(bridge.next_request_id(), 3);
    }
}

#[cfg(test)]
mod basic_tests {
    use super::*;

    #[test]
    fn event_name_mapping() {
        assert_eq!(notification_event_name("codex/event/agentMessage"), "chat.message");
        assert_eq!(notification_event_name("codex/event/taskStarted"), "turn.started");
        assert_eq!(notification_event_name("unknown/event"), "unknown/event");
    }
}
