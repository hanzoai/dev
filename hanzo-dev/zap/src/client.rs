//! ZAP client for connecting to ZAP servers and gateways.

use crate::error::{Error, Result};
use crate::message::{ClientInfo, MessageType, ServerInfo, Tool, ToolCall, ToolResult};
use crate::transport::ZapTransport;
use serde_json::Value;
use std::sync::atomic::{AtomicU64, Ordering};
use tracing::{debug, info};

/// A ZAP client for connecting to ZAP servers.
pub struct Client {
    transport: ZapTransport,
    server_info: Option<ServerInfo>,
    request_id: AtomicU64,
}

impl Client {
    /// Connect to a ZAP server.
    ///
    /// # Arguments
    /// * `url` - ZAP server URL (e.g., "zap://localhost:9999")
    ///
    /// # Example
    /// ```rust,no_run
    /// use hanzo_zap::Client;
    ///
    /// # async fn example() -> anyhow::Result<()> {
    /// let client = Client::connect("zap://localhost:9999").await?;
    /// # Ok(())
    /// # }
    /// ```
    pub async fn connect(url: &str) -> Result<Self> {
        let transport = ZapTransport::connect(url).await?;
        let mut client = Self {
            transport,
            server_info: None,
            request_id: AtomicU64::new(1),
        };

        // Perform handshake
        client.handshake().await?;

        Ok(client)
    }

    /// Perform protocol handshake.
    async fn handshake(&mut self) -> Result<()> {
        let client_info = ClientInfo {
            name: "hanzo-zap".to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
        };

        // Send init
        let payload = serde_json::to_vec(&client_info)?;
        self.transport.send(MessageType::Init, &payload).await?;

        // Receive init ack
        let (msg_type, payload) = self.transport.recv().await?;
        if msg_type != MessageType::InitAck {
            return Err(Error::Protocol(format!(
                "expected InitAck, got {:?}",
                msg_type
            )));
        }

        let server_info: ServerInfo = serde_json::from_slice(payload)?;
        info!(
            "connected to {} v{} (tools={}, resources={}, prompts={})",
            server_info.name,
            server_info.version,
            server_info.capabilities.tools,
            server_info.capabilities.resources,
            server_info.capabilities.prompts
        );

        self.server_info = Some(server_info);
        Ok(())
    }

    /// Get server info.
    pub fn server_info(&self) -> Option<&ServerInfo> {
        self.server_info.as_ref()
    }

    /// Generate a unique request ID.
    fn next_request_id(&self) -> String {
        let id = self.request_id.fetch_add(1, Ordering::SeqCst);
        format!("req-{}", id)
    }

    /// List available tools.
    pub async fn list_tools(&mut self) -> Result<Vec<Tool>> {
        self.transport.send(MessageType::ListTools, &[]).await?;

        let (msg_type, payload) = self.transport.recv().await?;
        match msg_type {
            MessageType::ListToolsResponse => {
                let tools: Vec<Tool> = serde_json::from_slice(payload)?;
                debug!("received {} tools", tools.len());
                Ok(tools)
            }
            MessageType::Error => {
                let err: crate::message::ErrorMessage = serde_json::from_slice(payload)?;
                Err(Error::Protocol(err.message))
            }
            _ => Err(Error::Protocol(format!(
                "expected ListToolsResponse, got {:?}",
                msg_type
            ))),
        }
    }

    /// Call a tool.
    ///
    /// # Arguments
    /// * `name` - Tool name
    /// * `args` - Tool arguments as JSON value
    ///
    /// # Example
    /// ```rust,no_run
    /// use hanzo_zap::Client;
    /// use serde_json::json;
    ///
    /// # async fn example() -> anyhow::Result<()> {
    /// let mut client = Client::connect("zap://localhost:9999").await?;
    /// let result = client.call_tool("search", json!({"query": "AI"})).await?;
    /// # Ok(())
    /// # }
    /// ```
    pub async fn call_tool(&mut self, name: &str, args: Value) -> Result<ToolResult> {
        let call = ToolCall {
            id: self.next_request_id(),
            name: name.to_string(),
            args,
            metadata: Default::default(),
        };

        let payload = serde_json::to_vec(&call)?;
        self.transport.send(MessageType::CallTool, &payload).await?;

        let (msg_type, payload) = self.transport.recv().await?;
        match msg_type {
            MessageType::CallToolResponse => {
                let result: ToolResult = serde_json::from_slice(payload)?;
                if let Some(err) = &result.error {
                    debug!("tool {} returned error: {}", name, err);
                } else {
                    debug!("tool {} completed successfully", name);
                }
                Ok(result)
            }
            MessageType::Error => {
                let err: crate::message::ErrorMessage = serde_json::from_slice(payload)?;
                Err(Error::Protocol(err.message))
            }
            _ => Err(Error::Protocol(format!(
                "expected CallToolResponse, got {:?}",
                msg_type
            ))),
        }
    }

    /// List available resources.
    pub async fn list_resources(&mut self) -> Result<Vec<crate::message::Resource>> {
        self.transport.send(MessageType::ListResources, &[]).await?;

        let (msg_type, payload) = self.transport.recv().await?;
        match msg_type {
            MessageType::ListResourcesResponse => {
                let resources = serde_json::from_slice(payload)?;
                Ok(resources)
            }
            MessageType::Error => {
                let err: crate::message::ErrorMessage = serde_json::from_slice(payload)?;
                Err(Error::Protocol(err.message))
            }
            _ => Err(Error::Protocol(format!(
                "expected ListResourcesResponse, got {:?}",
                msg_type
            ))),
        }
    }

    /// Read a resource.
    pub async fn read_resource(&mut self, uri: &str) -> Result<crate::message::ResourceContent> {
        let payload = serde_json::to_vec(&uri)?;
        self.transport
            .send(MessageType::ReadResource, &payload)
            .await?;

        let (msg_type, payload) = self.transport.recv().await?;
        match msg_type {
            MessageType::ReadResourceResponse => {
                let content = serde_json::from_slice(payload)?;
                Ok(content)
            }
            MessageType::Error => {
                let err: crate::message::ErrorMessage = serde_json::from_slice(payload)?;
                Err(Error::Protocol(err.message))
            }
            _ => Err(Error::Protocol(format!(
                "expected ReadResourceResponse, got {:?}",
                msg_type
            ))),
        }
    }

    /// List available prompts.
    pub async fn list_prompts(&mut self) -> Result<Vec<crate::message::Prompt>> {
        self.transport.send(MessageType::ListPrompts, &[]).await?;

        let (msg_type, payload) = self.transport.recv().await?;
        match msg_type {
            MessageType::ListPromptsResponse => {
                let prompts = serde_json::from_slice(payload)?;
                Ok(prompts)
            }
            MessageType::Error => {
                let err: crate::message::ErrorMessage = serde_json::from_slice(payload)?;
                Err(Error::Protocol(err.message))
            }
            _ => Err(Error::Protocol(format!(
                "expected ListPromptsResponse, got {:?}",
                msg_type
            ))),
        }
    }

    /// Close the connection.
    pub async fn close(self) -> Result<()> {
        self.transport.close().await
    }
}
