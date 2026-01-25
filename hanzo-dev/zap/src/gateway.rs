//! ZAP Gateway for bridging MCP servers.
//!
//! The Gateway aggregates multiple MCP servers (stdio, HTTP, WebSocket)
//! and exposes them through a single ZAP interface.

use crate::config::{GatewayConfig, McpServerConfig, Transport};
use crate::error::{Error, Result};
use crate::message::{
    Capabilities, ClientInfo, ConnectedServer, MessageType, ServerInfo, ServerStatus, Tool,
    ToolCall, ToolResult,
};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

/// A bridged MCP server.
struct BridgedServer {
    config: McpServerConfig,
    status: ServerStatus,
    tools: Vec<Tool>,
}

/// ZAP Gateway for bridging MCP servers.
pub struct Gateway {
    config: GatewayConfig,
    servers: Arc<RwLock<HashMap<String, BridgedServer>>>,
}

impl Gateway {
    /// Create a new gateway.
    pub async fn new(config: GatewayConfig) -> Result<Self> {
        let gateway = Self {
            config,
            servers: Arc::new(RwLock::new(HashMap::new())),
        };

        // Connect to configured servers
        for server_config in &gateway.config.servers {
            if let Err(e) = gateway.add_server(server_config.clone()).await {
                warn!("failed to connect to {}: {}", server_config.name, e);
            }
        }

        Ok(gateway)
    }

    /// Add an MCP server to the gateway.
    pub async fn add_server(&self, config: McpServerConfig) -> Result<String> {
        let id = config.name.clone();

        info!("adding server: {} ({:?})", config.name, config.transport);

        // TODO: Actually connect to the MCP server and discover tools
        // For now, create a placeholder
        let server = BridgedServer {
            config,
            status: ServerStatus::Connected,
            tools: Vec::new(),
        };

        let mut servers = self.servers.write().await;
        servers.insert(id.clone(), server);

        Ok(id)
    }

    /// Remove an MCP server from the gateway.
    pub async fn remove_server(&self, id: &str) -> Result<bool> {
        let mut servers = self.servers.write().await;
        Ok(servers.remove(id).is_some())
    }

    /// List connected servers.
    pub async fn list_servers(&self) -> Vec<ConnectedServer> {
        let servers = self.servers.read().await;
        servers
            .iter()
            .map(|(id, server)| ConnectedServer {
                id: id.clone(),
                name: server.config.name.clone(),
                url: match &server.config.transport {
                    Transport::Stdio { command, .. } => format!("stdio://{}", command),
                    Transport::Http { url } => url.clone(),
                    Transport::WebSocket { url } => url.clone(),
                    Transport::Zap { url } => url.clone(),
                    Transport::Unix { path } => format!("unix://{}", path),
                },
                status: server.status.clone(),
                tools: server.tools.len() as u32,
                resources: 0,
            })
            .collect()
    }

    /// List all tools from all connected servers.
    pub async fn list_tools(&self) -> Vec<Tool> {
        let servers = self.servers.read().await;
        servers
            .values()
            .flat_map(|s| s.tools.iter().cloned())
            .collect()
    }

    /// Call a tool (routes to appropriate server).
    pub async fn call_tool(&self, call: ToolCall) -> Result<ToolResult> {
        // Find server with this tool
        let servers = self.servers.read().await;
        for (id, server) in servers.iter() {
            if server.tools.iter().any(|t| t.name == call.name) {
                // TODO: Actually forward to MCP server
                debug!("routing tool {} to server {}", call.name, id);
                return Ok(ToolResult {
                    id: call.id,
                    content: serde_json::json!({"status": "ok"}),
                    error: None,
                    metadata: Default::default(),
                });
            }
        }

        Err(Error::ToolNotFound(call.name))
    }

    /// Start serving ZAP connections.
    pub async fn serve(&self) -> Result<()> {
        let listener = TcpListener::bind(&self.config.listen)
            .await
            .map_err(|e| Error::Connection(e.to_string()))?;

        info!("ZAP gateway listening on {}", self.config.listen);

        loop {
            match listener.accept().await {
                Ok((stream, addr)) => {
                    debug!("accepted connection from {}", addr);
                    let gateway = self.clone_handle();
                    tokio::spawn(async move {
                        if let Err(e) = gateway.handle_connection(stream).await {
                            error!("connection error: {}", e);
                        }
                    });
                }
                Err(e) => {
                    error!("accept error: {}", e);
                }
            }
        }
    }

    /// Create a cloneable handle to the gateway.
    fn clone_handle(&self) -> GatewayHandle {
        GatewayHandle {
            servers: Arc::clone(&self.servers),
            max_message_size: self.config.max_message_size,
        }
    }
}

/// A cloneable handle to the gateway for connection handling.
struct GatewayHandle {
    servers: Arc<RwLock<HashMap<String, BridgedServer>>>,
    max_message_size: usize,
}

impl GatewayHandle {
    async fn handle_connection(&self, mut stream: TcpStream) -> Result<()> {
        stream.set_nodelay(true).ok();

        // Handle handshake
        let (msg_type, payload) = self.recv_message(&mut stream).await?;
        if msg_type != MessageType::Init {
            return Err(Error::Protocol("expected Init message".to_string()));
        }

        let _client_info: ClientInfo = serde_json::from_slice(payload)?;

        // Send server info
        let server_info = ServerInfo {
            name: "hanzo-zap-gateway".to_string(),
            version: env!("CARGO_PKG_VERSION").to_string(),
            capabilities: Capabilities {
                tools: true,
                resources: true,
                prompts: true,
                logging: true,
            },
        };
        let payload = serde_json::to_vec(&server_info)?;
        self.send_message(&mut stream, MessageType::InitAck, &payload)
            .await?;

        // Handle requests
        loop {
            let (msg_type, payload) = match self.recv_message(&mut stream).await {
                Ok(msg) => msg,
                Err(Error::Io(e)) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                    debug!("client disconnected");
                    break;
                }
                Err(e) => return Err(e),
            };

            match msg_type {
                MessageType::ListTools => {
                    let tools = self.list_tools().await;
                    let payload = serde_json::to_vec(&tools)?;
                    self.send_message(&mut stream, MessageType::ListToolsResponse, &payload)
                        .await?;
                }
                MessageType::CallTool => {
                    let call: ToolCall = serde_json::from_slice(payload)?;
                    let result = self.call_tool(call).await?;
                    let payload = serde_json::to_vec(&result)?;
                    self.send_message(&mut stream, MessageType::CallToolResponse, &payload)
                        .await?;
                }
                MessageType::ListServers => {
                    let servers = self.list_servers().await;
                    let payload = serde_json::to_vec(&servers)?;
                    self.send_message(&mut stream, MessageType::ListServersResponse, &payload)
                        .await?;
                }
                MessageType::Close => {
                    debug!("client requested close");
                    break;
                }
                _ => {
                    warn!("unhandled message type: {:?}", msg_type);
                }
            }
        }

        Ok(())
    }

    async fn recv_message<'a>(&self, stream: &mut TcpStream) -> Result<(MessageType, &'a [u8])> {
        // Read length header
        let mut header = [0u8; 4];
        stream.read_exact(&mut header).await?;
        let length = u32::from_le_bytes(header) as usize;

        if length > self.max_message_size {
            return Err(Error::MessageTooLarge {
                size: length,
                max: self.max_message_size,
            });
        }

        // Read message body
        let mut body = vec![0u8; length];
        stream.read_exact(&mut body).await?;

        if body.is_empty() {
            return Err(Error::Protocol("empty message".to_string()));
        }

        let msg_type = MessageType::try_from(body[0])?;

        // SAFETY: We're returning a reference that lives as long as body,
        // but body is local. This is a simplification - in production,
        // we'd use a proper buffer management strategy.
        let payload: &'static [u8] = Box::leak(body[1..].to_vec().into_boxed_slice());

        Ok((msg_type, payload))
    }

    async fn send_message(
        &self,
        stream: &mut TcpStream,
        msg_type: MessageType,
        payload: &[u8],
    ) -> Result<()> {
        let total_len = 1 + payload.len();
        stream.write_all(&(total_len as u32).to_le_bytes()).await?;
        stream.write_all(&[msg_type as u8]).await?;
        stream.write_all(payload).await?;
        Ok(())
    }

    async fn list_tools(&self) -> Vec<Tool> {
        let servers = self.servers.read().await;
        servers
            .values()
            .flat_map(|s| s.tools.iter().cloned())
            .collect()
    }

    async fn list_servers(&self) -> Vec<ConnectedServer> {
        let servers = self.servers.read().await;
        servers
            .iter()
            .map(|(id, server)| ConnectedServer {
                id: id.clone(),
                name: server.config.name.clone(),
                url: match &server.config.transport {
                    Transport::Stdio { command, .. } => format!("stdio://{}", command),
                    Transport::Http { url } => url.clone(),
                    Transport::WebSocket { url } => url.clone(),
                    Transport::Zap { url } => url.clone(),
                    Transport::Unix { path } => format!("unix://{}", path),
                },
                status: server.status.clone(),
                tools: server.tools.len() as u32,
                resources: 0,
            })
            .collect()
    }

    async fn call_tool(&self, call: ToolCall) -> Result<ToolResult> {
        // Find server with this tool
        let servers = self.servers.read().await;
        for (id, server) in servers.iter() {
            if server.tools.iter().any(|t| t.name == call.name) {
                debug!("routing tool {} to server {}", call.name, id);
                return Ok(ToolResult {
                    id: call.id,
                    content: serde_json::json!({"status": "ok"}),
                    error: None,
                    metadata: Default::default(),
                });
            }
        }

        Err(Error::ToolNotFound(call.name))
    }
}
