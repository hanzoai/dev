//! # hanzo-zap
//!
//! ZAP (Zero-copy Agent Protocol) implementation for high-performance AI agent communication.
//!
//! ZAP provides a binary wire protocol that replaces JSON-RPC for performance-critical
//! agent workloads. Key features:
//!
//! - **Zero-copy parsing**: Messages read directly from network buffers
//! - **Zero allocations**: Buffer pooling eliminates GC pressure
//! - **MCP compatibility**: Gateway bridges existing MCP servers
//! - **10-100x performance**: Faster than JSON-RPC for agent communication
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use hanzo_zap::{Client, Gateway};
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     // Connect to ZAP gateway
//!     let client = Client::connect("zap://localhost:9999").await?;
//!
//!     // List available tools (from all bridged MCP servers)
//!     let tools = client.list_tools().await?;
//!
//!     // Call a tool
//!     let result = client.call_tool("search", serde_json::json!({
//!         "query": "Hanzo AI"
//!     })).await?;
//!
//!     Ok(())
//! }
//! ```
//!
//! ## Gateway Mode
//!
//! The ZAP Gateway can bridge multiple MCP servers:
//!
//! ```rust,no_run
//! use hanzo_zap::{Gateway, GatewayConfig, McpServerConfig, Transport};
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     let config = GatewayConfig {
//!         listen: "0.0.0.0:9999".to_string(),
//!         servers: vec![
//!             McpServerConfig {
//!                 name: "filesystem".to_string(),
//!                 transport: Transport::Stdio {
//!                     command: "mcp-server-filesystem".to_string(),
//!                     args: vec!["/home".to_string()],
//!                 },
//!             },
//!             McpServerConfig {
//!                 name: "search".to_string(),
//!                 transport: Transport::Zap {
//!                     url: "zaps://search.hanzo.ai:9999".to_string(),
//!                 },
//!             },
//!         ],
//!     };
//!
//!     let gateway = Gateway::new(config).await?;
//!     gateway.serve().await
//! }
//! ```
//!
//! ## Wire Protocol
//!
//! ZAP uses a simple length-prefixed binary format:
//!
//! ```text
//! +----------+----------+------------------+
//! | Length   | MsgType  | Payload          |
//! | (4 bytes)| (1 byte) | (variable)       |
//! | LE u32   |          |                  |
//! +----------+----------+------------------+
//! ```
//!
//! ## Related Specifications
//!
//! - **HIP-007**: ZAP Protocol Specification (Hanzo AI)
//! - **LP-120**: ZAP Transport Protocol (Lux Network)
//! - **MCP**: Model Context Protocol (Anthropic)

mod buffer;
mod client;
mod config;
mod error;
pub mod executor;
mod gateway;
mod message;
mod tools;
mod transport;
mod wire;

pub use buffer::{Buffer, BufferPool};
pub use client::Client;
pub use config::{Auth, GatewayConfig, McpServerConfig, Transport};
pub use error::{Error, Result};
pub use executor::{ExecutorContext, ToolDispatcher, ToolExecutor, default_dispatcher};
pub use gateway::Gateway;
pub use message::{Message, MessageType, Tool, ToolCall, ToolResult};
pub use tools::{ToolCategory, ToolDef, default_tools};
pub use transport::ZapTransport;
pub use wire::{Reader, Writer};

/// Tool modules for native tool implementations
pub mod native {
    pub use crate::tools::*;
}

/// Executor implementations for each tool category
pub mod executors {
    pub use crate::executor::*;
}

/// Default ZAP port
pub const DEFAULT_PORT: u16 = 9999;

/// Protocol version
pub const PROTOCOL_VERSION: u32 = 1;
