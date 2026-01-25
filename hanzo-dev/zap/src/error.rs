//! Error types for ZAP protocol.

use thiserror::Error;

/// ZAP protocol errors.
#[derive(Error, Debug)]
pub enum Error {
    /// Connection error
    #[error("connection error: {0}")]
    Connection(String),

    /// Invalid URL scheme
    #[error("invalid URL scheme: expected zap:// or zaps://, got {0}")]
    InvalidScheme(String),

    /// Protocol error
    #[error("protocol error: {0}")]
    Protocol(String),

    /// Message too large
    #[error("message too large: {size} bytes exceeds max {max}")]
    MessageTooLarge { size: usize, max: usize },

    /// Invalid message type
    #[error("invalid message type: 0x{0:02x}")]
    InvalidMessageType(u8),

    /// Tool not found
    #[error("tool not found: {0}")]
    ToolNotFound(String),

    /// Server not found
    #[error("server not found: {0}")]
    ServerNotFound(String),

    /// Timeout
    #[error("operation timed out after {0:?}")]
    Timeout(std::time::Duration),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Serialization error
    #[error("serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    /// MCP client error
    #[error("MCP client error: {0}")]
    McpClient(String),

    /// Gateway error
    #[error("gateway error: {0}")]
    Gateway(String),

    /// Tool execution error
    #[error("tool error: {0}")]
    Tool(String),
}

/// Result type for ZAP operations.
pub type Result<T> = std::result::Result<T, Error>;
