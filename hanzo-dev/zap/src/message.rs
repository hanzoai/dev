//! ZAP message types.
//!
//! These types are compatible with MCP semantics but use binary encoding.

use crate::error::{Error, Result};
use serde::{Deserialize, Serialize};

/// Message types for ZAP protocol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum MessageType {
    // Handshake
    Init = 0x01,
    InitAck = 0x02,

    // Tool operations
    ListTools = 0x10,
    ListToolsResponse = 0x11,
    CallTool = 0x12,
    CallToolResponse = 0x13,

    // Resource operations
    ListResources = 0x20,
    ListResourcesResponse = 0x21,
    ReadResource = 0x22,
    ReadResourceResponse = 0x23,

    // Prompt operations
    ListPrompts = 0x30,
    ListPromptsResponse = 0x31,
    GetPrompt = 0x32,
    GetPromptResponse = 0x33,

    // Gateway operations
    AddServer = 0x40,
    AddServerResponse = 0x41,
    RemoveServer = 0x42,
    RemoveServerResponse = 0x43,
    ListServers = 0x44,
    ListServersResponse = 0x45,

    // Control
    Error = 0xFE,
    Close = 0xFF,
}

impl TryFrom<u8> for MessageType {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self> {
        match value {
            0x01 => Ok(Self::Init),
            0x02 => Ok(Self::InitAck),
            0x10 => Ok(Self::ListTools),
            0x11 => Ok(Self::ListToolsResponse),
            0x12 => Ok(Self::CallTool),
            0x13 => Ok(Self::CallToolResponse),
            0x20 => Ok(Self::ListResources),
            0x21 => Ok(Self::ListResourcesResponse),
            0x22 => Ok(Self::ReadResource),
            0x23 => Ok(Self::ReadResourceResponse),
            0x30 => Ok(Self::ListPrompts),
            0x31 => Ok(Self::ListPromptsResponse),
            0x32 => Ok(Self::GetPrompt),
            0x33 => Ok(Self::GetPromptResponse),
            0x40 => Ok(Self::AddServer),
            0x41 => Ok(Self::AddServerResponse),
            0x42 => Ok(Self::RemoveServer),
            0x43 => Ok(Self::RemoveServerResponse),
            0x44 => Ok(Self::ListServers),
            0x45 => Ok(Self::ListServersResponse),
            0xFE => Ok(Self::Error),
            0xFF => Ok(Self::Close),
            _ => Err(Error::InvalidMessageType(value)),
        }
    }
}

/// A ZAP message.
#[derive(Debug, Clone)]
pub enum Message {
    // Handshake
    Init(ClientInfo),
    InitAck(ServerInfo),

    // Tools
    ListTools,
    ListToolsResponse(Vec<Tool>),
    CallTool(ToolCall),
    CallToolResponse(ToolResult),

    // Resources
    ListResources,
    ListResourcesResponse(Vec<Resource>),
    ReadResource(String),
    ReadResourceResponse(ResourceContent),

    // Prompts
    ListPrompts,
    ListPromptsResponse(Vec<Prompt>),
    GetPrompt(GetPromptRequest),
    GetPromptResponse(Vec<PromptMessage>),

    // Gateway
    AddServer(AddServerRequest),
    AddServerResponse(String),
    RemoveServer(String),
    RemoveServerResponse(bool),
    ListServers,
    ListServersResponse(Vec<ConnectedServer>),

    // Control
    Error(ErrorMessage),
    Close,
}

/// Client information for handshake.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClientInfo {
    pub name: String,
    pub version: String,
}

/// Server information for handshake.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerInfo {
    pub name: String,
    pub version: String,
    pub capabilities: Capabilities,
}

/// Server capabilities.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Capabilities {
    pub tools: bool,
    pub resources: bool,
    pub prompts: bool,
    pub logging: bool,
}

/// A tool definition (MCP-compatible).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tool {
    pub name: String,
    pub description: String,
    #[serde(rename = "inputSchema")]
    pub input_schema: serde_json::Value,
    #[serde(default)]
    pub annotations: std::collections::HashMap<String, String>,
}

/// A tool call request.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolCall {
    pub id: String,
    pub name: String,
    #[serde(rename = "arguments")]
    pub args: serde_json::Value,
    #[serde(default)]
    pub metadata: std::collections::HashMap<String, String>,
}

/// A tool call result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolResult {
    pub id: String,
    pub content: serde_json::Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
    #[serde(default)]
    pub metadata: std::collections::HashMap<String, String>,
}

/// A resource definition (MCP-compatible).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resource {
    pub uri: String,
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(rename = "mimeType", skip_serializing_if = "Option::is_none")]
    pub mime_type: Option<String>,
}

/// Resource content.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceContent {
    pub uri: String,
    #[serde(rename = "mimeType")]
    pub mime_type: String,
    pub content: ResourceContentType,
}

/// Resource content type (text or binary).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ResourceContentType {
    Text(String),
    Binary(Vec<u8>),
}

/// A prompt definition (MCP-compatible).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Prompt {
    pub name: String,
    pub description: String,
    #[serde(default)]
    pub arguments: Vec<PromptArgument>,
}

/// A prompt argument.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PromptArgument {
    pub name: String,
    pub description: String,
    pub required: bool,
}

/// A prompt message.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PromptMessage {
    pub role: PromptRole,
    pub content: String,
}

/// Prompt message role.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PromptRole {
    User,
    Assistant,
    System,
}

/// Get prompt request.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetPromptRequest {
    pub name: String,
    #[serde(default)]
    pub arguments: std::collections::HashMap<String, String>,
}

/// Add server request.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AddServerRequest {
    pub name: String,
    pub url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth: Option<String>,
}

/// Connected server info.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectedServer {
    pub id: String,
    pub name: String,
    pub url: String,
    pub status: ServerStatus,
    pub tools: u32,
    pub resources: u32,
}

/// Server status.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ServerStatus {
    Connecting,
    Connected,
    Disconnected,
    Error,
}

/// Error message.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorMessage {
    pub code: i32,
    pub message: String,
}
