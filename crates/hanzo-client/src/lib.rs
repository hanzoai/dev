//! Hanzo AI SDK - Unified Client
//!
//! This crate provides a single, composable client interface for all Hanzo AI operations.
//!
//! # Example
//!
//! ```no_run
//! use hanzo_client::{Client, Auth};
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     // Create client with builder pattern
//!     let client = Client::builder()
//!         .auth(Auth::from_env()?)
//!         .model("gpt-5")
//!         .build()?;
//!
//!     // Simple chat
//!     let response = client.chat("Hello, world!").await?;
//!     println!("{}", response);
//!
//!     Ok(())
//! }
//! ```

use std::sync::Arc;
use thiserror::Error;

pub mod builder;
pub mod chat;
pub mod stream;

pub use builder::ClientBuilder;
pub use chat::{ChatRequest, ChatResponse};

/// Client errors
#[derive(Error, Debug)]
pub enum ClientError {
    #[error("Transport error: {0}")]
    Transport(String),

    #[error("Authentication error: {0}")]
    Auth(String),

    #[error("Model not found: {0}")]
    ModelNotFound(String),

    #[error("Tool error: {0}")]
    Tool(String),

    #[error("Invalid configuration: {0}")]
    Config(String),
}

pub type Result<T> = std::result::Result<T, ClientError>;

/// The main client interface
pub struct Client {
    inner: Arc<ClientInner>,
}

pub struct ClientInner {
    transport: Box<dyn Transport<Error = Box<dyn std::error::Error + Send + Sync>>>,
    auth: Box<dyn AuthProvider>,
    models: Arc<ModelRegistry>,
    tools: Arc<ToolRegistry>,
}

impl Client {
    /// Create a new client builder
    pub fn builder() -> ClientBuilder {
        ClientBuilder::new()
    }

    /// Send a simple chat message
    pub async fn chat(&self, message: impl Into<String>) -> Result<String> {
        let request = ChatRequest::new(message.into());
        let response = self.send(request).await?;
        Ok(response.text())
    }

    /// Send a chat request
    pub async fn send(&self, request: ChatRequest) -> Result<ChatResponse> {
        // Validate model exists
        let model = self.inner.models
            .get(&request.model)
            .ok_or_else(|| ClientError::ModelNotFound(request.model.clone()))?;

        // Apply authentication
        let mut wire_request = request.to_wire(model);
        self.inner.auth.authenticate(&mut wire_request).await
            .map_err(|e| ClientError::Auth(e.to_string()))?;

        // Send via transport
        let response = self.inner.transport
            .send(wire_request).await
            .map_err(|e| ClientError::Transport(format!("{}", e)))?;

        Ok(ChatResponse::from_wire(response))
    }

    /// Stream a chat request
    pub fn stream(&self, request: ChatRequest) -> impl futures::Stream<Item = Result<Event>> {
        stream::chat_stream(self.inner.clone(), request)
    }
}

// Trait definitions (these would normally be in separate crates)

#[async_trait::async_trait]
trait Transport: Send + Sync {
    type Error: std::error::Error;

    async fn send(&self, request: WireRequest) -> std::result::Result<WireResponse, Self::Error>;
    fn stream(&self, request: WireRequest) -> BoxedEventStream;
}

#[async_trait::async_trait]
trait AuthProvider: Send + Sync {
    async fn authenticate(&self, request: &mut WireRequest) -> std::result::Result<(), Box<dyn std::error::Error>>;
}

struct ModelRegistry {
    models: std::collections::HashMap<String, Model>,
}

impl ModelRegistry {
    fn get(&self, id: &str) -> Option<&Model> {
        self.models.get(id)
    }
}

struct ToolRegistry {
    tools: std::collections::HashMap<String, Box<dyn Tool>>,
}

#[async_trait::async_trait]
trait Tool: Send + Sync {
    async fn execute(&self, params: serde_json::Value) -> std::result::Result<serde_json::Value, Box<dyn std::error::Error>>;
}

// Placeholder types (would be in hanzo-protocol)
use hanzo_protocol::{Event, BoxedEventStream};

pub struct WireRequest {
    pub model: String,
    pub messages: Vec<Message>,
}

pub struct WireResponse {
    pub content: String,
}

pub struct Model {
    pub id: String,
    pub family: String,
    pub context_window: usize,
}

pub struct Message {
    pub role: String,
    pub content: String,
}
/// Authentication provider
pub struct Auth;

impl Auth {
    pub fn from_env() -> Result<Self> {
        Ok(Self)
    }
}
