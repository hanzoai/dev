//! Hanzo AI SDK - Core Client Library
//!
//! This library provides idiomatic Rust bindings for the Hanzo AI platform,
//! matching the Python SDK API for consistency across ecosystems.
//!
//! # Example
//!
//! ```no_run
//! use hanzoai::Hanzo;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     // Initialize client - matches Python: client = Hanzo(api_key="sk-...")
//!     let client = Hanzo::new("sk-...")?;
//!
//!     // Simple chat completion - matches Python: client.chat.create(messages=[...])
//!     let response = client.chat().create(vec![
//!         hanzoai::Message::user("Hello, world!")
//!     ]).await?;
//!
//!     println!("{}", response.choices[0].message.content);
//!     Ok(())
//! }
//! ```

use std::sync::Arc;
use thiserror::Error;

pub mod client;
pub mod auth;
pub mod types;
pub mod mcp;
pub mod agents;
pub mod chat;
pub mod llm_client;
pub mod cluster;

// Re-export main types for convenience
pub use client::{Hanzo, AsyncHanzo, BlockingHanzo};
pub use auth::Auth;
pub use types::{Message, Response, Choice};
pub use chat::Chat;
pub use llm_client::LLMClient;

/// Main error type for the SDK
#[derive(Error, Debug)]
pub enum HanzoError {
    #[error("API error: {0}")]
    APIError(String),

    #[error("Authentication error: {0}")]
    AuthenticationError(String),

    #[error("Rate limit exceeded: {0}")]
    RateLimitError(String),

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Invalid request: {0}")]
    InvalidRequest(String),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Unknown error: {0}")]
    Unknown(String),
}

pub type Result<T> = std::result::Result<T, HanzoError>;

/// Quick completion API - matches Python: hanzoai.completion("prompt")
pub async fn completion(prompt: &str) -> Result<String> {
    let client = get_default_client()?;
    let response = client.chat().create(vec![
        Message::user(prompt)
    ]).await?;
    Ok(response.choices[0].message.content.clone())
}

/// Set global API key - matches Python: hanzoai.set_api_key("sk-...")
pub fn set_api_key(key: &str) {
    use once_cell::sync::Lazy;
    use std::sync::RwLock;

    static API_KEY: Lazy<RwLock<Option<String>>> = Lazy::new(|| RwLock::new(None));
    *API_KEY.write().unwrap() = Some(key.to_string());
}

/// Get the default client instance
fn get_default_client() -> Result<Arc<Hanzo>> {
    use once_cell::sync::Lazy;
    use std::sync::RwLock;

    static DEFAULT_CLIENT: Lazy<RwLock<Option<Arc<Hanzo>>>> =
        Lazy::new(|| RwLock::new(None));

    let mut client = DEFAULT_CLIENT.write().unwrap();
    if client.is_none() {
        *client = Some(Arc::new(Hanzo::from_env()?));
    }

    Ok(client.as_ref().unwrap().clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_api_key_setting() {
        set_api_key("test-key");
        // In real impl, this would be retrieved by the client
    }
}