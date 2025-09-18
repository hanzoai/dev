//! Main client implementation - matches Python's Hanzo class

use std::sync::Arc;
use crate::{Result, HanzoError, auth::Auth, chat::Chat};

/// Main Hanzo client - async by default (matches Python's Hanzo class)
#[derive(Clone)]
pub struct Hanzo {
    inner: Arc<ClientInner>,
}

/// Async client - explicit async version (matches Python's AsyncHanzo)
pub struct AsyncHanzo {
    client: Hanzo,
}

/// Blocking client wrapper (matches Python's sync client)
pub struct BlockingHanzo {
    client: Hanzo,
    runtime: tokio::runtime::Runtime,
}

pub(crate) struct ClientInner {
    pub(crate) auth: Auth,
    pub(crate) base_url: String,
    pub(crate) http_client: reqwest::Client,
    pub(crate) default_model: String,
}

impl Hanzo {
    /// Create new client with API key - matches Python: Hanzo(api_key="sk-...")
    pub fn new(api_key: impl Into<String>) -> Result<Self> {
        Self::builder()
            .api_key(api_key)
            .build()
    }

    /// Create client from environment - matches Python: Hanzo()
    pub fn from_env() -> Result<Self> {
        Self::builder()
            .auth(crate::auth::Auth::from_env()?)
            .build()
    }

    /// Builder pattern for advanced configuration
    pub fn builder() -> ClientBuilder {
        ClientBuilder::default()
    }

    /// Get chat interface - matches Python: client.chat
    pub fn chat(&self) -> Chat {
        Chat::new(self.clone())
    }

    /// Get the inner client for internal use
    pub(crate) fn inner(&self) -> &Arc<ClientInner> {
        &self.inner
    }

    /// Make authenticated request
    pub(crate) async fn request(&self, req: reqwest::Request) -> Result<reqwest::Response> {
        // Apply authentication
        let req = self.inner.auth.apply(req)?;

        // Send request
        self.inner.http_client
            .execute(req)
            .await
            .map_err(|e| HanzoError::NetworkError(e.to_string()))
    }
}

/// Builder for client configuration
pub struct ClientBuilder {
    api_key: Option<String>,
    auth: Option<Auth>,
    base_url: Option<String>,
    model: Option<String>,
    timeout: Option<std::time::Duration>,
}

impl Default for ClientBuilder {
    fn default() -> Self {
        Self {
            api_key: None,
            auth: None,
            base_url: Some("https://api.hanzo.ai/v1".to_string()),
            model: Some("gpt-5".to_string()),
            timeout: Some(std::time::Duration::from_secs(60)),
        }
    }
}

impl ClientBuilder {
    pub fn api_key(mut self, key: impl Into<String>) -> Self {
        self.api_key = Some(key.into());
        self
    }

    pub fn auth(mut self, auth: Auth) -> Self {
        self.auth = Some(auth);
        self
    }

    pub fn base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = Some(url.into());
        self
    }

    pub fn model(mut self, model: impl Into<String>) -> Self {
        self.model = Some(model.into());
        self
    }

    pub fn timeout(mut self, timeout: std::time::Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    pub fn build(self) -> Result<Hanzo> {
        // Determine authentication
        let auth = if let Some(auth) = self.auth {
            auth
        } else if let Some(key) = self.api_key {
            Auth::api_key(key)
        } else {
            Auth::from_env()?
        };

        // Build HTTP client
        let mut http_builder = reqwest::Client::builder();
        if let Some(timeout) = self.timeout {
            http_builder = http_builder.timeout(timeout);
        }
        let http_client = http_builder
            .build()
            .map_err(|e| HanzoError::Unknown(e.to_string()))?;

        Ok(Hanzo {
            inner: Arc::new(ClientInner {
                auth,
                base_url: self.base_url.unwrap_or_else(|| "https://api.hanzo.ai/v1".to_string()),
                http_client,
                default_model: self.model.unwrap_or_else(|| "gpt-5".to_string()),
            }),
        })
    }
}

// Async client implementation
impl AsyncHanzo {
    pub fn new(api_key: impl Into<String>) -> Result<Self> {
        Ok(Self {
            client: Hanzo::new(api_key)?,
        })
    }

    pub fn chat(&self) -> Chat {
        self.client.chat()
    }
}

// Blocking client implementation
impl BlockingHanzo {
    pub fn new(api_key: impl Into<String>) -> Result<Self> {
        let runtime = tokio::runtime::Runtime::new()
            .map_err(|e| HanzoError::Unknown(e.to_string()))?;

        Ok(Self {
            client: Hanzo::new(api_key)?,
            runtime,
        })
    }

    pub fn chat(&self) -> BlockingChat {
        BlockingChat {
            chat: self.client.chat(),
            runtime: &self.runtime,
        }
    }
}

/// Blocking wrapper for chat
pub struct BlockingChat<'a> {
    chat: Chat,
    runtime: &'a tokio::runtime::Runtime,
}

impl<'a> BlockingChat<'a> {
    pub fn create(&self, messages: Vec<crate::Message>) -> Result<crate::Response> {
        self.runtime.block_on(self.chat.create(messages))
    }
}