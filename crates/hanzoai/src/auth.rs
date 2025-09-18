//! Authentication module - matches Python's auth system

use crate::{Result, HanzoError};
use std::env;

/// Authentication configuration
#[derive(Debug, Clone)]
pub enum Auth {
    /// API Key authentication
    ApiKey(String),
    /// Bearer token authentication
    Bearer(String),
    /// No authentication
    None,
}

impl Auth {
    /// Create from API key - matches Python
    pub fn api_key(key: impl Into<String>) -> Self {
        Auth::ApiKey(key.into())
    }

    /// Create from bearer token
    pub fn bearer(token: impl Into<String>) -> Self {
        Auth::Bearer(token.into())
    }

    /// Load from environment variables - matches Python behavior
    pub fn from_env() -> Result<Self> {
        // Check multiple environment variables in order of preference
        if let Ok(key) = env::var("HANZO_API_KEY") {
            return Ok(Auth::ApiKey(key));
        }

        if let Ok(key) = env::var("OPENAI_API_KEY") {
            return Ok(Auth::ApiKey(key));
        }

        if let Ok(key) = env::var("ANTHROPIC_API_KEY") {
            return Ok(Auth::ApiKey(key));
        }

        if let Ok(token) = env::var("HANZO_BEARER_TOKEN") {
            return Ok(Auth::Bearer(token));
        }

        Err(HanzoError::AuthenticationError(
            "No authentication found in environment. Set HANZO_API_KEY or OPENAI_API_KEY".to_string()
        ))
    }

    /// Apply authentication to a request
    pub(crate) fn apply(&self, mut req: reqwest::Request) -> Result<reqwest::Request> {
        match self {
            Auth::ApiKey(key) => {
                req.headers_mut().insert(
                    "Authorization",
                    format!("Bearer {}", key)
                        .parse()
                        .map_err(|e: reqwest::header::InvalidHeaderValue| {
                            HanzoError::AuthenticationError(e.to_string())
                        })?,
                );
            }
            Auth::Bearer(token) => {
                req.headers_mut().insert(
                    "Authorization",
                    format!("Bearer {}", token)
                        .parse()
                        .map_err(|e: reqwest::header::InvalidHeaderValue| {
                            HanzoError::AuthenticationError(e.to_string())
                        })?,
                );
            }
            Auth::None => {}
        }

        Ok(req)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_auth_from_api_key() {
        let auth = Auth::api_key("test-key");
        matches!(auth, Auth::ApiKey(k) if k == "test-key");
    }

    #[test]
    fn test_auth_from_bearer() {
        let auth = Auth::bearer("test-token");
        matches!(auth, Auth::Bearer(t) if t == "test-token");
    }
}