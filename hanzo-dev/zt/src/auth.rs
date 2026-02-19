use crate::error::{Result, ZtError};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Trait for providing authentication credentials to the ZT controller
pub trait Credentials: Send + Sync + std::fmt::Debug {
    /// Returns the auth method identifier for the controller
    fn auth_method(&self) -> &str;

    /// Returns the authentication payload to send to the controller
    fn auth_payload(&self) -> Result<serde_json::Value>;

    /// Human-readable description for display
    fn display(&self) -> String;
}

/// Hanzo IAM JWT credentials — authenticates via external JWT with the ZT controller
#[derive(Debug, Clone)]
pub struct HanzoJwtCredentials {
    /// The JWT token from Hanzo IAM
    pub token: String,
    /// User email (extracted from token or auth file)
    pub email: Option<String>,
}

impl HanzoJwtCredentials {
    /// Create from an explicit token
    pub fn from_token(token: impl Into<String>) -> Self {
        Self {
            token: token.into(),
            email: None,
        }
    }

    /// Resolve credentials from environment:
    /// 1. HANZO_API_KEY env var
    /// 2. ~/.hanzo/auth.json file
    pub fn resolve() -> Result<Self> {
        // Try HANZO_API_KEY env
        if let Ok(key) = std::env::var("HANZO_API_KEY") {
            if !key.is_empty() {
                return Ok(Self {
                    token: key,
                    email: None,
                });
            }
        }

        // Try auth file
        let auth_path = auth_file_path()?;
        if auth_path.exists() {
            let contents = std::fs::read_to_string(&auth_path).map_err(|e| {
                ZtError::AuthFailed(format!("failed to read auth file: {e}"))
            })?;
            let auth: AuthFile = serde_json::from_str(&contents).map_err(|e| {
                ZtError::AuthFailed(format!("failed to parse auth file: {e}"))
            })?;

            if let Some(token) = auth.token.or(auth.api_key) {
                return Ok(Self {
                    token,
                    email: auth.email,
                });
            }
        }

        Err(ZtError::AuthFailed(
            "no credentials found — set HANZO_API_KEY or run `dev login`".into(),
        ))
    }
}

impl Credentials for HanzoJwtCredentials {
    fn auth_method(&self) -> &str {
        "ext-jwt"
    }

    fn auth_payload(&self) -> Result<serde_json::Value> {
        Ok(serde_json::json!({
            "configTypes": [],
        }))
    }

    fn display(&self) -> String {
        match &self.email {
            Some(email) => format!("Hanzo IAM ({email})"),
            None => {
                let masked = if self.token.len() > 8 {
                    format!("...{}", &self.token[self.token.len() - 5..])
                } else {
                    "***".to_string()
                };
                format!("Hanzo API key ({masked})")
            }
        }
    }
}

/// API key credentials — for direct controller API key auth
#[derive(Debug, Clone)]
pub struct ApiKeyCredentials {
    pub api_token: String,
}

impl ApiKeyCredentials {
    pub fn new(token: impl Into<String>) -> Self {
        Self {
            api_token: token.into(),
        }
    }
}

impl Credentials for ApiKeyCredentials {
    fn auth_method(&self) -> &str {
        "password"
    }

    fn auth_payload(&self) -> Result<serde_json::Value> {
        Ok(serde_json::json!({
            "configTypes": [],
        }))
    }

    fn display(&self) -> String {
        let masked = if self.api_token.len() > 8 {
            format!("...{}", &self.api_token[self.api_token.len() - 5..])
        } else {
            "***".to_string()
        };
        format!("API key ({masked})")
    }
}

/// Auth file at ~/.hanzo/auth.json
#[derive(Debug, Deserialize, Serialize)]
struct AuthFile {
    token: Option<String>,
    api_key: Option<String>,
    email: Option<String>,
}

fn auth_file_path() -> Result<PathBuf> {
    let home = std::env::var("HOME").or_else(|_| std::env::var("USERPROFILE")).map_err(|_| {
        ZtError::AuthFailed("cannot determine home directory".into())
    })?;
    Ok(PathBuf::from(home).join(".hanzo").join("auth.json"))
}
