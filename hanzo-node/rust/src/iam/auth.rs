//! Authentication types and context

use super::jwt::Claims;
use super::rbac::Role;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Authentication method used
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AuthMethod {
    /// JWT Bearer token
    Bearer,
    /// API key
    ApiKey,
    /// Anonymous (no authentication)
    Anonymous,
    /// mTLS client certificate
    Certificate,
}

/// Identity of the authenticated entity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Identity {
    /// Unique identifier (subject from JWT or API key ID)
    pub id: String,
    /// Display name
    pub name: Option<String>,
    /// Email address
    pub email: Option<String>,
    /// Organization/tenant ID
    pub organization: Option<String>,
    /// Additional metadata
    pub metadata: std::collections::HashMap<String, String>,
}

impl Identity {
    /// Create identity from JWT claims
    pub fn from_claims(claims: &Claims) -> Self {
        Self {
            id: claims.sub.clone(),
            name: claims.name.clone(),
            email: claims.email.clone(),
            organization: claims.org.clone(),
            metadata: claims.custom.clone(),
        }
    }

    /// Create identity for API key
    pub fn api_key(id: &str, name: &str) -> Self {
        Self {
            id: id.to_string(),
            name: Some(name.to_string()),
            email: None,
            organization: None,
            metadata: std::collections::HashMap::new(),
        }
    }

    /// Create anonymous identity
    pub fn anonymous() -> Self {
        Self {
            id: "anonymous".to_string(),
            name: None,
            email: None,
            organization: None,
            metadata: std::collections::HashMap::new(),
        }
    }
}

/// Authentication context for a request
#[derive(Debug, Clone)]
pub struct AuthContext {
    /// Identity of the authenticated entity
    pub identity: Identity,
    /// Assigned role
    pub role: Role,
    /// How the user authenticated
    pub method: AuthMethod,
    /// When this context was created
    pub authenticated_at: DateTime<Utc>,
}

impl AuthContext {
    /// Create a new auth context
    pub fn new(identity: Identity, role: Role, method: AuthMethod) -> Self {
        Self {
            identity,
            role,
            method,
            authenticated_at: Utc::now(),
        }
    }

    /// Create anonymous context
    pub fn anonymous() -> Self {
        Self {
            identity: Identity::anonymous(),
            role: Role::anonymous(),
            method: AuthMethod::Anonymous,
            authenticated_at: Utc::now(),
        }
    }

    /// Check if this is an anonymous context
    pub fn is_anonymous(&self) -> bool {
        self.method == AuthMethod::Anonymous
    }

    /// Get the subject ID
    pub fn subject(&self) -> &str {
        &self.identity.id
    }

    /// Get the organization/tenant ID
    pub fn organization(&self) -> Option<&str> {
        self.identity.organization.as_deref()
    }
}

/// API key for programmatic access
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiKey {
    /// Unique identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// The actual key value (hashed in storage)
    #[serde(skip_serializing)]
    pub key: String,
    /// Associated role
    pub role: String,
    /// Organization/tenant this key belongs to
    pub organization: Option<String>,
    /// When the key was created
    pub created_at: DateTime<Utc>,
    /// When the key expires (None = never)
    pub expires_at: Option<DateTime<Utc>>,
    /// Allowed IP addresses (empty = any)
    pub allowed_ips: Vec<String>,
    /// Rate limit (requests per minute, 0 = unlimited)
    pub rate_limit: u32,
    /// Whether the key is active
    pub active: bool,
}

impl ApiKey {
    /// Create a new API key
    pub fn new(name: &str, role: &str) -> Self {
        let id = uuid::Uuid::new_v4().to_string();
        let key = generate_api_key();

        Self {
            id,
            name: name.to_string(),
            key,
            role: role.to_string(),
            organization: None,
            created_at: Utc::now(),
            expires_at: None,
            allowed_ips: vec![],
            rate_limit: 0,
            active: true,
        }
    }

    /// Create a new API key with expiration
    pub fn with_expiry(name: &str, role: &str, expires_in: chrono::Duration) -> Self {
        let mut key = Self::new(name, role);
        key.expires_at = Some(Utc::now() + expires_in);
        key
    }

    /// Set organization
    pub fn with_organization(mut self, org: &str) -> Self {
        self.organization = Some(org.to_string());
        self
    }

    /// Set rate limit
    pub fn with_rate_limit(mut self, limit: u32) -> Self {
        self.rate_limit = limit;
        self
    }

    /// Check if the key is valid (not expired and active)
    pub fn is_valid(&self) -> bool {
        if !self.active {
            return false;
        }
        if let Some(expires_at) = self.expires_at {
            if expires_at < Utc::now() {
                return false;
            }
        }
        true
    }
}

/// Generate a random API key
fn generate_api_key() -> String {
    use std::io::Read;

    let mut bytes = [0u8; 32];

    #[cfg(unix)]
    {
        if let Ok(mut f) = std::fs::File::open("/dev/urandom") {
            let _ = f.read_exact(&mut bytes);
        }
    }

    #[cfg(windows)]
    {
        // On Windows, fill with pseudo-random values
        for (i, byte) in bytes.iter_mut().enumerate() {
            *byte = (std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_nanos())
                .unwrap_or(0) as u8)
                .wrapping_add(i as u8);
        }
    }

    // Encode as base64url (URL-safe base64 without padding)
    base64_encode(&bytes)
}

/// Base64url encode without padding
fn base64_encode(data: &[u8]) -> String {
    const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

    let mut result = String::with_capacity((data.len() * 4 + 2) / 3);

    for chunk in data.chunks(3) {
        let b0 = chunk[0] as usize;
        let b1 = chunk.get(1).copied().unwrap_or(0) as usize;
        let b2 = chunk.get(2).copied().unwrap_or(0) as usize;

        result.push(ALPHABET[b0 >> 2] as char);
        result.push(ALPHABET[((b0 & 0x03) << 4) | (b1 >> 4)] as char);

        if chunk.len() > 1 {
            result.push(ALPHABET[((b1 & 0x0f) << 2) | (b2 >> 6)] as char);
        }
        if chunk.len() > 2 {
            result.push(ALPHABET[b2 & 0x3f] as char);
        }
    }

    // Prefix with "hzk_" for easy identification
    format!("hzk_{result}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_api_key_generation() {
        let key = generate_api_key();
        assert!(key.starts_with("hzk_"));
        assert!(key.len() > 40); // "hzk_" + base64 encoded 32 bytes
    }

    #[test]
    fn test_api_key_validity() {
        let key = ApiKey::new("test", "admin");
        assert!(key.is_valid());

        let expired_key = ApiKey::with_expiry("test", "admin", chrono::Duration::seconds(-1));
        assert!(!expired_key.is_valid());
    }
}
