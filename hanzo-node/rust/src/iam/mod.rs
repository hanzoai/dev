//! Identity and Access Management (IAM) module for Hanzo Node
//!
//! Provides:
//! - JWT token validation (OIDC/OAuth2 compatible)
//! - Role-Based Access Control (RBAC)
//! - API key management with optional sled persistence
//! - gRPC and HTTP authentication middleware

mod auth;
mod error;
mod jwt;
mod middleware;
mod permission;
mod rbac;

pub use auth::{ApiKey, AuthContext, AuthMethod, Identity};
pub use error::{IamError, IamResult};
pub use jwt::{Claims, JwtValidator};
pub use middleware::{GrpcAuthInterceptor, HttpAuthLayer};
pub use permission::{Action, Permission, Resource};
pub use rbac::{Role, RoleBinding, RoleManager};

use crate::storage::KeyValueStore;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Storage tree for API keys
const API_KEYS_TREE: &str = "iam:apikeys";

/// IAM configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IamConfig {
    /// Enable authentication (set to false for development)
    #[serde(default = "default_enabled")]
    pub enabled: bool,

    /// JWT issuer URL (OIDC provider)
    pub issuer: Option<String>,

    /// JWT audience (client ID)
    pub audience: Option<String>,

    /// JWKS endpoint for public key fetching
    pub jwks_uri: Option<String>,

    /// Shared secret for HS256 tokens (development only)
    #[serde(skip_serializing)]
    pub secret: Option<String>,

    /// API key header name
    #[serde(default = "default_api_key_header")]
    pub api_key_header: String,

    /// Authorization header name
    #[serde(default = "default_auth_header")]
    pub auth_header: String,

    /// Default role for authenticated users without explicit role
    #[serde(default = "default_role")]
    pub default_role: String,

    /// Allow anonymous access to health endpoints
    #[serde(default = "default_allow_anonymous_health")]
    pub allow_anonymous_health: bool,

    /// Casdoor/IAM service base URL
    pub iam_base_url: Option<String>,
}

fn default_enabled() -> bool {
    true
}

fn default_api_key_header() -> String {
    "x-api-key".to_string()
}

fn default_auth_header() -> String {
    "authorization".to_string()
}

fn default_role() -> String {
    "viewer".to_string()
}

fn default_allow_anonymous_health() -> bool {
    true
}

impl Default for IamConfig {
    fn default() -> Self {
        Self {
            enabled: false, // Disabled by default for easy development
            issuer: None,
            audience: None,
            jwks_uri: None,
            secret: None,
            api_key_header: default_api_key_header(),
            auth_header: default_auth_header(),
            default_role: default_role(),
            allow_anonymous_health: true,
            iam_base_url: None,
        }
    }
}

/// IAM service - main entry point for authentication and authorization
pub struct IamService {
    config: IamConfig,
    jwt_validator: Arc<JwtValidator>,
    role_manager: Arc<RoleManager>,
    /// In-memory cache of API keys (key value -> ApiKey)
    api_keys: Arc<tokio::sync::RwLock<std::collections::HashMap<String, ApiKey>>>,
    /// Optional persistent storage for API keys
    storage: Option<Arc<dyn KeyValueStore>>,
}

impl IamService {
    /// Create a new IAM service without persistent storage
    pub async fn new(config: IamConfig) -> IamResult<Self> {
        Self::with_storage(config, None).await
    }

    /// Create a new IAM service with optional persistent storage
    pub async fn with_storage(
        config: IamConfig,
        storage: Option<Arc<dyn KeyValueStore>>,
    ) -> IamResult<Self> {
        let jwt_validator = Arc::new(JwtValidator::new(&config).await?);
        let role_manager = Arc::new(RoleManager::new_with_defaults());

        let mut api_keys = std::collections::HashMap::new();

        // Load API keys from storage if available
        if let Some(ref store) = storage {
            if let Ok(items) = store.kv_list(API_KEYS_TREE) {
                for (_, value) in items {
                    if let Ok(key) = serde_json::from_slice::<ApiKey>(&value) {
                        api_keys.insert(key.key.clone(), key);
                    }
                }
            }
        }

        Ok(Self {
            config,
            jwt_validator,
            role_manager,
            api_keys: Arc::new(tokio::sync::RwLock::new(api_keys)),
            storage,
        })
    }

    /// Check if IAM is enabled
    pub fn is_enabled(&self) -> bool {
        self.config.enabled
    }

    /// Get the JWT validator
    pub fn jwt_validator(&self) -> Arc<JwtValidator> {
        self.jwt_validator.clone()
    }

    /// Get the role manager
    pub fn role_manager(&self) -> Arc<RoleManager> {
        self.role_manager.clone()
    }

    /// Authenticate a request using bearer token
    pub async fn authenticate_bearer(&self, token: &str) -> IamResult<AuthContext> {
        if !self.config.enabled {
            return Ok(AuthContext::anonymous());
        }

        let claims = self.jwt_validator.validate(token).await?;
        let identity = Identity::from_claims(&claims);

        // Get role from claims or use default
        let role_name = claims
            .roles
            .first()
            .cloned()
            .unwrap_or_else(|| self.config.default_role.clone());

        let role = self
            .role_manager
            .get_role(&role_name)
            .unwrap_or_else(|| self.role_manager.default_role());

        Ok(AuthContext::new(identity, role, AuthMethod::Bearer))
    }

    /// Authenticate a request using API key
    pub async fn authenticate_api_key(&self, key: &str) -> IamResult<AuthContext> {
        if !self.config.enabled {
            return Ok(AuthContext::anonymous());
        }

        let api_keys = self.api_keys.read().await;
        let api_key = api_keys.get(key).ok_or(IamError::InvalidApiKey)?;

        // Check if key is expired
        if let Some(expires_at) = api_key.expires_at {
            if expires_at < chrono::Utc::now() {
                return Err(IamError::ApiKeyExpired);
            }
        }

        let identity = Identity::api_key(&api_key.id, &api_key.name);
        let role = self
            .role_manager
            .get_role(&api_key.role)
            .unwrap_or_else(|| self.role_manager.default_role());

        Ok(AuthContext::new(identity, role, AuthMethod::ApiKey))
    }

    /// Register an API key
    pub async fn register_api_key(&self, api_key: ApiKey) -> IamResult<String> {
        let key = api_key.key.clone();

        // Persist to storage if available
        if let Some(ref store) = self.storage {
            let data = serde_json::to_vec(&api_key)
                .map_err(|e| IamError::Internal(format!("Serialization error: {e}")))?;
            store
                .kv_put(API_KEYS_TREE, api_key.id.as_bytes(), &data)
                .map_err(|e| IamError::Internal(format!("Storage error: {e}")))?;
        }

        let mut api_keys = self.api_keys.write().await;
        api_keys.insert(key.clone(), api_key);
        Ok(key)
    }

    /// Revoke an API key
    pub async fn revoke_api_key(&self, key: &str) -> IamResult<bool> {
        let mut api_keys = self.api_keys.write().await;

        if let Some(removed) = api_keys.remove(key) {
            // Remove from storage if available
            if let Some(ref store) = self.storage {
                let _ = store.kv_delete(API_KEYS_TREE, removed.id.as_bytes());
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Check if an action is permitted
    pub fn authorize(&self, ctx: &AuthContext, resource: &Resource, action: &Action) -> bool {
        if !self.config.enabled {
            return true;
        }

        // Check if this is an anonymous health check (allowed)
        if self.config.allow_anonymous_health
            && ctx.is_anonymous()
            && *resource == Resource::Health
            && *action == Action::Read
        {
            return true;
        }

        ctx.role.has_permission(resource, action)
    }

    /// Create a gRPC auth interceptor
    pub fn grpc_interceptor(&self) -> GrpcAuthInterceptor {
        GrpcAuthInterceptor::new(
            self.config.clone(),
            self.jwt_validator.clone(),
            self.role_manager.clone(),
            self.api_keys.clone(),
        )
    }

    /// Create an HTTP auth layer for axum
    pub fn http_layer(&self) -> HttpAuthLayer {
        HttpAuthLayer::new(
            self.config.clone(),
            self.jwt_validator.clone(),
            self.role_manager.clone(),
            self.api_keys.clone(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::Storage;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_iam_service_with_storage() {
        let tmp = TempDir::new().unwrap();
        let storage = Arc::new(Storage::new(tmp.path()).unwrap());

        let config = IamConfig {
            enabled: true,
            ..Default::default()
        };

        let iam = IamService::with_storage(config, Some(storage.clone()))
            .await
            .unwrap();

        // Register an API key
        let api_key = ApiKey::new("test-key", "operator");
        let key_value = iam.register_api_key(api_key.clone()).await.unwrap();

        // Verify it's in storage
        let stored = storage
            .kv_get(API_KEYS_TREE, api_key.id.as_bytes())
            .unwrap();
        assert!(stored.is_some());

        // Authenticate within the same service instance
        let result = iam.authenticate_api_key(&key_value).await;
        assert!(result.is_ok());

        let ctx = result.unwrap();
        assert_eq!(ctx.identity.id, api_key.id);
    }

    #[tokio::test]
    async fn test_api_key_revocation_with_storage() {
        let tmp = TempDir::new().unwrap();
        let storage = Arc::new(Storage::new(tmp.path()).unwrap());

        let config = IamConfig {
            enabled: true,
            ..Default::default()
        };

        let iam = IamService::with_storage(config, Some(storage.clone()))
            .await
            .unwrap();

        let api_key = ApiKey::new("revoke-test", "viewer");
        let key_value = iam.register_api_key(api_key.clone()).await.unwrap();

        // Revoke
        let revoked = iam.revoke_api_key(&key_value).await.unwrap();
        assert!(revoked);

        // Verify removed from storage
        let stored = storage
            .kv_get(API_KEYS_TREE, api_key.id.as_bytes())
            .unwrap();
        assert!(stored.is_none());
    }
}
