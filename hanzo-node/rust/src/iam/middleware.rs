//! Authentication middleware for gRPC and HTTP

use super::auth::{ApiKey, AuthContext, AuthMethod, Identity};
use super::error::{IamError, IamResult};
use super::jwt::JwtValidator;
use super::permission::{Action, Resource};
use super::rbac::RoleManager;
use super::IamConfig;
use std::collections::HashMap;
use std::sync::Arc;
use std::task::{Context, Poll};
use tokio::sync::RwLock;
use tonic::codegen::http;

// ============================================================================
// gRPC Authentication Interceptor
// ============================================================================

/// gRPC authentication interceptor for tonic
#[derive(Clone)]
pub struct GrpcAuthInterceptor {
    config: IamConfig,
    jwt_validator: Arc<JwtValidator>,
    role_manager: Arc<RoleManager>,
    api_keys: Arc<RwLock<HashMap<String, ApiKey>>>,
}

impl GrpcAuthInterceptor {
    /// Create a new gRPC auth interceptor
    pub fn new(
        config: IamConfig,
        jwt_validator: Arc<JwtValidator>,
        role_manager: Arc<RoleManager>,
        api_keys: Arc<RwLock<HashMap<String, ApiKey>>>,
    ) -> Self {
        Self {
            config,
            jwt_validator,
            role_manager,
            api_keys,
        }
    }

    /// Extract authentication from request metadata
    pub async fn authenticate(
        &self,
        metadata: &tonic::metadata::MetadataMap,
    ) -> IamResult<AuthContext> {
        if !self.config.enabled {
            return Ok(AuthContext::anonymous());
        }

        // Try bearer token first
        if let Some(auth) = metadata.get(&self.config.auth_header) {
            let auth_str = auth
                .to_str()
                .map_err(|_| IamError::InvalidToken("Invalid authorization header".to_string()))?;

            if let Some(token) = auth_str.strip_prefix("Bearer ") {
                return self.authenticate_bearer(token).await;
            }
        }

        // Try API key
        if let Some(api_key) = metadata.get(&self.config.api_key_header) {
            let key = api_key
                .to_str()
                .map_err(|_| IamError::InvalidApiKey)?;
            return self.authenticate_api_key(key).await;
        }

        // No authentication provided
        if self.config.allow_anonymous_health {
            // Allow anonymous for health checks
            Ok(AuthContext::anonymous())
        } else {
            Err(IamError::Unauthenticated)
        }
    }

    /// Authenticate using bearer token
    async fn authenticate_bearer(&self, token: &str) -> IamResult<AuthContext> {
        let claims = self.jwt_validator.validate(token).await?;
        let identity = Identity::from_claims(&claims);

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

    /// Authenticate using API key
    async fn authenticate_api_key(&self, key: &str) -> IamResult<AuthContext> {
        let api_keys = self.api_keys.read().await;
        let api_key = api_keys.get(key).ok_or(IamError::InvalidApiKey)?;

        if !api_key.is_valid() {
            return Err(IamError::ApiKeyExpired);
        }

        let identity = Identity::api_key(&api_key.id, &api_key.name);
        let role = self
            .role_manager
            .get_role(&api_key.role)
            .unwrap_or_else(|| self.role_manager.default_role());

        Ok(AuthContext::new(identity, role, AuthMethod::ApiKey))
    }

    /// Check authorization for a specific operation
    pub fn authorize(&self, ctx: &AuthContext, resource: &Resource, action: &Action) -> bool {
        if !self.config.enabled {
            return true;
        }

        // Allow anonymous health checks
        if self.config.allow_anonymous_health
            && ctx.is_anonymous()
            && *resource == Resource::Health
            && *action == Action::Read
        {
            return true;
        }

        ctx.role.has_permission(resource, action)
    }
}

/// Extension trait for tonic Request to extract AuthContext
#[allow(dead_code)]
pub trait RequestAuthExt {
    /// Get the authentication context from request extensions
    fn auth_context(&self) -> Option<&AuthContext>;
}

impl<T> RequestAuthExt for tonic::Request<T> {
    fn auth_context(&self) -> Option<&AuthContext> {
        self.extensions().get::<AuthContext>()
    }
}

// ============================================================================
// HTTP Authentication Layer for Axum
// ============================================================================

/// Axum layer for authentication
#[derive(Clone)]
pub struct HttpAuthLayer {
    config: IamConfig,
    jwt_validator: Arc<JwtValidator>,
    role_manager: Arc<RoleManager>,
    api_keys: Arc<RwLock<HashMap<String, ApiKey>>>,
}

impl HttpAuthLayer {
    /// Create a new HTTP auth layer
    pub fn new(
        config: IamConfig,
        jwt_validator: Arc<JwtValidator>,
        role_manager: Arc<RoleManager>,
        api_keys: Arc<RwLock<HashMap<String, ApiKey>>>,
    ) -> Self {
        Self {
            config,
            jwt_validator,
            role_manager,
            api_keys,
        }
    }
}

impl<S> tower::Layer<S> for HttpAuthLayer {
    type Service = HttpAuthMiddleware<S>;

    fn layer(&self, inner: S) -> Self::Service {
        HttpAuthMiddleware {
            inner,
            config: self.config.clone(),
            jwt_validator: self.jwt_validator.clone(),
            role_manager: self.role_manager.clone(),
            api_keys: self.api_keys.clone(),
        }
    }
}

/// Axum middleware service for authentication
#[derive(Clone)]
pub struct HttpAuthMiddleware<S> {
    inner: S,
    config: IamConfig,
    jwt_validator: Arc<JwtValidator>,
    role_manager: Arc<RoleManager>,
    api_keys: Arc<RwLock<HashMap<String, ApiKey>>>,
}

impl<S, ReqBody, ResBody> tower::Service<http::Request<ReqBody>> for HttpAuthMiddleware<S>
where
    S: tower::Service<http::Request<ReqBody>, Response = http::Response<ResBody>>
        + Clone
        + Send
        + 'static,
    S::Future: Send,
    ReqBody: Send + 'static,
    ResBody: Default + Send + 'static,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = std::pin::Pin<
        Box<dyn std::future::Future<Output = Result<Self::Response, Self::Error>> + Send>,
    >;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }

    fn call(&mut self, mut req: http::Request<ReqBody>) -> Self::Future {
        let inner = self.inner.clone();
        let mut inner = std::mem::replace(&mut self.inner, inner);

        let config = self.config.clone();
        let jwt_validator = self.jwt_validator.clone();
        let role_manager = self.role_manager.clone();
        let api_keys = self.api_keys.clone();

        Box::pin(async move {
            // Skip auth if disabled
            if !config.enabled {
                let ctx = AuthContext::anonymous();
                req.extensions_mut().insert(ctx);
                return inner.call(req).await;
            }

            // Check if this is a health endpoint (allow anonymous)
            let path = req.uri().path();
            let is_health = path == "/health" || path == "/healthz" || path == "/ready";

            // Try to authenticate
            let auth_result = authenticate_http_request(
                req.headers(),
                &config,
                &jwt_validator,
                &role_manager,
                &api_keys,
            )
            .await;

            match auth_result {
                Ok(ctx) => {
                    req.extensions_mut().insert(ctx);
                    inner.call(req).await
                }
                Err(e) => {
                    // Allow anonymous health checks
                    if is_health && config.allow_anonymous_health {
                        let ctx = AuthContext::anonymous();
                        req.extensions_mut().insert(ctx);
                        return inner.call(req).await;
                    }

                    // Return 401 Unauthorized
                    tracing::warn!(error = %e, "Authentication failed");
                    let response = http::Response::builder()
                        .status(e.to_http_status())
                        .header("WWW-Authenticate", "Bearer")
                        .body(ResBody::default())
                        .unwrap_or_else(|_| http::Response::new(ResBody::default()));
                    Ok(response)
                }
            }
        })
    }
}

/// Authenticate an HTTP request
async fn authenticate_http_request(
    headers: &http::HeaderMap,
    config: &IamConfig,
    jwt_validator: &JwtValidator,
    role_manager: &RoleManager,
    api_keys: &RwLock<HashMap<String, ApiKey>>,
) -> IamResult<AuthContext> {
    // Try bearer token
    if let Some(auth) = headers.get(http::header::AUTHORIZATION) {
        let auth_str = auth
            .to_str()
            .map_err(|_| IamError::InvalidToken("Invalid authorization header".to_string()))?;

        if let Some(token) = auth_str.strip_prefix("Bearer ") {
            let claims = jwt_validator.validate(token).await?;
            let identity = Identity::from_claims(&claims);

            let role_name = claims
                .roles
                .first()
                .cloned()
                .unwrap_or_else(|| config.default_role.clone());

            let role = role_manager
                .get_role(&role_name)
                .unwrap_or_else(|| role_manager.default_role());

            return Ok(AuthContext::new(identity, role, AuthMethod::Bearer));
        }
    }

    // Try API key
    if let Some(api_key_header) = headers.get(&config.api_key_header) {
        let key = api_key_header
            .to_str()
            .map_err(|_| IamError::InvalidApiKey)?;

        let api_keys = api_keys.read().await;
        let api_key = api_keys.get(key).ok_or(IamError::InvalidApiKey)?;

        if !api_key.is_valid() {
            return Err(IamError::ApiKeyExpired);
        }

        let identity = Identity::api_key(&api_key.id, &api_key.name);
        let role = role_manager
            .get_role(&api_key.role)
            .unwrap_or_else(|| role_manager.default_role());

        return Ok(AuthContext::new(identity, role, AuthMethod::ApiKey));
    }

    // No authentication provided
    Err(IamError::Unauthenticated)
}

// ============================================================================
// Authorization helpers
// ============================================================================

/// Map gRPC method to resource/action
#[allow(dead_code)]
pub fn grpc_method_to_permission(method: &str) -> (Resource, Action) {
    // Method format: /hanzo.node.v1.NodeService/MethodName
    let method_name = method.rsplit('/').next().unwrap_or(method);

    match method_name {
        // Health and Status
        "GetHealth" => (Resource::Health, Action::Read),
        "GetStatus" => (Resource::Metrics, Action::Read),
        "GetMetrics" => (Resource::Metrics, Action::Read),

        // Node registration
        "RegisterNode" => (Resource::Node, Action::Create),
        "Heartbeat" => (Resource::Node, Action::Update),
        "UpdateCapabilities" => (Resource::Node, Action::Update),

        // Deployments
        "Deploy" => (Resource::Deployment, Action::Create),
        "Stop" => (Resource::Deployment, Action::Update),
        "Remove" => (Resource::Deployment, Action::Delete),
        "Scale" => (Resource::Deployment, Action::Update),

        // Logs and Events
        "GetLogs" => (Resource::Logs, Action::Read),
        "GetEvents" => (Resource::Events, Action::Read),

        // Containers
        "ListContainers" => (Resource::Container, Action::Read),
        "InspectContainer" => (Resource::Container, Action::Read),
        "ExecInContainer" => (Resource::Container, Action::Execute),

        // Default
        _ => (Resource::All, Action::All),
    }
}

/// Map HTTP method + path to resource/action
#[allow(dead_code)]
pub fn http_method_to_permission(method: &http::Method, path: &str) -> (Resource, Action) {
    // Health endpoints
    if path == "/health" || path == "/healthz" || path == "/ready" {
        return (Resource::Health, Action::Read);
    }

    // Metrics endpoints
    if path.starts_with("/metrics") {
        return (Resource::Metrics, Action::Read);
    }

    // Map HTTP method to action
    let action = match *method {
        http::Method::GET | http::Method::HEAD => Action::Read,
        http::Method::POST => Action::Create,
        http::Method::PUT | http::Method::PATCH => Action::Update,
        http::Method::DELETE => Action::Delete,
        _ => Action::Read,
    };

    // Map path to resource
    let resource = if path.starts_with("/api/v1/deployments") {
        Resource::Deployment
    } else if path.starts_with("/api/v1/containers") {
        Resource::Container
    } else if path.starts_with("/api/v1/logs") {
        Resource::Logs
    } else if path.starts_with("/api/v1/events") {
        Resource::Events
    } else if path.starts_with("/api/v1/nodes") {
        Resource::Node
    } else if path.starts_with("/api/v1/keys") || path.starts_with("/api/v1/apikeys") {
        Resource::ApiKey
    } else if path.starts_with("/api/v1/users") || path.starts_with("/api/v1/roles") {
        Resource::User
    } else if path.starts_with("/admin") {
        Resource::Admin
    } else {
        Resource::All
    };

    (resource, action)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_grpc_method_mapping() {
        let (resource, action) = grpc_method_to_permission("/hanzo.node.v1.NodeService/GetHealth");
        assert_eq!(resource, Resource::Health);
        assert_eq!(action, Action::Read);

        let (resource, action) = grpc_method_to_permission("/hanzo.node.v1.NodeService/Deploy");
        assert_eq!(resource, Resource::Deployment);
        assert_eq!(action, Action::Create);

        let (resource, action) =
            grpc_method_to_permission("/hanzo.node.v1.NodeService/ExecInContainer");
        assert_eq!(resource, Resource::Container);
        assert_eq!(action, Action::Execute);
    }

    #[test]
    fn test_http_method_mapping() {
        let (resource, action) = http_method_to_permission(&http::Method::GET, "/health");
        assert_eq!(resource, Resource::Health);
        assert_eq!(action, Action::Read);

        let (resource, action) =
            http_method_to_permission(&http::Method::POST, "/api/v1/deployments");
        assert_eq!(resource, Resource::Deployment);
        assert_eq!(action, Action::Create);

        let (resource, action) =
            http_method_to_permission(&http::Method::DELETE, "/api/v1/containers/abc");
        assert_eq!(resource, Resource::Container);
        assert_eq!(action, Action::Delete);
    }
}
