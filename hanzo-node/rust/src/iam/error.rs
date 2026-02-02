//! IAM error types

use thiserror::Error;

/// IAM error type
#[derive(Debug, Error)]
pub enum IamError {
    /// Token validation failed
    #[error("Invalid token: {0}")]
    InvalidToken(String),

    /// Token has expired
    #[error("Token has expired")]
    TokenExpired,

    /// Token signature invalid
    #[error("Invalid token signature")]
    InvalidSignature,

    /// Missing required claim
    #[error("Missing required claim: {0}")]
    MissingClaim(String),

    /// Invalid API key
    #[error("Invalid API key")]
    InvalidApiKey,

    /// API key has expired
    #[error("API key has expired")]
    ApiKeyExpired,

    /// Missing authentication
    #[error("Authentication required")]
    Unauthenticated,

    /// Permission denied
    #[error("Permission denied: {0}")]
    PermissionDenied(String),

    /// Role not found
    #[error("Role not found: {0}")]
    RoleNotFound(String),

    /// Configuration error
    #[error("IAM configuration error: {0}")]
    Config(String),

    /// JWKS fetch error
    #[error("Failed to fetch JWKS: {0}")]
    JwksFetch(String),

    /// Key not found in JWKS
    #[error("Key not found in JWKS: {0}")]
    KeyNotFound(String),

    /// Internal error
    #[error("Internal IAM error: {0}")]
    Internal(String),
}

impl IamError {
    /// Convert to gRPC status code
    pub fn to_grpc_status(&self) -> tonic::Status {
        match self {
            IamError::Unauthenticated => {
                tonic::Status::unauthenticated(self.to_string())
            }
            IamError::InvalidToken(_)
            | IamError::TokenExpired
            | IamError::InvalidSignature
            | IamError::InvalidApiKey
            | IamError::ApiKeyExpired => {
                tonic::Status::unauthenticated(self.to_string())
            }
            IamError::PermissionDenied(_) => {
                tonic::Status::permission_denied(self.to_string())
            }
            IamError::MissingClaim(_) | IamError::RoleNotFound(_) => {
                tonic::Status::invalid_argument(self.to_string())
            }
            IamError::Config(_) | IamError::JwksFetch(_) | IamError::KeyNotFound(_) => {
                tonic::Status::unavailable(self.to_string())
            }
            IamError::Internal(_) => {
                tonic::Status::internal(self.to_string())
            }
        }
    }

    /// Convert to HTTP status code
    pub fn to_http_status(&self) -> axum::http::StatusCode {
        match self {
            IamError::Unauthenticated
            | IamError::InvalidToken(_)
            | IamError::TokenExpired
            | IamError::InvalidSignature
            | IamError::InvalidApiKey
            | IamError::ApiKeyExpired => axum::http::StatusCode::UNAUTHORIZED,
            IamError::PermissionDenied(_) => axum::http::StatusCode::FORBIDDEN,
            IamError::MissingClaim(_) | IamError::RoleNotFound(_) => {
                axum::http::StatusCode::BAD_REQUEST
            }
            IamError::Config(_) | IamError::JwksFetch(_) | IamError::KeyNotFound(_) => {
                axum::http::StatusCode::SERVICE_UNAVAILABLE
            }
            IamError::Internal(_) => axum::http::StatusCode::INTERNAL_SERVER_ERROR,
        }
    }
}

/// Result type for IAM operations
pub type IamResult<T> = std::result::Result<T, IamError>;
