use thiserror::Error;

/// Errors from the ZT SDK
#[derive(Debug, Error)]
pub enum ZtError {
    #[error("authentication failed: {0}")]
    AuthFailed(String),

    #[error("not authenticated — call authenticate() first")]
    NotAuthenticated,

    #[error("controller request failed: {0}")]
    Controller(String),

    #[error("service not found: {0}")]
    ServiceNotFound(String),

    #[error("no edge routers available for service: {0}")]
    NoEdgeRouters(String),

    #[error("connection failed: {0}")]
    ConnectionFailed(String),

    #[error("connection closed")]
    ConnectionClosed,

    #[error("insufficient balance for service: {0}")]
    InsufficientBalance(String),

    #[error("billing error: {0}")]
    BillingError(String),

    #[error("invalid configuration: {0}")]
    InvalidConfig(String),

    #[error("identity error: {0}")]
    Identity(String),

    #[error("timeout: {0}")]
    Timeout(String),

    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    #[error("http error: {0}")]
    Http(#[from] reqwest::Error),

    #[error("json error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("url parse error: {0}")]
    UrlParse(#[from] url::ParseError),
}

pub type Result<T> = std::result::Result<T, ZtError>;
