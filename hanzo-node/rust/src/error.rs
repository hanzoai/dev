//! Error types for hanzo-node

use thiserror::Error;

/// Result type alias for hanzo-node
pub type Result<T> = std::result::Result<T, Error>;

/// Error type for hanzo-node
#[derive(Error, Debug)]
pub enum Error {
    /// I/O error
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// Storage error
    #[error("Storage error: {0}")]
    Storage(#[from] sled::Error),

    /// P2P network error
    #[error("P2P network error: {0}")]
    P2P(String),

    /// RPC error
    #[error("RPC error: {0}")]
    Rpc(#[from] tonic::transport::Error),

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    /// Configuration error
    #[error("Configuration error: {0}")]
    Config(String),

    /// Deployment error
    #[error("Deployment error: {0}")]
    Deployment(String),

    /// Container runtime error
    #[error("Container runtime error: {0}")]
    ContainerRuntime(String),

    /// Not found error
    #[error("{0} not found: {1}")]
    NotFound(String, String),

    /// Already exists error
    #[error("{0} already exists: {1}")]
    AlreadyExists(String, String),

    /// Invalid state error
    #[error("Invalid state: {0}")]
    InvalidState(String),

    /// Timeout error
    #[error("Timeout: {0}")]
    Timeout(String),

    /// Internal error
    #[error("Internal error: {0}")]
    Internal(String),

    /// Consensus/replication error
    #[error("Consensus error: {0}")]
    Consensus(String),
}

impl From<anyhow::Error> for Error {
    fn from(err: anyhow::Error) -> Self {
        Error::Internal(err.to_string())
    }
}

impl From<Error> for tonic::Status {
    fn from(err: Error) -> Self {
        match err {
            Error::NotFound(resource, id) => {
                tonic::Status::not_found(format!("{resource} not found: {id}"))
            }
            Error::AlreadyExists(resource, id) => {
                tonic::Status::already_exists(format!("{resource} already exists: {id}"))
            }
            Error::InvalidState(msg) => tonic::Status::failed_precondition(msg),
            Error::Config(msg) => tonic::Status::invalid_argument(msg),
            Error::Timeout(msg) => tonic::Status::deadline_exceeded(msg),
            _ => tonic::Status::internal(err.to_string()),
        }
    }
}
