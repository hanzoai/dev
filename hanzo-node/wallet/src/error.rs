//! Error types for the MPC wallet

use thiserror::Error;

/// Result type alias for wallet operations
pub type WalletResult<T> = Result<T, WalletError>;

/// Errors that can occur during wallet operations
#[derive(Error, Debug)]
pub enum WalletError {
    /// DKG protocol error
    #[error("DKG error: {0}")]
    Dkg(String),

    /// Signing protocol error
    #[error("Signing error: {0}")]
    Signing(String),

    /// Invalid threshold configuration
    #[error("Invalid threshold: {threshold} of {total} (threshold must be <= total and > 0)")]
    InvalidThreshold { threshold: u16, total: u16 },

    /// Not enough participants
    #[error("Not enough participants: have {have}, need {need}")]
    NotEnoughParticipants { have: usize, need: usize },

    /// Invalid participant identifier
    #[error("Invalid participant identifier: {0}")]
    InvalidParticipant(String),

    /// Key share not found
    #[error("Key share not found for key_id: {0}")]
    KeyShareNotFound(String),

    /// Invalid key share
    #[error("Invalid key share: {0}")]
    InvalidKeyShare(String),

    /// Signature verification failed
    #[error("Signature verification failed: {0}")]
    SignatureVerificationFailed(String),

    /// Policy violation
    #[error("Policy violation: {0}")]
    PolicyViolation(String),

    /// Insufficient approvals
    #[error("Insufficient approvals: have {have}, need {need}")]
    InsufficientApprovals { have: usize, need: usize },

    /// Rate limit exceeded
    #[error("Rate limit exceeded: {0} transactions per hour")]
    RateLimitExceeded(u32),

    /// Address not whitelisted
    #[error("Address not whitelisted: {0}")]
    AddressNotWhitelisted(String),

    /// Storage error
    #[error("Storage error: {0}")]
    Storage(String),

    /// Encryption error
    #[error("Encryption error: {0}")]
    Encryption(String),

    /// Decryption error
    #[error("Decryption error: {0}")]
    Decryption(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(String),

    /// gRPC error
    #[error("gRPC error: {0}")]
    Grpc(String),

    /// Timeout error
    #[error("Operation timed out: {0}")]
    Timeout(String),

    /// Internal error
    #[error("Internal error: {0}")]
    Internal(String),

    /// Curve mismatch error
    #[error("Curve mismatch: expected {expected}, got {got}")]
    CurveMismatch { expected: String, got: String },

    /// Invalid message to sign
    #[error("Invalid message: {0}")]
    InvalidMessage(String),

    /// Commitment verification failed
    #[error("Commitment verification failed for participant {participant}")]
    CommitmentVerificationFailed { participant: String },

    /// Share verification failed
    #[error("Share verification failed for participant {participant}: {reason}")]
    ShareVerificationFailed { participant: String, reason: String },
}

impl From<serde_json::Error> for WalletError {
    fn from(err: serde_json::Error) -> Self {
        WalletError::Serialization(err.to_string())
    }
}

impl From<bincode::Error> for WalletError {
    fn from(err: bincode::Error) -> Self {
        WalletError::Serialization(err.to_string())
    }
}

impl From<sled::Error> for WalletError {
    fn from(err: sled::Error) -> Self {
        WalletError::Storage(err.to_string())
    }
}

impl From<tonic::Status> for WalletError {
    fn from(err: tonic::Status) -> Self {
        WalletError::Grpc(err.to_string())
    }
}

impl From<WalletError> for tonic::Status {
    fn from(err: WalletError) -> Self {
        match err {
            WalletError::KeyShareNotFound(_) => tonic::Status::not_found(err.to_string()),
            WalletError::PolicyViolation(_) => tonic::Status::permission_denied(err.to_string()),
            WalletError::InsufficientApprovals { .. } => {
                tonic::Status::failed_precondition(err.to_string())
            }
            WalletError::RateLimitExceeded(_) => {
                tonic::Status::resource_exhausted(err.to_string())
            }
            WalletError::InvalidThreshold { .. }
            | WalletError::InvalidParticipant(_)
            | WalletError::InvalidKeyShare(_)
            | WalletError::InvalidMessage(_)
            | WalletError::CurveMismatch { .. } => {
                tonic::Status::invalid_argument(err.to_string())
            }
            WalletError::Timeout(_) => tonic::Status::deadline_exceeded(err.to_string()),
            _ => tonic::Status::internal(err.to_string()),
        }
    }
}
