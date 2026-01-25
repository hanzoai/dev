//! Error types for the QoS Challenge System.

use thiserror::Error;

use crate::types::ChallengeId;

/// Result type for QoS operations.
pub type Result<T> = std::result::Result<T, QoSError>;

/// Errors that can occur in the QoS Challenge System.
#[derive(Debug, Error)]
pub enum QoSError {
    /// Challenge has expired.
    #[error("challenge {0} has expired")]
    ChallengeExpired(ChallengeId),

    /// Challenge not found.
    #[error("challenge {0} not found")]
    ChallengeNotFound(ChallengeId),

    /// Invalid proof submitted.
    #[error("invalid proof: {0}")]
    InvalidProof(String),

    /// Proof type mismatch with challenge type.
    #[error("proof type mismatch: expected {expected}, got {actual}")]
    ProofTypeMismatch { expected: String, actual: String },

    /// Hash verification failed.
    #[error("hash verification failed: {0}")]
    HashMismatch(String),

    /// Intermediate hash verification failed.
    #[error("intermediate hash mismatch at index {index}")]
    IntermediateHashMismatch { index: usize },

    /// Signature verification failed.
    #[error("signature verification failed: {0}")]
    SignatureInvalid(String),

    /// TEE attestation verification failed.
    #[error("TEE attestation verification failed: {0}")]
    TeeAttestationFailed(String),

    /// Provider not found.
    #[error("provider {0} not found")]
    ProviderNotFound(String),

    /// Provider is banned.
    #[error("provider {provider} is banned: {reason}")]
    ProviderBanned { provider: String, reason: String },

    /// Provider is not active.
    #[error("provider {0} is not active")]
    ProviderNotActive(String),

    /// Insufficient stake.
    #[error("insufficient stake: required {required}, available {available}")]
    InsufficientStake { required: u64, available: u64 },

    /// Performance below threshold.
    #[error("performance below threshold: {score} < {threshold}")]
    PerformanceBelowThreshold { score: f64, threshold: f64 },

    /// Deadline exceeded.
    #[error("deadline exceeded: took {actual_ms}ms, allowed {deadline_ms}ms")]
    DeadlineExceeded { actual_ms: u64, deadline_ms: u64 },

    /// Cryptographic operation failed.
    #[error("cryptographic error: {0}")]
    CryptoError(String),

    /// Serialization/deserialization error.
    #[error("serialization error: {0}")]
    SerializationError(String),

    /// Internal error.
    #[error("internal error: {0}")]
    Internal(String),
}

impl From<serde_json::Error> for QoSError {
    fn from(err: serde_json::Error) -> Self {
        Self::SerializationError(err.to_string())
    }
}

/// Reason for verification failure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FailureReason {
    /// Hash of result does not match expected.
    HashMismatch,
    /// Intermediate hash check failed.
    IntermediateHashMismatch { index: usize },
    /// Proof type does not match challenge type.
    WrongProofType,
    /// Signature is invalid.
    InvalidSignature,
    /// TEE attestation is invalid.
    InvalidTeeAttestation,
    /// Challenge deadline was exceeded.
    DeadlineExceeded,
    /// Performance was below minimum threshold.
    PerformanceTooLow,
    /// Latency exceeded maximum allowed.
    LatencyExceeded,
    /// Bandwidth below minimum threshold.
    BandwidthTooLow,
    /// Nonce response was invalid.
    InvalidNonceResponse,
    /// Model output hash mismatch.
    ModelOutputMismatch,
}

impl std::fmt::Display for FailureReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HashMismatch => write!(f, "result hash mismatch"),
            Self::IntermediateHashMismatch { index } => {
                write!(f, "intermediate hash mismatch at index {index}")
            }
            Self::WrongProofType => write!(f, "wrong proof type for challenge"),
            Self::InvalidSignature => write!(f, "invalid signature"),
            Self::InvalidTeeAttestation => write!(f, "invalid TEE attestation"),
            Self::DeadlineExceeded => write!(f, "deadline exceeded"),
            Self::PerformanceTooLow => write!(f, "performance below threshold"),
            Self::LatencyExceeded => write!(f, "latency exceeded"),
            Self::BandwidthTooLow => write!(f, "bandwidth below threshold"),
            Self::InvalidNonceResponse => write!(f, "invalid nonce response"),
            Self::ModelOutputMismatch => write!(f, "model output hash mismatch"),
        }
    }
}
