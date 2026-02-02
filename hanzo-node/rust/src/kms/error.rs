//! KMS error types

use thiserror::Error;

/// KMS-specific errors
#[derive(Error, Debug)]
pub enum KmsError {
    #[error("Invalid KMS provider: {0}")]
    InvalidProvider(String),

    #[error("Key not found: {0}")]
    KeyNotFound(String),

    #[error("Key already exists: {0}")]
    KeyAlreadyExists(String),

    #[error("Invalid key type: {0}")]
    InvalidKeyType(String),

    #[error("Encryption failed: {0}")]
    EncryptionFailed(String),

    #[error("Decryption failed: {0}")]
    DecryptionFailed(String),

    #[error("Signing failed: {0}")]
    SigningFailed(String),

    #[error("Key rotation failed: {0}")]
    RotationFailed(String),

    #[error("Configuration error: {0}")]
    ConfigError(String),

    #[error("Backend error: {0}")]
    BackendError(String),

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Authentication failed: {0}")]
    AuthenticationFailed(String),

    #[error("Permission denied: {0}")]
    PermissionDenied(String),

    #[error("Invalid ciphertext")]
    InvalidCiphertext,

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(String),
}

pub type Result<T> = std::result::Result<T, KmsError>;
