//! Error types for device onboarding

use thiserror::Error;

/// Errors that can occur during QR code operations
#[derive(Error, Debug)]
pub enum QRError {
    #[error("Failed to generate QR code: {0}")]
    GenerationFailed(String),

    #[error("Failed to parse QR payload: {0}")]
    ParseFailed(String),

    #[error("QR payload expired")]
    Expired,

    #[error("Invalid signature")]
    InvalidSignature,

    #[error("Unsupported protocol version: {0}")]
    UnsupportedVersion(u8),

    #[error("Serialization error: {0}")]
    SerializationError(String),

    #[error("Image rendering error: {0}")]
    RenderError(String),
}

/// Errors that can occur during pairing
#[derive(Error, Debug)]
pub enum PairingError {
    #[error("Pairing failed: {0}")]
    Failed(String),

    #[error("Pairing timeout")]
    Timeout,

    #[error("Invalid pairing request")]
    InvalidRequest,

    #[error("Challenge verification failed")]
    ChallengeVerificationFailed,

    #[error("Key exchange failed: {0}")]
    KeyExchangeFailed(String),

    #[error("Encryption error: {0}")]
    EncryptionError(String),

    #[error("Decryption error: {0}")]
    DecryptionError(String),

    #[error("QR error: {0}")]
    QRError(#[from] QRError),
}

/// Errors that can occur during key operations
#[derive(Error, Debug)]
pub enum CryptoError {
    #[error("Key generation failed: {0}")]
    KeyGenerationFailed(String),

    #[error("Key derivation failed: {0}")]
    KeyDerivationFailed(String),

    #[error("Signature failed: {0}")]
    SignatureFailed(String),

    #[error("Signature verification failed")]
    VerificationFailed,

    #[error("Invalid key length")]
    InvalidKeyLength,
}

impl From<serde_json::Error> for QRError {
    fn from(err: serde_json::Error) -> Self {
        QRError::SerializationError(err.to_string())
    }
}

impl From<base64::DecodeError> for QRError {
    fn from(err: base64::DecodeError) -> Self {
        QRError::ParseFailed(err.to_string())
    }
}
