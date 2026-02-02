//! KMS shared types

use serde::{Deserialize, Serialize};
use std::time::SystemTime;

/// Key type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum KeyType {
    /// AES-256-GCM symmetric key
    Aes256Gcm,
    /// RSA 2048-bit key pair
    Rsa2048,
    /// RSA 4096-bit key pair
    Rsa4096,
    /// ECDSA P-256 key pair
    EcdsaP256,
    /// ECDSA secp256k1 key pair (Ethereum/Bitcoin)
    EcdsaSecp256k1,
    /// Ed25519 key pair
    Ed25519,
}

impl KeyType {
    /// Check if this key type supports symmetric encryption
    pub fn is_symmetric(&self) -> bool {
        matches!(self, Self::Aes256Gcm)
    }

    /// Check if this key type supports signing
    pub fn supports_signing(&self) -> bool {
        !self.is_symmetric()
    }
}

/// Key usage flags
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum KeyUsage {
    /// Key can be used for encryption/decryption
    EncryptDecrypt,
    /// Key can be used for signing/verification
    SignVerify,
    /// Key can be used for both
    All,
}

/// Key metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeyMetadata {
    /// Unique key identifier
    pub key_id: String,
    /// Key type
    pub key_type: KeyType,
    /// Key usage
    pub usage: KeyUsage,
    /// Creation timestamp
    pub created_at: SystemTime,
    /// Last rotation timestamp
    pub rotated_at: Option<SystemTime>,
    /// Key version (incremented on rotation)
    pub version: u32,
    /// Whether the key is enabled
    pub enabled: bool,
    /// Optional description
    pub description: Option<String>,
    /// Custom tags
    #[serde(default)]
    pub tags: std::collections::HashMap<String, String>,
}

impl KeyMetadata {
    /// Create new key metadata
    pub fn new(key_id: impl Into<String>, key_type: KeyType, usage: KeyUsage) -> Self {
        Self {
            key_id: key_id.into(),
            key_type,
            usage,
            created_at: SystemTime::now(),
            rotated_at: None,
            version: 1,
            enabled: true,
            description: None,
            tags: std::collections::HashMap::new(),
        }
    }
}

/// Encrypted data with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EncryptedData {
    /// Key ID used for encryption
    pub key_id: String,
    /// Key version used
    pub key_version: u32,
    /// Initialization vector / nonce
    pub iv: Vec<u8>,
    /// Ciphertext
    pub ciphertext: Vec<u8>,
    /// Authentication tag (for AEAD)
    pub tag: Option<Vec<u8>>,
}
