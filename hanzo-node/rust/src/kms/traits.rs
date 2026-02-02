//! KMS core traits

use async_trait::async_trait;

use crate::kms::error::Result;
use crate::kms::types::{EncryptedData, KeyMetadata, KeyType, KeyUsage};

/// Core KMS trait - all backends must implement this
#[async_trait]
pub trait KeyManagementService: Send + Sync {
    /// Create a new key
    async fn create_key(
        &self,
        key_id: &str,
        key_type: KeyType,
        usage: KeyUsage,
    ) -> Result<KeyMetadata>;

    /// Get key metadata
    async fn get_key(&self, key_id: &str) -> Result<KeyMetadata>;

    /// List all keys
    async fn list_keys(&self) -> Result<Vec<KeyMetadata>>;

    /// Delete a key
    async fn delete_key(&self, key_id: &str) -> Result<()>;

    /// Enable a key
    async fn enable_key(&self, key_id: &str) -> Result<()>;

    /// Disable a key
    async fn disable_key(&self, key_id: &str) -> Result<()>;

    /// Encrypt data using the specified key
    async fn encrypt(&self, key_id: &str, plaintext: &[u8]) -> Result<EncryptedData>;

    /// Decrypt data
    async fn decrypt(&self, encrypted: &EncryptedData) -> Result<Vec<u8>>;

    /// Sign data using the specified key
    async fn sign(&self, key_id: &str, message: &[u8]) -> Result<Vec<u8>>;

    /// Verify a signature
    async fn verify(&self, key_id: &str, message: &[u8], signature: &[u8]) -> Result<bool>;

    /// Rotate a key (create new version, keep old for decryption)
    async fn rotate_key(&self, key_id: &str) -> Result<KeyMetadata>;

    /// Get the provider name
    fn provider_name(&self) -> &'static str;
}

/// Extension trait for data encryption key (DEK) operations
#[async_trait]
pub trait DataKeyService: KeyManagementService {
    /// Generate a data encryption key, encrypted with the master key
    async fn generate_data_key(
        &self,
        master_key_id: &str,
    ) -> Result<(Vec<u8>, EncryptedData)>;

    /// Decrypt a data encryption key
    async fn decrypt_data_key(&self, encrypted_key: &EncryptedData) -> Result<Vec<u8>>;
}
