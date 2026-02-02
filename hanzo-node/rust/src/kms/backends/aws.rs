//! AWS KMS backend

use async_trait::async_trait;
use std::collections::HashMap;
use std::sync::RwLock;

use crate::kms::config::KmsConfig;
use crate::kms::error::{KmsError, Result};
use crate::kms::traits::KeyManagementService;
use crate::kms::types::{EncryptedData, KeyMetadata, KeyType, KeyUsage};

/// AWS KMS client
pub struct AwsKms {
    region: String,
    // In production, use aws-sdk-kms
    // For now, store metadata locally and simulate API calls
    keys: RwLock<HashMap<String, KeyMetadata>>,
}

impl AwsKms {
    /// Create a new AWS KMS client
    pub async fn new(config: &KmsConfig) -> Result<Self> {
        let region = config
            .aws_region
            .as_ref()
            .ok_or_else(|| KmsError::ConfigError("AWS_REGION required".to_string()))?
            .clone();

        Ok(Self {
            region,
            keys: RwLock::new(HashMap::new()),
        })
    }

    /// Format AWS KMS key ARN
    fn key_arn(&self, key_id: &str) -> String {
        format!("arn:aws:kms:{}:*:key/{}", self.region, key_id)
    }
}

#[async_trait]
impl KeyManagementService for AwsKms {
    async fn create_key(
        &self,
        key_id: &str,
        key_type: KeyType,
        usage: KeyUsage,
    ) -> Result<KeyMetadata> {
        // In production: aws_sdk_kms::Client::create_key()
        // For now, simulate the response

        let mut keys = self.keys.write().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        if keys.contains_key(key_id) {
            return Err(KmsError::KeyAlreadyExists(key_id.to_string()));
        }

        let metadata = KeyMetadata::new(key_id, key_type, usage);
        keys.insert(key_id.to_string(), metadata.clone());

        Ok(metadata)
    }

    async fn get_key(&self, key_id: &str) -> Result<KeyMetadata> {
        // In production: aws_sdk_kms::Client::describe_key()
        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        keys.get(key_id)
            .cloned()
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))
    }

    async fn list_keys(&self) -> Result<Vec<KeyMetadata>> {
        // In production: aws_sdk_kms::Client::list_keys()
        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        Ok(keys.values().cloned().collect())
    }

    async fn delete_key(&self, key_id: &str) -> Result<()> {
        // In production: aws_sdk_kms::Client::schedule_key_deletion()
        let mut keys = self.keys.write().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        if keys.remove(key_id).is_none() {
            return Err(KmsError::KeyNotFound(key_id.to_string()));
        }

        Ok(())
    }

    async fn enable_key(&self, key_id: &str) -> Result<()> {
        // In production: aws_sdk_kms::Client::enable_key()
        let mut keys = self.keys.write().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let metadata = keys
            .get_mut(key_id)
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;
        metadata.enabled = true;
        Ok(())
    }

    async fn disable_key(&self, key_id: &str) -> Result<()> {
        // In production: aws_sdk_kms::Client::disable_key()
        let mut keys = self.keys.write().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let metadata = keys
            .get_mut(key_id)
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;
        metadata.enabled = false;
        Ok(())
    }

    async fn encrypt(&self, key_id: &str, plaintext: &[u8]) -> Result<EncryptedData> {
        // In production: aws_sdk_kms::Client::encrypt()
        // AWS KMS returns ciphertext blob that includes key ID and context

        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let metadata = keys
            .get(key_id)
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        if !metadata.enabled {
            return Err(KmsError::PermissionDenied("Key is disabled".to_string()));
        }

        // Simulate AWS KMS encryption (in production, this calls AWS API)
        // AWS returns a ciphertext blob that's self-describing
        let mut ciphertext = Vec::new();
        ciphertext.extend_from_slice(b"AWSKMS:"); // Marker
        ciphertext.extend_from_slice(key_id.as_bytes());
        ciphertext.push(0); // Null separator
        ciphertext.extend_from_slice(plaintext); // Simulated encryption

        Ok(EncryptedData {
            key_id: key_id.to_string(),
            key_version: metadata.version,
            iv: vec![], // AWS KMS manages IVs internally
            ciphertext,
            tag: None,
        })
    }

    async fn decrypt(&self, encrypted: &EncryptedData) -> Result<Vec<u8>> {
        // In production: aws_sdk_kms::Client::decrypt()

        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let metadata = keys
            .get(&encrypted.key_id)
            .ok_or_else(|| KmsError::KeyNotFound(encrypted.key_id.clone()))?;

        if !metadata.enabled {
            return Err(KmsError::PermissionDenied("Key is disabled".to_string()));
        }

        // Simulate AWS KMS decryption
        let marker = b"AWSKMS:";
        if !encrypted.ciphertext.starts_with(marker) {
            return Err(KmsError::InvalidCiphertext);
        }

        // Find null separator
        let key_start = marker.len();
        let null_pos = encrypted.ciphertext[key_start..]
            .iter()
            .position(|&b| b == 0)
            .ok_or(KmsError::InvalidCiphertext)?;

        let plaintext_start = key_start + null_pos + 1;
        Ok(encrypted.ciphertext[plaintext_start..].to_vec())
    }

    async fn sign(&self, key_id: &str, message: &[u8]) -> Result<Vec<u8>> {
        // In production: aws_sdk_kms::Client::sign()
        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let metadata = keys
            .get(key_id)
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        if !metadata.enabled {
            return Err(KmsError::PermissionDenied("Key is disabled".to_string()));
        }

        if !metadata.key_type.supports_signing() {
            return Err(KmsError::InvalidKeyType(
                "Key does not support signing".to_string(),
            ));
        }

        // Simulate signature (in production, AWS KMS performs the signing)
        let mut signature = Vec::new();
        signature.extend_from_slice(b"AWSSIG:");
        signature.extend_from_slice(key_id.as_bytes());
        signature.push(0);
        signature.extend_from_slice(message);

        Ok(signature)
    }

    async fn verify(&self, key_id: &str, message: &[u8], signature: &[u8]) -> Result<bool> {
        // In production: aws_sdk_kms::Client::verify()
        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let _metadata = keys
            .get(key_id)
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        // Simulate verification
        let marker = b"AWSSIG:";
        if !signature.starts_with(marker) {
            return Ok(false);
        }

        let key_start = marker.len();
        let null_pos = signature[key_start..]
            .iter()
            .position(|&b| b == 0)
            .ok_or(KmsError::BackendError("Invalid signature format".to_string()))?;

        let msg_start = key_start + null_pos + 1;
        Ok(&signature[msg_start..] == message)
    }

    async fn rotate_key(&self, key_id: &str) -> Result<KeyMetadata> {
        // In production: AWS KMS automatic key rotation or create new key version
        let mut keys = self.keys.write().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let metadata = keys
            .get_mut(key_id)
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        metadata.version += 1;
        metadata.rotated_at = Some(std::time::SystemTime::now());

        Ok(metadata.clone())
    }

    fn provider_name(&self) -> &'static str {
        "aws-kms"
    }
}
