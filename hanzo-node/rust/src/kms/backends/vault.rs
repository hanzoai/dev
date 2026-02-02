//! HashiCorp Vault KMS backend (Transit secrets engine)

use async_trait::async_trait;
use std::collections::HashMap;
use std::sync::RwLock;

use crate::kms::config::KmsConfig;
use crate::kms::error::{KmsError, Result};
use crate::kms::traits::KeyManagementService;
use crate::kms::types::{EncryptedData, KeyMetadata, KeyType, KeyUsage};

/// HashiCorp Vault Transit engine client
pub struct VaultKms {
    addr: String,
    token: String,
    mount_path: String,
    // In production, use reqwest to call Vault API
    // For now, store metadata locally and simulate API calls
    keys: RwLock<HashMap<String, KeyMetadata>>,
}

impl VaultKms {
    /// Create a new Vault KMS client
    pub async fn new(config: &KmsConfig) -> Result<Self> {
        let addr = config
            .vault_addr
            .as_ref()
            .ok_or_else(|| KmsError::ConfigError("VAULT_ADDR required".to_string()))?
            .clone();

        let token = config
            .vault_token
            .as_ref()
            .ok_or_else(|| KmsError::ConfigError("VAULT_TOKEN required".to_string()))?
            .clone();

        let mount_path = config
            .vault_mount_path
            .clone()
            .unwrap_or_else(|| "transit".to_string());

        Ok(Self {
            addr,
            token,
            mount_path,
            keys: RwLock::new(HashMap::new()),
        })
    }

    /// Build Vault API URL
    fn api_url(&self, path: &str) -> String {
        format!("{}/v1/{}/{}", self.addr, self.mount_path, path)
    }

    /// Map KeyType to Vault key type
    fn vault_key_type(key_type: KeyType) -> &'static str {
        match key_type {
            KeyType::Aes256Gcm => "aes256-gcm96",
            KeyType::Ed25519 => "ed25519",
            KeyType::EcdsaP256 => "ecdsa-p256",
            KeyType::EcdsaSecp256k1 => "ecdsa-p256", // Vault doesn't have secp256k1 native
            KeyType::Rsa2048 => "rsa-2048",
            KeyType::Rsa4096 => "rsa-4096",
        }
    }
}

#[async_trait]
impl KeyManagementService for VaultKms {
    async fn create_key(
        &self,
        key_id: &str,
        key_type: KeyType,
        usage: KeyUsage,
    ) -> Result<KeyMetadata> {
        // In production: POST to /v1/transit/keys/:name
        // Request body: { "type": "aes256-gcm96", "exportable": false }

        let mut keys = self.keys.write().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        if keys.contains_key(key_id) {
            return Err(KmsError::KeyAlreadyExists(key_id.to_string()));
        }

        let metadata = KeyMetadata::new(key_id, key_type, usage);
        keys.insert(key_id.to_string(), metadata.clone());

        // Log the simulated API call
        tracing::debug!(
            url = %self.api_url(&format!("keys/{}", key_id)),
            vault_type = %Self::vault_key_type(key_type),
            "Creating Vault transit key"
        );

        Ok(metadata)
    }

    async fn get_key(&self, key_id: &str) -> Result<KeyMetadata> {
        // In production: GET /v1/transit/keys/:name
        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        keys.get(key_id)
            .cloned()
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))
    }

    async fn list_keys(&self) -> Result<Vec<KeyMetadata>> {
        // In production: LIST /v1/transit/keys
        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        Ok(keys.values().cloned().collect())
    }

    async fn delete_key(&self, key_id: &str) -> Result<()> {
        // In production:
        // 1. POST /v1/transit/keys/:name/config with deletion_allowed=true
        // 2. DELETE /v1/transit/keys/:name
        let mut keys = self.keys.write().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        if keys.remove(key_id).is_none() {
            return Err(KmsError::KeyNotFound(key_id.to_string()));
        }

        Ok(())
    }

    async fn enable_key(&self, key_id: &str) -> Result<()> {
        // In production: POST /v1/transit/keys/:name/config with min_decryption_version
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
        // In production: POST /v1/transit/encrypt/:name
        // Request: { "plaintext": "<base64>" }
        // Response: { "data": { "ciphertext": "vault:v1:..." } }

        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let metadata = keys
            .get(key_id)
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        if !metadata.enabled {
            return Err(KmsError::PermissionDenied("Key is disabled".to_string()));
        }

        // Simulate Vault ciphertext format: vault:v{version}:{base64_ciphertext}
        let ciphertext_header = format!("vault:v{}:", metadata.version);
        let mut ciphertext = ciphertext_header.into_bytes();
        ciphertext.extend(base64::Engine::encode(
            &base64::engine::general_purpose::STANDARD,
            plaintext,
        ).into_bytes());

        Ok(EncryptedData {
            key_id: key_id.to_string(),
            key_version: metadata.version,
            iv: vec![],
            ciphertext,
            tag: None,
        })
    }

    async fn decrypt(&self, encrypted: &EncryptedData) -> Result<Vec<u8>> {
        // In production: POST /v1/transit/decrypt/:name
        // Request: { "ciphertext": "vault:v1:..." }
        // Response: { "data": { "plaintext": "<base64>" } }

        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let metadata = keys
            .get(&encrypted.key_id)
            .ok_or_else(|| KmsError::KeyNotFound(encrypted.key_id.clone()))?;

        if !metadata.enabled {
            return Err(KmsError::PermissionDenied("Key is disabled".to_string()));
        }

        // Parse Vault ciphertext format
        let ciphertext_str = String::from_utf8(encrypted.ciphertext.clone())
            .map_err(|_| KmsError::InvalidCiphertext)?;

        let parts: Vec<&str> = ciphertext_str.splitn(3, ':').collect();
        if parts.len() != 3 || parts[0] != "vault" {
            return Err(KmsError::InvalidCiphertext);
        }

        let plaintext = base64::Engine::decode(
            &base64::engine::general_purpose::STANDARD,
            parts[2],
        )
        .map_err(|_| KmsError::InvalidCiphertext)?;

        Ok(plaintext)
    }

    async fn sign(&self, key_id: &str, message: &[u8]) -> Result<Vec<u8>> {
        // In production: POST /v1/transit/sign/:name
        // Request: { "input": "<base64>" }
        // Response: { "data": { "signature": "vault:v1:..." } }

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

        // Simulate Vault signature format
        let sig_header = format!("vault:v{}:", metadata.version);
        let mut signature = sig_header.into_bytes();
        signature.extend(base64::Engine::encode(
            &base64::engine::general_purpose::STANDARD,
            message,
        ).into_bytes());

        Ok(signature)
    }

    async fn verify(&self, key_id: &str, message: &[u8], signature: &[u8]) -> Result<bool> {
        // In production: POST /v1/transit/verify/:name
        // Request: { "input": "<base64>", "signature": "vault:v1:..." }
        // Response: { "data": { "valid": true } }

        let keys = self.keys.read().map_err(|_| {
            KmsError::BackendError("Lock poisoned".to_string())
        })?;

        let _metadata = keys
            .get(key_id)
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        // Parse signature
        let sig_str = String::from_utf8(signature.to_vec())
            .map_err(|_| KmsError::BackendError("Invalid signature".to_string()))?;

        let parts: Vec<&str> = sig_str.splitn(3, ':').collect();
        if parts.len() != 3 || parts[0] != "vault" {
            return Ok(false);
        }

        let signed_msg = base64::Engine::decode(
            &base64::engine::general_purpose::STANDARD,
            parts[2],
        )
        .map_err(|_| KmsError::BackendError("Invalid signature".to_string()))?;

        Ok(signed_msg == message)
    }

    async fn rotate_key(&self, key_id: &str) -> Result<KeyMetadata> {
        // In production: POST /v1/transit/keys/:name/rotate
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
        "hashicorp-vault"
    }
}
