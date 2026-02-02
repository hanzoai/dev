//! Local KMS backend with sled storage
//!
//! Stores key material in sled via the KeyValueStore trait,
//! enabling consensus replication for key state.

use async_trait::async_trait;
use ring::aead::{self, Aad, LessSafeKey, Nonce, UnboundKey, AES_256_GCM};
use ring::rand::{SecureRandom, SystemRandom};
use ring::signature::{self, Ed25519KeyPair, KeyPair};
use std::sync::Arc;

use crate::kms::error::{KmsError, Result};
use crate::kms::traits::KeyManagementService;
use crate::kms::types::{EncryptedData, KeyMetadata, KeyType, KeyUsage};
use crate::storage::KeyValueStore;

/// Storage tree for KMS keys
const KMS_KEYS_TREE: &str = "kms:keys";

/// Stored key material (serialized to storage)
#[derive(serde::Serialize, serde::Deserialize)]
struct StoredKey {
    metadata: KeyMetadata,
    material: Vec<u8>,
}

/// Local KMS implementation backed by sled storage
pub struct LocalKms {
    storage: Arc<dyn KeyValueStore>,
    rng: SystemRandom,
}

impl LocalKms {
    /// Create a new local KMS with sled storage backend
    pub fn new(storage: Arc<dyn KeyValueStore>) -> Result<Self> {
        Ok(Self {
            storage,
            rng: SystemRandom::new(),
        })
    }

    /// Generate key material for the specified key type
    fn generate_key_material(&self, key_type: KeyType) -> Result<Vec<u8>> {
        match key_type {
            KeyType::Aes256Gcm => {
                let mut key = vec![0u8; 32];
                self.rng
                    .fill(&mut key)
                    .map_err(|_| KmsError::BackendError("RNG failed".to_string()))?;
                Ok(key)
            }
            KeyType::Ed25519 => {
                let mut seed = vec![0u8; 32];
                self.rng
                    .fill(&mut seed)
                    .map_err(|_| KmsError::BackendError("RNG failed".to_string()))?;
                // Store seed, can regenerate keypair from it
                Ok(seed)
            }
            KeyType::EcdsaSecp256k1 | KeyType::EcdsaP256 => {
                let mut key = vec![0u8; 32];
                self.rng
                    .fill(&mut key)
                    .map_err(|_| KmsError::BackendError("RNG failed".to_string()))?;
                Ok(key)
            }
            _ => Err(KmsError::InvalidKeyType(format!("{:?}", key_type))),
        }
    }

    /// Persist key to storage
    fn persist_key(&self, key_id: &str, stored: &StoredKey) -> Result<()> {
        let data = serde_json::to_vec(stored)
            .map_err(|e| KmsError::SerializationError(e.to_string()))?;
        self.storage
            .kv_put(KMS_KEYS_TREE, key_id.as_bytes(), &data)
            .map_err(|e| KmsError::BackendError(e.to_string()))?;
        Ok(())
    }

    /// Load key from storage
    fn load_key(&self, key_id: &str) -> Result<Option<StoredKey>> {
        match self
            .storage
            .kv_get(KMS_KEYS_TREE, key_id.as_bytes())
            .map_err(|e| KmsError::BackendError(e.to_string()))?
        {
            Some(data) => {
                let stored: StoredKey = serde_json::from_slice(&data)
                    .map_err(|e| KmsError::SerializationError(e.to_string()))?;
                Ok(Some(stored))
            }
            None => Ok(None),
        }
    }

    /// Delete key from storage
    fn delete_key_from_storage(&self, key_id: &str) -> Result<bool> {
        self.storage
            .kv_delete(KMS_KEYS_TREE, key_id.as_bytes())
            .map_err(|e| KmsError::BackendError(e.to_string()))
    }
}

#[async_trait]
impl KeyManagementService for LocalKms {
    async fn create_key(
        &self,
        key_id: &str,
        key_type: KeyType,
        usage: KeyUsage,
    ) -> Result<KeyMetadata> {
        // Check if key already exists
        if self.load_key(key_id)?.is_some() {
            return Err(KmsError::KeyAlreadyExists(key_id.to_string()));
        }

        let material = self.generate_key_material(key_type)?;
        let metadata = KeyMetadata::new(key_id, key_type, usage);

        let stored = StoredKey {
            metadata: metadata.clone(),
            material,
        };

        // Persist to storage (fires change callback for consensus)
        self.persist_key(key_id, &stored)?;

        Ok(metadata)
    }

    async fn get_key(&self, key_id: &str) -> Result<KeyMetadata> {
        match self.load_key(key_id)? {
            Some(stored) => Ok(stored.metadata),
            None => Err(KmsError::KeyNotFound(key_id.to_string())),
        }
    }

    async fn list_keys(&self) -> Result<Vec<KeyMetadata>> {
        let items = self
            .storage
            .kv_list(KMS_KEYS_TREE)
            .map_err(|e| KmsError::BackendError(e.to_string()))?;

        let mut keys = Vec::with_capacity(items.len());
        for (_, value) in items {
            let stored: StoredKey = serde_json::from_slice(&value)
                .map_err(|e| KmsError::SerializationError(e.to_string()))?;
            keys.push(stored.metadata);
        }
        Ok(keys)
    }

    async fn delete_key(&self, key_id: &str) -> Result<()> {
        if !self.delete_key_from_storage(key_id)? {
            return Err(KmsError::KeyNotFound(key_id.to_string()));
        }
        Ok(())
    }

    async fn enable_key(&self, key_id: &str) -> Result<()> {
        let mut stored = self
            .load_key(key_id)?
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        stored.metadata.enabled = true;
        self.persist_key(key_id, &stored)?;
        Ok(())
    }

    async fn disable_key(&self, key_id: &str) -> Result<()> {
        let mut stored = self
            .load_key(key_id)?
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        stored.metadata.enabled = false;
        self.persist_key(key_id, &stored)?;
        Ok(())
    }

    async fn encrypt(&self, key_id: &str, plaintext: &[u8]) -> Result<EncryptedData> {
        let stored = self
            .load_key(key_id)?
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        if !stored.metadata.enabled {
            return Err(KmsError::PermissionDenied("Key is disabled".to_string()));
        }

        if !matches!(stored.metadata.key_type, KeyType::Aes256Gcm) {
            return Err(KmsError::InvalidKeyType(
                "Key does not support encryption".to_string(),
            ));
        }

        // Generate nonce
        let mut nonce_bytes = vec![0u8; 12];
        self.rng
            .fill(&mut nonce_bytes)
            .map_err(|_| KmsError::EncryptionFailed("RNG failed".to_string()))?;

        // Encrypt
        let unbound_key = UnboundKey::new(&AES_256_GCM, &stored.material)
            .map_err(|_| KmsError::EncryptionFailed("Invalid key".to_string()))?;
        let key = LessSafeKey::new(unbound_key);

        let nonce = Nonce::try_assume_unique_for_key(&nonce_bytes)
            .map_err(|_| KmsError::EncryptionFailed("Invalid nonce".to_string()))?;

        let mut in_out = plaintext.to_vec();
        key.seal_in_place_append_tag(nonce, Aad::empty(), &mut in_out)
            .map_err(|_| KmsError::EncryptionFailed("Encryption failed".to_string()))?;

        Ok(EncryptedData {
            key_id: key_id.to_string(),
            key_version: stored.metadata.version,
            iv: nonce_bytes,
            ciphertext: in_out,
            tag: None, // Tag is appended to ciphertext
        })
    }

    async fn decrypt(&self, encrypted: &EncryptedData) -> Result<Vec<u8>> {
        let stored = self
            .load_key(&encrypted.key_id)?
            .ok_or_else(|| KmsError::KeyNotFound(encrypted.key_id.clone()))?;

        if !stored.metadata.enabled {
            return Err(KmsError::PermissionDenied("Key is disabled".to_string()));
        }

        let unbound_key = UnboundKey::new(&AES_256_GCM, &stored.material)
            .map_err(|_| KmsError::DecryptionFailed("Invalid key".to_string()))?;
        let key = LessSafeKey::new(unbound_key);

        let nonce = Nonce::try_assume_unique_for_key(&encrypted.iv)
            .map_err(|_| KmsError::DecryptionFailed("Invalid nonce".to_string()))?;

        let mut in_out = encrypted.ciphertext.clone();
        let plaintext = key
            .open_in_place(nonce, Aad::empty(), &mut in_out)
            .map_err(|_| KmsError::DecryptionFailed("Decryption failed".to_string()))?;

        Ok(plaintext.to_vec())
    }

    async fn sign(&self, key_id: &str, message: &[u8]) -> Result<Vec<u8>> {
        let stored = self
            .load_key(key_id)?
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        if !stored.metadata.enabled {
            return Err(KmsError::PermissionDenied("Key is disabled".to_string()));
        }

        match stored.metadata.key_type {
            KeyType::Ed25519 => {
                let key_pair = Ed25519KeyPair::from_seed_unchecked(&stored.material)
                    .map_err(|_| KmsError::SigningFailed("Invalid key".to_string()))?;
                let sig = key_pair.sign(message);
                Ok(sig.as_ref().to_vec())
            }
            _ => Err(KmsError::InvalidKeyType(
                "Key type does not support signing".to_string(),
            )),
        }
    }

    async fn verify(&self, key_id: &str, message: &[u8], sig: &[u8]) -> Result<bool> {
        let stored = self
            .load_key(key_id)?
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        match stored.metadata.key_type {
            KeyType::Ed25519 => {
                let key_pair = Ed25519KeyPair::from_seed_unchecked(&stored.material)
                    .map_err(|_| KmsError::BackendError("Invalid key".to_string()))?;
                let public_key = key_pair.public_key();
                let peer_public_key =
                    signature::UnparsedPublicKey::new(&signature::ED25519, public_key.as_ref());
                Ok(peer_public_key.verify(message, sig).is_ok())
            }
            _ => Err(KmsError::InvalidKeyType(
                "Key type does not support verification".to_string(),
            )),
        }
    }

    async fn rotate_key(&self, key_id: &str) -> Result<KeyMetadata> {
        let mut stored = self
            .load_key(key_id)?
            .ok_or_else(|| KmsError::KeyNotFound(key_id.to_string()))?;

        // Generate new key material
        let new_material = self.generate_key_material(stored.metadata.key_type)?;
        stored.material = new_material;
        stored.metadata.version += 1;
        stored.metadata.rotated_at = Some(std::time::SystemTime::now());

        self.persist_key(key_id, &stored)?;

        Ok(stored.metadata)
    }

    fn provider_name(&self) -> &'static str {
        "local-sled"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::Storage;
    use tempfile::TempDir;

    fn create_test_kms() -> (TempDir, LocalKms) {
        let tmp = TempDir::new().unwrap();
        let storage = Arc::new(Storage::new(tmp.path()).unwrap());
        let kms = LocalKms::new(storage).unwrap();
        (tmp, kms)
    }

    #[tokio::test]
    async fn test_create_and_get_key() {
        let (_tmp, kms) = create_test_kms();

        let metadata = kms
            .create_key("test-key", KeyType::Aes256Gcm, KeyUsage::EncryptDecrypt)
            .await
            .unwrap();

        assert_eq!(metadata.key_id, "test-key");
        assert!(metadata.enabled);
        assert_eq!(metadata.version, 1);

        let fetched = kms.get_key("test-key").await.unwrap();
        assert_eq!(fetched.key_id, "test-key");
    }

    #[tokio::test]
    async fn test_encrypt_decrypt() {
        let (_tmp, kms) = create_test_kms();

        kms.create_key("enc-key", KeyType::Aes256Gcm, KeyUsage::EncryptDecrypt)
            .await
            .unwrap();

        let plaintext = b"hello world";
        let encrypted = kms.encrypt("enc-key", plaintext).await.unwrap();
        let decrypted = kms.decrypt(&encrypted).await.unwrap();

        assert_eq!(decrypted, plaintext);
    }

    #[tokio::test]
    async fn test_sign_verify() {
        let (_tmp, kms) = create_test_kms();

        kms.create_key("sign-key", KeyType::Ed25519, KeyUsage::SignVerify)
            .await
            .unwrap();

        let message = b"test message";
        let signature = kms.sign("sign-key", message).await.unwrap();
        let valid = kms.verify("sign-key", message, &signature).await.unwrap();

        assert!(valid);

        // Invalid signature should fail
        let invalid = kms.verify("sign-key", b"wrong message", &signature).await.unwrap();
        assert!(!invalid);
    }

    #[tokio::test]
    async fn test_key_rotation() {
        let (_tmp, kms) = create_test_kms();

        kms.create_key("rotate-key", KeyType::Aes256Gcm, KeyUsage::EncryptDecrypt)
            .await
            .unwrap();

        let rotated = kms.rotate_key("rotate-key").await.unwrap();
        assert_eq!(rotated.version, 2);
        assert!(rotated.rotated_at.is_some());
    }

    #[tokio::test]
    async fn test_list_keys() {
        let (_tmp, kms) = create_test_kms();

        kms.create_key("key1", KeyType::Aes256Gcm, KeyUsage::EncryptDecrypt)
            .await
            .unwrap();
        kms.create_key("key2", KeyType::Ed25519, KeyUsage::SignVerify)
            .await
            .unwrap();

        let keys = kms.list_keys().await.unwrap();
        assert_eq!(keys.len(), 2);
    }

    #[tokio::test]
    async fn test_delete_key() {
        let (_tmp, kms) = create_test_kms();

        kms.create_key("del-key", KeyType::Aes256Gcm, KeyUsage::EncryptDecrypt)
            .await
            .unwrap();

        kms.delete_key("del-key").await.unwrap();

        let result = kms.get_key("del-key").await;
        assert!(matches!(result, Err(KmsError::KeyNotFound(_))));
    }

    #[tokio::test]
    async fn test_enable_disable_key() {
        let (_tmp, kms) = create_test_kms();

        kms.create_key("toggle-key", KeyType::Aes256Gcm, KeyUsage::EncryptDecrypt)
            .await
            .unwrap();

        kms.disable_key("toggle-key").await.unwrap();
        let disabled = kms.get_key("toggle-key").await.unwrap();
        assert!(!disabled.enabled);

        kms.enable_key("toggle-key").await.unwrap();
        let enabled = kms.get_key("toggle-key").await.unwrap();
        assert!(enabled.enabled);
    }
}
