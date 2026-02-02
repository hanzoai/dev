//! Encrypted Key Share Storage
//!
//! Provides secure storage for MPC key shares using:
//! - AES-256-GCM for encryption
//! - Argon2id for key derivation from passwords
//! - Sled embedded database for persistence

use crate::dkg::KeyShare;
use crate::error::{WalletError, WalletResult};
use crate::CurveType;

use aes_gcm::{
    aead::{Aead, KeyInit},
    Aes256Gcm, Nonce,
};
use argon2::{password_hash::SaltString, Argon2, PasswordHasher};
use hkdf::Hkdf;
use parking_lot::RwLock;
use rand::{rngs::OsRng, RngCore};
use serde::{Deserialize, Serialize};
use sha2::Sha256;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use zeroize::{Zeroize, ZeroizeOnDrop};

/// Metadata about a stored key (not sensitive)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeyMetadata {
    /// Unique key identifier
    pub key_id: String,
    /// Curve type
    pub curve: CurveType,
    /// Threshold required for signing
    pub threshold: u16,
    /// Total number of participants
    pub total_participants: u16,
    /// Group public key (hex encoded)
    pub group_public_key: String,
    /// Creation timestamp (Unix epoch)
    pub created_at: u64,
    /// Last used timestamp (Unix epoch)
    pub last_used_at: Option<u64>,
    /// Optional description
    pub description: Option<String>,
    /// Labels for organization
    pub labels: HashMap<String, String>,
}

impl KeyMetadata {
    /// Create metadata from a key share
    pub fn from_key_share(key_share: &KeyShare, description: Option<String>) -> Self {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        Self {
            key_id: key_share.key_id.clone(),
            curve: key_share.curve,
            threshold: key_share.threshold,
            total_participants: key_share.total_participants,
            group_public_key: key_share.group_public_key_hex(),
            created_at: now,
            last_used_at: None,
            description,
            labels: HashMap::new(),
        }
    }
}

/// Encrypted key share data stored on disk
#[derive(Serialize, Deserialize)]
struct EncryptedKeyData {
    /// Version for future format changes
    version: u8,
    /// Salt used for key derivation
    #[serde(with = "hex")]
    salt: Vec<u8>,
    /// Nonce for AES-GCM
    #[serde(with = "hex")]
    nonce: Vec<u8>,
    /// Encrypted key share data
    #[serde(with = "hex")]
    ciphertext: Vec<u8>,
}

/// Derived encryption key (zeroize on drop)
#[derive(Zeroize, ZeroizeOnDrop)]
struct DerivedKey([u8; 32]);

impl DerivedKey {
    fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }
}

/// Encrypted key store using sled database
pub struct EncryptedKeyStore {
    db: sled::Db,
    /// Cache of metadata (not sensitive)
    metadata_cache: Arc<RwLock<HashMap<String, KeyMetadata>>>,
}

impl EncryptedKeyStore {
    /// Open or create a key store at the given path
    pub fn open<P: AsRef<Path>>(path: P) -> WalletResult<Self> {
        let db = sled::open(path).map_err(|e| WalletError::Storage(e.to_string()))?;

        let store = Self {
            db,
            metadata_cache: Arc::new(RwLock::new(HashMap::new())),
        };

        // Load metadata cache
        store.load_metadata_cache()?;

        Ok(store)
    }

    /// Open an in-memory key store (for testing)
    pub fn open_in_memory() -> WalletResult<Self> {
        let db = sled::Config::new()
            .temporary(true)
            .open()
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        Ok(Self {
            db,
            metadata_cache: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    /// Store an encrypted key share
    pub fn store(
        &self,
        key_share: &KeyShare,
        password: &str,
        description: Option<String>,
    ) -> WalletResult<()> {
        // Serialize the key share
        let plaintext = bincode::serialize(key_share)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        // Derive encryption key from password
        let (derived_key, salt) = self.derive_key(password, None)?;

        // Generate random nonce
        let mut nonce_bytes = [0u8; 12];
        OsRng.fill_bytes(&mut nonce_bytes);

        // Encrypt
        let cipher = Aes256Gcm::new_from_slice(derived_key.as_bytes())
            .map_err(|e| WalletError::Encryption(e.to_string()))?;

        let nonce = Nonce::from_slice(&nonce_bytes);
        let ciphertext = cipher
            .encrypt(nonce, plaintext.as_ref())
            .map_err(|e| WalletError::Encryption(e.to_string()))?;

        // Create encrypted data structure
        let encrypted_data = EncryptedKeyData {
            version: 1,
            salt,
            nonce: nonce_bytes.to_vec(),
            ciphertext,
        };

        let encrypted_bytes = bincode::serialize(&encrypted_data)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        // Store encrypted key share
        let key = format!("key:{}", key_share.key_id);
        self.db
            .insert(key.as_bytes(), encrypted_bytes)
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        // Store and cache metadata
        let metadata = KeyMetadata::from_key_share(key_share, description);
        let metadata_bytes = bincode::serialize(&metadata)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let meta_key = format!("meta:{}", key_share.key_id);
        self.db
            .insert(meta_key.as_bytes(), metadata_bytes)
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        self.metadata_cache
            .write()
            .insert(key_share.key_id.clone(), metadata);

        // Flush to disk
        self.db
            .flush()
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        Ok(())
    }

    /// Load an encrypted key share
    pub fn load(&self, key_id: &str, password: &str) -> WalletResult<KeyShare> {
        let key = format!("key:{key_id}");

        let encrypted_bytes = self
            .db
            .get(key.as_bytes())
            .map_err(|e| WalletError::Storage(e.to_string()))?
            .ok_or_else(|| WalletError::KeyShareNotFound(key_id.to_string()))?;

        let encrypted_data: EncryptedKeyData = bincode::deserialize(&encrypted_bytes)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        if encrypted_data.version != 1 {
            return Err(WalletError::Storage(format!(
                "Unsupported key format version: {}",
                encrypted_data.version
            )));
        }

        // Derive key using stored salt
        let (derived_key, _) = self.derive_key(password, Some(&encrypted_data.salt))?;

        // Decrypt
        let cipher = Aes256Gcm::new_from_slice(derived_key.as_bytes())
            .map_err(|e| WalletError::Decryption(e.to_string()))?;

        let nonce = Nonce::from_slice(&encrypted_data.nonce);
        let plaintext = cipher
            .decrypt(nonce, encrypted_data.ciphertext.as_ref())
            .map_err(|_| WalletError::Decryption("Decryption failed - wrong password?".into()))?;

        let key_share: KeyShare = bincode::deserialize(&plaintext)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        // Update last used timestamp
        self.update_last_used(key_id)?;

        Ok(key_share)
    }

    /// Delete a key share
    pub fn delete(&self, key_id: &str) -> WalletResult<()> {
        let key = format!("key:{key_id}");
        let meta_key = format!("meta:{key_id}");

        self.db
            .remove(key.as_bytes())
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        self.db
            .remove(meta_key.as_bytes())
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        self.metadata_cache.write().remove(key_id);

        self.db
            .flush()
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        Ok(())
    }

    /// Check if a key exists
    pub fn exists(&self, key_id: &str) -> bool {
        self.metadata_cache.read().contains_key(key_id)
    }

    /// Get metadata for a key
    pub fn get_metadata(&self, key_id: &str) -> Option<KeyMetadata> {
        self.metadata_cache.read().get(key_id).cloned()
    }

    /// List all key metadata
    pub fn list(&self) -> Vec<KeyMetadata> {
        self.metadata_cache.read().values().cloned().collect()
    }

    /// List keys by curve type
    pub fn list_by_curve(&self, curve: CurveType) -> Vec<KeyMetadata> {
        self.metadata_cache
            .read()
            .values()
            .filter(|m| m.curve == curve)
            .cloned()
            .collect()
    }

    /// Update key metadata description
    pub fn update_description(&self, key_id: &str, description: Option<String>) -> WalletResult<()> {
        let mut cache = self.metadata_cache.write();

        if let Some(metadata) = cache.get_mut(key_id) {
            metadata.description = description;

            let metadata_bytes = bincode::serialize(metadata)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

            let meta_key = format!("meta:{key_id}");
            self.db
                .insert(meta_key.as_bytes(), metadata_bytes)
                .map_err(|e| WalletError::Storage(e.to_string()))?;

            Ok(())
        } else {
            Err(WalletError::KeyShareNotFound(key_id.to_string()))
        }
    }

    /// Add or update a label on a key
    pub fn set_label(&self, key_id: &str, key: &str, value: &str) -> WalletResult<()> {
        let mut cache = self.metadata_cache.write();

        if let Some(metadata) = cache.get_mut(key_id) {
            metadata.labels.insert(key.to_string(), value.to_string());

            let metadata_bytes = bincode::serialize(metadata)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

            let meta_key = format!("meta:{key_id}");
            self.db
                .insert(meta_key.as_bytes(), metadata_bytes)
                .map_err(|e| WalletError::Storage(e.to_string()))?;

            Ok(())
        } else {
            Err(WalletError::KeyShareNotFound(key_id.to_string()))
        }
    }

    /// Change the password for an encrypted key
    pub fn change_password(
        &self,
        key_id: &str,
        old_password: &str,
        new_password: &str,
    ) -> WalletResult<()> {
        // Load with old password
        let key_share = self.load(key_id, old_password)?;

        // Get existing metadata
        let metadata = self
            .get_metadata(key_id)
            .ok_or_else(|| WalletError::KeyShareNotFound(key_id.to_string()))?;

        // Re-store with new password
        self.store(&key_share, new_password, metadata.description)?;

        Ok(())
    }

    /// Export key share (encrypted) for backup
    pub fn export(&self, key_id: &str) -> WalletResult<Vec<u8>> {
        let key = format!("key:{key_id}");

        let encrypted_bytes = self
            .db
            .get(key.as_bytes())
            .map_err(|e| WalletError::Storage(e.to_string()))?
            .ok_or_else(|| WalletError::KeyShareNotFound(key_id.to_string()))?;

        Ok(encrypted_bytes.to_vec())
    }

    /// Import an encrypted key share backup
    pub fn import(&self, key_id: &str, encrypted_data: &[u8], password: &str) -> WalletResult<()> {
        // Verify we can decrypt it
        let encrypted: EncryptedKeyData = bincode::deserialize(encrypted_data)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let (derived_key, _) = self.derive_key(password, Some(&encrypted.salt))?;

        let cipher = Aes256Gcm::new_from_slice(derived_key.as_bytes())
            .map_err(|e| WalletError::Decryption(e.to_string()))?;

        let nonce = Nonce::from_slice(&encrypted.nonce);
        let plaintext = cipher
            .decrypt(nonce, encrypted.ciphertext.as_ref())
            .map_err(|_| WalletError::Decryption("Cannot decrypt imported data".into()))?;

        let key_share: KeyShare = bincode::deserialize(&plaintext)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        // Verify key_id matches
        if key_share.key_id != key_id {
            return Err(WalletError::InvalidKeyShare(format!(
                "Key ID mismatch: expected {key_id}, got {}",
                key_share.key_id
            )));
        }

        // Store the encrypted data directly
        let key = format!("key:{key_id}");
        self.db
            .insert(key.as_bytes(), encrypted_data)
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        // Create and store metadata
        let metadata = KeyMetadata::from_key_share(&key_share, Some("Imported".to_string()));
        let metadata_bytes = bincode::serialize(&metadata)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;

        let meta_key = format!("meta:{key_id}");
        self.db
            .insert(meta_key.as_bytes(), metadata_bytes)
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        self.metadata_cache.write().insert(key_id.to_string(), metadata);

        self.db
            .flush()
            .map_err(|e| WalletError::Storage(e.to_string()))?;

        Ok(())
    }

    // Helper: Derive encryption key from password
    fn derive_key(
        &self,
        password: &str,
        salt: Option<&[u8]>,
    ) -> WalletResult<(DerivedKey, Vec<u8>)> {
        let salt_string = if let Some(s) = salt {
            // Reconstruct salt string from stored bytes (which are base64 string bytes)
            let salt_str = std::str::from_utf8(s)
                .map_err(|e| WalletError::Encryption(format!("Invalid salt encoding: {e}")))?;
            SaltString::from_b64(salt_str)
                .map_err(|e| WalletError::Encryption(format!("Invalid salt: {e}")))?
        } else {
            SaltString::generate(&mut OsRng)
        };

        let argon2 = Argon2::default();

        let password_hash = argon2
            .hash_password(password.as_bytes(), &salt_string)
            .map_err(|e| WalletError::Encryption(format!("Key derivation failed: {e}")))?;

        let hash_bytes = password_hash
            .hash
            .ok_or_else(|| WalletError::Encryption("No hash output".to_string()))?;

        // Use HKDF to expand to exactly 32 bytes
        let hk = Hkdf::<Sha256>::new(None, hash_bytes.as_bytes());
        let mut key = [0u8; 32];
        hk.expand(b"hanzo-wallet-aes-key", &mut key)
            .map_err(|e| WalletError::Encryption(format!("HKDF expansion failed: {e}")))?;

        Ok((DerivedKey(key), salt_string.as_str().as_bytes().to_vec()))
    }

    // Helper: Load metadata cache from database
    fn load_metadata_cache(&self) -> WalletResult<()> {
        let mut cache = self.metadata_cache.write();

        for result in self.db.scan_prefix(b"meta:") {
            let (_, value) = result.map_err(|e| WalletError::Storage(e.to_string()))?;

            let metadata: KeyMetadata = bincode::deserialize(&value)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

            cache.insert(metadata.key_id.clone(), metadata);
        }

        Ok(())
    }

    // Helper: Update last used timestamp
    fn update_last_used(&self, key_id: &str) -> WalletResult<()> {
        let mut cache = self.metadata_cache.write();

        if let Some(metadata) = cache.get_mut(key_id) {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0);

            metadata.last_used_at = Some(now);

            let metadata_bytes = bincode::serialize(metadata)
                .map_err(|e| WalletError::Serialization(e.to_string()))?;

            let meta_key = format!("meta:{key_id}");
            self.db
                .insert(meta_key.as_bytes(), metadata_bytes)
                .map_err(|e| WalletError::Storage(e.to_string()))?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dkg::DkgParticipant;

    fn create_test_key_share() -> KeyShare {
        let threshold = 2u16;
        let total = 3u16;

        let mut participants: Vec<DkgParticipant> = (1..=total)
            .map(|i| {
                DkgParticipant::new(i.to_string(), CurveType::Secp256k1, threshold, total).unwrap()
            })
            .collect();

        let round1_packages: Vec<_> = participants
            .iter_mut()
            .map(|p| p.generate_round1().unwrap())
            .collect();

        let mut all_round2_packages = Vec::new();
        for participant in &mut participants {
            let packages = participant.process_round1(&round1_packages).unwrap();
            all_round2_packages.extend(packages);
        }

        participants[0]
            .finalize(
                "test-storage-key".to_string(),
                &round1_packages,
                &all_round2_packages,
            )
            .unwrap()
    }

    #[test]
    fn test_store_and_load() {
        let store = EncryptedKeyStore::open_in_memory().unwrap();
        let key_share = create_test_key_share();
        let password = "test-password-123";

        // Store
        store
            .store(&key_share, password, Some("Test key".to_string()))
            .unwrap();

        // Check exists
        assert!(store.exists(&key_share.key_id));

        // Load
        let loaded = store.load(&key_share.key_id, password).unwrap();
        assert_eq!(loaded.key_id, key_share.key_id);
        assert_eq!(loaded.curve, key_share.curve);
        assert_eq!(loaded.group_public_key, key_share.group_public_key);
    }

    #[test]
    fn test_wrong_password() {
        let store = EncryptedKeyStore::open_in_memory().unwrap();
        let key_share = create_test_key_share();

        store
            .store(&key_share, "correct-password", None)
            .unwrap();

        let result = store.load(&key_share.key_id, "wrong-password");
        assert!(result.is_err());
    }

    #[test]
    fn test_delete() {
        let store = EncryptedKeyStore::open_in_memory().unwrap();
        let key_share = create_test_key_share();

        store.store(&key_share, "password", None).unwrap();
        assert!(store.exists(&key_share.key_id));

        store.delete(&key_share.key_id).unwrap();
        assert!(!store.exists(&key_share.key_id));
    }

    #[test]
    fn test_list_keys() {
        let store = EncryptedKeyStore::open_in_memory().unwrap();

        // Create multiple key shares
        let threshold = 2u16;
        let total = 3u16;

        let mut participants: Vec<DkgParticipant> = (1..=total)
            .map(|i| {
                DkgParticipant::new(i.to_string(), CurveType::Secp256k1, threshold, total).unwrap()
            })
            .collect();

        let round1_packages: Vec<_> = participants
            .iter_mut()
            .map(|p| p.generate_round1().unwrap())
            .collect();

        let mut all_round2_packages = Vec::new();
        for participant in &mut participants {
            let packages = participant.process_round1(&round1_packages).unwrap();
            all_round2_packages.extend(packages);
        }

        let key1 = participants[0]
            .finalize("key-1".to_string(), &round1_packages, &all_round2_packages)
            .unwrap();

        let key2 = participants[1]
            .finalize("key-2".to_string(), &round1_packages, &all_round2_packages)
            .unwrap();

        store.store(&key1, "pass1", None).unwrap();
        store.store(&key2, "pass2", None).unwrap();

        let keys = store.list();
        assert_eq!(keys.len(), 2);
    }

    #[test]
    fn test_change_password() {
        let store = EncryptedKeyStore::open_in_memory().unwrap();
        let key_share = create_test_key_share();

        store.store(&key_share, "old-password", None).unwrap();

        store
            .change_password(&key_share.key_id, "old-password", "new-password")
            .unwrap();

        // Old password should fail
        assert!(store.load(&key_share.key_id, "old-password").is_err());

        // New password should work
        let loaded = store.load(&key_share.key_id, "new-password").unwrap();
        assert_eq!(loaded.key_id, key_share.key_id);
    }

    #[test]
    fn test_export_import() {
        let store1 = EncryptedKeyStore::open_in_memory().unwrap();
        let store2 = EncryptedKeyStore::open_in_memory().unwrap();
        let key_share = create_test_key_share();
        let password = "export-password";

        store1.store(&key_share, password, None).unwrap();

        // Export
        let exported = store1.export(&key_share.key_id).unwrap();

        // Import into second store
        store2.import(&key_share.key_id, &exported, password).unwrap();

        // Verify
        let loaded = store2.load(&key_share.key_id, password).unwrap();
        assert_eq!(loaded.key_id, key_share.key_id);
    }
}
