//! Envelope encryption (DEK/KEK pattern)
//!
//! This implements the envelope encryption pattern where:
//! - A Data Encryption Key (DEK) is generated for each encryption operation
//! - The DEK is encrypted with a Key Encryption Key (KEK) stored in KMS
//! - Data is encrypted with the DEK
//! - The encrypted DEK is stored alongside the encrypted data

use ring::aead::{self, Aad, LessSafeKey, Nonce, UnboundKey, AES_256_GCM};
use ring::rand::{SecureRandom, SystemRandom};
use serde::{Deserialize, Serialize};

use crate::kms::error::{KmsError, Result};
use crate::kms::traits::KeyManagementService;
use crate::kms::types::EncryptedData;

/// Envelope encrypted data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvelopeEncryptedData {
    /// The encrypted Data Encryption Key
    pub encrypted_dek: EncryptedData,
    /// Nonce used for data encryption
    pub nonce: Vec<u8>,
    /// The encrypted data
    pub ciphertext: Vec<u8>,
}

/// Envelope encryption helper
pub struct EnvelopeEncryption {
    rng: SystemRandom,
}

impl EnvelopeEncryption {
    /// Create a new envelope encryption instance
    pub fn new() -> Self {
        Self {
            rng: SystemRandom::new(),
        }
    }

    /// Encrypt data using envelope encryption
    pub async fn encrypt(
        &self,
        kms: &dyn KeyManagementService,
        master_key_id: &str,
        plaintext: &[u8],
    ) -> Result<EnvelopeEncryptedData> {
        // Generate a random DEK (256 bits for AES-256)
        let mut dek = vec![0u8; 32];
        self.rng
            .fill(&mut dek)
            .map_err(|_| KmsError::EncryptionFailed("Failed to generate DEK".to_string()))?;

        // Encrypt the DEK with the master key
        let encrypted_dek = kms.encrypt(master_key_id, &dek).await?;

        // Generate nonce for data encryption
        let mut nonce_bytes = vec![0u8; 12];
        self.rng
            .fill(&mut nonce_bytes)
            .map_err(|_| KmsError::EncryptionFailed("Failed to generate nonce".to_string()))?;

        // Encrypt the data with the DEK
        let ciphertext = self.encrypt_with_dek(&dek, &nonce_bytes, plaintext)?;

        // Zero out the DEK from memory
        // Note: In production, use zeroize crate for secure zeroing
        dek.iter_mut().for_each(|b| *b = 0);

        Ok(EnvelopeEncryptedData {
            encrypted_dek,
            nonce: nonce_bytes,
            ciphertext,
        })
    }

    /// Decrypt envelope encrypted data
    pub async fn decrypt(
        &self,
        kms: &dyn KeyManagementService,
        encrypted: &EnvelopeEncryptedData,
    ) -> Result<Vec<u8>> {
        // Decrypt the DEK using KMS
        let mut dek = kms.decrypt(&encrypted.encrypted_dek).await?;

        // Decrypt the data with the DEK
        let plaintext = self.decrypt_with_dek(&dek, &encrypted.nonce, &encrypted.ciphertext)?;

        // Zero out the DEK from memory
        dek.iter_mut().for_each(|b| *b = 0);

        Ok(plaintext)
    }

    /// Encrypt data with a DEK using AES-256-GCM
    fn encrypt_with_dek(&self, dek: &[u8], nonce: &[u8], plaintext: &[u8]) -> Result<Vec<u8>> {
        let unbound_key = UnboundKey::new(&AES_256_GCM, dek)
            .map_err(|_| KmsError::EncryptionFailed("Invalid DEK".to_string()))?;
        let key = LessSafeKey::new(unbound_key);

        let nonce = Nonce::try_assume_unique_for_key(nonce)
            .map_err(|_| KmsError::EncryptionFailed("Invalid nonce".to_string()))?;

        let mut in_out = plaintext.to_vec();
        key.seal_in_place_append_tag(nonce, Aad::empty(), &mut in_out)
            .map_err(|_| KmsError::EncryptionFailed("Encryption failed".to_string()))?;

        Ok(in_out)
    }

    /// Decrypt data with a DEK using AES-256-GCM
    fn decrypt_with_dek(&self, dek: &[u8], nonce: &[u8], ciphertext: &[u8]) -> Result<Vec<u8>> {
        let unbound_key = UnboundKey::new(&AES_256_GCM, dek)
            .map_err(|_| KmsError::DecryptionFailed("Invalid DEK".to_string()))?;
        let key = LessSafeKey::new(unbound_key);

        let nonce = Nonce::try_assume_unique_for_key(nonce)
            .map_err(|_| KmsError::DecryptionFailed("Invalid nonce".to_string()))?;

        let mut in_out = ciphertext.to_vec();
        let plaintext = key
            .open_in_place(nonce, Aad::empty(), &mut in_out)
            .map_err(|_| KmsError::DecryptionFailed("Decryption failed".to_string()))?;

        Ok(plaintext.to_vec())
    }
}

impl Default for EnvelopeEncryption {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encrypt_decrypt_with_dek() {
        let envelope = EnvelopeEncryption::new();

        let dek = vec![0u8; 32]; // Test key
        let nonce = vec![0u8; 12]; // Test nonce
        let plaintext = b"Hello, World!";

        let ciphertext = envelope.encrypt_with_dek(&dek, &nonce, plaintext).unwrap();
        let decrypted = envelope.decrypt_with_dek(&dek, &nonce, &ciphertext).unwrap();

        assert_eq!(decrypted, plaintext);
    }
}
