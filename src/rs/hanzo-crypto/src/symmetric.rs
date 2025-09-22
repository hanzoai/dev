/// Symmetric encryption module
use aes_gcm::{
    aead::{Aead, AeadCore, KeyInit, OsRng},
    Aes256Gcm, Key, Nonce
};
use chacha20poly1305::ChaCha20Poly1305;
use zeroize::{Zeroize, ZeroizeOnDrop};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum SymmetricError {
    #[error("Encryption failed")]
    EncryptionFailed,
    #[error("Decryption failed")]
    DecryptionFailed,
    #[error("Invalid key length")]
    InvalidKeyLength,
}

/// Symmetric encryption key wrapper
#[derive(Zeroize, ZeroizeOnDrop)]
pub struct SymmetricKey {
    key: Vec<u8>,
}

impl SymmetricKey {
    /// Generate a new random key
    pub fn generate() -> Self {
        let key = Aes256Gcm::generate_key(OsRng);
        Self {
            key: key.to_vec(),
        }
    }

    /// Create from raw bytes
    pub fn from_bytes(bytes: Vec<u8>) -> Result<Self, SymmetricError> {
        if bytes.len() != 32 {
            return Err(SymmetricError::InvalidKeyLength);
        }
        Ok(Self { key: bytes })
    }

    /// Get key bytes
    pub fn as_bytes(&self) -> &[u8] {
        &self.key
    }
}

/// Encrypt data with AES-256-GCM
pub fn encrypt(key: &SymmetricKey, plaintext: &[u8]) -> Result<Vec<u8>, SymmetricError> {
    let cipher = Aes256Gcm::new(Key::<Aes256Gcm>::from_slice(&key.key));
    let nonce = Aes256Gcm::generate_nonce(&mut OsRng);

    let ciphertext = cipher
        .encrypt(&nonce, plaintext)
        .map_err(|_| SymmetricError::EncryptionFailed)?;

    // Prepend nonce to ciphertext
    let mut result = nonce.to_vec();
    result.extend_from_slice(&ciphertext);
    Ok(result)
}

/// Decrypt data with AES-256-GCM
pub fn decrypt(key: &SymmetricKey, ciphertext: &[u8]) -> Result<Vec<u8>, SymmetricError> {
    if ciphertext.len() < 12 {
        return Err(SymmetricError::DecryptionFailed);
    }

    let (nonce_bytes, actual_ciphertext) = ciphertext.split_at(12);
    let nonce = Nonce::from_slice(nonce_bytes);
    let cipher = Aes256Gcm::new(Key::<Aes256Gcm>::from_slice(&key.key));

    cipher
        .decrypt(nonce, actual_ciphertext)
        .map_err(|_| SymmetricError::DecryptionFailed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encrypt_decrypt() {
        let key = SymmetricKey::generate();
        let plaintext = b"Secret message";

        let ciphertext = encrypt(&key, plaintext).unwrap();
        let decrypted = decrypt(&key, &ciphertext).unwrap();

        assert_eq!(plaintext.to_vec(), decrypted);
    }
}