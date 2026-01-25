//! Cryptographic primitives for device pairing

use chacha20poly1305::{
    aead::{Aead, KeyInit},
    ChaCha20Poly1305, Nonce,
};
use hkdf::Hkdf;
use rand::rngs::OsRng;
use sha2::Sha256;
use x25519_dalek::{PublicKey as X25519PublicKey, StaticSecret as X25519Secret};
use zeroize::Zeroizing;

use crate::error::CryptoError;

/// Domain separation label for key derivation
const KDF_LABEL: &[u8] = b"hanzo-pairing-v1";

/// Hybrid key exchange result
pub struct SharedSecret {
    /// The derived shared secret (zeroized on drop)
    secret: Zeroizing<[u8; 32]>,
}

impl SharedSecret {
    /// Create from raw bytes
    pub fn from_bytes(bytes: [u8; 32]) -> Self {
        Self {
            secret: Zeroizing::new(bytes),
        }
    }

    /// Get the secret bytes
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.secret
    }

    /// Derive an encryption key from the shared secret
    ///
    /// # Panics
    /// This function will never panic as HKDF-SHA256 always supports 32-byte expansion.
    pub fn derive_encryption_key(&self, context: &[u8]) -> [u8; 32] {
        let hk = Hkdf::<Sha256>::new(None, &*self.secret);
        let mut key = [0u8; 32];
        // SAFETY: HKDF-SHA256 can expand to up to 255*32=8160 bytes, so 32 is always valid
        if hk.expand(context, &mut key).is_err() {
            unreachable!("HKDF-SHA256 supports 32-byte output");
        }
        key
    }
}

/// X25519 key exchange
pub fn x25519_key_exchange(
    our_secret: &X25519Secret,
    peer_public: &X25519PublicKey,
) -> SharedSecret {
    let shared = our_secret.diffie_hellman(peer_public);
    SharedSecret::from_bytes(*shared.as_bytes())
}

/// Derive shared secret with domain separation
pub fn derive_shared_secret(
    x25519_shared: &[u8; 32],
    additional_secret: Option<&[u8]>,
) -> SharedSecret {
    let hk = Hkdf::<Sha256>::new(None, x25519_shared);

    let mut info = Vec::from(KDF_LABEL);
    if let Some(additional) = additional_secret {
        info.extend_from_slice(additional);
    }

    let mut output = [0u8; 32];
    // SAFETY: HKDF-SHA256 can expand to up to 255*32=8160 bytes, so 32 is always valid
    if hk.expand(&info, &mut output).is_err() {
        unreachable!("HKDF-SHA256 supports 32-byte output");
    }

    SharedSecret::from_bytes(output)
}

/// Combine multiple shared secrets (for hybrid PQ scheme)
pub fn combine_shared_secrets(secrets: &[&[u8]]) -> SharedSecret {
    let mut combined = Vec::new();
    for secret in secrets {
        combined.extend_from_slice(secret);
    }

    let hk = Hkdf::<Sha256>::new(None, &combined);
    let mut output = [0u8; 32];
    // SAFETY: HKDF-SHA256 can expand to up to 255*32=8160 bytes, so 32 is always valid
    if hk.expand(KDF_LABEL, &mut output).is_err() {
        unreachable!("HKDF-SHA256 supports 32-byte output");
    }

    SharedSecret::from_bytes(output)
}

/// Encrypt data using ChaCha20-Poly1305
pub fn encrypt(
    key: &[u8; 32],
    plaintext: &[u8],
    associated_data: Option<&[u8]>,
) -> Result<(Vec<u8>, [u8; 12]), CryptoError> {
    let cipher = ChaCha20Poly1305::new(key.into());

    let mut nonce_bytes = [0u8; 12];
    rand::RngCore::fill_bytes(&mut OsRng, &mut nonce_bytes);
    let nonce = Nonce::from_slice(&nonce_bytes);

    let ciphertext = if let Some(aad) = associated_data {
        cipher
            .encrypt(
                nonce,
                chacha20poly1305::aead::Payload {
                    msg: plaintext,
                    aad,
                },
            )
            .map_err(|e| CryptoError::KeyGenerationFailed(e.to_string()))?
    } else {
        cipher
            .encrypt(nonce, plaintext)
            .map_err(|e| CryptoError::KeyGenerationFailed(e.to_string()))?
    };

    Ok((ciphertext, nonce_bytes))
}

/// Decrypt data using ChaCha20-Poly1305
pub fn decrypt(
    key: &[u8; 32],
    ciphertext: &[u8],
    nonce: &[u8; 12],
    associated_data: Option<&[u8]>,
) -> Result<Vec<u8>, CryptoError> {
    let cipher = ChaCha20Poly1305::new(key.into());
    let nonce = Nonce::from_slice(nonce);

    let plaintext = if let Some(aad) = associated_data {
        cipher
            .decrypt(
                nonce,
                chacha20poly1305::aead::Payload {
                    msg: ciphertext,
                    aad,
                },
            )
            .map_err(|_| CryptoError::VerificationFailed)?
    } else {
        cipher
            .decrypt(nonce, ciphertext)
            .map_err(|_| CryptoError::VerificationFailed)?
    };

    Ok(plaintext)
}

/// Generate a random challenge
pub fn generate_challenge() -> [u8; 32] {
    let mut challenge = [0u8; 32];
    rand::RngCore::fill_bytes(&mut OsRng, &mut challenge);
    challenge
}

/// Generate a random nonce
pub fn generate_nonce() -> [u8; 16] {
    let mut nonce = [0u8; 16];
    rand::RngCore::fill_bytes(&mut OsRng, &mut nonce);
    nonce
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_x25519_key_exchange() {
        let alice_secret = X25519Secret::random_from_rng(OsRng);
        let alice_public = X25519PublicKey::from(&alice_secret);

        let bob_secret = X25519Secret::random_from_rng(OsRng);
        let bob_public = X25519PublicKey::from(&bob_secret);

        let alice_shared = x25519_key_exchange(&alice_secret, &bob_public);
        let bob_shared = x25519_key_exchange(&bob_secret, &alice_public);

        assert_eq!(alice_shared.as_bytes(), bob_shared.as_bytes());
    }

    #[test]
    fn test_derive_shared_secret() {
        let shared = [0x42u8; 32];
        let derived1 = derive_shared_secret(&shared, None);
        let derived2 = derive_shared_secret(&shared, None);

        // Same input should produce same output
        assert_eq!(derived1.as_bytes(), derived2.as_bytes());

        // Different additional data should produce different output
        let derived3 = derive_shared_secret(&shared, Some(b"extra"));
        assert_ne!(derived1.as_bytes(), derived3.as_bytes());
    }

    #[test]
    fn test_combine_shared_secrets() {
        let secret1 = [0x01u8; 32];
        let secret2 = [0x02u8; 32];

        let combined = combine_shared_secrets(&[&secret1, &secret2]);

        // Should be deterministic
        let combined2 = combine_shared_secrets(&[&secret1, &secret2]);
        assert_eq!(combined.as_bytes(), combined2.as_bytes());

        // Order matters
        let combined3 = combine_shared_secrets(&[&secret2, &secret1]);
        assert_ne!(combined.as_bytes(), combined3.as_bytes());
    }

    #[test]
    fn test_encrypt_decrypt() {
        let key = [0x42u8; 32];
        let plaintext = b"Hello, World!";

        let (ciphertext, nonce) = encrypt(&key, plaintext, None).expect("Encryption failed");

        let decrypted = decrypt(&key, &ciphertext, &nonce, None).expect("Decryption failed");

        assert_eq!(decrypted, plaintext);
    }

    #[test]
    fn test_encrypt_decrypt_with_aad() {
        let key = [0x42u8; 32];
        let plaintext = b"Hello, World!";
        let aad = b"associated data";

        let (ciphertext, nonce) =
            encrypt(&key, plaintext, Some(aad)).expect("Encryption failed");

        // Correct AAD should succeed
        let decrypted =
            decrypt(&key, &ciphertext, &nonce, Some(aad)).expect("Decryption failed");
        assert_eq!(decrypted, plaintext);

        // Wrong AAD should fail
        let result = decrypt(&key, &ciphertext, &nonce, Some(b"wrong aad"));
        assert!(result.is_err());

        // Missing AAD should fail
        let result = decrypt(&key, &ciphertext, &nonce, None);
        assert!(result.is_err());
    }

    #[test]
    fn test_derive_encryption_key() {
        let shared = SharedSecret::from_bytes([0x42u8; 32]);

        let key1 = shared.derive_encryption_key(b"context1");
        let key2 = shared.derive_encryption_key(b"context2");

        // Different contexts should produce different keys
        assert_ne!(key1, key2);

        // Same context should produce same key
        let key1_again = shared.derive_encryption_key(b"context1");
        assert_eq!(key1, key1_again);
    }
}
