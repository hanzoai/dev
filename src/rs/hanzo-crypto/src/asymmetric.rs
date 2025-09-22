/// Asymmetric cryptography module
use ed25519_dalek::{Signer, SigningKey, VerifyingKey, Signature};
use x25519_dalek::{EphemeralSecret, PublicKey as X25519PublicKey};
use rand::{rngs::OsRng, Rng};
use zeroize::{Zeroize, ZeroizeOnDrop};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum AsymmetricError {
    #[error("Signature verification failed")]
    VerificationFailed,
    #[error("Invalid key")]
    InvalidKey,
}

/// Ed25519 public key
pub struct PublicKey {
    key: VerifyingKey,
}

/// Ed25519 secret key
#[derive(Zeroize, ZeroizeOnDrop)]
pub struct SecretKey {
    #[zeroize(skip)]
    key: SigningKey,
}

/// Ed25519 key pair
pub struct KeyPair {
    public: PublicKey,
    secret: SecretKey,
}

impl KeyPair {
    /// Generate a new key pair
    pub fn generate() -> Self {
        let mut csprng = OsRng;
        let secret_bytes: [u8; 32] = csprng.gen();
        let signing_key = SigningKey::from_bytes(&secret_bytes);
        let verifying_key = signing_key.verifying_key();

        Self {
            public: PublicKey { key: verifying_key },
            secret: SecretKey { key: signing_key },
        }
    }

    /// Get public key
    pub fn public(&self) -> &PublicKey {
        &self.public
    }

    /// Get secret key
    pub fn secret(&self) -> &SecretKey {
        &self.secret
    }
}

/// Sign a message
pub fn sign(secret_key: &SecretKey, message: &[u8]) -> Vec<u8> {
    let signature = secret_key.key.sign(message);
    signature.to_vec()
}

/// Verify a signature
pub fn verify(
    public_key: &PublicKey,
    message: &[u8],
    signature: &[u8],
) -> Result<(), AsymmetricError> {
    let sig = Signature::from_slice(signature)
        .map_err(|_| AsymmetricError::InvalidKey)?;

    public_key.key
        .verify_strict(message, &sig)
        .map_err(|_| AsymmetricError::VerificationFailed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sign_verify() {
        let keypair = KeyPair::generate();
        let message = b"Test message";

        let signature = sign(keypair.secret(), message);
        let result = verify(keypair.public(), message, &signature);

        assert!(result.is_ok());
    }

    #[test]
    fn test_verify_fails_with_wrong_key() {
        let keypair1 = KeyPair::generate();
        let keypair2 = KeyPair::generate();
        let message = b"Test message";

        let signature = sign(keypair1.secret(), message);
        let result = verify(keypair2.public(), message, &signature);

        assert!(result.is_err());
    }
}