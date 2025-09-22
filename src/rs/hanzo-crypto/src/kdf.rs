/// Key Derivation Functions module
use argon2::{Argon2, PasswordHasher, password_hash::SaltString};
use rand::rngs::OsRng;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum KDFError {
    #[error("Key derivation failed")]
    DerivationFailed,
    #[error("Verification failed")]
    VerificationFailed,
}

/// Supported KDF algorithms
pub enum KeyDerivationFunction {
    Argon2id,
}

/// Derive a key from a password
pub fn derive_key(
    password: &[u8],
    salt: Option<&[u8]>,
    _algorithm: KeyDerivationFunction,
) -> Result<Vec<u8>, KDFError> {
    let argon2 = Argon2::default();

    let salt = if let Some(s) = salt {
        SaltString::encode_b64(s).map_err(|_| KDFError::DerivationFailed)?
    } else {
        SaltString::generate(&mut OsRng)
    };

    let password_str = std::str::from_utf8(password)
        .map_err(|_| KDFError::DerivationFailed)?;

    let hash = argon2
        .hash_password(password_str.as_bytes(), &salt)
        .map_err(|_| KDFError::DerivationFailed)?;

    Ok(hash.hash.unwrap().as_bytes().to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_derive_key() {
        let password = b"strong_password";
        let key = derive_key(password, None, KeyDerivationFunction::Argon2id);
        assert!(key.is_ok());
    }
}