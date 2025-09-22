/// Hanzo Crypto - Pure Rust cryptographic primitives
///
/// Provides both classical and post-quantum cryptographic algorithms
/// for the Hanzo ecosystem.

pub mod hash;
pub mod symmetric;
pub mod asymmetric;
pub mod pqc;
pub mod kdf;
pub mod random;

pub use hash::{Hash, HashAlgorithm};
pub use symmetric::{encrypt, decrypt, SymmetricKey};
pub use asymmetric::{KeyPair, PublicKey, SecretKey, sign, verify};
pub use pqc::{PQCKeyPair, PQCPublicKey, PQCSecretKey};
pub use kdf::{derive_key, KeyDerivationFunction};
pub use random::SecureRandom;

/// Prelude module for convenient imports
pub mod prelude {
    pub use crate::{
        Hash, HashAlgorithm,
        encrypt, decrypt, SymmetricKey,
        KeyPair, sign, verify,
        PQCKeyPair,
        derive_key,
        SecureRandom,
    };
}