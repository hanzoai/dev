/// Hash functions module
use sha2::{Sha256, Sha512, Digest};
use sha3::{Sha3_256, Sha3_512};
use blake3;
use std::fmt;

/// Supported hash algorithms
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HashAlgorithm {
    Sha256,
    Sha512,
    Sha3_256,
    Sha3_512,
    Blake3,
}

/// Hash output wrapper
#[derive(Clone, PartialEq, Eq)]
pub struct Hash {
    bytes: Vec<u8>,
    algorithm: HashAlgorithm,
}

impl Hash {
    /// Create a new hash from the given data using the specified algorithm
    pub fn new(data: &[u8], algorithm: HashAlgorithm) -> Self {
        let bytes = match algorithm {
            HashAlgorithm::Sha256 => {
                let mut hasher = Sha256::new();
                hasher.update(data);
                hasher.finalize().to_vec()
            }
            HashAlgorithm::Sha512 => {
                let mut hasher = Sha512::new();
                hasher.update(data);
                hasher.finalize().to_vec()
            }
            HashAlgorithm::Sha3_256 => {
                let mut hasher = Sha3_256::new();
                hasher.update(data);
                hasher.finalize().to_vec()
            }
            HashAlgorithm::Sha3_512 => {
                let mut hasher = Sha3_512::new();
                hasher.update(data);
                hasher.finalize().to_vec()
            }
            HashAlgorithm::Blake3 => {
                blake3::hash(data).as_bytes().to_vec()
            }
        };

        Self { bytes, algorithm }
    }

    /// Get the hash bytes
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// Get hex representation
    pub fn to_hex(&self) -> String {
        hex::encode(&self.bytes)
    }

    /// Get the algorithm used
    pub fn algorithm(&self) -> HashAlgorithm {
        self.algorithm
    }
}

impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}

impl fmt::Debug for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Hash({:?}, {}...)", self.algorithm, &self.to_hex()[..8])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sha256() {
        let data = b"hello world";
        let hash = Hash::new(data, HashAlgorithm::Sha256);
        assert_eq!(hash.as_bytes().len(), 32);
    }

    #[test]
    fn test_blake3() {
        let data = b"hello world";
        let hash = Hash::new(data, HashAlgorithm::Blake3);
        assert_eq!(hash.as_bytes().len(), 32);
    }
}