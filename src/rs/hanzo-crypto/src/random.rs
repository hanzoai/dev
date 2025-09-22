/// Cryptographically secure random number generation
use rand::{Rng, RngCore};
use rand::rngs::OsRng;

/// Secure random number generator
pub struct SecureRandom {
    rng: OsRng,
}

impl SecureRandom {
    /// Create a new secure random generator
    pub fn new() -> Self {
        Self { rng: OsRng }
    }

    /// Generate random bytes
    pub fn random_bytes(&mut self, length: usize) -> Vec<u8> {
        let mut bytes = vec![0u8; length];
        self.rng.fill_bytes(&mut bytes);
        bytes
    }

    /// Generate a random u32
    pub fn random_u32(&mut self) -> u32 {
        self.rng.gen()
    }

    /// Generate a random u64
    pub fn random_u64(&mut self) -> u64 {
        self.rng.gen()
    }

    /// Create a deterministic RNG from a seed (for testing)
    #[cfg(test)]
    pub fn from_seed(seed: [u8; 32]) -> ChaCha20Rng {
        ChaCha20Rng::from_seed(seed)
    }
}

impl Default for SecureRandom {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_random_bytes() {
        let mut rng = SecureRandom::new();
        let bytes1 = rng.random_bytes(32);
        let bytes2 = rng.random_bytes(32);

        assert_eq!(bytes1.len(), 32);
        assert_eq!(bytes2.len(), 32);
        assert_ne!(bytes1, bytes2);
    }
}