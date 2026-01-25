//! Cryptographic primitives for the QoS Challenge System.
//!
//! This module provides cryptographic utilities including:
//! - Hash computation (BLAKE3, SHA-256)
//! - Signature generation and verification
//! - Merkle tree operations
//! - Random number generation

use blake3::Hasher as Blake3Hasher;
use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;
use sha2::{Digest, Sha256};

use crate::error::Result;

/// Compute BLAKE3 hash of data.
pub fn blake3_hash(data: &[u8]) -> [u8; 32] {
    let mut hasher = Blake3Hasher::new();
    hasher.update(data);
    *hasher.finalize().as_bytes()
}

/// Compute BLAKE3 hash of multiple data chunks.
pub fn blake3_hash_chunks(chunks: &[&[u8]]) -> [u8; 32] {
    let mut hasher = Blake3Hasher::new();
    for chunk in chunks {
        hasher.update(chunk);
    }
    *hasher.finalize().as_bytes()
}

/// Compute SHA-256 hash of data.
pub fn sha256_hash(data: &[u8]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update(data);
    let result = hasher.finalize();
    let mut output = [0u8; 32];
    output.copy_from_slice(&result);
    output
}

/// Generate deterministic random bytes from a seed.
pub fn deterministic_random(seed: &[u8; 32], output: &mut [u8]) {
    let mut rng = StdRng::from_seed(*seed);
    rng.fill(output);
}

/// Derive a child seed from a parent seed and index.
pub fn derive_seed(parent: &[u8; 32], index: u64) -> [u8; 32] {
    blake3_hash_chunks(&[parent, &index.to_le_bytes()])
}

/// Simple signature structure for challenge proofs.
///
/// In production, this would integrate with ML-DSA (post-quantum) or ECDSA.
/// For now, we use BLAKE3-based HMAC for demonstration.
#[derive(Debug, Clone)]
pub struct Signature {
    /// The signature bytes
    pub bytes: Vec<u8>,
}

impl Signature {
    /// Sign data with a secret key.
    ///
    /// Note: This is a simplified implementation. Production should use
    /// proper signature schemes like Ed25519, ECDSA, or ML-DSA.
    pub fn sign(data: &[u8], secret_key: &[u8; 32]) -> Self {
        // HMAC-like construction: H(key || data || key)
        let sig = blake3_hash_chunks(&[secret_key, data, secret_key]);
        Self {
            bytes: sig.to_vec(),
        }
    }

    /// Verify signature against data and public key.
    ///
    /// Note: Simplified - in production use proper signature verification.
    pub fn verify(&self, data: &[u8], secret_key: &[u8; 32]) -> bool {
        let expected = Self::sign(data, secret_key);
        constant_time_eq(&self.bytes, &expected.bytes)
    }

    /// Create signature from raw bytes.
    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        Self { bytes }
    }

    /// Get signature as bytes.
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }
}

/// Constant-time byte array comparison to prevent timing attacks.
fn constant_time_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut diff = 0u8;
    for (x, y) in a.iter().zip(b.iter()) {
        diff |= x ^ y;
    }
    diff == 0
}

/// Merkle tree for bandwidth verification.
#[derive(Debug, Clone)]
pub struct MerkleTree {
    /// Leaf hashes
    leaves: Vec<[u8; 32]>,
    /// Internal nodes (stored level by level, bottom-up)
    nodes: Vec<Vec<[u8; 32]>>,
    /// Root hash
    root: [u8; 32],
}

impl MerkleTree {
    /// Build a Merkle tree from data chunks.
    pub fn from_chunks(chunks: &[&[u8]]) -> Self {
        if chunks.is_empty() {
            return Self {
                leaves: vec![],
                nodes: vec![],
                root: [0u8; 32],
            };
        }

        // Hash all leaves
        let leaves: Vec<[u8; 32]> = chunks.iter().map(|c| blake3_hash(c)).collect();

        // Build tree bottom-up
        let mut nodes = Vec::new();
        let mut current_level = leaves.clone();

        while current_level.len() > 1 {
            let mut next_level = Vec::new();
            for pair in current_level.chunks(2) {
                let hash = if pair.len() == 2 {
                    blake3_hash_chunks(&[&pair[0], &pair[1]])
                } else {
                    // Odd number of nodes - duplicate the last one
                    blake3_hash_chunks(&[&pair[0], &pair[0]])
                };
                next_level.push(hash);
            }
            nodes.push(current_level);
            current_level = next_level;
        }

        let root = current_level.first().copied().unwrap_or([0u8; 32]);

        Self {
            leaves,
            nodes,
            root,
        }
    }

    /// Get the root hash.
    pub fn root(&self) -> [u8; 32] {
        self.root
    }

    /// Generate a proof for a specific leaf index.
    pub fn generate_proof(&self, index: usize) -> Option<MerkleProof> {
        if index >= self.leaves.len() {
            return None;
        }

        let mut path = Vec::new();
        let mut current_index = index;

        for level in &self.nodes {
            let sibling_index = if current_index % 2 == 0 {
                current_index + 1
            } else {
                current_index - 1
            };

            if sibling_index < level.len() {
                path.push(level[sibling_index]);
            } else if !level.is_empty() {
                // Duplicate last node for odd levels
                path.push(level[level.len() - 1]);
            }

            current_index /= 2;
        }

        Some(MerkleProof {
            leaf_hash: self.leaves[index],
            index,
            path,
        })
    }

    /// Verify a proof against the root.
    pub fn verify_proof(root: &[u8; 32], proof: &MerkleProof) -> bool {
        let mut current_hash = proof.leaf_hash;
        let mut current_index = proof.index;

        for sibling in &proof.path {
            current_hash = if current_index % 2 == 0 {
                blake3_hash_chunks(&[&current_hash, sibling])
            } else {
                blake3_hash_chunks(&[sibling, &current_hash])
            };
            current_index /= 2;
        }

        constant_time_eq(&current_hash, root)
    }
}

/// Merkle proof for a single leaf.
#[derive(Debug, Clone)]
pub struct MerkleProof {
    /// Hash of the leaf data
    pub leaf_hash: [u8; 32],
    /// Index of the leaf
    pub index: usize,
    /// Sibling hashes from leaf to root
    pub path: Vec<[u8; 32]>,
}

/// Verifiable Random Function (VRF) output.
///
/// Used for unpredictable challenge selection.
#[derive(Debug, Clone)]
pub struct VrfOutput {
    /// The random output
    pub output: [u8; 32],
    /// Proof of correct computation
    pub proof: Vec<u8>,
}

/// Simple VRF implementation using BLAKE3.
///
/// Note: Production should use a proper VRF like ECVRF or Ristretto VRF.
pub struct Vrf {
    secret_key: [u8; 32],
}

impl Vrf {
    /// Create a new VRF instance with the given secret key.
    pub fn new(secret_key: [u8; 32]) -> Self {
        Self { secret_key }
    }

    /// Compute VRF output for input.
    pub fn prove(&self, input: &[u8]) -> VrfOutput {
        // H(sk || input)
        let output = blake3_hash_chunks(&[&self.secret_key, input]);

        // Proof is a signature over the output
        let proof = Signature::sign(&output, &self.secret_key);

        VrfOutput {
            output,
            proof: proof.bytes,
        }
    }

    /// Verify VRF proof.
    pub fn verify(
        &self,
        input: &[u8],
        output: &[u8; 32],
        proof: &[u8],
    ) -> Result<bool> {
        let expected = self.prove(input);
        if !constant_time_eq(output, &expected.output) {
            return Ok(false);
        }

        let sig = Signature::from_bytes(proof.to_vec());
        Ok(sig.verify(output, &self.secret_key))
    }
}

/// Generate a challenge seed from VRF output and block hash.
pub fn generate_challenge_seed(
    vrf_output: &[u8; 32],
    block_hash: &[u8; 32],
    provider_id: &str,
) -> [u8; 32] {
    blake3_hash_chunks(&[vrf_output, block_hash, provider_id.as_bytes()])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_blake3_hash() {
        let data = b"hello world";
        let hash = blake3_hash(data);
        assert_eq!(hash.len(), 32);

        // Same input produces same output
        let hash2 = blake3_hash(data);
        assert_eq!(hash, hash2);

        // Different input produces different output
        let hash3 = blake3_hash(b"different");
        assert_ne!(hash, hash3);
    }

    #[test]
    fn test_signature() {
        let key = [42u8; 32];
        let data = b"test message";

        let sig = Signature::sign(data, &key);
        assert!(sig.verify(data, &key));

        // Wrong key fails
        let wrong_key = [0u8; 32];
        assert!(!sig.verify(data, &wrong_key));

        // Wrong data fails
        assert!(!sig.verify(b"wrong", &key));
    }

    #[test]
    fn test_merkle_tree() {
        let chunks: Vec<&[u8]> = vec![b"chunk1", b"chunk2", b"chunk3", b"chunk4"];
        let tree = MerkleTree::from_chunks(&chunks);

        // Generate and verify proofs
        for i in 0..chunks.len() {
            let proof = tree.generate_proof(i).expect("proof should exist");
            assert!(MerkleTree::verify_proof(&tree.root(), &proof));
        }
    }

    #[test]
    fn test_vrf() {
        let key = [1u8; 32];
        let vrf = Vrf::new(key);

        let input = b"test input";
        let output = vrf.prove(input);

        // Verify succeeds
        let result = vrf.verify(input, &output.output, &output.proof).expect("verify should work");
        assert!(result);

        // Same input produces same output
        let output2 = vrf.prove(input);
        assert_eq!(output.output, output2.output);
    }

    #[test]
    fn test_deterministic_random() {
        let seed = [99u8; 32];
        let mut buf1 = [0u8; 64];
        let mut buf2 = [0u8; 64];

        deterministic_random(&seed, &mut buf1);
        deterministic_random(&seed, &mut buf2);

        assert_eq!(buf1, buf2);
    }
}
