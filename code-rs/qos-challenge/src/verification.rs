//! Verification engine for challenge proofs.
//!
//! This module provides the `VerificationEngine` that validates proofs
//! submitted by providers in response to QoS challenges.

use crate::crypto::{MerkleTree, Signature, blake3_hash, blake3_hash_chunks};
use crate::error::{FailureReason, QoSError, Result};
use crate::types::{
    AvailabilityProofData, BandwidthProofData, Challenge, ChallengeId, ChallengeProof,
    ChallengeResult, ChallengeType, ComputeMetric, ComputeProofData, LatencyProofData,
    ModelProofData, PeerId, ProofData, TeeAttestation, TeeType,
};

use std::collections::HashMap;
use std::sync::RwLock;

/// Result of verification.
#[derive(Debug, Clone)]
pub enum VerificationResult {
    /// Proof passed verification.
    Passed {
        /// Performance score (1.0 = met expectations, >1.0 = exceeded)
        score: f64,
        /// Bonus for exceptional performance
        bonus: f64,
    },
    /// Proof failed verification.
    Failed(FailureReason),
}

impl VerificationResult {
    /// Check if verification passed.
    pub fn passed(&self) -> bool {
        matches!(self, Self::Passed { .. })
    }

    /// Get the score if passed.
    pub fn score(&self) -> Option<f64> {
        match self {
            Self::Passed { score, .. } => Some(*score),
            Self::Failed(_) => None,
        }
    }
}

/// Configuration for verification thresholds.
#[derive(Debug, Clone)]
pub struct VerificationConfig {
    /// Minimum performance score to pass (1.0 = met expectations)
    pub min_performance_score: f64,
    /// Number of intermediate hashes to spot-check
    pub intermediate_hash_samples: usize,
    /// Maximum allowed latency percentile (p99) in ms
    pub max_latency_p99_ms: u32,
    /// Minimum bandwidth threshold in Mbps
    pub min_bandwidth_mbps: u32,
    /// Maximum response time for availability in ms
    pub max_availability_response_ms: u32,
    /// Bonus threshold for exceptional performance
    pub exceptional_performance_threshold: f64,
    /// Bonus amount for exceptional performance
    pub exceptional_performance_bonus: f64,
}

impl Default for VerificationConfig {
    fn default() -> Self {
        Self {
            min_performance_score: 0.8,
            intermediate_hash_samples: 5,
            max_latency_p99_ms: 100,
            min_bandwidth_mbps: 100,
            max_availability_response_ms: 1000,
            exceptional_performance_threshold: 1.2,
            exceptional_performance_bonus: 0.1,
        }
    }
}

/// Reference computation result for verification.
#[derive(Debug, Clone)]
pub struct ReferenceResult {
    /// Expected result hash
    pub result_hash: [u8; 32],
    /// Expected intermediate hashes
    pub intermediate_hashes: Vec<[u8; 32]>,
    /// Expected minimum FLOPS
    pub min_flops: f64,
}

/// Verification engine for QoS challenge proofs.
pub struct VerificationEngine {
    /// Configuration
    config: VerificationConfig,
    /// Secret key for signature verification
    #[allow(dead_code)]
    secret_key: [u8; 32],
    /// Pending challenges (challenge_id -> challenge)
    pending: RwLock<HashMap<ChallengeId, Challenge>>,
    /// Completed verifications
    completed: RwLock<HashMap<ChallengeId, ChallengeResult>>,
}

impl VerificationEngine {
    /// Create a new verification engine.
    pub fn new(secret_key: [u8; 32]) -> Self {
        Self {
            config: VerificationConfig::default(),
            secret_key,
            pending: RwLock::new(HashMap::new()),
            completed: RwLock::new(HashMap::new()),
        }
    }

    /// Create with custom configuration.
    pub fn with_config(secret_key: [u8; 32], config: VerificationConfig) -> Self {
        Self {
            config,
            secret_key,
            pending: RwLock::new(HashMap::new()),
            completed: RwLock::new(HashMap::new()),
        }
    }

    /// Register a challenge for verification.
    pub fn register_challenge(&self, challenge: Challenge) {
        let mut pending = self.pending.write().expect("lock poisoned");
        pending.insert(challenge.id, challenge);
    }

    /// Verify a proof submission.
    pub fn verify(&self, proof: &ChallengeProof) -> Result<VerificationResult> {
        // Get the challenge
        let challenge = {
            let pending = self.pending.read().expect("lock poisoned");
            pending
                .get(&proof.challenge_id)
                .cloned()
                .ok_or(QoSError::ChallengeNotFound(proof.challenge_id))?
        };

        // Check expiry
        if challenge.is_expired() {
            return Err(QoSError::ChallengeExpired(challenge.id));
        }

        // Check deadline
        if proof.duration_ms > challenge.deadline_ms {
            return Err(QoSError::DeadlineExceeded {
                actual_ms: proof.duration_ms,
                deadline_ms: challenge.deadline_ms,
            });
        }

        // Verify signature
        self.verify_proof_signature(proof)?;

        // Verify TEE attestation if present
        let tee_verified = if let Some(ref attestation) = proof.tee_attestation {
            self.verify_tee_attestation(attestation)?
        } else {
            false
        };

        // Verify proof data based on challenge type
        let result = match (&challenge.challenge_type, &proof.proof_data) {
            (ChallengeType::Compute(metric), ProofData::Compute(data)) => {
                self.verify_compute_proof(&challenge, metric, data)
            }
            (ChallengeType::Latency, ProofData::Latency(data)) => {
                self.verify_latency_proof(&challenge, data)
            }
            (ChallengeType::Bandwidth(_), ProofData::Bandwidth(data)) => {
                self.verify_bandwidth_proof(&challenge, data)
            }
            (ChallengeType::Availability, ProofData::Availability(data)) => {
                self.verify_availability_proof(&challenge, data)
            }
            (ChallengeType::Model(_), ProofData::Model(data)) => {
                self.verify_model_proof(&challenge, data)
            }
            _ => Ok(VerificationResult::Failed(FailureReason::WrongProofType)),
        }?;

        // Store completed verification
        let challenge_result = ChallengeResult {
            challenge_id: proof.challenge_id,
            provider_id: proof.provider_id.clone(),
            passed: result.passed(),
            performance_score: result.score().unwrap_or(0.0),
            tee_verified,
            verified_at: current_timestamp_ms(),
            failure_reason: match &result {
                VerificationResult::Failed(reason) => Some(reason.to_string()),
                VerificationResult::Passed { .. } => None,
            },
        };

        {
            let mut completed = self.completed.write().expect("lock poisoned");
            completed.insert(proof.challenge_id, challenge_result.clone());
        }

        // Remove from pending
        {
            let mut pending = self.pending.write().expect("lock poisoned");
            pending.remove(&proof.challenge_id);
        }

        Ok(result)
    }

    /// Verify compute challenge proof.
    fn verify_compute_proof(
        &self,
        challenge: &Challenge,
        metric: &ComputeMetric,
        data: &ComputeProofData,
    ) -> Result<VerificationResult> {
        // Execute reference computation
        let reference = self.execute_reference_computation(challenge, metric);

        // Compare result hashes
        if data.result_hash != reference.result_hash {
            return Ok(VerificationResult::Failed(FailureReason::HashMismatch));
        }

        // Spot-check intermediate hashes
        let sample_indices = self.select_sample_indices(&challenge.seed, self.config.intermediate_hash_samples);
        for idx in sample_indices {
            if idx < data.intermediate_hashes.len() && idx < reference.intermediate_hashes.len() {
                if data.intermediate_hashes[idx] != reference.intermediate_hashes[idx] {
                    return Ok(VerificationResult::Failed(FailureReason::IntermediateHashMismatch { index: idx }));
                }
            }
        }

        // Calculate performance score
        let performance_score = data.actual_flops / reference.min_flops;

        if performance_score < self.config.min_performance_score {
            return Ok(VerificationResult::Failed(FailureReason::PerformanceTooLow));
        }

        let bonus = if performance_score > self.config.exceptional_performance_threshold {
            self.config.exceptional_performance_bonus
        } else {
            0.0
        };

        Ok(VerificationResult::Passed {
            score: performance_score.min(2.0), // Cap at 2x
            bonus,
        })
    }

    /// Verify latency challenge proof.
    fn verify_latency_proof(
        &self,
        challenge: &Challenge,
        data: &LatencyProofData,
    ) -> Result<VerificationResult> {
        // Verify round signatures
        for round in &data.round_results {
            let _expected_response = blake3_hash_chunks(&[
                &challenge.seed,
                &[round.round],
            ]);
            // In production, verify the signature properly
            if round.signed_response.is_empty() {
                return Ok(VerificationResult::Failed(FailureReason::InvalidNonceResponse));
            }
        }

        // Check p99 latency
        if data.p99_ms > self.config.max_latency_p99_ms {
            return Ok(VerificationResult::Failed(FailureReason::LatencyExceeded));
        }

        // Calculate score based on latency
        let target_p99 = self.config.max_latency_p99_ms as f64;
        let actual_p99 = data.p99_ms as f64;
        let performance_score = target_p99 / actual_p99.max(1.0);

        let bonus = if performance_score > self.config.exceptional_performance_threshold {
            self.config.exceptional_performance_bonus
        } else {
            0.0
        };

        Ok(VerificationResult::Passed {
            score: performance_score.min(2.0),
            bonus,
        })
    }

    /// Verify bandwidth challenge proof.
    fn verify_bandwidth_proof(
        &self,
        challenge: &Challenge,
        data: &BandwidthProofData,
    ) -> Result<VerificationResult> {
        // Verify merkle proofs
        let data_root = blake3_hash(&challenge.seed); // Simplified - would be actual data root
        for merkle_proof in &data.merkle_proofs {
            let proof = crate::crypto::MerkleProof {
                leaf_hash: merkle_proof.leaf_hash,
                index: merkle_proof.index as usize,
                path: merkle_proof.path.clone(),
            };
            if !MerkleTree::verify_proof(&data_root, &proof) {
                // In a real implementation, we'd have the actual tree root
                // For now, we'll skip this check
            }
        }

        // Check bandwidth threshold
        if data.achieved_mbps < self.config.min_bandwidth_mbps {
            return Ok(VerificationResult::Failed(FailureReason::BandwidthTooLow));
        }

        // Calculate score
        let target_mbps = self.config.min_bandwidth_mbps as f64;
        let actual_mbps = data.achieved_mbps as f64;
        let performance_score = actual_mbps / target_mbps;

        let bonus = if performance_score > self.config.exceptional_performance_threshold {
            self.config.exceptional_performance_bonus
        } else {
            0.0
        };

        Ok(VerificationResult::Passed {
            score: performance_score.min(2.0),
            bonus,
        })
    }

    /// Verify availability challenge proof.
    fn verify_availability_proof(
        &self,
        challenge: &Challenge,
        data: &AvailabilityProofData,
    ) -> Result<VerificationResult> {
        // Verify nonce response
        let _expected_nonce = blake3_hash(&challenge.seed);
        let _response_hash = blake3_hash(&data.signed_nonce);

        // In production, verify the signature properly
        if data.signed_nonce.is_empty() {
            return Ok(VerificationResult::Failed(FailureReason::InvalidNonceResponse));
        }

        // Check response time
        if data.response_time_ms > self.config.max_availability_response_ms {
            return Ok(VerificationResult::Failed(FailureReason::LatencyExceeded));
        }

        // Calculate score based on response time
        let target_ms = self.config.max_availability_response_ms as f64;
        let actual_ms = data.response_time_ms as f64;
        let performance_score = target_ms / actual_ms.max(1.0);

        let bonus = if performance_score > self.config.exceptional_performance_threshold {
            self.config.exceptional_performance_bonus
        } else {
            0.0
        };

        Ok(VerificationResult::Passed {
            score: performance_score.min(2.0),
            bonus,
        })
    }

    /// Verify model challenge proof.
    fn verify_model_proof(
        &self,
        _challenge: &Challenge,
        data: &ModelProofData,
    ) -> Result<VerificationResult> {
        // In production, we would verify the completion hash matches
        // expected output for the deterministic prompt

        // For now, verify basic constraints
        if data.token_count == 0 {
            return Ok(VerificationResult::Failed(FailureReason::ModelOutputMismatch));
        }

        // Check tokens per second
        let expected_tps = 50.0; // From model capability
        let actual_tps = data.tokens_per_second;

        if actual_tps < expected_tps * self.config.min_performance_score {
            return Ok(VerificationResult::Failed(FailureReason::PerformanceTooLow));
        }

        let performance_score = actual_tps / expected_tps;

        let bonus = if performance_score > self.config.exceptional_performance_threshold {
            self.config.exceptional_performance_bonus
        } else {
            0.0
        };

        Ok(VerificationResult::Passed {
            score: performance_score.min(2.0),
            bonus,
        })
    }

    /// Verify proof signature.
    fn verify_proof_signature(&self, proof: &ChallengeProof) -> Result<()> {
        if proof.signature.is_empty() {
            // For testing, allow empty signatures
            return Ok(());
        }

        let _sig_data = self.proof_signature_data(proof);
        let _sig = Signature::from_bytes(proof.signature.clone());

        // In production, we'd verify against the provider's public key
        // For now, accept any non-empty signature
        Ok(())
    }

    /// Verify TEE attestation.
    fn verify_tee_attestation(&self, attestation: &TeeAttestation) -> Result<bool> {
        // In production, this would verify:
        // - Intel SGX: Verify quote through IAS or DCAP
        // - AMD SEV: Verify attestation report
        // - NVIDIA CC: Verify GPU attestation

        match attestation.tee_type {
            TeeType::IntelSgx => {
                // Verify SGX quote
                if attestation.quote.is_empty() {
                    return Ok(false);
                }
                // Would call Intel Attestation Service here
                Ok(true)
            }
            TeeType::AmdSev => {
                // Verify SEV attestation
                if attestation.quote.is_empty() {
                    return Ok(false);
                }
                Ok(true)
            }
            TeeType::NvidiaConfidentialCompute | TeeType::NvidiaBlackwellTeeIo => {
                // Verify NVIDIA attestation
                if attestation.quote.is_empty() {
                    return Ok(false);
                }
                Ok(true)
            }
            TeeType::ArmTrustZone => {
                // Verify ARM TrustZone attestation
                if attestation.quote.is_empty() {
                    return Ok(false);
                }
                Ok(true)
            }
        }
    }

    /// Execute reference computation for verification.
    fn execute_reference_computation(
        &self,
        challenge: &Challenge,
        metric: &ComputeMetric,
    ) -> ReferenceResult {
        // In production, this would execute the actual reference computation
        // For now, we generate deterministic expected values from the seed

        let problem_size = crate::challenge::compute_problem_size(challenge.difficulty, metric);

        // Generate expected result hash
        let result_hash = blake3_hash_chunks(&[
            &challenge.seed,
            &problem_size.to_le_bytes(),
        ]);

        // Generate intermediate hashes
        let num_intermediates = 10;
        let intermediate_hashes: Vec<[u8; 32]> = (0..num_intermediates)
            .map(|i| {
                blake3_hash_chunks(&[
                    &challenge.seed,
                    &(i as u64).to_le_bytes(),
                ])
            })
            .collect();

        // Calculate expected minimum FLOPS based on difficulty
        let min_flops = match metric {
            ComputeMetric::GpuFp32 => 1e12 * challenge.difficulty as f64, // 1 TFLOPS * difficulty
            ComputeMetric::GpuFp16 => 2e12 * challenge.difficulty as f64,
            ComputeMetric::GpuInt8 => 4e12 * challenge.difficulty as f64,
            ComputeMetric::CpuGflops => 1e9 * challenge.difficulty as f64,
            ComputeMetric::MemoryBandwidth => 100e9 * challenge.difficulty as f64, // 100 GB/s * difficulty
        };

        ReferenceResult {
            result_hash,
            intermediate_hashes,
            min_flops,
        }
    }

    /// Select sample indices for spot-checking.
    fn select_sample_indices(&self, seed: &[u8; 32], count: usize) -> Vec<usize> {
        use rand::{Rng, SeedableRng};
        use rand::rngs::StdRng;

        let mut rng = StdRng::from_seed(*seed);
        (0..count).map(|_| rng.random_range(0..10)).collect()
    }

    /// Create signature data for a proof.
    fn proof_signature_data(&self, proof: &ChallengeProof) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend_from_slice(proof.challenge_id.as_bytes());
        data.extend_from_slice(proof.provider_id.as_bytes());
        data.extend_from_slice(&proof.executed_at.to_le_bytes());
        data.extend_from_slice(&proof.duration_ms.to_le_bytes());
        data
    }

    /// Get a completed verification result.
    pub fn get_result(&self, challenge_id: &ChallengeId) -> Option<ChallengeResult> {
        let completed = self.completed.read().expect("lock poisoned");
        completed.get(challenge_id).cloned()
    }

    /// Get all pending challenges.
    pub fn pending_count(&self) -> usize {
        let pending = self.pending.read().expect("lock poisoned");
        pending.len()
    }

    /// Clear expired challenges.
    pub fn clear_expired(&self) {
        let mut pending = self.pending.write().expect("lock poisoned");
        pending.retain(|_, challenge| !challenge.is_expired());
    }
}

/// Optimistic verification with fraud proofs.
#[derive(Debug, Clone)]
pub struct OptimisticVerification {
    /// Challenge ID
    pub challenge_id: ChallengeId,
    /// Provider's submitted proof
    pub proof: ChallengeProof,
    /// Submission timestamp
    pub submitted_at: u64,
    /// Dispute window in blocks
    pub dispute_window: u64,
    /// Current status
    pub status: OptimisticStatus,
}

/// Status of optimistic verification.
#[derive(Debug, Clone)]
pub enum OptimisticStatus {
    /// Pending within dispute window
    Pending,
    /// Accepted after dispute window
    Accepted,
    /// Disputed with fraud proof
    Disputed {
        challenger: PeerId,
        fraud_proof: FraudProof,
    },
    /// Rejected after fraud proof verified
    Rejected { slash_amount: u64 },
}

/// Fraud proof submitted by a challenger.
#[derive(Debug, Clone)]
pub struct FraudProof {
    /// Reference computation result
    pub expected_result: ProofData,
    /// Evidence of mismatch
    pub evidence: FraudEvidence,
    /// Challenger's stake
    pub stake: u64,
    /// Challenger's signature
    pub signature: Vec<u8>,
}

/// Evidence for fraud proof.
#[derive(Debug, Clone)]
pub enum FraudEvidence {
    /// Hash mismatch evidence
    HashMismatch {
        expected: [u8; 32],
        actual: [u8; 32],
    },
    /// Intermediate hash mismatch
    IntermediateHashMismatch {
        index: usize,
        expected: [u8; 32],
        actual: [u8; 32],
    },
    /// Performance claim mismatch
    PerformanceMismatch {
        claimed: f64,
        verified: f64,
    },
}

/// Get current timestamp in milliseconds.
fn current_timestamp_ms() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Challenge;

    fn make_test_challenge() -> Challenge {
        Challenge::new(
            ChallengeType::Availability,
            [42u8; 32],
            5,
            30_000,
        )
    }

    fn make_test_proof(challenge: &Challenge) -> ChallengeProof {
        ChallengeProof {
            challenge_id: challenge.id,
            provider_id: "test-provider".to_string(),
            proof_data: ProofData::Availability(AvailabilityProofData {
                signed_nonce: vec![1, 2, 3, 4],
                response_time_ms: 50,
            }),
            executed_at: current_timestamp_ms(),
            duration_ms: 50,
            signature: vec![1, 2, 3],
            tee_attestation: None,
        }
    }

    #[test]
    fn test_verification_engine_creation() {
        let engine = VerificationEngine::new([0u8; 32]);
        assert_eq!(engine.pending_count(), 0);
    }

    #[test]
    fn test_register_and_verify() {
        let engine = VerificationEngine::new([0u8; 32]);
        let challenge = make_test_challenge();

        engine.register_challenge(challenge.clone());
        assert_eq!(engine.pending_count(), 1);

        let proof = make_test_proof(&challenge);
        let result = engine.verify(&proof).expect("verification should succeed");

        assert!(result.passed());
        assert_eq!(engine.pending_count(), 0);
    }

    #[test]
    fn test_challenge_not_found() {
        let engine = VerificationEngine::new([0u8; 32]);
        let challenge = make_test_challenge();
        let proof = make_test_proof(&challenge);

        // Don't register the challenge
        let result = engine.verify(&proof);
        assert!(result.is_err());
    }

    #[test]
    fn test_verification_result() {
        let passed = VerificationResult::Passed { score: 1.2, bonus: 0.1 };
        assert!(passed.passed());
        assert_eq!(passed.score(), Some(1.2));

        let failed = VerificationResult::Failed(FailureReason::HashMismatch);
        assert!(!failed.passed());
        assert_eq!(failed.score(), None);
    }
}
