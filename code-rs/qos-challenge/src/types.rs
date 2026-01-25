//! Core types for the QoS Challenge System.
//!
//! This module defines all the fundamental data structures used throughout
//! the challenge system, including challenges, proofs, scores, and provider state.

use std::time::Duration;

use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Unique identifier for a challenge.
pub type ChallengeId = Uuid;

/// Unique identifier for a provider/peer.
pub type PeerId = String;

/// Compute performance metric being tested.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ComputeMetric {
    /// GPU FP32 TFLOPS
    GpuFp32,
    /// GPU FP16 TFLOPS
    GpuFp16,
    /// GPU INT8 TOPS
    GpuInt8,
    /// CPU GFLOPS
    CpuGflops,
    /// Memory bandwidth GB/s
    MemoryBandwidth,
}

/// Model capability identifier.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModelCapability {
    /// Model identifier (e.g., "llama-3-70b")
    pub model_id: String,
    /// Required VRAM in GB
    pub required_vram_gb: u32,
    /// Expected tokens per second (scaled by 100 for integer storage)
    pub expected_tps_scaled: u32,
}

impl ModelCapability {
    /// Create a new model capability with the given tokens per second.
    pub fn new(model_id: String, required_vram_gb: u32, expected_tps: f64) -> Self {
        Self {
            model_id,
            required_vram_gb,
            expected_tps_scaled: (expected_tps * 100.0) as u32,
        }
    }

    /// Get expected tokens per second as f64.
    pub fn expected_tps(&self) -> f64 {
        self.expected_tps_scaled as f64 / 100.0
    }
}

impl Eq for ModelCapability {}

impl std::hash::Hash for ModelCapability {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.model_id.hash(state);
        self.required_vram_gb.hash(state);
        self.expected_tps_scaled.hash(state);
    }
}

/// Direction for bandwidth tests.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TransferDirection {
    Upload,
    Download,
    Bidirectional,
}

/// Type of challenge issued to a provider.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ChallengeType {
    /// Compute performance challenge
    Compute(ComputeMetric),
    /// Network latency challenge
    Latency,
    /// Bandwidth throughput challenge
    Bandwidth(TransferDirection),
    /// Availability/uptime challenge
    Availability,
    /// AI model capability challenge
    Model(ModelCapability),
}

/// A challenge issued to a provider for QoS verification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Challenge {
    /// Unique challenge identifier
    pub id: ChallengeId,
    /// Type of challenge
    pub challenge_type: ChallengeType,
    /// Random seed for deterministic computation
    pub seed: [u8; 32],
    /// Difficulty level (1-10, determines workload size)
    pub difficulty: u8,
    /// Maximum time allowed in milliseconds
    pub deadline_ms: u64,
    /// Timestamp when challenge was issued (Unix ms)
    pub issued_at: u64,
    /// Challenger's signature (ML-DSA or ECDSA)
    pub signature: Vec<u8>,
}

impl Challenge {
    /// Create a new challenge with the given parameters.
    pub fn new(
        challenge_type: ChallengeType,
        seed: [u8; 32],
        difficulty: u8,
        deadline_ms: u64,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            challenge_type,
            seed,
            difficulty: difficulty.clamp(1, 10),
            deadline_ms,
            issued_at: current_timestamp_ms(),
            signature: Vec::new(),
        }
    }

    /// Check if the challenge has expired.
    pub fn is_expired(&self) -> bool {
        let now = current_timestamp_ms();
        now > self.issued_at + self.deadline_ms
    }

    /// Get remaining time until deadline.
    pub fn time_remaining(&self) -> Option<Duration> {
        let now = current_timestamp_ms();
        let deadline = self.issued_at + self.deadline_ms;
        if now >= deadline {
            None
        } else {
            Some(Duration::from_millis(deadline - now))
        }
    }
}

/// Proof data for compute challenges.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComputeProofData {
    /// Hash of the final computation result
    pub result_hash: [u8; 32],
    /// Intermediate hashes for spot-checking
    pub intermediate_hashes: Vec<[u8; 32]>,
    /// Actual FLOPS achieved
    pub actual_flops: f64,
}

/// Single round result for latency challenges.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LatencyRound {
    /// Round number
    pub round: u8,
    /// Round-trip time in milliseconds
    pub rtt_ms: u32,
    /// Signed nonce response
    pub signed_response: Vec<u8>,
}

/// Proof data for latency challenges.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LatencyProofData {
    /// Results from each round
    pub round_results: Vec<LatencyRound>,
    /// 50th percentile latency
    pub p50_ms: u32,
    /// 95th percentile latency
    pub p95_ms: u32,
    /// 99th percentile latency
    pub p99_ms: u32,
}

/// Merkle proof for bandwidth verification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleProof {
    /// Leaf index
    pub index: u64,
    /// Proof path (sibling hashes)
    pub path: Vec<[u8; 32]>,
    /// Leaf data hash
    pub leaf_hash: [u8; 32],
}

/// Proof data for bandwidth challenges.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BandwidthProofData {
    /// Merkle proofs for transferred data
    pub merkle_proofs: Vec<MerkleProof>,
    /// Achieved throughput in Mbps
    pub achieved_mbps: u32,
    /// Total bytes transferred
    pub bytes_transferred: u64,
}

/// Proof data for availability challenges.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AvailabilityProofData {
    /// Signed nonce response
    pub signed_nonce: Vec<u8>,
    /// Response time in milliseconds
    pub response_time_ms: u32,
}

/// Proof data for model capability challenges.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelProofData {
    /// Hash of model completion
    pub completion_hash: [u8; 32],
    /// Number of tokens generated
    pub token_count: u32,
    /// Tokens per second achieved
    pub tokens_per_second: f64,
}

/// Proof data variants for different challenge types.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProofData {
    Compute(ComputeProofData),
    Latency(LatencyProofData),
    Bandwidth(BandwidthProofData),
    Availability(AvailabilityProofData),
    Model(ModelProofData),
}

/// TEE attestation information.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TeeAttestation {
    /// TEE type
    pub tee_type: TeeType,
    /// Attestation quote/report
    pub quote: Vec<u8>,
    /// Expected MRENCLAVE/measurement
    pub measurement: [u8; 32],
}

/// Supported TEE types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TeeType {
    IntelSgx,
    AmdSev,
    ArmTrustZone,
    NvidiaConfidentialCompute,
    NvidiaBlackwellTeeIo,
}

/// A proof submitted by a provider in response to a challenge.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChallengeProof {
    /// Challenge being responded to
    pub challenge_id: ChallengeId,
    /// Provider submitting the proof
    pub provider_id: PeerId,
    /// Proof data (varies by challenge type)
    pub proof_data: ProofData,
    /// Timestamp when execution completed (Unix ms)
    pub executed_at: u64,
    /// Execution duration in milliseconds
    pub duration_ms: u64,
    /// Provider's signature over the proof
    pub signature: Vec<u8>,
    /// Optional TEE attestation
    pub tee_attestation: Option<TeeAttestation>,
}

/// Response to a challenge (simplified for RPC).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChallengeResponse {
    /// Challenge ID being responded to
    pub challenge_id: ChallengeId,
    /// Response payload (serialized proof data)
    pub response: Vec<u8>,
    /// Measured latency in milliseconds
    pub latency_ms: u64,
}

/// Component scores for QoS evaluation.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct QoSComponents {
    /// Compute performance vs claimed (0-1000)
    pub compute_score: u32,
    /// Latency vs SLA (0-1000)
    pub latency_score: u32,
    /// Bandwidth vs claimed (0-1000)
    pub bandwidth_score: u32,
    /// Uptime percentage * 10 (0-1000)
    pub availability_score: u32,
    /// Consistency of performance (0-1000)
    pub consistency_score: u32,
}

/// Weights for QoS score calculation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScoreWeights {
    pub compute: f64,
    pub latency: f64,
    pub bandwidth: f64,
    pub availability: f64,
    pub consistency: f64,
}

impl Default for ScoreWeights {
    fn default() -> Self {
        Self {
            compute: 0.30,
            latency: 0.20,
            bandwidth: 0.15,
            availability: 0.25,
            consistency: 0.10,
        }
    }
}

impl ScoreWeights {
    /// Sum of all weights.
    pub fn total(&self) -> f64 {
        self.compute + self.latency + self.bandwidth + self.availability + self.consistency
    }
}

/// Composite QoS score for a provider.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct QoSScore {
    /// Overall composite score (0-1000)
    pub composite: u32,
    /// Individual component scores
    pub components: QoSComponents,
    /// Score confidence (0.0-1.0, based on sample size)
    pub confidence: f64,
    /// Last updated timestamp (Unix ms)
    pub updated_at: u64,
    /// Historical trend (positive = improving)
    pub trend: f64,
}

impl QoSScore {
    /// Calculate composite score from components using weighted geometric mean.
    pub fn calculate(components: &QoSComponents, weights: &ScoreWeights) -> Self {
        let total_weight = weights.total();
        if total_weight == 0.0 {
            return Self::default();
        }

        // Use weighted geometric mean
        let log_sum = (components.compute_score as f64).max(1.0).ln() * weights.compute
            + (components.latency_score as f64).max(1.0).ln() * weights.latency
            + (components.bandwidth_score as f64).max(1.0).ln() * weights.bandwidth
            + (components.availability_score as f64).max(1.0).ln() * weights.availability
            + (components.consistency_score as f64).max(1.0).ln() * weights.consistency;

        let composite = (log_sum / total_weight).exp() as u32;

        Self {
            composite: composite.min(1000),
            components: components.clone(),
            confidence: 0.0,
            updated_at: current_timestamp_ms(),
            trend: 0.0,
        }
    }
}

/// Reason for provider ban.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BanReason {
    ExcessiveFailures,
    FraudDetected,
    TeeAttestationRevoked,
    ManualReview,
}

/// Provider status.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ProviderStatus {
    Active,
    TemporarilyBanned { until: u64 },
    PermanentlyBanned { reason: BanReason },
    Suspended { reason: String },
}

impl Default for ProviderStatus {
    fn default() -> Self {
        Self::Active
    }
}

/// State of a compute provider.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProviderState {
    /// Provider identifier
    pub id: PeerId,
    /// Staked amount (in smallest token unit)
    pub stake: u64,
    /// Current reputation score
    pub reputation: f64,
    /// QoS score
    pub qos_score: QoSScore,
    /// Total challenges passed
    pub challenges_passed: u64,
    /// Total challenges failed
    pub challenges_failed: u64,
    /// Consecutive failures
    pub consecutive_failures: u32,
    /// Current challenge streak (consecutive passes)
    pub challenge_streak: u32,
    /// Total challenges received
    pub total_challenges: u32,
    /// Total rewards earned
    pub total_rewards: u64,
    /// Total stake slashed
    pub total_slashed: u64,
    /// Last active timestamp (Unix ms)
    pub last_active: u64,
    /// Current status
    pub status: ProviderStatus,
}

impl ProviderState {
    /// Create a new provider state with default values.
    pub fn new(id: PeerId, stake: u64) -> Self {
        Self {
            id,
            stake,
            reputation: 100.0,
            qos_score: QoSScore::default(),
            challenges_passed: 0,
            challenges_failed: 0,
            consecutive_failures: 0,
            challenge_streak: 0,
            total_challenges: 0,
            total_rewards: 0,
            total_slashed: 0,
            last_active: current_timestamp_ms(),
            status: ProviderStatus::Active,
        }
    }

    /// Check if provider is active.
    pub fn is_active(&self) -> bool {
        matches!(self.status, ProviderStatus::Active)
    }

    /// Get challenge pass rate.
    pub fn pass_rate(&self) -> f64 {
        let total = self.challenges_passed + self.challenges_failed;
        if total == 0 {
            return 0.0;
        }
        self.challenges_passed as f64 / total as f64
    }
}

/// Result of a challenge verification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChallengeResult {
    /// Challenge ID
    pub challenge_id: ChallengeId,
    /// Provider ID
    pub provider_id: PeerId,
    /// Whether the challenge passed
    pub passed: bool,
    /// Performance score (1.0 = met expectations, >1.0 = exceeded)
    pub performance_score: f64,
    /// Whether TEE attestation was verified
    pub tee_verified: bool,
    /// Timestamp of verification
    pub verified_at: u64,
    /// Optional failure reason
    pub failure_reason: Option<String>,
}

/// Penalty applied to a provider.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Penalty {
    /// Warning only (grace period)
    Warning,
    /// Slash stake and reduce reputation
    Slash {
        amount: u64,
        reputation_penalty: f64,
    },
    /// Temporary ban with slash
    TemporaryBan {
        slash_amount: u64,
        reputation_penalty: f64,
        duration: u64,
    },
    /// Permanent ban with full slash
    PermanentBan { slash_amount: u64, reason: BanReason },
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

    #[test]
    fn test_challenge_expiry() {
        let challenge = Challenge::new(
            ChallengeType::Availability,
            [0u8; 32],
            5,
            1000, // 1 second deadline
        );

        assert!(!challenge.is_expired());
        assert!(challenge.time_remaining().is_some());
    }

    #[test]
    fn test_qos_score_calculation() {
        let components = QoSComponents {
            compute_score: 800,
            latency_score: 900,
            bandwidth_score: 700,
            availability_score: 950,
            consistency_score: 850,
        };

        let weights = ScoreWeights::default();
        let score = QoSScore::calculate(&components, &weights);

        // Geometric mean should be between min and max
        assert!(score.composite >= 700);
        assert!(score.composite <= 950);
    }

    #[test]
    fn test_provider_state() {
        let provider = ProviderState::new("test-provider".to_string(), 1_000_000);

        assert!(provider.is_active());
        assert_eq!(provider.pass_rate(), 0.0);
    }

    #[test]
    fn test_score_weights_total() {
        let weights = ScoreWeights::default();
        let total = weights.total();
        // Should sum to approximately 1.0
        assert!((total - 1.0).abs() < 0.001);
    }
}
