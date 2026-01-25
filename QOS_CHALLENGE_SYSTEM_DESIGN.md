# QoS Challenge System Design - Platform Phase 1

## Executive Summary

This document specifies the Quality of Service (QoS) Challenge System for Hanzo Network's decentralized compute marketplace. The system ensures compute providers maintain advertised service levels through cryptographic challenges, verifiable proofs, and economic incentives.

## Table of Contents

1. [Problem Statement](#problem-statement)
2. [Design Goals](#design-goals)
3. [Architecture Overview](#architecture-overview)
4. [QoS Challenge Protocol](#qos-challenge-protocol)
5. [Verification and Scoring System](#verification-and-scoring-system)
6. [Penalty/Reward Mechanisms](#penaltyreward-mechanisms)
7. [Security Analysis](#security-analysis)
8. [Implementation Plan](#implementation-plan)

---

## Problem Statement

In a decentralized compute marketplace, providers may:
- Claim capabilities they do not possess (GPU TFLOPS, VRAM, etc.)
- Provide inconsistent service quality over time
- Game the reputation system through selective performance
- Fail to maintain advertised SLAs during peak demand

The QoS Challenge System addresses these issues by continuously verifying provider capabilities and performance through cryptographic proofs.

---

## Design Goals

### Primary Goals
1. **Verifiable Performance**: Prove providers can deliver claimed compute capabilities
2. **Continuous Monitoring**: Ongoing verification, not just registration-time
3. **Sybil Resistance**: Prevent gaming through multiple identities
4. **Economic Alignment**: Honest behavior more profitable than cheating
5. **Low Overhead**: Minimal impact on actual compute workloads

### Non-Goals
- Real-time latency guarantees (handled by SLA monitoring)
- Content verification (handled by result verifier)
- Network topology optimization (handled by routing layer)

---

## Architecture Overview

```
                           QoS Challenge System Architecture
+================================================================================+
|                                                                                |
|  +------------------+     +-------------------+     +--------------------+     |
|  |   Challenge      |     |    Verification   |     |     Scoring        |     |
|  |   Generator      |---->|    Engine         |---->|     System         |     |
|  +------------------+     +-------------------+     +--------------------+     |
|         |                        |                         |                   |
|         v                        v                         v                   |
|  +------------------+     +-------------------+     +--------------------+     |
|  |  Challenge Types |     |   Proof Types     |     |   Score Factors    |     |
|  |  - Compute       |     |   - Hash Proof    |     |   - Latency        |     |
|  |  - Latency       |     |   - TEE Attest    |     |   - Throughput     |     |
|  |  - Bandwidth     |     |   - Merkle Proof  |     |   - Availability   |     |
|  |  - Availability  |     |   - ZK Proof      |     |   - Consistency    |     |
|  +------------------+     +-------------------+     +--------------------+     |
|                                    |                                           |
|                                    v                                           |
|  +------------------------------------------------------------------+         |
|  |                      On-Chain Settlement                          |         |
|  |  +-------------------+  +-------------------+  +-----------------+ |         |
|  |  | Stake Management  |  | Reward/Slash     |  | Reputation      | |         |
|  |  | (hanzo-mining)    |  | Distribution     |  | Registry        | |         |
|  |  +-------------------+  +-------------------+  +-----------------+ |         |
|  +------------------------------------------------------------------+         |
|                                                                                |
+================================================================================+
```

### Integration with Existing Components

The QoS Challenge System integrates with:

| Component | Integration Point | Purpose |
|-----------|------------------|---------|
| `hanzo-compute` | `ComputeSwarm`, `Peer` | Challenge issuance, result collection |
| `hanzo-mining` | `MiningManager`, `Consensus` | On-chain settlement, stake management |
| `hanzo-libp2p` | P2P messaging | Challenge delivery, proof collection |
| `hanzo-pqc` | Cryptographic primitives | Quantum-safe signatures for proofs |

---

## QoS Challenge Protocol

### Challenge Types

#### 1. Compute Challenge (CC)
Verifies actual compute capability (TFLOPS/GFLOPS)

```rust
pub struct ComputeChallenge {
    /// Unique challenge identifier
    pub id: ChallengeId,
    /// Challenge type marker
    pub challenge_type: ChallengeType,
    /// Random seed for deterministic computation
    pub seed: [u8; 32],
    /// Difficulty level (determines workload size)
    pub difficulty: u8,
    /// Expected compute metric being tested
    pub metric: ComputeMetric,
    /// Maximum time allowed (milliseconds)
    pub deadline_ms: u64,
    /// Challenger's signature
    pub signature: Vec<u8>,
    /// Timestamp of challenge issuance
    pub issued_at: u64,
}

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
pub enum ChallengeType {
    Compute(ComputeMetric),
    Latency,
    Bandwidth,
    Availability,
    Model(ModelCapability),
}
```

**Verification Method**: Provider executes a deterministic computation kernel (e.g., matrix multiplication with specific seed). Result hash and execution time are submitted.

**Difficulty Scaling**:
```python
# Difficulty determines problem size
def compute_problem_size(difficulty: int, metric: str) -> int:
    """
    Returns dimensions for matrix operations
    difficulty 1-10 maps to increasingly large workloads
    """
    base_sizes = {
        'gpu_fp32': 1024,  # Starting matrix size
        'gpu_fp16': 2048,
        'cpu_gflops': 512,
        'memory_bandwidth': 1073741824,  # 1GB base
    }
    return base_sizes[metric] * (2 ** (difficulty - 1))
```

#### 2. Latency Challenge (LC)
Verifies network latency and response time

```rust
pub struct LatencyChallenge {
    pub id: ChallengeId,
    /// Nonce to prevent replay
    pub nonce: [u8; 16],
    /// Target percentile (p50, p95, p99)
    pub target_percentile: u8,
    /// Expected max latency in milliseconds
    pub max_latency_ms: u32,
    /// Number of round trips required
    pub rounds: u8,
    pub signature: Vec<u8>,
}
```

**Verification Method**: Echo-response pattern with cryptographic binding. Provider must respond within deadline with signed nonce.

#### 3. Bandwidth Challenge (BC)
Verifies data transfer capabilities

```rust
pub struct BandwidthChallenge {
    pub id: ChallengeId,
    /// Random data to transfer (merkle root)
    pub data_root: [u8; 32],
    /// Size of transfer in bytes
    pub transfer_size: u64,
    /// Direction of test
    pub direction: TransferDirection,
    /// Expected minimum throughput Mbps
    pub min_throughput_mbps: u32,
    pub signature: Vec<u8>,
}

pub enum TransferDirection {
    Upload,
    Download,
    Bidirectional,
}
```

#### 4. Availability Challenge (AC)
Verifies provider is online and ready to serve

```rust
pub struct AvailabilityChallenge {
    pub id: ChallengeId,
    /// Random nonce
    pub nonce: [u8; 32],
    /// Challenge window (must respond within)
    pub window_ms: u32,
    /// Current epoch
    pub epoch: u64,
    pub signature: Vec<u8>,
}
```

**Verification Method**: Heartbeat-style challenges issued at random intervals. Missing responses count against availability score.

#### 5. Model Capability Challenge (MC)
Verifies provider can run specific AI models

```rust
pub struct ModelChallenge {
    pub id: ChallengeId,
    /// Model identifier to test
    pub model: String,
    /// Deterministic prompt seed
    pub prompt_seed: [u8; 32],
    /// Expected completion hash prefix (first N bytes)
    pub expected_prefix_len: u8,
    /// Max generation time
    pub deadline_ms: u64,
    pub signature: Vec<u8>,
}
```

### Challenge Issuance Protocol

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  Challenge  │     │   Provider  │     │   Verifier  │     │  On-Chain   │
│  Committee  │     │    Node     │     │   Network   │     │  Contract   │
└──────┬──────┘     └──────┬──────┘     └──────┬──────┘     └──────┬──────┘
       │                   │                   │                   │
       │  1. SELECT RANDOM PROVIDERS           │                   │
       ├───────────────────────────────────────┤                   │
       │                   │                   │                   │
       │  2. ISSUE CHALLENGE                   │                   │
       ├──────────────────>│                   │                   │
       │                   │                   │                   │
       │  3. EXECUTE CHALLENGE                 │                   │
       │                   ├───┐               │                   │
       │                   │   │               │                   │
       │                   │<──┘               │                   │
       │                   │                   │                   │
       │  4. SUBMIT PROOF                      │                   │
       │<──────────────────┤                   │                   │
       │                   │                   │                   │
       │  5. FORWARD FOR VERIFICATION          │                   │
       ├──────────────────────────────────────>│                   │
       │                   │                   │                   │
       │  6. VERIFY PROOF                      │                   │
       │                   │                   ├───┐               │
       │                   │                   │   │               │
       │                   │                   │<──┘               │
       │                   │                   │                   │
       │  7. SUBMIT RESULT (if passing)        │                   │
       │                   │                   ├──────────────────>│
       │                   │                   │                   │
       │  8. UPDATE SCORE & DISTRIBUTE REWARDS │                   │
       │                   │<─────────────────────────────────────│
       │                   │                   │                   │
```

### Challenge Frequency

| Provider Tier | Challenge Frequency | Challenge Mix |
|---------------|--------------------|--------------|
| New (< 10 jobs) | Every 10 minutes | 40% Compute, 30% Avail, 20% Latency, 10% Model |
| Established (10-100) | Every 30 minutes | 30% Compute, 30% Avail, 25% Latency, 15% Model |
| Trusted (100+) | Every 60 minutes | 25% Compute, 35% Avail, 20% Latency, 20% Model |
| Elite (1000+, TEE) | Every 2 hours | 20% Compute, 40% Avail, 20% Latency, 20% Model |

### Proof Structure

```rust
pub struct ChallengeProof {
    /// Challenge being responded to
    pub challenge_id: ChallengeId,
    /// Provider submitting proof
    pub provider_id: PeerId,
    /// Proof data (varies by challenge type)
    pub proof_data: ProofData,
    /// Execution timestamp
    pub executed_at: u64,
    /// Execution duration in milliseconds
    pub duration_ms: u64,
    /// ML-DSA signature over proof
    pub signature: Vec<u8>,
    /// Optional TEE attestation
    pub tee_attestation: Option<TeeAttestation>,
}

pub enum ProofData {
    /// Compute proof: result hash + intermediate hashes
    Compute {
        result_hash: [u8; 32],
        intermediate_hashes: Vec<[u8; 32]>,
        actual_flops: f64,
    },
    /// Latency proof: signed responses per round
    Latency {
        round_results: Vec<LatencyRound>,
        p50_ms: u32,
        p95_ms: u32,
        p99_ms: u32,
    },
    /// Bandwidth proof: merkle proof of transferred data
    Bandwidth {
        merkle_proofs: Vec<MerkleProof>,
        achieved_mbps: u32,
        bytes_transferred: u64,
    },
    /// Availability proof: signed nonce response
    Availability {
        signed_nonce: Vec<u8>,
        response_time_ms: u32,
    },
    /// Model proof: completion hash and token count
    Model {
        completion_hash: [u8; 32],
        token_count: u32,
        tokens_per_second: f64,
    },
}
```

---

## Verification and Scoring System

### Verification Methods

#### 1. Hash Verification
For compute challenges, verifiers execute the same deterministic computation:

```rust
impl ComputeChallengeVerifier {
    pub fn verify(&self, challenge: &ComputeChallenge, proof: &ProofData) -> VerificationResult {
        match proof {
            ProofData::Compute { result_hash, intermediate_hashes, actual_flops } => {
                // Recreate the computation with same seed
                let expected = self.execute_reference(challenge.seed, challenge.difficulty);

                // Compare hashes
                if expected.result_hash != *result_hash {
                    return VerificationResult::Failed(FailureReason::HashMismatch);
                }

                // Verify intermediate hashes (spot check)
                let sample_indices = self.select_sample_indices(challenge.seed, 5);
                for idx in sample_indices {
                    if expected.intermediate_hashes[idx] != intermediate_hashes[idx] {
                        return VerificationResult::Failed(FailureReason::IntermediateHashMismatch);
                    }
                }

                // Verify performance claim
                let expected_min_flops = self.difficulty_to_min_flops(challenge.difficulty);
                let performance_score = actual_flops / expected_min_flops;

                VerificationResult::Passed {
                    score: performance_score.min(1.0),
                    bonus: if performance_score > 1.2 { 0.1 } else { 0.0 },
                }
            }
            _ => VerificationResult::Failed(FailureReason::WrongProofType),
        }
    }
}
```

#### 2. Optimistic Verification with Fraud Proofs
For efficiency, most verifications are optimistic:

```rust
pub struct OptimisticVerification {
    /// Challenge ID
    pub challenge_id: ChallengeId,
    /// Provider's submitted proof
    pub proof: ChallengeProof,
    /// Submission timestamp
    pub submitted_at: u64,
    /// Dispute window (blocks)
    pub dispute_window: u64,
    /// Current status
    pub status: VerificationStatus,
}

pub enum VerificationStatus {
    /// Pending - within dispute window
    Pending,
    /// Accepted - dispute window passed
    Accepted,
    /// Disputed - fraud proof submitted
    Disputed { challenger: PeerId, fraud_proof: FraudProof },
    /// Rejected - fraud proof verified
    Rejected { slash_amount: u64 },
}

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
```

#### 3. TEE Attestation Verification
For TEE-enabled providers:

```rust
pub trait TeeAttestationVerifier {
    /// Verify remote attestation quote
    fn verify_attestation(&self, quote: &[u8], expected_mrenclave: &[u8; 32]) -> bool;

    /// Verify signed report from enclave
    fn verify_report(&self, report: &TeeReport) -> bool;

    /// Get supported TEE types
    fn supported_types(&self) -> Vec<TeeType>;
}

pub enum TeeType {
    IntelSgx,
    AmdSev,
    ArmTrustZone,
    NvidiaConfidentialCompute,
    NvidiaBlackwellTeeIo,
}
```

### QoS Score Calculation

The QoS score is a composite metric:

```rust
pub struct QoSScore {
    /// Overall composite score (0-1000)
    pub composite: u32,
    /// Individual component scores
    pub components: QoSComponents,
    /// Score confidence (based on sample size)
    pub confidence: f64,
    /// Last updated timestamp
    pub updated_at: u64,
    /// Historical trend (positive = improving)
    pub trend: f64,
}

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

impl QoSScore {
    pub fn calculate(components: &QoSComponents, weights: &ScoreWeights) -> Self {
        // Weighted geometric mean for composite score
        let composite = (
            (components.compute_score as f64).powf(weights.compute) *
            (components.latency_score as f64).powf(weights.latency) *
            (components.bandwidth_score as f64).powf(weights.bandwidth) *
            (components.availability_score as f64).powf(weights.availability) *
            (components.consistency_score as f64).powf(weights.consistency)
        ).powf(1.0 / (weights.total())) as u32;

        Self {
            composite,
            components: components.clone(),
            confidence: 0.0,  // Set based on sample count
            updated_at: now_ms(),
            trend: 0.0,  // Set based on historical data
        }
    }
}

pub struct ScoreWeights {
    pub compute: f64,     // Default: 0.30
    pub latency: f64,     // Default: 0.20
    pub bandwidth: f64,   // Default: 0.15
    pub availability: f64, // Default: 0.25
    pub consistency: f64,  // Default: 0.10
}
```

### Consistency Measurement

```rust
impl ConsistencyCalculator {
    /// Calculate consistency score from recent challenge results
    pub fn calculate(&self, results: &[ChallengeResult]) -> u32 {
        if results.len() < 5 {
            return 500; // Default neutral score for insufficient data
        }

        // Group by challenge type
        let by_type = self.group_by_type(results);

        let mut consistency_scores = Vec::new();

        for (challenge_type, type_results) in by_type {
            // Calculate coefficient of variation (CV = std_dev / mean)
            let scores: Vec<f64> = type_results.iter()
                .map(|r| r.performance_score)
                .collect();

            let mean = scores.iter().sum::<f64>() / scores.len() as f64;
            let variance = scores.iter()
                .map(|s| (s - mean).powi(2))
                .sum::<f64>() / scores.len() as f64;
            let std_dev = variance.sqrt();
            let cv = std_dev / mean;

            // Lower CV = higher consistency
            // CV of 0.05 = perfect (1000), CV of 0.5 = poor (0)
            let type_score = ((1.0 - (cv / 0.5).min(1.0)) * 1000.0) as u32;
            consistency_scores.push(type_score);
        }

        // Average across challenge types
        consistency_scores.iter().sum::<u32>() / consistency_scores.len() as u32
    }
}
```

---

## Penalty/Reward Mechanisms

### Economic Model

```
                            Economic Flow Diagram
+============================================================================+
|                                                                            |
|   Provider Stake Pool              Challenge Rewards Pool                  |
|   ┌─────────────────┐              ┌─────────────────┐                     |
|   │                 │              │                 │                     |
|   │   AI Tokens     │              │   AI Tokens     │                     |
|   │   (Locked)      │              │   (Available)   │                     |
|   │                 │              │                 │                     |
|   └────────┬────────┘              └────────┬────────┘                     |
|            │                                │                              |
|            │ ┌──────────────────────────────┘                              |
|            │ │                                                             |
|            v v                                                             |
|   ┌─────────────────────────────────────────────────────────┐             |
|   │                  QoS Challenge Engine                    │             |
|   │                                                         │             |
|   │   Challenge Passed?                                     │             |
|   │        │                                                │             |
|   │        ├── YES ──> Reward Distribution                  │             |
|   │        │              │                                 │             |
|   │        │              ├── Base Reward (from pool)       │             |
|   │        │              ├── Performance Bonus             │             |
|   │        │              └── Reputation Increase           │             |
|   │        │                                                │             |
|   │        └── NO ───> Penalty Application                  │             |
|   │                       │                                 │             |
|   │                       ├── Stake Slash (to pool)         │             |
|   │                       ├── Reputation Decrease           │             |
|   │                       └── Potential Ban                 │             |
|   │                                                         │             |
|   └─────────────────────────────────────────────────────────┘             |
|                                                                            |
+============================================================================+
```

### Reward Structure

```rust
pub struct RewardConfig {
    /// Base reward per successful challenge (AI tokens)
    pub base_reward: u64,
    /// Performance bonus multiplier (max 2x)
    pub max_performance_bonus: f64,
    /// Streak bonus for consecutive passes
    pub streak_bonus_per_pass: f64,
    /// Maximum streak bonus
    pub max_streak_bonus: f64,
    /// TEE attestation bonus
    pub tee_bonus: f64,
    /// Elite provider bonus threshold
    pub elite_threshold_score: u32,
    /// Elite provider bonus
    pub elite_bonus: f64,
}

impl Default for RewardConfig {
    fn default() -> Self {
        Self {
            base_reward: 100_000_000_000_000_000, // 0.1 AI tokens (18 decimals)
            max_performance_bonus: 2.0,
            streak_bonus_per_pass: 0.01,
            max_streak_bonus: 0.5,
            tee_bonus: 0.25,
            elite_threshold_score: 900,
            elite_bonus: 0.15,
        }
    }
}

pub struct RewardCalculator {
    config: RewardConfig,
}

impl RewardCalculator {
    pub fn calculate_reward(
        &self,
        challenge_result: &ChallengeResult,
        provider_state: &ProviderState,
    ) -> u64 {
        if !challenge_result.passed {
            return 0;
        }

        let mut reward = self.config.base_reward as f64;

        // Performance bonus (0-100% additional)
        let performance_bonus = (challenge_result.performance_score - 1.0)
            .max(0.0)
            .min(1.0)
            * self.config.max_performance_bonus;
        reward *= 1.0 + performance_bonus;

        // Streak bonus
        let streak_bonus = (provider_state.challenge_streak as f64
            * self.config.streak_bonus_per_pass)
            .min(self.config.max_streak_bonus);
        reward *= 1.0 + streak_bonus;

        // TEE bonus
        if challenge_result.tee_verified {
            reward *= 1.0 + self.config.tee_bonus;
        }

        // Elite bonus
        if provider_state.qos_score.composite >= self.config.elite_threshold_score {
            reward *= 1.0 + self.config.elite_bonus;
        }

        reward as u64
    }
}
```

### Penalty Structure

```rust
pub struct PenaltyConfig {
    /// Base slash percentage for failed challenge
    pub base_slash_percent: f64,
    /// Increased slash for consecutive failures
    pub consecutive_failure_multiplier: f64,
    /// Maximum slash percentage per challenge
    pub max_slash_percent: f64,
    /// Reputation penalty per failure
    pub reputation_penalty: f64,
    /// Failures before temporary ban
    pub failures_before_temp_ban: u32,
    /// Temp ban duration (seconds)
    pub temp_ban_duration: u64,
    /// Failures before permanent ban
    pub failures_before_perm_ban: u32,
    /// Grace period for new providers
    pub grace_period_challenges: u32,
}

impl Default for PenaltyConfig {
    fn default() -> Self {
        Self {
            base_slash_percent: 0.01,         // 1%
            consecutive_failure_multiplier: 1.5,
            max_slash_percent: 0.10,          // 10% max per challenge
            reputation_penalty: 10.0,
            failures_before_temp_ban: 5,
            temp_ban_duration: 86400,          // 24 hours
            failures_before_perm_ban: 20,
            grace_period_challenges: 3,
        }
    }
}

pub struct PenaltyCalculator {
    config: PenaltyConfig,
}

impl PenaltyCalculator {
    pub fn calculate_penalty(
        &self,
        challenge_result: &ChallengeResult,
        provider_state: &ProviderState,
    ) -> Penalty {
        // Grace period for new providers
        if provider_state.total_challenges < self.config.grace_period_challenges {
            return Penalty::Warning;
        }

        // Calculate slash amount
        let consecutive_failures = provider_state.consecutive_failures;
        let slash_multiplier = self.config.consecutive_failure_multiplier
            .powi(consecutive_failures as i32);
        let slash_percent = (self.config.base_slash_percent * slash_multiplier)
            .min(self.config.max_slash_percent);

        let slash_amount = (provider_state.stake as f64 * slash_percent) as u64;

        // Calculate reputation penalty
        let rep_penalty = self.config.reputation_penalty
            * (1.0 + 0.1 * consecutive_failures as f64);

        // Check for ban conditions
        if consecutive_failures >= self.config.failures_before_perm_ban {
            return Penalty::PermanentBan {
                slash_amount: provider_state.stake, // Slash all
                reason: BanReason::ExcessiveFailures,
            };
        }

        if consecutive_failures >= self.config.failures_before_temp_ban {
            return Penalty::TemporaryBan {
                slash_amount,
                reputation_penalty: rep_penalty,
                duration: self.config.temp_ban_duration,
            };
        }

        Penalty::Slash {
            amount: slash_amount,
            reputation_penalty: rep_penalty,
        }
    }
}

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
    PermanentBan {
        slash_amount: u64,
        reason: BanReason,
    },
}

pub enum BanReason {
    ExcessiveFailures,
    FraudDetected,
    TeeAttestationRevoked,
    ManualReview,
}
```

### Reputation System Integration

```rust
pub struct ReputationManager {
    /// Minimum reputation to participate
    min_reputation: f64,
    /// Starting reputation for new providers
    starting_reputation: f64,
    /// Maximum reputation
    max_reputation: f64,
    /// Decay rate per day of inactivity
    decay_rate: f64,
}

impl ReputationManager {
    pub fn update_reputation(
        &self,
        provider: &mut ProviderState,
        result: &ChallengeResult,
        penalty: &Penalty,
        reward: u64,
    ) {
        match penalty {
            Penalty::Warning => {
                // Slight decrease for warning
                provider.reputation = (provider.reputation - 1.0).max(self.min_reputation);
            }
            Penalty::Slash { reputation_penalty, .. } |
            Penalty::TemporaryBan { reputation_penalty, .. } => {
                provider.reputation = (provider.reputation - reputation_penalty)
                    .max(self.min_reputation);
            }
            Penalty::PermanentBan { .. } => {
                provider.reputation = 0.0;
            }
        }

        // Reward case - increase reputation
        if result.passed {
            // Asymptotic increase toward max
            let increase_factor = (self.max_reputation - provider.reputation) / self.max_reputation;
            let increase = result.performance_score * 2.0 * increase_factor;
            provider.reputation = (provider.reputation + increase).min(self.max_reputation);
        }

        // Track metrics
        provider.total_rewards += reward;
        if result.passed {
            provider.challenges_passed += 1;
            provider.consecutive_failures = 0;
            provider.challenge_streak += 1;
        } else {
            provider.challenges_failed += 1;
            provider.consecutive_failures += 1;
            provider.challenge_streak = 0;
        }
    }

    /// Apply daily reputation decay for inactive providers
    pub fn apply_decay(&self, provider: &mut ProviderState, days_inactive: f64) {
        if days_inactive > 0.0 {
            let decay = self.decay_rate.powf(days_inactive);
            provider.reputation *= decay;
            provider.reputation = provider.reputation.max(self.min_reputation);
        }
    }
}

pub struct ProviderState {
    pub id: PeerId,
    pub stake: u64,
    pub reputation: f64,
    pub qos_score: QoSScore,
    pub challenges_passed: u64,
    pub challenges_failed: u64,
    pub consecutive_failures: u32,
    pub challenge_streak: u32,
    pub total_challenges: u32,
    pub total_rewards: u64,
    pub total_slashed: u64,
    pub last_active: u64,
    pub status: ProviderStatus,
}

pub enum ProviderStatus {
    Active,
    TemporarilyBanned { until: u64 },
    PermanentlyBanned { reason: BanReason },
    Suspended { reason: String },
}
```

### On-Chain Settlement

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";

/**
 * @title QoSChallengeSettlement
 * @notice On-chain settlement for QoS challenges
 */
contract QoSChallengeSettlement is ReentrancyGuard {
    IERC20 public immutable aiToken;

    struct Provider {
        uint256 stake;
        uint256 reputation;
        uint256 totalRewards;
        uint256 totalSlashed;
        uint32 consecutiveFailures;
        uint64 lastChallengeTime;
        ProviderStatus status;
    }

    enum ProviderStatus { Active, TempBanned, PermBanned }

    struct ChallengeResult {
        bytes32 challengeId;
        address provider;
        bool passed;
        uint256 performanceScore; // 18 decimals, 1e18 = 100%
        bool teeVerified;
        uint256 timestamp;
    }

    mapping(address => Provider) public providers;
    mapping(bytes32 => ChallengeResult) public results;

    uint256 public constant MIN_STAKE = 100 ether; // 100 AI tokens
    uint256 public constant BASE_REWARD = 0.1 ether;
    uint256 public constant BASE_SLASH_PERCENT = 1; // 1%

    event ChallengeSettled(
        bytes32 indexed challengeId,
        address indexed provider,
        bool passed,
        uint256 reward,
        uint256 slash
    );

    event ProviderSlashed(
        address indexed provider,
        uint256 amount,
        string reason
    );

    event ProviderBanned(
        address indexed provider,
        bool permanent,
        string reason
    );

    constructor(address _aiToken) {
        aiToken = IERC20(_aiToken);
    }

    function settleChallengeResult(
        bytes32 challengeId,
        address providerAddr,
        bool passed,
        uint256 performanceScore,
        bool teeVerified,
        bytes calldata signature
    ) external nonReentrant {
        // Verify signature from challenge committee
        require(verifySettlementSignature(
            challengeId, providerAddr, passed, performanceScore, teeVerified, signature
        ), "Invalid signature");

        Provider storage provider = providers[providerAddr];
        require(provider.status == ProviderStatus.Active, "Provider not active");

        uint256 reward = 0;
        uint256 slash = 0;

        if (passed) {
            reward = calculateReward(provider, performanceScore, teeVerified);
            provider.consecutiveFailures = 0;
            provider.reputation += performanceScore / 1e17; // Scaled increase
        } else {
            (slash, bool tempBan, bool permBan) = calculatePenalty(provider);

            if (slash > 0) {
                provider.stake -= slash;
                provider.totalSlashed += slash;
                emit ProviderSlashed(providerAddr, slash, "Challenge failed");
            }

            provider.consecutiveFailures++;
            provider.reputation = provider.reputation > 10 ?
                provider.reputation - 10 : 0;

            if (permBan) {
                provider.status = ProviderStatus.PermBanned;
                emit ProviderBanned(providerAddr, true, "Excessive failures");
            } else if (tempBan) {
                provider.status = ProviderStatus.TempBanned;
                emit ProviderBanned(providerAddr, false, "Multiple failures");
            }
        }

        provider.lastChallengeTime = uint64(block.timestamp);

        // Store result
        results[challengeId] = ChallengeResult({
            challengeId: challengeId,
            provider: providerAddr,
            passed: passed,
            performanceScore: performanceScore,
            teeVerified: teeVerified,
            timestamp: block.timestamp
        });

        // Transfer reward
        if (reward > 0) {
            provider.totalRewards += reward;
            require(aiToken.transfer(providerAddr, reward), "Reward transfer failed");
        }

        emit ChallengeSettled(challengeId, providerAddr, passed, reward, slash);
    }

    function calculateReward(
        Provider storage provider,
        uint256 performanceScore,
        bool teeVerified
    ) internal view returns (uint256) {
        uint256 reward = BASE_REWARD;

        // Performance bonus (up to 2x)
        if (performanceScore > 1e18) {
            uint256 bonus = (performanceScore - 1e18) * reward / 1e18;
            reward += bonus > reward ? reward : bonus;
        }

        // TEE bonus (25%)
        if (teeVerified) {
            reward = reward * 125 / 100;
        }

        // Elite bonus (15%) for high reputation
        if (provider.reputation >= 900) {
            reward = reward * 115 / 100;
        }

        return reward;
    }

    function calculatePenalty(
        Provider storage provider
    ) internal view returns (uint256 slash, bool tempBan, bool permBan) {
        // Slash increases with consecutive failures
        uint256 slashPercent = BASE_SLASH_PERCENT *
            (150 ** provider.consecutiveFailures) / (100 ** provider.consecutiveFailures);

        // Cap at 10%
        if (slashPercent > 10) slashPercent = 10;

        slash = provider.stake * slashPercent / 100;

        // Ban conditions
        tempBan = provider.consecutiveFailures >= 4;
        permBan = provider.consecutiveFailures >= 19;

        return (slash, tempBan, permBan);
    }

    function verifySettlementSignature(
        bytes32 challengeId,
        address provider,
        bool passed,
        uint256 performanceScore,
        bool teeVerified,
        bytes calldata signature
    ) internal view returns (bool) {
        // Implementation: verify ECDSA/ML-DSA signature from challenge committee
        // This would verify against a multisig or threshold signature
        return true; // Placeholder
    }
}
```

---

## Security Analysis

### Threat Model

| Threat | Impact | Mitigation |
|--------|--------|------------|
| **Sybil Attack** | Multiple fake identities to game reputation | Minimum stake requirement, unique hardware attestation |
| **Computation Outsourcing** | Provider outsources challenge to faster node | Strict deadlines, random challenge timing, TEE verification |
| **Selective Participation** | Only respond to easy challenges | Random assignment, penalty for missed challenges |
| **Collusion** | Verifiers and providers collude | Multiple independent verifiers, fraud proofs, slashing |
| **Replay Attacks** | Reuse old challenge responses | Unique nonces, timestamp validation, challenge expiry |
| **Eclipse Attack** | Isolate provider from network | Multiple bootstrap nodes, diverse verifier set |
| **TEE Compromise** | Fake attestation quotes | Quote freshness checks, revocation lists, multi-vendor TEE |

### Security Properties

1. **Unpredictability**: Challenges are generated using VRF (Verifiable Random Function) to prevent prediction
2. **Non-repudiation**: All proofs are cryptographically signed
3. **Atomicity**: On-chain settlement is atomic (reward/slash happens together)
4. **Consistency**: Distributed verifier consensus prevents single points of failure

### Challenge Randomness

```rust
use vrf::VRF;

pub struct ChallengeSelector {
    vrf: VRF,
    last_beacon: [u8; 32],
}

impl ChallengeSelector {
    /// Select providers for next challenge round
    pub fn select_providers(
        &mut self,
        all_providers: &[PeerId],
        count: usize,
        block_hash: &[u8; 32],
    ) -> Vec<PeerId> {
        // Update beacon with latest block hash for unpredictability
        let mut input = Vec::new();
        input.extend_from_slice(&self.last_beacon);
        input.extend_from_slice(block_hash);

        let (output, proof) = self.vrf.prove(&input);
        self.last_beacon = output;

        // Use VRF output to deterministically select providers
        let mut selected = Vec::new();
        let mut rng = ChaCha20Rng::from_seed(output);

        let mut indices: Vec<usize> = (0..all_providers.len()).collect();
        indices.shuffle(&mut rng);

        for idx in indices.into_iter().take(count) {
            selected.push(all_providers[idx].clone());
        }

        selected
    }

    /// Select challenge type for a provider
    pub fn select_challenge_type(
        &self,
        provider: &ProviderState,
        beacon: &[u8; 32],
    ) -> ChallengeType {
        let mut hasher = blake3::Hasher::new();
        hasher.update(beacon);
        hasher.update(provider.id.as_bytes());
        let hash = hasher.finalize();

        // Use first byte to select type based on provider tier weights
        let weights = provider.challenge_weights();
        let roll = hash.as_bytes()[0] as u32 * 100 / 256;

        weights.select(roll)
    }
}
```

---

## Implementation Plan

### Phase 1: Foundation (Weeks 1-4)

#### Week 1-2: Core Data Structures
- [ ] Define `Challenge`, `Proof`, `ChallengeResult` types in `hanzo-compute`
- [ ] Implement `ChallengeGenerator` with VRF-based randomness
- [ ] Add `QoSScore` and `ProviderState` to peer management
- [ ] Unit tests for all data structures

#### Week 3-4: Challenge Protocol
- [ ] Implement P2P challenge delivery via `hanzo-libp2p`
- [ ] Build `ComputeChallengeVerifier` with reference computation
- [ ] Implement `LatencyChallengeVerifier` with timing verification
- [ ] Integration tests for challenge round-trip

### Phase 2: Verification (Weeks 5-8)

#### Week 5-6: Verification Engine
- [ ] Build optimistic verification framework
- [ ] Implement fraud proof generation and validation
- [ ] Add TEE attestation verification (Intel SGX, AMD SEV)
- [ ] Integration with `hanzo-pqc` for quantum-safe signatures

#### Week 7-8: Scoring System
- [ ] Implement `QoSScoreCalculator` with component weighting
- [ ] Build `ConsistencyCalculator` with time-series analysis
- [ ] Add score persistence to `hanzo-database`
- [ ] API endpoints for score queries

### Phase 3: Economics (Weeks 9-12)

#### Week 9-10: On-Chain Contracts
- [ ] Deploy `QoSChallengeSettlement` contract to testnet
- [ ] Implement reward distribution logic
- [ ] Add slashing mechanics with circuit breakers
- [ ] Security audit of smart contracts

#### Week 11-12: Integration & Testing
- [ ] End-to-end integration with `hanzo-mining`
- [ ] Stress testing with simulated adversaries
- [ ] Documentation and API reference
- [ ] Mainnet deployment preparation

### Milestones

| Milestone | Deliverable | Date |
|-----------|-------------|------|
| M1 | Challenge protocol specification finalized | Week 2 |
| M2 | Testnet deployment with basic challenges | Week 6 |
| M3 | Full verification engine operational | Week 8 |
| M4 | Economic settlement on testnet | Week 10 |
| M5 | Security audit complete | Week 11 |
| M6 | Mainnet launch | Week 12 |

### Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| Challenge completion rate | > 98% | Successful responses / total challenges |
| False positive rate | < 0.1% | Incorrect failures / total failures |
| Verification latency | < 5 seconds | Time from proof submission to verification |
| Provider churn | < 5% monthly | Providers leaving due to failed challenges |
| Fraud detection rate | > 99% | Detected fraudulent proofs / total fraudulent |

---

## Appendix A: API Reference

### Challenge Service gRPC API

```protobuf
syntax = "proto3";

package hanzo.qos.v1;

service QoSChallengeService {
    // Issue a new challenge to a provider
    rpc IssueChallenge(IssueChallengeRequest) returns (Challenge);

    // Submit proof for a challenge
    rpc SubmitProof(SubmitProofRequest) returns (SubmitProofResponse);

    // Get challenge status
    rpc GetChallengeStatus(GetChallengeStatusRequest) returns (ChallengeStatus);

    // Get provider QoS score
    rpc GetQoSScore(GetQoSScoreRequest) returns (QoSScore);

    // Stream challenge events
    rpc StreamChallenges(StreamChallengesRequest) returns (stream ChallengeEvent);
}

message Challenge {
    bytes id = 1;
    ChallengeType type = 2;
    bytes seed = 3;
    uint32 difficulty = 4;
    uint64 deadline_ms = 5;
    uint64 issued_at = 6;
    bytes signature = 7;
}

enum ChallengeType {
    CHALLENGE_TYPE_UNSPECIFIED = 0;
    CHALLENGE_TYPE_COMPUTE = 1;
    CHALLENGE_TYPE_LATENCY = 2;
    CHALLENGE_TYPE_BANDWIDTH = 3;
    CHALLENGE_TYPE_AVAILABILITY = 4;
    CHALLENGE_TYPE_MODEL = 5;
}

message ChallengeProof {
    bytes challenge_id = 1;
    string provider_id = 2;
    bytes proof_data = 3;
    uint64 executed_at = 4;
    uint64 duration_ms = 5;
    bytes signature = 6;
    optional bytes tee_attestation = 7;
}

message QoSScore {
    uint32 composite = 1;
    uint32 compute_score = 2;
    uint32 latency_score = 3;
    uint32 bandwidth_score = 4;
    uint32 availability_score = 5;
    uint32 consistency_score = 6;
    double confidence = 7;
    uint64 updated_at = 8;
}
```

### REST API Endpoints

```yaml
openapi: 3.0.0
info:
  title: QoS Challenge API
  version: 1.0.0

paths:
  /v1/challenges:
    get:
      summary: List active challenges for provider
      parameters:
        - name: provider_id
          in: query
          required: true
          schema:
            type: string
      responses:
        '200':
          description: Active challenges
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/Challenge'

  /v1/challenges/{challenge_id}/proof:
    post:
      summary: Submit proof for challenge
      parameters:
        - name: challenge_id
          in: path
          required: true
          schema:
            type: string
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/ChallengeProof'
      responses:
        '200':
          description: Proof accepted
        '400':
          description: Invalid proof

  /v1/providers/{provider_id}/qos:
    get:
      summary: Get provider QoS score
      parameters:
        - name: provider_id
          in: path
          required: true
          schema:
            type: string
      responses:
        '200':
          description: QoS score
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/QoSScore'

  /v1/providers/{provider_id}/history:
    get:
      summary: Get challenge history
      parameters:
        - name: provider_id
          in: path
          required: true
          schema:
            type: string
        - name: limit
          in: query
          schema:
            type: integer
            default: 100
      responses:
        '200':
          description: Challenge history
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/ChallengeResult'

components:
  schemas:
    Challenge:
      type: object
      properties:
        id:
          type: string
          format: byte
        type:
          type: string
          enum: [compute, latency, bandwidth, availability, model]
        seed:
          type: string
          format: byte
        difficulty:
          type: integer
        deadline_ms:
          type: integer
          format: int64
        issued_at:
          type: integer
          format: int64

    ChallengeProof:
      type: object
      properties:
        challenge_id:
          type: string
          format: byte
        provider_id:
          type: string
        proof_data:
          type: object
        duration_ms:
          type: integer
          format: int64
        signature:
          type: string
          format: byte
        tee_attestation:
          type: string
          format: byte

    QoSScore:
      type: object
      properties:
        composite:
          type: integer
        compute_score:
          type: integer
        latency_score:
          type: integer
        bandwidth_score:
          type: integer
        availability_score:
          type: integer
        consistency_score:
          type: integer
        confidence:
          type: number
          format: double
        updated_at:
          type: integer
          format: int64

    ChallengeResult:
      type: object
      properties:
        challenge_id:
          type: string
        passed:
          type: boolean
        performance_score:
          type: number
        reward:
          type: integer
          format: int64
        slash:
          type: integer
          format: int64
        timestamp:
          type: integer
          format: int64
```

---

## Appendix B: Reference Implementations

### Compute Challenge Kernel (GPU)

```cuda
// Reference CUDA kernel for compute challenges
// Uses deterministic matrix multiplication with seed-based initialization

__global__ void compute_challenge_kernel(
    float* A, float* B, float* C,
    int N, uint32_t seed
) {
    int row = blockIdx.y * blockDim.y + threadIdx.y;
    int col = blockIdx.x * blockDim.x + threadIdx.x;

    if (row < N && col < N) {
        float sum = 0.0f;
        for (int k = 0; k < N; k++) {
            sum += A[row * N + k] * B[k * N + col];
        }
        C[row * N + col] = sum;
    }
}

// Initialize matrices deterministically from seed
void init_challenge_matrices(float* A, float* B, int N, uint32_t seed) {
    // Use PCG random generator seeded with challenge seed
    pcg32_random_t rng;
    pcg32_srandom_r(&rng, seed, seed ^ 0xDEADBEEF);

    for (int i = 0; i < N * N; i++) {
        // Generate deterministic values in [-1, 1]
        A[i] = (pcg32_random_r(&rng) / (float)UINT32_MAX) * 2.0f - 1.0f;
        B[i] = (pcg32_random_r(&rng) / (float)UINT32_MAX) * 2.0f - 1.0f;
    }
}

// Compute challenge proof
ChallengeProof execute_compute_challenge(ComputeChallenge* challenge) {
    int N = difficulty_to_size(challenge->difficulty);

    float *h_A, *h_B, *h_C;
    float *d_A, *d_B, *d_C;

    // Allocate and initialize
    h_A = (float*)malloc(N * N * sizeof(float));
    h_B = (float*)malloc(N * N * sizeof(float));
    h_C = (float*)malloc(N * N * sizeof(float));

    init_challenge_matrices(h_A, h_B, N, *(uint32_t*)challenge->seed);

    cudaMalloc(&d_A, N * N * sizeof(float));
    cudaMalloc(&d_B, N * N * sizeof(float));
    cudaMalloc(&d_C, N * N * sizeof(float));

    cudaMemcpy(d_A, h_A, N * N * sizeof(float), cudaMemcpyHostToDevice);
    cudaMemcpy(d_B, h_B, N * N * sizeof(float), cudaMemcpyHostToDevice);

    // Execute timed kernel
    dim3 threads(16, 16);
    dim3 blocks((N + 15) / 16, (N + 15) / 16);

    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    cudaEventRecord(start);
    compute_challenge_kernel<<<blocks, threads>>>(d_A, d_B, d_C, N,
        *(uint32_t*)challenge->seed);
    cudaEventRecord(stop);

    cudaEventSynchronize(stop);

    float milliseconds = 0;
    cudaEventElapsedTime(&milliseconds, start, stop);

    // Copy result back
    cudaMemcpy(h_C, d_C, N * N * sizeof(float), cudaMemcpyDeviceToHost);

    // Compute result hash
    uint8_t result_hash[32];
    blake3_hasher hasher;
    blake3_hasher_init(&hasher);
    blake3_hasher_update(&hasher, h_C, N * N * sizeof(float));
    blake3_hasher_finalize(&hasher, result_hash, 32);

    // Calculate TFLOPS
    double flops = 2.0 * N * N * N; // 2 ops per multiply-add
    double tflops = flops / (milliseconds * 1e9);

    // Build proof
    ChallengeProof proof = {
        .challenge_id = challenge->id,
        .duration_ms = (uint64_t)milliseconds,
        .actual_flops = tflops,
    };
    memcpy(proof.result_hash, result_hash, 32);

    // Cleanup
    free(h_A); free(h_B); free(h_C);
    cudaFree(d_A); cudaFree(d_B); cudaFree(d_C);

    return proof;
}
```

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-24 | Architecture Team | Initial design document |

---

## References

1. Hanzo Network Architecture Documentation
2. AI Token Economics Specification
3. hanzo-compute crate documentation
4. hanzo-mining consensus implementation
5. Intel SGX Remote Attestation Guide
6. NVIDIA Confidential Computing Documentation
