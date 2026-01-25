//! Challenge generation and management.
//!
//! This module provides the `ChallengeGenerator` for creating cryptographically
//! random challenges that providers must complete to prove their QoS capabilities.

use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;
use uuid::Uuid;

use crate::crypto::{Signature, Vrf, blake3_hash_chunks, generate_challenge_seed};
use crate::error::Result;
use crate::types::{
    Challenge, ChallengeType, ComputeMetric, ModelCapability, PeerId,
    ProviderState,
};

/// Configuration for challenge frequency based on provider tier.
#[derive(Debug, Clone)]
pub struct ChallengeFrequencyConfig {
    /// Challenges per hour for new providers (< 10 jobs)
    pub new_provider_per_hour: u32,
    /// Challenges per hour for established providers (10-100 jobs)
    pub established_per_hour: u32,
    /// Challenges per hour for trusted providers (100+ jobs)
    pub trusted_per_hour: u32,
    /// Challenges per hour for elite providers (1000+, TEE)
    pub elite_per_hour: u32,
}

impl Default for ChallengeFrequencyConfig {
    fn default() -> Self {
        Self {
            new_provider_per_hour: 6,       // Every 10 minutes
            established_per_hour: 2,         // Every 30 minutes
            trusted_per_hour: 1,             // Every 60 minutes
            elite_per_hour: 1,               // Every 2 hours (0.5 rounded up)
        }
    }
}

/// Challenge type weights for different provider tiers.
#[derive(Debug, Clone)]
pub struct ChallengeWeights {
    pub compute: u8,
    pub availability: u8,
    pub latency: u8,
    pub model: u8,
}

impl ChallengeWeights {
    /// Get weights for new providers.
    pub fn new_provider() -> Self {
        Self {
            compute: 40,
            availability: 30,
            latency: 20,
            model: 10,
        }
    }

    /// Get weights for established providers.
    pub fn established() -> Self {
        Self {
            compute: 30,
            availability: 30,
            latency: 25,
            model: 15,
        }
    }

    /// Get weights for trusted providers.
    pub fn trusted() -> Self {
        Self {
            compute: 25,
            availability: 35,
            latency: 20,
            model: 20,
        }
    }

    /// Get weights for elite providers.
    pub fn elite() -> Self {
        Self {
            compute: 20,
            availability: 40,
            latency: 20,
            model: 20,
        }
    }

    /// Select a challenge type based on a random value (0-99).
    pub fn select(&self, roll: u8) -> ChallengeType {
        let compute_threshold = self.compute;
        let availability_threshold = compute_threshold + self.availability;
        let latency_threshold = availability_threshold + self.latency;

        if roll < compute_threshold {
            // Randomly select compute metric
            ChallengeType::Compute(ComputeMetric::GpuFp32)
        } else if roll < availability_threshold {
            ChallengeType::Availability
        } else if roll < latency_threshold {
            ChallengeType::Latency
        } else {
            ChallengeType::Model(ModelCapability::new(
                "default-model".to_string(),
                16,
                50.0,
            ))
        }
    }
}

/// Generator for QoS challenges.
///
/// Uses VRF for unpredictable challenge selection and cryptographically
/// secure random seeds for challenge parameters.
pub struct ChallengeGenerator {
    /// VRF for random selection
    vrf: Vrf,
    /// Last beacon value
    last_beacon: [u8; 32],
    /// Secret key for signing challenges
    secret_key: [u8; 32],
    /// Frequency configuration
    frequency_config: ChallengeFrequencyConfig,
}

impl ChallengeGenerator {
    /// Create a new challenge generator with the given secret key.
    pub fn new(secret_key: [u8; 32]) -> Self {
        Self {
            vrf: Vrf::new(secret_key),
            last_beacon: [0u8; 32],
            secret_key,
            frequency_config: ChallengeFrequencyConfig::default(),
        }
    }

    /// Create with custom frequency configuration.
    pub fn with_frequency_config(secret_key: [u8; 32], config: ChallengeFrequencyConfig) -> Self {
        Self {
            vrf: Vrf::new(secret_key),
            last_beacon: [0u8; 32],
            secret_key,
            frequency_config: config,
        }
    }

    /// Update the beacon with a new block hash.
    pub fn update_beacon(&mut self, block_hash: &[u8; 32]) {
        let mut input = Vec::new();
        input.extend_from_slice(&self.last_beacon);
        input.extend_from_slice(block_hash);

        let output = self.vrf.prove(&input);
        self.last_beacon = output.output;
    }

    /// Generate a challenge of the specified type.
    pub fn generate_challenge(
        &self,
        challenge_type: ChallengeType,
        difficulty: u8,
        deadline_ms: u64,
    ) -> Challenge {
        let id = Uuid::new_v4();
        let seed = generate_challenge_seed(&self.last_beacon, &self.last_beacon, &id.to_string());

        let mut challenge = Challenge::new(challenge_type, seed, difficulty, deadline_ms);

        // Sign the challenge
        let sig_data = self.challenge_signature_data(&challenge);
        let signature = Signature::sign(&sig_data, &self.secret_key);
        challenge.signature = signature.bytes;

        challenge
    }

    /// Generate a random challenge for a provider based on their tier.
    pub fn generate_random_challenge(&self, provider: &ProviderState) -> Challenge {
        let weights = self.weights_for_provider(provider);
        let challenge_type = self.select_challenge_type(provider, &weights);
        let difficulty = self.select_difficulty(provider);
        let deadline_ms = self.deadline_for_type(&challenge_type, difficulty);

        self.generate_challenge(challenge_type, difficulty, deadline_ms)
    }

    /// Select providers for the next challenge round.
    pub fn select_providers(
        &mut self,
        all_providers: &[PeerId],
        count: usize,
        block_hash: &[u8; 32],
    ) -> Vec<PeerId> {
        if all_providers.is_empty() || count == 0 {
            return vec![];
        }

        // Update beacon
        self.update_beacon(block_hash);

        // Use VRF output to deterministically select providers
        let mut rng = StdRng::from_seed(self.last_beacon);

        let mut indices: Vec<usize> = (0..all_providers.len()).collect();

        // Fisher-Yates shuffle
        for i in (1..indices.len()).rev() {
            let j = rng.random_range(0..=i);
            indices.swap(i, j);
        }

        indices
            .into_iter()
            .take(count)
            .map(|i| all_providers[i].clone())
            .collect()
    }

    /// Get challenge weights for a provider based on their tier.
    fn weights_for_provider(&self, provider: &ProviderState) -> ChallengeWeights {
        let total = provider.total_challenges;

        if total < 10 {
            ChallengeWeights::new_provider()
        } else if total < 100 {
            ChallengeWeights::established()
        } else if total < 1000 {
            ChallengeWeights::trusted()
        } else {
            ChallengeWeights::elite()
        }
    }

    /// Select challenge type for a provider.
    fn select_challenge_type(
        &self,
        provider: &ProviderState,
        weights: &ChallengeWeights,
    ) -> ChallengeType {
        let hash = blake3_hash_chunks(&[&self.last_beacon, provider.id.as_bytes()]);
        let roll = hash[0] % 100;
        weights.select(roll)
    }

    /// Select difficulty based on provider performance.
    fn select_difficulty(&self, provider: &ProviderState) -> u8 {
        // Base difficulty on provider's QoS score
        let score = provider.qos_score.composite;

        if score >= 900 {
            8 // High performers get harder challenges
        } else if score >= 700 {
            6
        } else if score >= 500 {
            5
        } else {
            4 // Lower performers get easier challenges
        }
    }

    /// Get deadline for a challenge type and difficulty.
    fn deadline_for_type(&self, challenge_type: &ChallengeType, difficulty: u8) -> u64 {
        let base_ms = match challenge_type {
            ChallengeType::Compute(_) => 30_000,     // 30 seconds base
            ChallengeType::Latency => 5_000,         // 5 seconds
            ChallengeType::Bandwidth(_) => 60_000,   // 60 seconds
            ChallengeType::Availability => 3_000,    // 3 seconds
            ChallengeType::Model(_) => 120_000,      // 2 minutes
        };

        // Scale by difficulty
        base_ms * (difficulty as u64) / 5
    }

    /// Create data for signature.
    fn challenge_signature_data(&self, challenge: &Challenge) -> Vec<u8> {
        let mut data = Vec::new();
        data.extend_from_slice(challenge.id.as_bytes());
        data.extend_from_slice(&challenge.seed);
        data.push(challenge.difficulty);
        data.extend_from_slice(&challenge.deadline_ms.to_le_bytes());
        data.extend_from_slice(&challenge.issued_at.to_le_bytes());
        data
    }

    /// Verify a challenge signature.
    pub fn verify_challenge_signature(&self, challenge: &Challenge) -> bool {
        let sig_data = self.challenge_signature_data(challenge);
        let sig = Signature::from_bytes(challenge.signature.clone());
        sig.verify(&sig_data, &self.secret_key)
    }

    /// Get expected challenge interval for a provider tier (in milliseconds).
    pub fn challenge_interval_ms(&self, total_challenges: u32) -> u64 {
        let per_hour = if total_challenges < 10 {
            self.frequency_config.new_provider_per_hour
        } else if total_challenges < 100 {
            self.frequency_config.established_per_hour
        } else if total_challenges < 1000 {
            self.frequency_config.trusted_per_hour
        } else {
            self.frequency_config.elite_per_hour
        };

        if per_hour == 0 {
            return u64::MAX;
        }

        3_600_000 / per_hour as u64 // ms per hour / challenges per hour
    }
}

/// Builder for creating challenges with specific parameters.
pub struct ChallengeBuilder {
    challenge_type: Option<ChallengeType>,
    seed: Option<[u8; 32]>,
    difficulty: u8,
    deadline_ms: u64,
    signature: Vec<u8>,
}

impl Default for ChallengeBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ChallengeBuilder {
    /// Create a new challenge builder.
    pub fn new() -> Self {
        Self {
            challenge_type: None,
            seed: None,
            difficulty: 5,
            deadline_ms: 30_000,
            signature: Vec::new(),
        }
    }

    /// Set the challenge type.
    pub fn challenge_type(mut self, ct: ChallengeType) -> Self {
        self.challenge_type = Some(ct);
        self
    }

    /// Set the seed.
    pub fn seed(mut self, seed: [u8; 32]) -> Self {
        self.seed = Some(seed);
        self
    }

    /// Set the difficulty (1-10).
    pub fn difficulty(mut self, d: u8) -> Self {
        self.difficulty = d.clamp(1, 10);
        self
    }

    /// Set the deadline in milliseconds.
    pub fn deadline_ms(mut self, ms: u64) -> Self {
        self.deadline_ms = ms;
        self
    }

    /// Set the signature.
    pub fn signature(mut self, sig: Vec<u8>) -> Self {
        self.signature = sig;
        self
    }

    /// Build the challenge.
    pub fn build(self) -> Result<Challenge> {
        let challenge_type = self
            .challenge_type
            .unwrap_or(ChallengeType::Availability);

        let seed = self.seed.unwrap_or([0u8; 32]);

        let mut challenge = Challenge::new(challenge_type, seed, self.difficulty, self.deadline_ms);
        challenge.signature = self.signature;

        Ok(challenge)
    }
}

/// Difficulty scaling for compute challenges.
pub fn compute_problem_size(difficulty: u8, metric: &ComputeMetric) -> u64 {
    let base_size: u64 = match metric {
        ComputeMetric::GpuFp32 => 1024,
        ComputeMetric::GpuFp16 => 2048,
        ComputeMetric::GpuInt8 => 4096,
        ComputeMetric::CpuGflops => 512,
        ComputeMetric::MemoryBandwidth => 1_073_741_824, // 1GB
    };

    base_size * (1u64 << (difficulty.saturating_sub(1) as u32))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_challenge_generator() {
        let generator = ChallengeGenerator::new([42u8; 32]);

        let challenge = generator.generate_challenge(ChallengeType::Availability, 5, 30_000);

        assert!(!challenge.is_expired());
        assert_eq!(challenge.difficulty, 5);
        assert!(!challenge.signature.is_empty());
    }

    #[test]
    fn test_challenge_signature_verification() {
        let generator = ChallengeGenerator::new([42u8; 32]);

        let challenge = generator.generate_challenge(
            ChallengeType::Compute(ComputeMetric::GpuFp32),
            5,
            30_000,
        );

        assert!(generator.verify_challenge_signature(&challenge));
    }

    #[test]
    fn test_provider_selection() {
        let mut generator = ChallengeGenerator::new([42u8; 32]);
        let providers: Vec<PeerId> = (0..10).map(|i| format!("provider-{i}")).collect();
        let block_hash = [1u8; 32];

        let selected = generator.select_providers(&providers, 3, &block_hash);

        assert_eq!(selected.len(), 3);
        // All selected should be from the original list
        for s in &selected {
            assert!(providers.contains(s));
        }
    }

    #[test]
    fn test_challenge_weights() {
        let weights = ChallengeWeights::new_provider();

        // Check that weights sum to 100
        let total = weights.compute + weights.availability + weights.latency + weights.model;
        assert_eq!(total, 100);
    }

    #[test]
    fn test_compute_problem_size() {
        let size1 = compute_problem_size(1, &ComputeMetric::GpuFp32);
        let size5 = compute_problem_size(5, &ComputeMetric::GpuFp32);
        let size10 = compute_problem_size(10, &ComputeMetric::GpuFp32);

        assert!(size1 < size5);
        assert!(size5 < size10);
    }

    #[test]
    fn test_challenge_builder() {
        let challenge = ChallengeBuilder::new()
            .challenge_type(ChallengeType::Latency)
            .difficulty(7)
            .deadline_ms(5000)
            .build()
            .expect("should build");

        assert_eq!(challenge.difficulty, 7);
        assert_eq!(challenge.deadline_ms, 5000);
    }
}
