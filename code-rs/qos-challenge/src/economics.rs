//! Economic incentive system for QoS challenges.
//!
//! This module implements reward distribution and penalty calculation
//! to align economic incentives with honest behavior.

use crate::types::{
    BanReason, ChallengeResult, Penalty, ProviderState, ProviderStatus,
};

/// Configuration for reward calculation.
#[derive(Debug, Clone)]
pub struct RewardConfig {
    /// Base reward per successful challenge (in smallest token unit)
    pub base_reward: u64,
    /// Maximum performance bonus multiplier (e.g., 2.0 = up to 2x base)
    pub max_performance_bonus: f64,
    /// Streak bonus per consecutive pass (e.g., 0.01 = 1% per pass)
    pub streak_bonus_per_pass: f64,
    /// Maximum streak bonus (e.g., 0.5 = 50% max)
    pub max_streak_bonus: f64,
    /// TEE attestation bonus (e.g., 0.25 = 25%)
    pub tee_bonus: f64,
    /// Elite provider score threshold
    pub elite_threshold_score: u32,
    /// Elite provider bonus (e.g., 0.15 = 15%)
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

/// Calculator for challenge rewards.
pub struct RewardCalculator {
    config: RewardConfig,
}

impl RewardCalculator {
    /// Create a new reward calculator.
    pub fn new() -> Self {
        Self {
            config: RewardConfig::default(),
        }
    }

    /// Create with custom configuration.
    pub fn with_config(config: RewardConfig) -> Self {
        Self { config }
    }

    /// Calculate reward for a successful challenge.
    pub fn calculate_reward(
        &self,
        result: &ChallengeResult,
        provider_state: &ProviderState,
    ) -> u64 {
        if !result.passed {
            return 0;
        }

        let mut reward = self.config.base_reward as f64;

        // Performance bonus (0-100% additional based on performance score)
        let performance_bonus = (result.performance_score - 1.0)
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
        if result.tee_verified {
            reward *= 1.0 + self.config.tee_bonus;
        }

        // Elite bonus
        if provider_state.qos_score.composite >= self.config.elite_threshold_score {
            reward *= 1.0 + self.config.elite_bonus;
        }

        reward as u64
    }

    /// Get the reward configuration.
    pub fn config(&self) -> &RewardConfig {
        &self.config
    }
}

impl Default for RewardCalculator {
    fn default() -> Self {
        Self::new()
    }
}

/// Configuration for penalty calculation.
#[derive(Debug, Clone)]
pub struct PenaltyConfig {
    /// Base slash percentage for failed challenge (e.g., 0.01 = 1%)
    pub base_slash_percent: f64,
    /// Multiplier for consecutive failures (e.g., 1.5 = 50% increase per failure)
    pub consecutive_failure_multiplier: f64,
    /// Maximum slash percentage per challenge (e.g., 0.10 = 10%)
    pub max_slash_percent: f64,
    /// Reputation penalty per failure
    pub reputation_penalty: f64,
    /// Consecutive failures before temporary ban
    pub failures_before_temp_ban: u32,
    /// Temporary ban duration in seconds
    pub temp_ban_duration: u64,
    /// Consecutive failures before permanent ban
    pub failures_before_perm_ban: u32,
    /// Grace period challenges (warnings only)
    pub grace_period_challenges: u32,
}

impl Default for PenaltyConfig {
    fn default() -> Self {
        Self {
            base_slash_percent: 0.01,
            consecutive_failure_multiplier: 1.5,
            max_slash_percent: 0.10,
            reputation_penalty: 10.0,
            failures_before_temp_ban: 5,
            temp_ban_duration: 86400, // 24 hours
            failures_before_perm_ban: 20,
            grace_period_challenges: 3,
        }
    }
}

/// Calculator for challenge penalties.
pub struct PenaltyCalculator {
    config: PenaltyConfig,
}

impl PenaltyCalculator {
    /// Create a new penalty calculator.
    pub fn new() -> Self {
        Self {
            config: PenaltyConfig::default(),
        }
    }

    /// Create with custom configuration.
    pub fn with_config(config: PenaltyConfig) -> Self {
        Self { config }
    }

    /// Calculate penalty for a failed challenge.
    pub fn calculate_penalty(
        &self,
        _result: &ChallengeResult,
        provider_state: &ProviderState,
    ) -> Penalty {
        // Grace period for new providers
        if provider_state.total_challenges < self.config.grace_period_challenges {
            return Penalty::Warning;
        }

        let consecutive_failures = provider_state.consecutive_failures;

        // Calculate slash amount
        let slash_multiplier =
            self.config.consecutive_failure_multiplier.powi(consecutive_failures as i32);
        let slash_percent =
            (self.config.base_slash_percent * slash_multiplier).min(self.config.max_slash_percent);
        let slash_amount = (provider_state.stake as f64 * slash_percent) as u64;

        // Calculate reputation penalty
        let rep_penalty =
            self.config.reputation_penalty * (1.0 + 0.1 * consecutive_failures as f64);

        // Check for permanent ban
        if consecutive_failures >= self.config.failures_before_perm_ban {
            return Penalty::PermanentBan {
                slash_amount: provider_state.stake, // Slash everything
                reason: BanReason::ExcessiveFailures,
            };
        }

        // Check for temporary ban
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

    /// Get the penalty configuration.
    pub fn config(&self) -> &PenaltyConfig {
        &self.config
    }
}

impl Default for PenaltyCalculator {
    fn default() -> Self {
        Self::new()
    }
}

/// Manager for provider reputation.
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
    /// Create a new reputation manager.
    pub fn new() -> Self {
        Self {
            min_reputation: 0.0,
            starting_reputation: 100.0,
            max_reputation: 1000.0,
            decay_rate: 0.99, // 1% decay per day
        }
    }

    /// Create with custom parameters.
    pub fn with_params(
        min_reputation: f64,
        starting_reputation: f64,
        max_reputation: f64,
        decay_rate: f64,
    ) -> Self {
        Self {
            min_reputation,
            starting_reputation,
            max_reputation,
            decay_rate,
        }
    }

    /// Update provider reputation based on challenge result and penalty.
    pub fn update_reputation(
        &self,
        provider: &mut ProviderState,
        result: &ChallengeResult,
        penalty: &Penalty,
        reward: u64,
    ) {
        // Apply penalty effects
        match penalty {
            Penalty::Warning => {
                // Slight decrease for warning
                provider.reputation = (provider.reputation - 1.0).max(self.min_reputation);
            }
            Penalty::Slash {
                reputation_penalty, ..
            }
            | Penalty::TemporaryBan {
                reputation_penalty, ..
            } => {
                provider.reputation =
                    (provider.reputation - reputation_penalty).max(self.min_reputation);
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

        // Update provider stats
        provider.total_rewards += reward;
        provider.total_challenges += 1;

        if result.passed {
            provider.challenges_passed += 1;
            provider.consecutive_failures = 0;
            provider.challenge_streak += 1;
        } else {
            provider.challenges_failed += 1;
            provider.consecutive_failures += 1;
            provider.challenge_streak = 0;

            // Apply slash
            if let Penalty::Slash { amount, .. }
            | Penalty::TemporaryBan {
                slash_amount: amount,
                ..
            }
            | Penalty::PermanentBan {
                slash_amount: amount,
                ..
            } = penalty
            {
                let slash = (*amount).min(provider.stake);
                provider.stake -= slash;
                provider.total_slashed += slash;
            }
        }

        // Update status for bans
        match penalty {
            Penalty::TemporaryBan { duration, .. } => {
                let until = current_timestamp_ms() + duration * 1000;
                provider.status = ProviderStatus::TemporarilyBanned { until };
            }
            Penalty::PermanentBan { reason, .. } => {
                provider.status = ProviderStatus::PermanentlyBanned {
                    reason: reason.clone(),
                };
            }
            _ => {}
        }

        // Update last active
        provider.last_active = current_timestamp_ms();
    }

    /// Apply daily reputation decay for inactive providers.
    pub fn apply_decay(&self, provider: &mut ProviderState, days_inactive: f64) {
        if days_inactive > 0.0 {
            let decay = self.decay_rate.powf(days_inactive);
            provider.reputation *= decay;
            provider.reputation = provider.reputation.max(self.min_reputation);
        }
    }

    /// Get starting reputation.
    pub fn starting_reputation(&self) -> f64 {
        self.starting_reputation
    }

    /// Check if provider has enough reputation to participate.
    pub fn can_participate(&self, provider: &ProviderState) -> bool {
        provider.reputation >= self.min_reputation && provider.is_active()
    }
}

impl Default for ReputationManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Combined economics engine for rewards and penalties.
pub struct EconomicsEngine {
    reward_calculator: RewardCalculator,
    penalty_calculator: PenaltyCalculator,
    reputation_manager: ReputationManager,
}

impl EconomicsEngine {
    /// Create a new economics engine.
    pub fn new() -> Self {
        Self {
            reward_calculator: RewardCalculator::new(),
            penalty_calculator: PenaltyCalculator::new(),
            reputation_manager: ReputationManager::new(),
        }
    }

    /// Create with custom components.
    pub fn with_components(
        reward_calculator: RewardCalculator,
        penalty_calculator: PenaltyCalculator,
        reputation_manager: ReputationManager,
    ) -> Self {
        Self {
            reward_calculator,
            penalty_calculator,
            reputation_manager,
        }
    }

    /// Process a challenge result and update provider state.
    pub fn process_result(
        &self,
        result: &ChallengeResult,
        provider: &mut ProviderState,
    ) -> EconomicOutcome {
        let reward = if result.passed {
            self.reward_calculator.calculate_reward(result, provider)
        } else {
            0
        };

        let penalty = if !result.passed {
            self.penalty_calculator.calculate_penalty(result, provider)
        } else {
            Penalty::Warning // No penalty for success
        };

        // Update reputation and stats
        self.reputation_manager
            .update_reputation(provider, result, &penalty, reward);

        EconomicOutcome {
            reward,
            penalty,
            new_reputation: provider.reputation,
            new_stake: provider.stake,
        }
    }

    /// Get the reward calculator.
    pub fn rewards(&self) -> &RewardCalculator {
        &self.reward_calculator
    }

    /// Get the penalty calculator.
    pub fn penalties(&self) -> &PenaltyCalculator {
        &self.penalty_calculator
    }

    /// Get the reputation manager.
    pub fn reputation(&self) -> &ReputationManager {
        &self.reputation_manager
    }
}

impl Default for EconomicsEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Outcome of economic processing.
#[derive(Debug, Clone)]
pub struct EconomicOutcome {
    /// Reward earned (0 if failed)
    pub reward: u64,
    /// Penalty applied
    pub penalty: Penalty,
    /// New reputation after update
    pub new_reputation: f64,
    /// New stake after any slashing
    pub new_stake: u64,
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
    use uuid::Uuid;

    fn make_provider(stake: u64) -> ProviderState {
        ProviderState::new("test-provider".to_string(), stake)
    }

    fn make_result(passed: bool, score: f64, tee: bool) -> ChallengeResult {
        ChallengeResult {
            challenge_id: Uuid::new_v4(),
            provider_id: "test-provider".to_string(),
            passed,
            performance_score: score,
            tee_verified: tee,
            verified_at: current_timestamp_ms(),
            failure_reason: if passed {
                None
            } else {
                Some("failed".to_string())
            },
        }
    }

    #[test]
    fn test_reward_calculation() {
        let calc = RewardCalculator::new();
        let provider = make_provider(1_000_000);
        let result = make_result(true, 1.0, false);

        let reward = calc.calculate_reward(&result, &provider);
        assert!(reward > 0);
        assert_eq!(reward, calc.config().base_reward);
    }

    #[test]
    fn test_reward_with_tee_bonus() {
        let calc = RewardCalculator::new();
        let provider = make_provider(1_000_000);

        let result_no_tee = make_result(true, 1.0, false);
        let result_with_tee = make_result(true, 1.0, true);

        let reward_no_tee = calc.calculate_reward(&result_no_tee, &provider);
        let reward_with_tee = calc.calculate_reward(&result_with_tee, &provider);

        assert!(reward_with_tee > reward_no_tee);
    }

    #[test]
    fn test_reward_with_performance_bonus() {
        let calc = RewardCalculator::new();
        let provider = make_provider(1_000_000);

        let result_normal = make_result(true, 1.0, false);
        let result_excellent = make_result(true, 1.5, false);

        let reward_normal = calc.calculate_reward(&result_normal, &provider);
        let reward_excellent = calc.calculate_reward(&result_excellent, &provider);

        assert!(reward_excellent > reward_normal);
    }

    #[test]
    fn test_penalty_grace_period() {
        let calc = PenaltyCalculator::new();
        let result = make_result(false, 0.0, false);
        let provider = ProviderState {
            total_challenges: 1, // Within grace period
            ..make_provider(1_000_000)
        };

        let penalty = calc.calculate_penalty(&result, &provider);
        assert!(matches!(penalty, Penalty::Warning));
    }

    #[test]
    fn test_penalty_slash() {
        let calc = PenaltyCalculator::new();
        let result = make_result(false, 0.0, false);
        let provider = ProviderState {
            total_challenges: 10, // Past grace period
            consecutive_failures: 1,
            ..make_provider(1_000_000)
        };

        let penalty = calc.calculate_penalty(&result, &provider);
        assert!(matches!(penalty, Penalty::Slash { .. }));
    }

    #[test]
    fn test_penalty_temp_ban() {
        let calc = PenaltyCalculator::new();
        let result = make_result(false, 0.0, false);
        let provider = ProviderState {
            total_challenges: 10,
            consecutive_failures: 5, // Threshold for temp ban
            ..make_provider(1_000_000)
        };

        let penalty = calc.calculate_penalty(&result, &provider);
        assert!(matches!(penalty, Penalty::TemporaryBan { .. }));
    }

    #[test]
    fn test_penalty_perm_ban() {
        let calc = PenaltyCalculator::new();
        let result = make_result(false, 0.0, false);
        let provider = ProviderState {
            total_challenges: 30,
            consecutive_failures: 20, // Threshold for perm ban
            ..make_provider(1_000_000)
        };

        let penalty = calc.calculate_penalty(&result, &provider);
        assert!(matches!(penalty, Penalty::PermanentBan { .. }));
    }

    #[test]
    fn test_reputation_increase_on_success() {
        let manager = ReputationManager::new();
        let mut provider = make_provider(1_000_000);
        let initial_rep = provider.reputation;

        let result = make_result(true, 1.2, false);
        let penalty = Penalty::Warning;

        manager.update_reputation(&mut provider, &result, &penalty, 100);

        assert!(provider.reputation > initial_rep);
    }

    #[test]
    fn test_reputation_decrease_on_failure() {
        let manager = ReputationManager::new();
        let mut provider = make_provider(1_000_000);
        let initial_rep = provider.reputation;

        let result = make_result(false, 0.0, false);
        let penalty = Penalty::Slash {
            amount: 10_000,
            reputation_penalty: 10.0,
        };

        manager.update_reputation(&mut provider, &result, &penalty, 0);

        assert!(provider.reputation < initial_rep);
    }

    #[test]
    fn test_economics_engine() {
        let engine = EconomicsEngine::new();
        let mut provider = make_provider(1_000_000);

        // Successful challenge
        let result = make_result(true, 1.1, false);
        let outcome = engine.process_result(&result, &mut provider);

        assert!(outcome.reward > 0);
        assert_eq!(provider.challenges_passed, 1);
        assert_eq!(provider.challenge_streak, 1);
    }

    #[test]
    fn test_stake_slashing() {
        let engine = EconomicsEngine::new();
        let mut provider = ProviderState {
            total_challenges: 10,
            consecutive_failures: 2,
            ..make_provider(1_000_000)
        };
        let initial_stake = provider.stake;

        let result = make_result(false, 0.0, false);
        let _outcome = engine.process_result(&result, &mut provider);

        assert!(provider.stake < initial_stake);
        assert!(provider.total_slashed > 0);
    }
}
