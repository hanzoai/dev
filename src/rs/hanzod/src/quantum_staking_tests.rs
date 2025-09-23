//! Unit tests for quantum staking module

#[cfg(test)]
mod quantum_staking_unit_tests {
    use crate::quantum_staking::*;
    use chrono::Utc;

    #[test]
    fn test_quantum_staking_config() {
        let config = QuantumStakingConfig {
            network_quantum_threshold: 1_000_000.0,
            min_stake_amount: 100.0,
            quantum_fee_percent: 1.0,
            zoo_labs_donation_percent: 1.0,
            user_opted_in_carbon_offset: true,
            p_chain_endpoint: "https://api.lux.network/ext/bc/P".to_string(),
        };

        assert_eq!(config.network_quantum_threshold, 1_000_000.0);
        assert_eq!(config.quantum_fee_percent, 1.0);
        assert_eq!(config.zoo_labs_donation_percent, 1.0);
        assert!(config.user_opted_in_carbon_offset);
    }

    #[test]
    fn test_staking_position() {
        let position = StakingPosition {
            staker_id: "staker-123".to_string(),
            amount_lux: 50000.0,
            stake_time: Utc::now(),
            lock_duration_days: 30,
            is_quantum_eligible: false,
            carbon_offset_opted_in: true,
            rewards_earned_lux: 100.0,
            last_reward_time: Utc::now(),
        };

        assert_eq!(position.amount_lux, 50000.0);
        assert!(!position.is_quantum_eligible);
        assert!(position.carbon_offset_opted_in);
        assert_eq!(position.rewards_earned_lux, 100.0);
    }

    #[test]
    fn test_quantum_state() {
        let state = QuantumState {
            network_staked_total: 1_500_000.0,
            quantum_achieved: true,
            participating_nodes: 15,
            finality_latency_ms: 100,
            carbon_offset_donations_usd: 50000.0,
            zoo_labs_address: "0xzoo...".to_string(),
        };

        assert!(state.quantum_achieved);
        assert_eq!(state.network_staked_total, 1_500_000.0);
        assert_eq!(state.participating_nodes, 15);
        assert_eq!(state.carbon_offset_donations_usd, 50000.0);
    }

    #[tokio::test]
    async fn test_quantum_staking_manager_creation() {
        let config = QuantumStakingConfig::default();
        let manager = QuantumStakingManager::new(config).await;
        
        assert!(manager.is_ok());
        let mgr = manager.unwrap();
        assert_eq!(mgr.config.network_quantum_threshold, 1_000_000.0);
    }

    #[tokio::test]
    async fn test_stake_lux() {
        let config = QuantumStakingConfig::default();
        let manager = QuantumStakingManager::new(config).await.unwrap();

        let result = manager.stake_lux("staker-1", 10000.0, 30, false).await;
        assert!(result.is_ok());
        
        let stake_id = result.unwrap();
        assert!(stake_id.starts_with("stake-"));
    }

    #[tokio::test]
    async fn test_check_quantum_status() {
        let config = QuantumStakingConfig::default();
        let manager = QuantumStakingManager::new(config).await.unwrap();

        let status = manager.check_quantum_status().await;
        assert!(status.is_ok());
        
        let state = status.unwrap();
        // In offline mode, should not have quantum achieved initially
        assert!(!state.quantum_achieved);
    }

    #[tokio::test]
    async fn test_process_quantum_fee() {
        let config = QuantumStakingConfig::default();
        let manager = QuantumStakingManager::new(config).await.unwrap();

        // First stake enough to achieve quantum
        let _ = manager.stake_lux("staker-1", 1_000_000.0, 365, false).await;
        
        let result = manager.process_quantum_fee(100.0).await;
        assert!(result.is_ok());
        
        let (fee, donation) = result.unwrap();
        assert_eq!(fee, 1.0); // 1% of 100
        assert_eq!(donation, 0.0); // No donation if not opted in
    }

    #[tokio::test]
    async fn test_process_quantum_fee_with_carbon_offset() {
        let mut config = QuantumStakingConfig::default();
        config.user_opted_in_carbon_offset = true;
        
        let manager = QuantumStakingManager::new(config).await.unwrap();

        // Stake enough for quantum
        let _ = manager.stake_lux("staker-1", 1_000_000.0, 365, true).await;
        
        let result = manager.process_quantum_fee(100.0).await;
        assert!(result.is_ok());
        
        let (fee, donation) = result.unwrap();
        assert_eq!(fee, 1.0); // 1% of 100
        assert_eq!(donation, 1.0); // 1% donation when opted in
    }

    #[tokio::test]
    async fn test_calculate_staking_rewards() {
        let config = QuantumStakingConfig::default();
        let manager = QuantumStakingManager::new(config).await.unwrap();

        // Stake some LUX
        let stake_id = manager.stake_lux("staker-1", 10000.0, 30, false).await.unwrap();
        
        // Calculate rewards
        let rewards = manager.calculate_staking_rewards(&stake_id).await;
        assert!(rewards.is_ok());
        
        let reward_amount = rewards.unwrap();
        // Should have some rewards (even if small)
        assert!(reward_amount >= 0.0);
    }

    #[test]
    fn test_carbon_offset_opt_in() {
        let mut config = QuantumStakingConfig::default();
        
        // Default should be false
        assert!(!config.user_opted_in_carbon_offset);
        
        // User opts in
        config.user_opted_in_carbon_offset = true;
        assert!(config.user_opted_in_carbon_offset);
        
        // Donation percent should remain 1%
        assert_eq!(config.zoo_labs_donation_percent, 1.0);
    }

    #[test]
    fn test_quantum_threshold_validation() {
        let config = QuantumStakingConfig::default();
        
        // Test threshold is 1M LUX
        assert_eq!(config.network_quantum_threshold, 1_000_000.0);
        
        // Test various stake amounts against threshold
        let small_stake = 1000.0;
        let medium_stake = 100_000.0;
        let large_stake = 500_000.0;
        let quantum_stake = 1_000_000.0;
        
        assert!(small_stake < config.network_quantum_threshold);
        assert!(medium_stake < config.network_quantum_threshold);
        assert!(large_stake < config.network_quantum_threshold);
        assert!(quantum_stake >= config.network_quantum_threshold);
    }
}
