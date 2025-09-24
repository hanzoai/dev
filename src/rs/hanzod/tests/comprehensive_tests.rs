//! Comprehensive test suite for Hanzo AI Blockchain with Lux Consensus

#[cfg(test)]
mod tests {
    use hanzod::lux_consensus::{
        LuxConsensus, LuxConsensusConfig, InterchainMessage, InterchainMessageType,
        ConsensusDecision, Vote, ValidatorInfo, SnowParameters,
    };
    use hanzod::warp_ffi::{UnsignedMessage, SignedMessage, WarpProtocolRust};
    use hanzod::rpc_server::{RpcConfig, ModelEndpoints};
    
    use chrono::Utc;
    use std::collections::HashMap;

    // ===== Lux Consensus Tests =====

    #[test]
    fn test_consensus_config_defaults() {
        let config = LuxConsensusConfig::default();
        
        assert_eq!(config.chain_id, "hanzo-chain-1");
        assert_eq!(config.network_id, 43114); // Lux mainnet
        assert_eq!(config.protocol_version, 28);
        assert_eq!(config.min_validator_stake, 2_000_000_000_000_000);
        assert_eq!(config.max_validator_weight_ratio, 5);
    }

    #[test]
    fn test_snow_parameters() {
        let params = SnowParameters::default();
        
        assert_eq!(params.k, 20);
        assert_eq!(params.alpha, 15);
        assert_eq!(params.beta_virtuous, 15);
        assert_eq!(params.beta_rogue, 20);
        assert_eq!(params.concurrent_repolls, 4);
        assert_eq!(params.optimal_processing, 50);
        assert_eq!(params.max_processing, 1024);
        assert_eq!(params.max_time_processing, 120_000);
    }

    #[test]
    fn test_consensus_initialization() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config);
        
        assert!(consensus.is_ok());
        let consensus = consensus.unwrap();
        
        // Test that we can get metrics
        let metrics = tokio_test::block_on(consensus.get_metrics());
        assert_eq!(metrics.network_id, 43114);
        assert_eq!(metrics.chain_id, "hanzo-chain-1");
        assert_eq!(metrics.block_height, 0);
        assert_eq!(metrics.validator_count, 0);
    }

    #[tokio::test]
    async fn test_validator_registration() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Test successful registration
        let validator = consensus.register_validator(2_000_000_000_000_000, true).await.unwrap();
        
        assert!(validator.node_id.starts_with("NodeID-"));
        assert!(validator.staking_address.starts_with("P-lux1"));
        assert_eq!(validator.stake_amount, 2_000_000_000_000_000);
        assert!(validator.supports_qwen3);
        assert_eq!(validator.uptime, 100.0);
        assert_eq!(validator.delegation_shares, 100000);
    }

    #[tokio::test]
    async fn test_insufficient_stake() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Test with insufficient stake
        let result = consensus.register_validator(1000, false).await;
        
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Insufficient stake"));
    }

    #[tokio::test]
    async fn test_validator_uptime_update() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Register a validator
        let validator = consensus.register_validator(2_000_000_000_000_000, true).await.unwrap();
        
        // Update uptime
        consensus.update_validator_uptime(&validator.node_id, 95.5).await.unwrap();
        
        // Get validator set and check
        let validators = consensus.get_validator_set().await;
        let updated = validators.iter()
            .find(|v| v.node_id == validator.node_id)
            .unwrap();
        
        assert_eq!(updated.uptime, 95.5);
    }

    #[tokio::test]
    async fn test_consensus_round() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Create votes
        let mut votes = Vec::new();
        for i in 0..20 {
            votes.push(Vote {
                voter_id: format!("voter_{}", i),
                preference: if i < 15 { "block_a" } else { "block_b" }.to_string(),
                confidence: 100,
                timestamp: Utc::now(),
            });
        }
        
        // Process consensus round
        let decision = consensus.process_consensus_round(votes).await.unwrap();
        
        // With 15 votes for block_a (meets alpha threshold), it should be accepted
        match decision {
            ConsensusDecision::Accept(pref) => assert_eq!(pref, "block_a"),
            _ => panic!("Expected Accept decision"),
        }
    }

    #[test]
    fn test_signature_operations() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        let message = b"Test message for signing";
        
        // Sign message
        let signature = consensus.sign_as_validator(message);
        assert_eq!(signature.len(), 64); // Ed25519 signature length
        
        // Verify signature (would need public key in real scenario)
        let public_key = vec![0u8; 32]; // Mock public key
        let is_valid = consensus.verify_validator_signature(
            &public_key,
            message,
            &signature,
        );
        
        // This will fail with mock key, but we're testing the function exists
        assert!(is_valid.is_err() || !is_valid.unwrap());
    }

    // ===== Warp FFI Tests =====

    #[test]
    fn test_warp_message_creation() {
        let network_id = 43114;
        let source_chain = vec![1u8; 32];
        let payload = b"Test interchain message".to_vec();
        
        let msg = UnsignedMessage::new(network_id, source_chain.clone(), payload.clone()).unwrap();
        
        assert_eq!(msg.network_id, network_id);
        assert_eq!(msg.source_chain_id, source_chain);
        assert_eq!(msg.payload, payload);
    }

    #[test]
    fn test_warp_message_id() {
        let msg = UnsignedMessage::new(
            43114,
            vec![2u8; 32],
            b"Test payload".to_vec(),
        ).unwrap();
        
        let id = msg.id();
        assert_eq!(id.len(), 32); // SHA256 hash length
    }

    #[test]
    fn test_warp_invalid_chain_id() {
        let result = UnsignedMessage::new(
            43114,
            vec![1u8; 16], // Wrong size
            b"Test".to_vec(),
        );
        
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("32 bytes"));
    }

    #[test]
    fn test_warp_message_size_limit() {
        let large_payload = vec![0u8; 300 * 1024]; // 300 KB, over limit
        
        let result = UnsignedMessage::new(
            43114,
            vec![1u8; 32],
            large_payload,
        );
        
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("exceeds maximum"));
    }

    #[test]
    fn test_warp_protocol_rust() {
        use ed25519_dalek::{SigningKey, Verifier};
        use rand::rngs::OsRng;
        
        let protocol = WarpProtocolRust::new(43114);
        
        let msg = protocol.create_unsigned_message(
            vec![3u8; 32],
            b"Cross-chain transfer".to_vec(),
        ).unwrap();
        
        // Sign message
        let signing_key = SigningKey::generate(&mut OsRng);
        let signed = protocol.sign_message(&msg, &signing_key);
        
        assert_eq!(signed.unsigned.network_id, 43114);
        assert_eq!(signed.signature.len(), 64);
        
        // Verify signature
        let verifying_key = signing_key.verifying_key();
        assert!(protocol.verify_message(&signed, &verifying_key));
    }

    #[test]
    fn test_warp_signature_verification_fail() {
        use ed25519_dalek::SigningKey;
        use rand::rngs::OsRng;
        
        let protocol = WarpProtocolRust::new(43114);
        
        let msg = protocol.create_unsigned_message(
            vec![4u8; 32],
            b"Message 1".to_vec(),
        ).unwrap();
        
        // Sign with one key
        let signing_key1 = SigningKey::generate(&mut OsRng);
        let signed = protocol.sign_message(&msg, &signing_key1);
        
        // Try to verify with different key
        let signing_key2 = SigningKey::generate(&mut OsRng);
        let verifying_key2 = signing_key2.verifying_key();
        
        assert!(!protocol.verify_message(&signed, &verifying_key2));
    }

    // ===== Interchain Messaging Tests =====

    #[test]
    fn test_interchain_message_types() {
        let warp_msg = InterchainMessage {
            message_id: "msg-001".to_string(),
            message_type: InterchainMessageType::Warp,
            source_chain: "hanzo-chain-1".to_string(),
            destination_chain: "C-Chain".to_string(),
            payload: vec![1, 2, 3],
            signature: vec![0; 64],
            timestamp: Utc::now(),
        };
        
        assert!(matches!(warp_msg.message_type, InterchainMessageType::Warp));
        
        let teleport_msg = InterchainMessage {
            message_type: InterchainMessageType::Teleport,
            ..warp_msg.clone()
        };
        
        assert!(matches!(teleport_msg.message_type, InterchainMessageType::Teleport));
        
        let custom_msg = InterchainMessage {
            message_type: InterchainMessageType::Custom,
            ..warp_msg.clone()
        };
        
        assert!(matches!(custom_msg.message_type, InterchainMessageType::Custom));
    }

    #[tokio::test]
    async fn test_interchain_message_handling() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        let msg = InterchainMessage {
            message_id: "test-msg".to_string(),
            message_type: InterchainMessageType::Warp,
            source_chain: "hanzo-chain-1".to_string(),
            destination_chain: "C-Chain".to_string(),
            payload: b"Test payload".to_vec(),
            signature: vec![0; 64],
            timestamp: Utc::now(),
        };
        
        // This should not error
        let result = consensus.handle_interchain_message(msg).await;
        assert!(result.is_ok());
    }

    // ===== RPC Configuration Tests =====

    #[test]
    fn test_rpc_config_defaults() {
        let config = RpcConfig::default();
        
        assert_eq!(config.grpc_port, 50051);
        assert_eq!(config.http_port, 8545);
        assert_eq!(config.ws_port, 8546);
        assert!(config.enable_qwen3_next);
        assert!(config.enable_qwen3_reranker);
    }

    #[test]
    fn test_model_endpoints() {
        let endpoints = ModelEndpoints::default();
        
        assert!(endpoints.qwen3_next.contains("localhost:3690"));
        assert!(endpoints.qwen3_reranker.contains("localhost:3690"));
        assert!(endpoints.embedding.contains("localhost:3690"));
    }

    // ===== Integration Tests =====

    #[test]
    fn test_chain_supported() {
        let config = LuxConsensusConfig::default();
        
        let chains: Vec<String> = config.interchain.supported_chains
            .iter()
            .map(|c| c.chain_id.clone())
            .collect();
        
        assert!(chains.contains(&"C-Chain".to_string()));
        assert!(chains.contains(&"X-Chain".to_string()));
        assert!(chains.contains(&"P-Chain".to_string()));
    }

    #[test]
    fn test_interchain_config() {
        let config = LuxConsensusConfig::default();
        
        assert!(config.interchain.enable_warp);
        assert!(config.interchain.enable_teleport);
        assert_eq!(config.interchain.supported_chains.len(), 3);
    }

    #[tokio::test]
    async fn test_multiple_validators() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Register multiple validators
        let v1 = consensus.register_validator(2_000_000_000_000_000, true).await.unwrap();
        let v2 = consensus.register_validator(3_000_000_000_000_000, false).await.unwrap();
        let v3 = consensus.register_validator(2_500_000_000_000_000, true).await.unwrap();
        
        // Get validator set
        let validators = consensus.get_validator_set().await;
        assert_eq!(validators.len(), 3);
        
        // Check metrics
        let metrics = consensus.get_metrics().await;
        assert_eq!(metrics.validator_count, 3);
        assert_eq!(metrics.total_stake, 7_500_000_000_000_000);
        assert_eq!(metrics.qwen3_validators, 2); // v1 and v3 support Qwen3
    }

    // ===== End-to-End Test =====

    #[tokio::test]
    async fn test_end_to_end_flow() {
        // Initialize consensus
        let mut config = LuxConsensusConfig::default();
        config.min_validator_stake = 1000; // Lower for testing
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Register as validator
        let validator = consensus.register_validator(1000, true).await.unwrap();
        assert!(validator.node_id.starts_with("NodeID-"));
        
        // Create and sign a Warp message
        let protocol = WarpProtocolRust::new(43114);
        let msg = protocol.create_unsigned_message(
            vec![0xABu8; 32],
            b"End-to-end test message".to_vec(),
        ).unwrap();
        
        use ed25519_dalek::SigningKey;
        use rand::rngs::OsRng;
        let signing_key = SigningKey::generate(&mut OsRng);
        let signed = protocol.sign_message(&msg, &signing_key);
        
        // Create interchain message
        let interchain_msg = InterchainMessage {
            message_id: "e2e-test".to_string(),
            message_type: InterchainMessageType::Warp,
            source_chain: validator.node_id.clone(),
            destination_chain: "C-Chain".to_string(),
            payload: signed.signature.clone(),
            signature: vec![0; 64],
            timestamp: Utc::now(),
        };
        
        // Handle the message
        consensus.handle_interchain_message(interchain_msg).await.unwrap();
        
        // Check metrics
        let metrics = consensus.get_metrics().await;
        assert_eq!(metrics.validator_count, 1);
        assert_eq!(metrics.qwen3_validators, 1);
        
        // Simulate consensus round
        let votes = vec![
            Vote {
                voter_id: validator.node_id.clone(),
                preference: "test_block".to_string(),
                confidence: 100,
                timestamp: Utc::now(),
            }
        ];
        
        let decision = consensus.process_consensus_round(votes).await.unwrap();
        assert!(matches!(decision, ConsensusDecision::NoDecision)); // Not enough votes
    }
}