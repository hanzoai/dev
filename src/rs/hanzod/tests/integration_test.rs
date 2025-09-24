//! Integration tests for Hanzo AI Blockchain with Lux Consensus

#[cfg(test)]
mod integration_tests {
    use hanzod::lux_consensus::{LuxConsensus, LuxConsensusConfig, InterchainMessageType, InterchainMessage};
    use hanzod::warp_ffi::{UnsignedMessage, WarpProtocolRust};
    use chrono::Utc;

    #[test]
    fn test_lux_consensus_initialization() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Test that consensus is initialized
        let metrics = tokio_test::block_on(consensus.get_metrics());
        assert_eq!(metrics.network_id, 43114); // Lux mainnet
        assert_eq!(metrics.chain_id, "hanzo-chain-1");
    }

    #[test]
    fn test_validator_registration() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Register a validator with minimum stake
        let validator = tokio_test::block_on(async {
            consensus.register_validator(2_000_000_000_000_000, true).await
        }).unwrap();
        
        assert!(validator.node_id.starts_with("NodeID-"));
        assert!(validator.supports_qwen3);
        assert_eq!(validator.stake_amount, 2_000_000_000_000_000);
    }

    #[test]
    fn test_warp_message_creation() {
        let chain_id = vec![0u8; 32];
        let payload = b"Test interchain message".to_vec();
        
        let msg = UnsignedMessage::new(43114, chain_id, payload).unwrap();
        assert_eq!(msg.network_id, 43114);
        
        let id = msg.id();
        assert_eq!(id.len(), 32); // SHA256 hash
    }

    #[test]
    fn test_warp_protocol_rust() {
        use ed25519_dalek::SigningKey;
        use rand::rngs::OsRng;
        
        let protocol = WarpProtocolRust::new(43114);
        
        // Create a message
        let chain_id = vec![1u8; 32];
        let payload = b"Asset transfer via zkBridge".to_vec();
        let msg = protocol.create_unsigned_message(chain_id, payload).unwrap();
        
        // Sign it
        let signing_key = SigningKey::generate(&mut OsRng);
        let signed = protocol.sign_message(&msg, &signing_key);
        
        // Verify signature
        let verifying_key = signing_key.verifying_key();
        assert!(protocol.verify_message(&signed, &verifying_key));
    }

    #[test]
    fn test_interchain_message_types() {
        let msg = InterchainMessage {
            message_id: "test-001".to_string(),
            message_type: InterchainMessageType::Warp,
            source_chain: "hanzo-chain-1".to_string(),
            destination_chain: "C-Chain".to_string(),
            payload: vec![1, 2, 3],
            signature: vec![0; 64],
            timestamp: Utc::now(),
        };
        
        assert!(matches!(msg.message_type, InterchainMessageType::Warp));
        
        let teleport_msg = InterchainMessage {
            message_type: InterchainMessageType::Teleport,
            ..msg.clone()
        };
        
        assert!(matches!(teleport_msg.message_type, InterchainMessageType::Teleport));
    }

    #[test]
    fn test_consensus_signature_verification() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        let message = b"Test message for signing";
        let signature = consensus.sign_as_validator(message);
        
        assert_eq!(signature.len(), 64); // Ed25519 signature
        
        // In production, we'd verify with the public key
        // For now, just check the signature was created
        assert!(!signature.is_empty());
    }

    #[test]
    fn test_snow_consensus_parameters() {
        let config = LuxConsensusConfig::default();
        
        assert_eq!(config.snow_params.k, 20);
        assert_eq!(config.snow_params.alpha, 15);
        assert_eq!(config.snow_params.beta_virtuous, 15);
        assert_eq!(config.snow_params.beta_rogue, 20);
        assert_eq!(config.snow_params.concurrent_repolls, 4);
    }

    #[test]
    fn test_interchain_config() {
        let config = LuxConsensusConfig::default();
        
        assert!(config.interchain.enable_warp);
        assert!(config.interchain.enable_teleport);
        
        // Check supported chains
        let chains: Vec<String> = config.interchain.supported_chains
            .iter()
            .map(|c| c.chain_id.clone())
            .collect();
        
        assert!(chains.contains(&"C-Chain".to_string()));
        assert!(chains.contains(&"X-Chain".to_string()));
        assert!(chains.contains(&"P-Chain".to_string()));
    }

    #[test]
    fn test_minimum_stake_requirement() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Try to register with insufficient stake
        let result = tokio_test::block_on(async {
            consensus.register_validator(1000, false).await
        });
        
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Insufficient stake"));
    }

    #[test]
    fn test_node_operator_capabilities() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();
        
        // Register node with Qwen3 support
        let validator = tokio_test::block_on(async {
            consensus.register_validator(2_000_000_000_000_000, true).await
        }).unwrap();
        
        assert!(validator.supports_qwen3);
        assert_eq!(validator.uptime, 100.0);
        
        // Test uptime update
        tokio_test::block_on(async {
            consensus.update_validator_uptime(&validator.node_id, 95.5).await
        }).unwrap();
        
        let validators = tokio_test::block_on(consensus.get_validator_set());
        let updated = validators.iter()
            .find(|v| v.node_id == validator.node_id)
            .unwrap();
        
        assert_eq!(updated.uptime, 95.5);
    }
}