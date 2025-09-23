//! Unit tests for blockchain module

#[cfg(test)]
mod blockchain_unit_tests {
    use crate::blockchain::*;
    use std::path::PathBuf;

    #[test]
    fn test_transaction_types() {
        // Test each transaction type can be created
        let container_tx = TransactionType::ContainerCreate {
            image: "alpine:latest".to_string(),
            config_hash: "abc123".to_string(),
        };

        let state_change_tx = TransactionType::ContainerStateChange {
            container_id: "container-123".to_string(),
            new_state: "running".to_string(),
        };

        let workload_tx = TransactionType::WorkloadSchedule {
            workload_id: "work-456".to_string(),
            resource_allocation: "2cpu,4gb".to_string(),
        };

        let provider_tx = TransactionType::ProviderRegistration {
            provider_id: "provider-789".to_string(),
            x_chain_address: "X-lux1test".to_string(),
            resources: r#"{"cpu": 10, "gpu": 0}"#.to_string(),
        };

        let consumption_start = TransactionType::ConsumptionStart {
            consumption_id: "cons-001".to_string(),
            provider_id: "prov-001".to_string(),
            consumer_address: "consumer-addr".to_string(),
            estimated_usd: 10.0,
        };

        let settlement = TransactionType::ConsumptionSettlement {
            consumption_id: "cons-001".to_string(),
            provider_earning_usd: 9.8,
            protocol_fee_usd: 0.2,
            x_chain_tx: "0xabc".to_string(),
        };

        let token_transfer = TransactionType::TokenTransfer {
            from: "addr1".to_string(),
            to: "addr2".to_string(),
            amount: 100.0,
            token_type: "USD".to_string(),
        };

        // Verify they can be matched
        match container_tx {
            TransactionType::ContainerCreate { image, .. } => {
                assert_eq!(image, "alpine:latest");
            }
            _ => panic!("Wrong type"),
        }

        match consumption_start {
            TransactionType::ConsumptionStart { estimated_usd, .. } => {
                assert_eq!(estimated_usd, 10.0);
            }
            _ => panic!("Wrong type"),
        }

        match settlement {
            TransactionType::ConsumptionSettlement { provider_earning_usd, protocol_fee_usd, .. } => {
                assert_eq!(provider_earning_usd, 9.8);
                assert_eq!(protocol_fee_usd, 0.2);
            }
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_transaction_status() {
        let pending = TransactionStatus::Pending;
        let confirmed = TransactionStatus::Confirmed;
        let failed = TransactionStatus::Failed("error".to_string());
        let finalized = TransactionStatus::Finalized;

        assert!(matches!(pending, TransactionStatus::Pending));
        assert!(matches!(confirmed, TransactionStatus::Confirmed));
        assert!(matches!(finalized, TransactionStatus::Finalized));

        match failed {
            TransactionStatus::Failed(msg) => assert_eq!(msg, "error"),
            _ => panic!("Expected Failed status"),
        }
    }

    #[test]
    fn test_network_types() {
        let mainnet = NetworkType::Mainnet;
        let testnet = NetworkType::Testnet;
        let local = NetworkType::Local;
        let custom = NetworkType::Custom("mynet".to_string());

        assert!(matches!(mainnet, NetworkType::Mainnet));
        assert!(matches!(testnet, NetworkType::Testnet));
        assert!(matches!(local, NetworkType::Local));

        match custom {
            NetworkType::Custom(name) => assert_eq!(name, "mynet"),
            _ => panic!("Expected Custom network"),
        }
    }

    #[test]
    fn test_blockchain_config_default() {
        let config = BlockchainConfig::default();

        assert!(matches!(config.network, NetworkType::Mainnet));
        assert_eq!(config.node_endpoint, "https://api.lux.network");
        assert_eq!(config.chain_id, "lux-mainnet");
        assert_eq!(config.quantum_finality, true);
        assert_eq!(config.keystore_path, PathBuf::from("/Users/z/.lux/keystore"));
    }

    #[test]
    fn test_key_manager_creation() {
        let keystore_path = PathBuf::from("/tmp/test_keystore");
        let key_manager = KeyManager::new(&keystore_path);

        assert!(key_manager.is_ok());
        let km = key_manager.unwrap();
        assert!(km.address.starts_with("lux"));
    }

    #[test]
    fn test_key_manager_sign_transaction() {
        let keystore_path = PathBuf::from("/tmp/test_keystore");
        let key_manager = KeyManager::new(&keystore_path).unwrap();

        let tx_data = b"test transaction data";
        let signature = key_manager.sign_transaction(tx_data);

        assert!(signature.is_ok());
        let sig = signature.unwrap();
        assert_eq!(sig.len(), 64); // Expected signature length
    }

    #[tokio::test]
    async fn test_lux_consensus_creation() {
        let mut config = BlockchainConfig::default();
        config.network = NetworkType::Local; // Use local for testing

        let consensus = LuxConsensus::new(config).await;
        assert!(consensus.is_ok());

        let lux = consensus.unwrap();
        assert_eq!(lux.key_manager.address, "lux1test...");
    }

    #[tokio::test]
    async fn test_submit_transaction_offline() {
        let mut config = BlockchainConfig::default();
        config.network = NetworkType::Local;

        let consensus = LuxConsensus::new(config).await.unwrap();

        let tx = TransactionType::ContainerCreate {
            image: "test:latest".to_string(),
            config_hash: "hash123".to_string(),
        };

        let tx_hash = consensus.submit_transaction(tx).await;
        assert!(tx_hash.is_ok());

        let hash = tx_hash.unwrap();
        assert!(hash.starts_with("0x"));
    }

    #[tokio::test]
    async fn test_wait_for_finality_offline() {
        let mut config = BlockchainConfig::default();
        config.network = NetworkType::Local;
        config.quantum_finality = true;

        let consensus = LuxConsensus::new(config).await.unwrap();

        // In offline mode, this should simulate finality
        let result = consensus.wait_for_finality("0x123abc").await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_get_block_height() {
        let mut config = BlockchainConfig::default();
        config.network = NetworkType::Local;

        let consensus = LuxConsensus::new(config).await.unwrap();

        let height = consensus.get_block_height().await;
        assert!(height.is_ok());
        assert_eq!(height.unwrap(), 0); // In offline mode
    }
}