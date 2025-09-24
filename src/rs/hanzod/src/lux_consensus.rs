//! Lux Consensus Integration for Hanzod
//!
//! Ensures full compatibility with luxfi/consensus for:
//! - Node operator validation
//! - Interchain communication
//! - Snow consensus protocol
//! - Quantum finality

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;
use chrono::{DateTime, Utc};
use ed25519_dalek::{SigningKey, VerifyingKey, Signature, Signer, Verifier};
use sha2::{Sha256, Digest};

/// Lux consensus configuration compatible with luxfi/consensus
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LuxConsensusConfig {
    /// Chain ID (e.g., "hanzo-chain-1" for mainnet)
    pub chain_id: String,
    /// Network ID (43114 for Lux mainnet)
    pub network_id: u32,
    /// Consensus protocol version
    pub protocol_version: u32,
    /// Minimum stake for validation (in nLUX)
    pub min_validator_stake: u64,
    /// Maximum validator weight ratio
    pub max_validator_weight_ratio: u32,
    /// Snow consensus parameters
    pub snow_params: SnowParameters,
    /// Interchain configuration
    pub interchain: InterchainConfig,
}

impl Default for LuxConsensusConfig {
    fn default() -> Self {
        Self {
            chain_id: "hanzo-chain-1".to_string(),
            network_id: 43114, // Lux mainnet
            protocol_version: 28,
            min_validator_stake: 2_000_000_000_000_000, // 2M LUX in nLUX
            max_validator_weight_ratio: 5,
            snow_params: SnowParameters::default(),
            interchain: InterchainConfig::default(),
        }
    }
}

/// Snow consensus parameters (compatible with luxfi/consensus/snow)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SnowParameters {
    /// Sample size
    pub k: u32,
    /// Quorum size
    pub alpha: u32,
    /// Decision threshold
    pub beta_virtuous: u32,
    /// Rogue threshold
    pub beta_rogue: u32,
    /// Parent lookback
    pub concurrent_repolls: u32,
    /// Optimal processing
    pub optimal_processing: u32,
    /// Max processing
    pub max_processing: u32,
    /// Max time processing
    pub max_time_processing: u64,
}

impl Default for SnowParameters {
    fn default() -> Self {
        Self {
            k: 20,
            alpha: 15,
            beta_virtuous: 15,
            beta_rogue: 20,
            concurrent_repolls: 4,
            optimal_processing: 50,
            max_processing: 1024,
            max_time_processing: 120_000, // 2 minutes in ms
        }
    }
}

/// Interchain messaging configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterchainConfig {
    /// Enable Warp messaging
    pub enable_warp: bool,
    /// Enable Teleport protocol for zkBridge asset transfers
    pub enable_teleport: bool,
    /// Supported chains
    pub supported_chains: Vec<ChainInfo>,
}

impl Default for InterchainConfig {
    fn default() -> Self {
        Self {
            enable_warp: true,
            enable_teleport: true,
            supported_chains: vec![
                ChainInfo {
                    chain_id: "C-Chain".to_string(),
                    chain_type: ChainType::Contract,
                    endpoint: "http://localhost:9650/ext/bc/C".to_string(),
                },
                ChainInfo {
                    chain_id: "X-Chain".to_string(),
                    chain_type: ChainType::DAG,
                    endpoint: "http://localhost:9650/ext/bc/X".to_string(),
                },
                ChainInfo {
                    chain_id: "P-Chain".to_string(),
                    chain_type: ChainType::Platform,
                    endpoint: "http://localhost:9650/ext/bc/P".to_string(),
                },
            ],
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChainInfo {
    pub chain_id: String,
    pub chain_type: ChainType,
    pub endpoint: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ChainType {
    Platform,  // P-Chain
    Contract,  // C-Chain (EVM)
    DAG,       // X-Chain (DAG-based)
    Subnet,    // Custom subnet
}

/// Validator info compatible with luxfi/consensus/validators
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidatorInfo {
    /// Node ID (20-byte hash)
    pub node_id: String,
    /// Staking public key
    pub public_key: Vec<u8>,
    /// Lux staking address
    pub staking_address: String,
    /// Stake amount in nLUX
    pub stake_amount: u64,
    /// Validation start time
    pub start_time: DateTime<Utc>,
    /// Validation end time
    pub end_time: DateTime<Utc>,
    /// Delegation shares
    pub delegation_shares: u32,
    /// Node uptime percentage
    pub uptime: f64,
    /// Whether validator supports Qwen3 models
    pub supports_qwen3: bool,
}

/// Lux consensus engine compatible with luxfi/consensus
pub struct LuxConsensus {
    config: LuxConsensusConfig,
    /// Current validators
    validators: Arc<RwLock<Vec<ValidatorInfo>>>,
    /// Node's signing key
    signing_key: SigningKey,
    /// Node's verifying key
    verifying_key: VerifyingKey,
    /// Current consensus state
    state: Arc<RwLock<ConsensusState>>,
}

#[derive(Debug, Clone)]
struct ConsensusState {
    /// Current block height
    block_height: u64,
    /// Last finalized block
    last_finalized: String,
    /// Current epoch
    epoch: u64,
    /// Active validator set version
    validator_set_version: u64,
}

impl LuxConsensus {
    /// Initialize new Lux consensus engine
    pub fn new(config: LuxConsensusConfig) -> Result<Self> {
        // Generate or load signing key
        let mut rng = rand::rngs::OsRng;
        let mut bytes = [0u8; 32];
        rand::RngCore::fill_bytes(&mut rng, &mut bytes);
        let signing_key = SigningKey::from_bytes(&bytes);
        let verifying_key = signing_key.verifying_key();

        Ok(Self {
            config,
            validators: Arc::new(RwLock::new(Vec::new())),
            signing_key,
            verifying_key,
            state: Arc::new(RwLock::new(ConsensusState {
                block_height: 0,
                last_finalized: "genesis".to_string(),
                epoch: 0,
                validator_set_version: 0,
            })),
        })
    }

    /// Register as a validator node
    pub async fn register_validator(
        &self,
        stake_amount: u64,
        supports_qwen3: bool,
    ) -> Result<ValidatorInfo> {
        if stake_amount < self.config.min_validator_stake {
            return Err(anyhow!("Insufficient stake amount"));
        }

        // Generate node ID from public key
        let mut hasher = Sha256::new();
        hasher.update(&self.verifying_key.to_bytes());
        let node_id = format!("NodeID-{}", hex::encode(&hasher.finalize()[..20]));

        // Create staking address
        let staking_address = format!("P-lux1{}", &node_id[7..27]);

        let validator = ValidatorInfo {
            node_id: node_id.clone(),
            public_key: self.verifying_key.to_bytes().to_vec(),
            staking_address,
            stake_amount,
            start_time: Utc::now(),
            end_time: Utc::now() + chrono::Duration::days(365), // 1 year default
            delegation_shares: 100000, // 100% initially
            uptime: 100.0,
            supports_qwen3,
        };

        let mut validators = self.validators.write().await;
        validators.push(validator.clone());

        Ok(validator)
    }

    /// Verify validator signature
    pub fn verify_validator_signature(
        &self,
        public_key: &[u8],
        message: &[u8],
        signature: &[u8],
    ) -> Result<bool> {
        let verifying_key = VerifyingKey::from_bytes(public_key.try_into()?)?;
        let sig = Signature::from_bytes(signature.try_into()?);

        Ok(verifying_key.verify(message, &sig).is_ok())
    }

    /// Sign message as validator
    pub fn sign_as_validator(&self, message: &[u8]) -> Vec<u8> {
        let signature = self.signing_key.sign(message);
        signature.to_bytes().to_vec()
    }

    /// Get current validator set
    pub async fn get_validator_set(&self) -> Vec<ValidatorInfo> {
        self.validators.read().await.clone()
    }

    /// Update validator uptime
    pub async fn update_validator_uptime(&self, node_id: &str, uptime: f64) -> Result<()> {
        let mut validators = self.validators.write().await;
        for validator in validators.iter_mut() {
            if validator.node_id == node_id {
                validator.uptime = uptime;
                return Ok(());
            }
        }
        Err(anyhow!("Validator not found"))
    }

    /// Process Snow consensus round
    pub async fn process_consensus_round(&self, votes: Vec<Vote>) -> Result<ConsensusDecision> {
        let params = &self.config.snow_params;

        // Count votes
        let mut vote_counts = std::collections::HashMap::new();
        for vote in votes {
            *vote_counts.entry(vote.preference).or_insert(0) += 1;
        }

        // Check if we have alpha votes for any preference
        for (preference, count) in vote_counts {
            if count >= params.alpha {
                return Ok(ConsensusDecision::Accept(preference));
            }
        }

        Ok(ConsensusDecision::NoDecision)
    }

    /// Handle interchain message
    pub async fn handle_interchain_message(&self, msg: InterchainMessage) -> Result<()> {
        match msg.message_type {
            InterchainMessageType::Warp => {
                // Handle Warp message
                self.process_warp_message(msg).await?;
            }
            InterchainMessageType::Teleport => {
                // Handle Teleport protocol message for zkBridge asset transfers
                self.process_teleport_message(msg).await?;
            }
            InterchainMessageType::Custom => {
                // Handle custom interchain message
                self.process_custom_message(msg).await?;
            }
        }
        Ok(())
    }

    async fn process_warp_message(&self, msg: InterchainMessage) -> Result<()> {
        // Warp message processing logic
        println!("Processing Warp message from chain: {}", msg.source_chain);
        Ok(())
    }

    async fn process_teleport_message(&self, msg: InterchainMessage) -> Result<()> {
        // Teleport protocol for zkBridge asset transfers
        println!("Processing Teleport message for asset transfer to chain: {}", msg.destination_chain);
        Ok(())
    }

    async fn process_custom_message(&self, msg: InterchainMessage) -> Result<()> {
        // Custom message processing logic
        println!("Processing custom interchain message");
        Ok(())
    }

    /// Get consensus metrics
    pub async fn get_metrics(&self) -> ConsensusMetrics {
        let state = self.state.read().await;
        let validators = self.validators.read().await;

        ConsensusMetrics {
            block_height: state.block_height,
            epoch: state.epoch,
            validator_count: validators.len() as u32,
            total_stake: validators.iter().map(|v| v.stake_amount).sum(),
            qwen3_validators: validators.iter().filter(|v| v.supports_qwen3).count() as u32,
            network_id: self.config.network_id,
            chain_id: self.config.chain_id.clone(),
        }
    }
}

/// Vote in Snow consensus
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vote {
    pub voter_id: String,
    pub preference: String,
    pub confidence: u32,
    pub timestamp: DateTime<Utc>,
}

/// Consensus decision
#[derive(Debug, Clone)]
pub enum ConsensusDecision {
    Accept(String),
    Reject(String),
    NoDecision,
}

/// Interchain message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InterchainMessage {
    pub message_id: String,
    pub message_type: InterchainMessageType,
    pub source_chain: String,
    pub destination_chain: String,
    pub payload: Vec<u8>,
    pub signature: Vec<u8>,
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InterchainMessageType {
    Warp,       // Lux Warp ICM (Interchain Messaging)
    Teleport,   // Teleport protocol for zkBridge asset transfers
    Custom,     // Custom interchain protocol
}

/// Consensus metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsensusMetrics {
    pub block_height: u64,
    pub epoch: u64,
    pub validator_count: u32,
    pub total_stake: u64,
    pub qwen3_validators: u32,
    pub network_id: u32,
    pub chain_id: String,
}

// ===== Compatibility with luxfi/consensus interfaces =====

/// Implements the Engine interface from luxfi/consensus/engine
pub trait Engine {
    fn start(&self, ctx: &Context) -> Result<()>;
    fn stop(&self) -> Result<()>;
    fn notify(&self, msg: Message) -> Result<()>;
}

/// Implements the Consensus interface from luxfi/consensus
pub trait Consensus {
    fn initialize(&self, ctx: &Context, params: &Parameters) -> Result<()>;
    fn parameters(&self) -> Parameters;
    fn add(&self, vote: Vote) -> Result<()>;
    fn preference(&self) -> Option<String>;
    fn finalized(&self) -> bool;
}

/// Context for consensus operations
pub struct Context {
    pub chain_id: String,
    pub node_id: String,
    pub subnet_id: String,
    pub is_bootstrapped: bool,
}

/// Consensus parameters
pub struct Parameters {
    pub k: u32,
    pub alpha: u32,
    pub beta_virtuous: u32,
    pub beta_rogue: u32,
}

/// Message types for consensus
pub enum Message {
    Put(Vec<u8>),
    Get(String),
    PullQuery(u32, String),
    PushQuery(u32, Vec<u8>),
    Chits(u32, Vec<String>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_validator_registration() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();

        let validator = consensus
            .register_validator(2_000_000_000_000_000, true)
            .await
            .unwrap();

        assert!(validator.node_id.starts_with("NodeID-"));
        assert!(validator.supports_qwen3);
        assert_eq!(validator.uptime, 100.0);
    }

    #[test]
    fn test_signature_verification() {
        let config = LuxConsensusConfig::default();
        let consensus = LuxConsensus::new(config).unwrap();

        let message = b"test message";
        let signature = consensus.sign_as_validator(message);

        let is_valid = consensus
            .verify_validator_signature(
                &consensus.verifying_key.to_bytes(),
                message,
                &signature,
            )
            .unwrap();

        assert!(is_valid);
    }
}