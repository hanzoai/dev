//! Blockchain integration module for hanzod
//! Integrates with Lux consensus for quantum finality and atomic transactions

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use chrono::{DateTime, Utc};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use sha2::{Sha256, Digest};
use std::fs;
use aes_gcm::{
    aead::{Aead, AeadCore, KeyInit, OsRng as AesOsRng},
    Aes256Gcm, Nonce, Key
};

/// Blockchain configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockchainConfig {
    /// Network to connect to (mainnet, testnet, local)
    pub network: NetworkType,
    /// Node endpoint for RPC calls
    pub node_endpoint: String,
    /// Path to Lux consensus binary
    pub consensus_path: PathBuf,
    /// Path to key store
    pub keystore_path: PathBuf,
    /// Chain ID for the network
    pub chain_id: String,
    /// Enable quantum finality features
    pub quantum_finality: bool,
}

/// Network types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NetworkType {
    Mainnet,
    Testnet,
    Local,
    Custom(String),
}

/// Transaction types for recording operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransactionType {
    /// Container creation transaction
    ContainerCreate {
        image: String,
        config_hash: String,
    },
    /// Container state change
    ContainerStateChange {
        container_id: String,
        new_state: String,
    },
    /// Workload scheduling
    WorkloadSchedule {
        workload_id: String,
        resource_allocation: String,
    },
    /// Sandbox operation
    SandboxOperation {
        sandbox_id: String,
        operation: String,
    },
    /// Federated transaction
    FederatedTx {
        chain_id: String,
        tx_hash: String,
    },
    /// Resource provider registration
    ProviderRegistration {
        provider_id: String,
        x_chain_address: String,
        resources: String, // JSON encoded
    },
    /// Resource consumption start
    ConsumptionStart {
        consumption_id: String,
        provider_id: String,
        consumer_address: String,
        estimated_usd: f64,
    },
    /// Resource consumption settlement
    ConsumptionSettlement {
        consumption_id: String,
        provider_earning_usd: f64,
        protocol_fee_usd: f64,
        x_chain_tx: String,
    },
    /// Token transfer
    TokenTransfer {
        from: String,
        to: String,
        amount: f64,
        token_type: String, // LUX (staking), USD (payments)
    },
}

/// Blockchain transaction
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockchainTransaction {
    pub id: String,
    pub tx_type: TransactionType,
    pub from_address: String,
    pub to_address: Option<String>,
    pub data: Vec<u8>,
    pub signature: Vec<u8>,
    pub timestamp: DateTime<Utc>,
    pub block_height: Option<u64>,
    pub tx_hash: String,
    pub status: TransactionStatus,
    pub gas_used: Option<u64>,
}

/// Transaction status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransactionStatus {
    Pending,
    Confirmed,
    Failed(String),
    Finalized, // Quantum finality achieved
}

/// Encrypted keystore entry
#[derive(Debug, Clone, Serialize, Deserialize)]
struct KeystoreEntry {
    /// Encrypted private key
    encrypted_key: Vec<u8>,
    /// Nonce for AES-GCM
    nonce: Vec<u8>,
    /// Salt for key derivation
    salt: Vec<u8>,
    /// Public key
    public_key: Vec<u8>,
    /// Lux address
    address: String,
    /// HD derivation path
    derivation_path: String,
}

/// Key management for transaction signing with real Ed25519 cryptography
#[derive(Clone)]
pub struct KeyManager {
    /// Ed25519 signing key
    signing_key: SigningKey,
    /// Ed25519 verifying key (public key)
    verifying_key: VerifyingKey,
    /// Lux address derived from public key
    pub address: String,
    /// Key derivation path for HD wallets
    derivation_path: String,
    /// Path to keystore file
    keystore_path: PathBuf,
}

impl KeyManager {
    /// Create a new key manager with a fresh keypair or load from keystore
    pub fn new(keystore_path: &PathBuf) -> Result<Self> {
        // Try to load existing keystore
        let keystore_file = keystore_path.join("hanzo_key.json");

        if keystore_file.exists() {
            // Load and decrypt existing key
            Self::load_from_keystore(&keystore_file)
        } else {
            // Generate new keypair and save to keystore
            let key_manager = Self::generate_new()?;
            key_manager.save_to_keystore(&keystore_file)?;
            Ok(key_manager)
        }
    }

    /// Generate a new Ed25519 keypair
    fn generate_new() -> Result<Self> {
        let mut csprng = OsRng;
        // Generate 32 random bytes for the secret key
        let mut secret_key_bytes = [0u8; 32];
        use rand::RngCore;
        csprng.fill_bytes(&mut secret_key_bytes);
        
        let signing_key = SigningKey::from_bytes(&secret_key_bytes);
        let verifying_key = signing_key.verifying_key();

        // Derive Lux address from public key
        // Lux uses bech32 encoding with 'lux' prefix
        let address = Self::derive_lux_address(&verifying_key)?;

        Ok(Self {
            signing_key,
            verifying_key,
            address,
            derivation_path: "m/44'/9999'/0'/0/0".to_string(),
            keystore_path: PathBuf::new(),
        })
    }

    /// Derive Lux address from public key
    fn derive_lux_address(verifying_key: &VerifyingKey) -> Result<String> {
        // Get public key bytes
        let pubkey_bytes = verifying_key.as_bytes();

        // Hash the public key with SHA256
        let mut hasher = Sha256::new();
        hasher.update(pubkey_bytes);
        let hash = hasher.finalize();

        // Take first 20 bytes as address (similar to Ethereum)
        let addr_bytes = &hash[..20];

        // Encode as Lux address with custom prefix
        // Using Base58 for Lux addresses (similar to Avalanche)
        let address = format!("lux1{}", bs58::encode(addr_bytes).into_string());

        Ok(address)
    }

    /// Load keypair from encrypted keystore
    fn load_from_keystore(keystore_file: &PathBuf) -> Result<Self> {
        let data = fs::read_to_string(keystore_file)?;
        let entry: KeystoreEntry = serde_json::from_str(&data)?;

        // For production, prompt for password
        // For now, use a default password (MUST be changed in production)
        let password = std::env::var("HANZO_KEYSTORE_PASSWORD")
            .unwrap_or_else(|_| "change_this_password_in_production".to_string());

        // Derive encryption key from password using Argon2
        let argon2 = argon2::Argon2::default();
        let mut key_bytes = [0u8; 32];
        argon2.hash_password_into(
            password.as_bytes(),
            &entry.salt,
            &mut key_bytes
        ).map_err(|e| anyhow!("Failed to derive key: {}", e))?;

        // Decrypt the private key
        let key = Key::<Aes256Gcm>::from_slice(&key_bytes);
        let cipher = Aes256Gcm::new(key);
        let nonce = Nonce::from_slice(&entry.nonce);

        let decrypted_key = cipher.decrypt(nonce, entry.encrypted_key.as_ref())
            .map_err(|_| anyhow!("Failed to decrypt private key"))?;

        // Reconstruct the signing key
        let signing_key = SigningKey::from_bytes(&decrypted_key.try_into()
            .map_err(|_| anyhow!("Invalid key length"))?);
        let verifying_key = signing_key.verifying_key();

        Ok(Self {
            signing_key,
            verifying_key,
            address: entry.address,
            derivation_path: entry.derivation_path,
            keystore_path: keystore_file.clone(),
        })
    }

    /// Save keypair to encrypted keystore
    fn save_to_keystore(&self, keystore_file: &PathBuf) -> Result<()> {
        // Create keystore directory if it doesn't exist
        if let Some(parent) = keystore_file.parent() {
            fs::create_dir_all(parent)?;
        }

        // Get password from environment or use default (MUST be changed in production)
        let password = std::env::var("HANZO_KEYSTORE_PASSWORD")
            .unwrap_or_else(|_| "change_this_password_in_production".to_string());

        // Generate salt for key derivation
        let mut salt = [0u8; 32];
        use rand::RngCore;
        OsRng.fill_bytes(&mut salt);

        // Derive encryption key from password using Argon2
        let argon2 = argon2::Argon2::default();
        let mut key_bytes = [0u8; 32];
        argon2.hash_password_into(
            password.as_bytes(),
            &salt,
            &mut key_bytes
        ).map_err(|e| anyhow!("Failed to derive key: {}", e))?;

        // Encrypt the private key with AES-GCM
        let key = Key::<Aes256Gcm>::from_slice(&key_bytes);
        let cipher = Aes256Gcm::new(key);
        let nonce = Aes256Gcm::generate_nonce(&mut AesOsRng);

        let encrypted_key = cipher.encrypt(&nonce, self.signing_key.as_bytes().as_ref())
            .map_err(|_| anyhow!("Failed to encrypt private key"))?;

        // Create keystore entry
        let entry = KeystoreEntry {
            encrypted_key,
            nonce: nonce.to_vec(),
            salt: salt.to_vec(),
            public_key: self.verifying_key.as_bytes().to_vec(),
            address: self.address.clone(),
            derivation_path: self.derivation_path.clone(),
        };

        // Save to file
        let json = serde_json::to_string_pretty(&entry)?;
        fs::write(keystore_file, json)?;

        // Set restrictive permissions (Unix only)
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let metadata = fs::metadata(keystore_file)?;
            let mut permissions = metadata.permissions();
            permissions.set_mode(0o600); // Read/write for owner only
            fs::set_permissions(keystore_file, permissions)?;
        }

        Ok(())
    }

    /// Sign a transaction with Ed25519
    pub fn sign_transaction(&self, tx_data: &[u8]) -> Result<Vec<u8>> {
        // Hash the transaction data first
        let mut hasher = Sha256::new();
        hasher.update(tx_data);
        let hash = hasher.finalize();

        // Sign the hash with Ed25519
        let signature = self.signing_key.sign(&hash);

        // Return the signature bytes (64 bytes for Ed25519)
        Ok(signature.to_bytes().to_vec())
    }

    /// Verify a signature
    pub fn verify_signature(&self, data: &[u8], signature: &[u8], address: &str) -> Result<bool> {
        // For self-verification
        if address == self.address {
            return self.verify_own_signature(data, signature);
        }

        // For external verification, we'd need to recover the public key from the address
        // This requires access to a registry or the public key to be provided
        // For now, we'll implement self-verification
        Err(anyhow!("External signature verification requires public key registry"))
    }

    /// Verify a signature with our own public key
    fn verify_own_signature(&self, data: &[u8], signature_bytes: &[u8]) -> Result<bool> {
        // Hash the data
        let mut hasher = Sha256::new();
        hasher.update(data);
        let hash = hasher.finalize();

        // Parse signature
        let signature = Signature::from_bytes(signature_bytes.try_into()
            .map_err(|_| anyhow!("Invalid signature length"))?);

        // Verify with our public key
        Ok(self.verifying_key.verify(&hash, &signature).is_ok())
    }

    /// Get the public key bytes
    pub fn public_key_bytes(&self) -> Vec<u8> {
        self.verifying_key.as_bytes().to_vec()
    }

    /// Export public key for sharing
    pub fn export_public_key(&self) -> String {
        bs58::encode(self.verifying_key.as_bytes()).into_string()
    }

    /// Verify signature with a provided public key
    pub fn verify_with_pubkey(data: &[u8], signature: &[u8], pubkey_bytes: &[u8]) -> Result<bool> {
        // Parse public key
        let verifying_key = VerifyingKey::from_bytes(pubkey_bytes.try_into()
            .map_err(|_| anyhow!("Invalid public key length"))?)?;

        // Hash the data
        let mut hasher = Sha256::new();
        hasher.update(data);
        let hash = hasher.finalize();

        // Parse signature
        let signature = Signature::from_bytes(signature.try_into()
            .map_err(|_| anyhow!("Invalid signature length"))?);

        // Verify
        Ok(verifying_key.verify(&hash, &signature).is_ok())
    }
}

/// Lux consensus integration
pub struct LuxConsensus {
    config: BlockchainConfig,
    key_manager: Arc<KeyManager>,
    /// Active connection to Lux node
    node_client: Option<LuxNodeClient>,
    /// Transaction pool
    tx_pool: Arc<RwLock<Vec<BlockchainTransaction>>>,
    /// Block height
    current_height: Arc<RwLock<u64>>,
}

impl LuxConsensus {
    /// Create new Lux consensus instance
    pub async fn new(config: BlockchainConfig) -> Result<Self> {
        let key_manager = Arc::new(KeyManager::new(&config.keystore_path)?);

        let mut consensus = Self {
            config: config.clone(),
            key_manager,
            node_client: None,
            tx_pool: Arc::new(RwLock::new(Vec::new())),
            current_height: Arc::new(RwLock::new(0)),
        };

        // Connect to Lux node
        consensus.connect().await?;

        Ok(consensus)
    }

    /// Connect to Lux mainnet or testnet
    pub async fn connect(&mut self) -> Result<()> {
        info!("Connecting to Lux network: {:?}", self.config.network);

        match LuxNodeClient::connect(&self.config.node_endpoint).await {
            Ok(client) => {
                // Get current block height
                let height = client.get_block_height().await?;
                *self.current_height.write().await = height;

                self.node_client = Some(client);

                info!("Connected to Lux network at height {}", height);
                Ok(())
            }
            Err(e) => {
                info!("Warning: Could not connect to Lux network: {}. Running in offline mode.", e);
                info!("Blockchain transactions will be recorded locally but not submitted to network.");
                Ok(())
            }
        }
    }

    /// Submit a transaction to the blockchain
    pub async fn submit_transaction(&self, tx_type: TransactionType) -> Result<String> {
        let tx_data = serde_json::to_vec(&tx_type)?;
        let signature = self.key_manager.sign_transaction(&tx_data)?;

        let tx_id = uuid::Uuid::new_v4().to_string();
        let tx_hash = format!("0x{}", hex::encode(&tx_id.as_bytes()[..8])); // Mock tx hash

        let tx = BlockchainTransaction {
            id: tx_id,
            tx_type,
            from_address: self.key_manager.address.clone(),
            to_address: None,
            data: tx_data,
            signature,
            timestamp: Utc::now(),
            block_height: Some(*self.current_height.read().await),
            tx_hash: tx_hash.clone(),
            status: TransactionStatus::Pending,
            gas_used: None,
        };

        // Add to pool
        self.tx_pool.write().await.push(tx.clone());

        // Submit to node if connected
        if let Some(client) = &self.node_client {
            match client.submit_transaction(&tx).await {
                Ok(real_tx_hash) => {
                    info!("Transaction submitted to Lux network: {}", real_tx_hash);
                    return Ok(real_tx_hash);
                }
                Err(e) => {
                    info!("Transaction recorded locally (network error: {}): {}", e, tx_hash);
                }
            }
        } else {
            info!("Transaction recorded locally (offline mode): {}", tx_hash);
        }

        Ok(tx_hash)
    }

    /// Get transaction by hash
    pub async fn get_transaction(&self, tx_hash: &str) -> Result<Option<BlockchainTransaction>> {
        if let Some(client) = &self.node_client {
            return client.get_transaction(tx_hash).await;
        }
        Err(anyhow!("Not connected to Lux network"))
    }

    /// Wait for quantum finality
    pub async fn wait_for_finality(&self, tx_hash: &str) -> Result<()> {
        if !self.config.quantum_finality {
            return Ok(());
        }

        info!("Waiting for quantum finality for tx: {}", tx_hash);

        // In offline mode, simulate finality
        if self.node_client.is_none() {
            // Simulate quantum finality delay
            tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

            // Update transaction status in pool
            let mut pool = self.tx_pool.write().await;
            if let Some(tx) = pool.iter_mut().find(|t| t.tx_hash == tx_hash) {
                tx.status = TransactionStatus::Finalized;
            }

            info!("Quantum finality simulated for tx: {} (offline mode)", tx_hash);
            return Ok(());
        }

        // Poll for finality when connected
        for _ in 0..60 { // Wait up to 60 seconds
            if let Ok(Some(tx)) = self.get_transaction(tx_hash).await {
                if matches!(tx.status, TransactionStatus::Finalized) {
                    info!("Quantum finality achieved for tx: {}", tx_hash);
                    return Ok(());
                }
            }
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
        }

        Err(anyhow!("Timeout waiting for quantum finality"))
    }

    /// Enable federated atomic transactions
    #[allow(dead_code)]
    pub async fn federated_transaction(
        &self,
        target_chain: &str,
        operation: TransactionType,
    ) -> Result<String> {
        info!("Initiating federated transaction to chain: {}", target_chain);

        // First, submit to our chain
        let our_tx = self.submit_transaction(operation.clone()).await?;

        // Wait for local confirmation
        self.wait_for_finality(&our_tx).await?;

        // Then initiate cross-chain atomic swap
        let federated_tx = TransactionType::FederatedTx {
            chain_id: target_chain.to_string(),
            tx_hash: our_tx.clone(),
        };

        let cross_chain_tx = self.submit_transaction(federated_tx).await?;

        info!("Federated transaction initiated: {} -> {}", our_tx, cross_chain_tx);

        Ok(cross_chain_tx)
    }

    /// Get current block height
    pub async fn get_block_height(&self) -> Result<u64> {
        Ok(*self.current_height.read().await)
    }

    /// Monitor blockchain for new blocks
    pub async fn start_block_monitor(&self) {
        let current_height = self.current_height.clone();
        let node_endpoint = self.config.node_endpoint.clone();

        tokio::spawn(async move {
            loop {
                // Poll for new blocks
                if let Ok(client) = LuxNodeClient::connect(&node_endpoint).await {
                    if let Ok(height) = client.get_block_height().await {
                        let mut current = current_height.write().await;
                        if height > *current {
                            info!("New block detected: {}", height);
                            *current = height;
                        }
                    }
                }

                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
        });
    }
}

/// Client for connecting to Lux node
struct LuxNodeClient {
    endpoint: String,
    client: reqwest::Client,
}

impl LuxNodeClient {
    /// Connect to Lux node
    async fn connect(endpoint: &str) -> Result<Self> {
        let client = reqwest::Client::new();

        // Test connection
        let response = client
            .post(format!("{}/ext/info", endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "info.getNetworkName",
                "params": {}
            }))
            .send()
            .await?;

        if response.status().is_success() {
            info!("Connected to Lux node at: {}", endpoint);
            Ok(Self {
                endpoint: endpoint.to_string(),
                client,
            })
        } else {
            Err(anyhow!("Failed to connect to Lux node"))
        }
    }

    /// Get current block height
    async fn get_block_height(&self) -> Result<u64> {
        let response = self.client
            .post(format!("{}/ext/bc/C/rpc", self.endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "eth_blockNumber",
                "params": []
            }))
            .send()
            .await?;

        let data: serde_json::Value = response.json().await?;

        if let Some(result) = data.get("result") {
            if let Some(height_str) = result.as_str() {
                // Parse hex block number
                let height = u64::from_str_radix(height_str.trim_start_matches("0x"), 16)?;
                return Ok(height);
            }
        }

        Ok(0)
    }

    /// Submit transaction to the network
    async fn submit_transaction(&self, tx: &BlockchainTransaction) -> Result<String> {
        // Encode transaction for submission
        let tx_data = hex::encode(&tx.data);

        let response = self.client
            .post(format!("{}/ext/bc/C/rpc", self.endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "eth_sendRawTransaction",
                "params": [tx_data]
            }))
            .send()
            .await?;

        let data: serde_json::Value = response.json().await?;

        if let Some(result) = data.get("result") {
            if let Some(tx_hash) = result.as_str() {
                return Ok(tx_hash.to_string());
            }
        }

        Err(anyhow!("Failed to submit transaction"))
    }

    /// Get transaction by hash
    async fn get_transaction(&self, tx_hash: &str) -> Result<Option<BlockchainTransaction>> {
        let response = self.client
            .post(format!("{}/ext/bc/C/rpc", self.endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "eth_getTransactionByHash",
                "params": [tx_hash]
            }))
            .send()
            .await?;

        let data: serde_json::Value = response.json().await?;

        if let Some(result) = data.get("result") {
            if !result.is_null() {
                // Parse transaction from JSON-RPC result
                // This is simplified - real implementation would parse full tx
                return Ok(None);
            }
        }

        Ok(None)
    }
}

/// Default configuration for Lux mainnet
impl Default for BlockchainConfig {
    fn default() -> Self {
        Self {
            network: NetworkType::Mainnet,
            node_endpoint: "https://api.lux.network".to_string(),
            consensus_path: PathBuf::from("/Users/z/work/lux/consensus/consensus"),
            keystore_path: PathBuf::from("/Users/z/.lux/keystore"),
            chain_id: "lux-mainnet".to_string(),
            quantum_finality: true,
        }
    }
}

// Import logging
use tracing::info;
use uuid;
use hex;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transaction_types_usd() {
        let consumption_start = TransactionType::ConsumptionStart {
            consumption_id: "test-123".to_string(),
            provider_id: "provider-456".to_string(),
            consumer_address: "consumer-789".to_string(),
            estimated_usd: 25.50, // USD not LUX
        };

        match consumption_start {
            TransactionType::ConsumptionStart { estimated_usd, .. } => {
                assert_eq!(estimated_usd, 25.50);
            }
            _ => panic!("Wrong type"),
        }
    }

    #[test]
    fn test_blockchain_config_paths() {
        let config = BlockchainConfig::default();
        assert!(config.consensus_path.to_string_lossy().contains("lux"));
        assert!(config.keystore_path.to_string_lossy().contains("lux"));
    }
}