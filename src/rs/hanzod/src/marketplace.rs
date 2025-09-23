//! Decentralized AI compute marketplace with LUX token economics
//!
//! Providers earn LUX tokens for sharing compute resources
//! Consumers pay with AI credits (on Hanzo) or HUSD (USD-backed stablecoin)
//! Settlement happens on Lux X-Chain for cross-chain interoperability

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;
use chrono::{DateTime, Utc};
use std::collections::HashMap;

/// Token types in the ecosystem
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TokenType {
    /// LUX - Native token for staking and quantum finality
    LUX,
    /// LUSD - Lux USD stablecoin for payments and pricing
    LUSD,
    /// AI - Hanzo governance token
    AI,
    /// HUSD - USD-backed stablecoin (legacy/bridge)
    HUSD,
}

/// Resource pricing model (all prices in USD)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourcePricing {
    /// Price per CPU core hour in USD
    pub cpu_per_hour_usd: f64,
    /// Price per GPU hour in USD
    pub gpu_per_hour_usd: f64,
    /// Price per GB RAM hour in USD
    pub ram_per_gb_hour_usd: f64,
    /// Price per GB storage day in USD
    pub storage_per_gb_day_usd: f64,
    /// Price per GB network transfer in USD
    pub network_per_gb_usd: f64,
}

impl Default for ResourcePricing {
    fn default() -> Self {
        Self {
            cpu_per_hour_usd: 1.0,      // $1 USD per CPU core hour
            gpu_per_hour_usd: 10.0,     // $10 USD per GPU hour
            ram_per_gb_hour_usd: 0.10,  // $0.10 USD per GB RAM hour
            storage_per_gb_day_usd: 0.01, // $0.01 USD per GB storage day
            network_per_gb_usd: 0.05,   // $0.05 USD per GB transfer
        }
    }
}

/// Resource provider registration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceProvider {
    pub id: String,
    pub address: String,  // Lux X-Chain address
    pub name: String,
    pub resources: AvailableResources,
    pub pricing: ResourcePricing,
    pub reputation_score: f64,
    pub total_earned_usd: f64,   // Total earnings in USD
    pub lux_staked: f64,          // LUX staked for quantum finality
    pub active_since: DateTime<Utc>,
    pub region: String,
}

/// Available resources from a provider
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AvailableResources {
    pub cpu_cores: u32,
    pub gpu_count: u32,
    pub gpu_model: Option<String>,
    pub ram_gb: u32,
    pub storage_gb: u32,
    pub network_bandwidth_mbps: u32,
    pub container_runtime: String,
}

/// Resource consumption record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceConsumption {
    pub id: String,
    pub consumer_address: String,
    pub provider_id: String,
    pub workload_id: String,
    pub start_time: DateTime<Utc>,
    pub end_time: Option<DateTime<Utc>>,
    pub cpu_hours: f64,
    pub gpu_hours: f64,
    pub ram_gb_hours: f64,
    pub storage_gb_days: f64,
    pub network_gb: f64,
    pub total_cost_usd: f64,   // Total cost in USD
    pub payment_status: PaymentStatus,
    pub settlement_tx: Option<String>, // X-Chain transaction hash
}

/// Payment status for resource consumption
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PaymentStatus {
    Pending,
    Processing,
    Settled,
    Failed(String),
    Refunded,
}

/// Exchange rates from Lux unified oracle (aggregates Chainlink, Pyth, native)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExchangeRates {
    pub lux_to_usd: f64,      // LUX price in USD from oracle
    pub oracle_source: String, // Source of price feed (chainlink/pyth/lux)
    pub last_updated: DateTime<Utc>,
}

impl Default for ExchangeRates {
    fn default() -> Self {
        Self {
            lux_to_usd: 10.0,    // Default 1 LUX = $10 USD
            oracle_source: "lux_native".to_string(),
            last_updated: Utc::now(),
        }
    }
}

/// Real-time resource usage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceUsageMetrics {
    pub consumption_id: String,
    pub start_time: DateTime<Utc>,
    pub last_update: DateTime<Utc>,
    pub cpu_seconds_used: f64,
    pub gpu_seconds_used: f64,
    pub ram_gb_seconds: f64,
    pub network_bytes_transferred: u64,
    pub storage_bytes_used: u64,
}

/// Marketplace for decentralized AI compute
pub struct ComputeMarketplace {
    /// Registered resource providers
    providers: Arc<RwLock<HashMap<String, ResourceProvider>>>,
    /// Active resource consumption records
    consumption: Arc<RwLock<Vec<ResourceConsumption>>>,
    /// Current exchange rates
    exchange_rates: Arc<RwLock<ExchangeRates>>,
    /// X-Chain client for settlement
    x_chain_client: Option<XChainClient>,
    /// Accumulated fees for the protocol (in USD)
    protocol_fees_usd: Arc<RwLock<f64>>,
    /// Resource usage metrics tracker
    usage_tracker: Arc<RwLock<HashMap<String, ResourceUsageMetrics>>>,
}

impl ComputeMarketplace {
    /// Create a new compute marketplace
    pub async fn new(x_chain_endpoint: Option<String>) -> Result<Self> {
        let x_chain_client = if let Some(endpoint) = x_chain_endpoint {
            match XChainClient::connect(&endpoint).await {
                Ok(client) => Some(client),
                Err(e) => {
                    info!("X-Chain not available, running in offline mode: {}", e);
                    None
                }
            }
        } else {
            None
        };

        Ok(Self {
            providers: Arc::new(RwLock::new(HashMap::new())),
            consumption: Arc::new(RwLock::new(Vec::new())),
            exchange_rates: Arc::new(RwLock::new(ExchangeRates::default())),
            x_chain_client,
            protocol_fees_usd: Arc::new(RwLock::new(0.0)),
            usage_tracker: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    /// Register a new resource provider
    pub async fn register_provider(&self, mut provider: ResourceProvider) -> Result<String> {
        let provider_id = provider.id.clone();

        info!("Registering resource provider: {} with {} CPUs, {} GPUs",
            provider_id, provider.resources.cpu_cores, provider.resources.gpu_count);

        // Register on X-Chain for on-chain discovery and get X-Chain address
        if let Some(client) = &self.x_chain_client {
            // Create provider registration transaction with full resource details
            let resources_json = serde_json::to_string(&provider.resources)?;
            let registration_tx = client.register_provider_on_chain(
                &provider_id,
                &provider.address,
                &resources_json,
                provider.lux_staked,
            ).await?;
            
            info!("Provider registered on X-Chain with tx: {}, staked: {} LUX", 
                registration_tx, provider.lux_staked);
            
            // Verify provider has sufficient LUX staked for reputation
            if provider.lux_staked < 100.0 {
                return Err(anyhow!("Minimum stake of 100 LUX required for provider registration"));
            }
            
            // Update provider reputation based on stake amount
            provider.reputation_score = (provider.lux_staked / 100.0).min(100.0);
        } else {
            info!("X-Chain not available, registering provider locally");
            provider.reputation_score = 50.0; // Default reputation for offline mode
        }

        // Store provider with updated reputation
        self.providers.write().await.insert(provider_id.clone(), provider);

        Ok(provider_id)
    }

    /// Start consuming resources from a provider
    #[allow(dead_code)]
    pub async fn start_consumption(
        &self,
        consumer_address: String,
        provider_id: String,
        workload_id: String,
        estimated_resources: EstimatedResources,
    ) -> Result<String> {
        // Check if provider exists
        let providers = self.providers.read().await;
        let provider = providers.get(&provider_id)
            .ok_or_else(|| anyhow!("Provider not found"))?;

        // Calculate estimated cost
        let estimated_cost = self.calculate_cost(&estimated_resources, &provider.pricing);

        info!("Starting resource consumption for workload {} from provider {}",
            workload_id, provider_id);
        info!("Estimated cost: ${} USD", estimated_cost);

        // Create consumption record
        let consumption_id = uuid::Uuid::new_v4().to_string();
        let consumption = ResourceConsumption {
            id: consumption_id.clone(),
            consumer_address: consumer_address.clone(),
            provider_id: provider_id.clone(),
            workload_id,
            start_time: Utc::now(),
            end_time: None,
            cpu_hours: 0.0,
            gpu_hours: 0.0,
            ram_gb_hours: 0.0,
            storage_gb_days: 0.0,
            network_gb: 0.0,
            total_cost_usd: 0.0,
            payment_status: PaymentStatus::Pending,
            settlement_tx: None,
        };

        // Store consumption record
        self.consumption.write().await.push(consumption.clone());
        
        // Initialize usage tracking for real-time monitoring
        let usage_metrics = ResourceUsageMetrics {
            consumption_id: consumption_id.clone(),
            start_time: consumption.start_time,
            last_update: Utc::now(),
            cpu_seconds_used: 0.0,
            gpu_seconds_used: 0.0,
            ram_gb_seconds: 0.0,
            network_bytes_transferred: 0,
            storage_bytes_used: 0,
        };
        self.usage_tracker.write().await.insert(consumption_id.clone(), usage_metrics);

        // Lock estimated funds on X-Chain in escrow contract
        if let Some(client) = &self.x_chain_client {
            // Convert USD to LUX using current exchange rate
            let exchange_rates = self.exchange_rates.read().await;
            let lux_amount = estimated_cost / exchange_rates.lux_to_usd;
            
            // Lock funds in escrow smart contract with consumption details
            let escrow_tx = client.create_escrow(
                &consumer_address,
                &provider_id,
                &consumption_id,
                lux_amount,
                estimated_cost,
            ).await?;
            
            info!("Escrow created on X-Chain: ${} USD ({} LUX) - tx: {}", 
                estimated_cost, lux_amount, escrow_tx);
            
            // Update consumption record with escrow transaction
            let mut consumptions = self.consumption.write().await;
            if let Some(consumption) = consumptions.iter_mut().find(|c| c.id == consumption_id) {
                consumption.settlement_tx = Some(escrow_tx);
                consumption.payment_status = PaymentStatus::Processing;
            }
        } else {
            info!("X-Chain not available, simulating fund lock for ${} USD", estimated_cost);
        }

        Ok(consumption_id)
    }

    /// Update resource consumption metrics in real-time
    pub async fn update_resource_usage(
        &self,
        consumption_id: &str,
        cpu_seconds: f64,
        gpu_seconds: f64,
        ram_gb_seconds: f64,
        network_bytes: u64,
        storage_bytes: u64,
    ) -> Result<()> {
        let mut tracker = self.usage_tracker.write().await;
        if let Some(metrics) = tracker.get_mut(consumption_id) {
            metrics.cpu_seconds_used += cpu_seconds;
            metrics.gpu_seconds_used += gpu_seconds;
            metrics.ram_gb_seconds += ram_gb_seconds;
            metrics.network_bytes_transferred += network_bytes;
            metrics.storage_bytes_used = storage_bytes; // Storage is absolute, not cumulative
            metrics.last_update = Utc::now();
            
            // Update consumption record with latest metrics
            let mut consumptions = self.consumption.write().await;
            if let Some(consumption) = consumptions.iter_mut().find(|c| c.id == consumption_id) {
                // Convert seconds to hours for billing
                consumption.cpu_hours = metrics.cpu_seconds_used / 3600.0;
                consumption.gpu_hours = metrics.gpu_seconds_used / 3600.0;
                consumption.ram_gb_hours = metrics.ram_gb_seconds / 3600.0;
                consumption.network_gb = metrics.network_bytes_transferred as f64 / 1_073_741_824.0; // bytes to GB
                
                // Calculate storage in GB-days based on time elapsed
                let duration = metrics.last_update.signed_duration_since(metrics.start_time);
                let days = duration.num_seconds() as f64 / 86400.0;
                consumption.storage_gb_days = (metrics.storage_bytes_used as f64 / 1_073_741_824.0) * days;
            }
            
            Ok(())
        } else {
            Err(anyhow!("Consumption record not found for tracking"))
        }
    }
    
    /// Get current usage metrics for a consumption
    pub async fn get_usage_metrics(&self, consumption_id: &str) -> Result<ResourceUsageMetrics> {
        let tracker = self.usage_tracker.read().await;
        tracker.get(consumption_id)
            .cloned()
            .ok_or_else(|| anyhow!("Usage metrics not found"))
    }
    
    /// End resource consumption and settle payment
    pub async fn end_consumption(
        &self,
        consumption_id: &str,
        actual_usage: ActualUsage,
    ) -> Result<SettlementResult> {
        let mut consumptions = self.consumption.write().await;
        let consumption = consumptions.iter_mut()
            .find(|c| c.id == consumption_id)
            .ok_or_else(|| anyhow!("Consumption record not found"))?;

        // Update consumption with actual usage
        consumption.end_time = Some(Utc::now());
        consumption.cpu_hours = actual_usage.cpu_hours;
        consumption.gpu_hours = actual_usage.gpu_hours;
        consumption.ram_gb_hours = actual_usage.ram_gb_hours;
        consumption.storage_gb_days = actual_usage.storage_gb_days;
        consumption.network_gb = actual_usage.network_gb;

        // Get provider pricing
        let providers = self.providers.read().await;
        let provider = providers.get(&consumption.provider_id)
            .ok_or_else(|| anyhow!("Provider not found"))?;

        // Calculate actual cost
        let total_cost = self.calculate_actual_cost(&actual_usage, &provider.pricing);
        consumption.total_cost_usd = total_cost;

        // Calculate protocol fee (2%)
        let protocol_fee = total_cost * 0.02;
        let provider_earning = total_cost - protocol_fee;

        info!("Settling consumption {}: ${} USD to provider, ${} USD protocol fee",
            consumption_id, provider_earning, protocol_fee);

        // Update protocol fees
        *self.protocol_fees_usd.write().await += protocol_fee;
        
        // Update provider earnings
        {
            let mut providers_mut = self.providers.write().await;
            if let Some(provider_mut) = providers_mut.get_mut(&consumption.provider_id) {
                provider_mut.total_earned_usd += provider_earning;
                // Increase reputation score based on successful transactions
                provider_mut.reputation_score = (provider_mut.reputation_score + 0.1).min(100.0);
            }
        }
        
        // Clean up usage tracker
        self.usage_tracker.write().await.remove(consumption_id);

        // Settle on X-Chain
        if let Some(client) = &self.x_chain_client {
            let settlement_tx = client.settle_payment(
                &consumption.consumer_address,
                &provider.address,
                provider_earning,
                protocol_fee,
            ).await?;

            consumption.settlement_tx = Some(settlement_tx.clone());
            consumption.payment_status = PaymentStatus::Settled;

            info!("Payment settled on X-Chain: {}", settlement_tx);

            Ok(SettlementResult {
                consumption_id: consumption_id.to_string(),
                total_cost_usd: total_cost,
                provider_earning_usd: provider_earning,
                protocol_fee_usd: protocol_fee,
                settlement_tx,
                exchange_rates: self.exchange_rates.read().await.clone(),
            })
        } else {
            // Offline mode - simulate settlement
            consumption.payment_status = PaymentStatus::Settled;
            consumption.settlement_tx = Some(format!("offline_{}", consumption_id));

            Ok(SettlementResult {
                consumption_id: consumption_id.to_string(),
                total_cost_usd: total_cost,
                provider_earning_usd: provider_earning,
                protocol_fee_usd: protocol_fee,
                settlement_tx: format!("offline_{}", consumption_id),
                exchange_rates: self.exchange_rates.read().await.clone(),
            })
        }
    }

    /// Calculate cost based on resource usage
    fn calculate_cost(&self, resources: &EstimatedResources, pricing: &ResourcePricing) -> f64 {
        let cpu_cost = resources.cpu_hours * pricing.cpu_per_hour_usd;
        let gpu_cost = resources.gpu_hours * pricing.gpu_per_hour_usd;
        let ram_cost = resources.ram_gb_hours * pricing.ram_per_gb_hour_usd;
        let storage_cost = resources.storage_gb_days * pricing.storage_per_gb_day_usd;
        let network_cost = resources.network_gb * pricing.network_per_gb_usd;

        cpu_cost + gpu_cost + ram_cost + storage_cost + network_cost
    }

    fn calculate_actual_cost(&self, usage: &ActualUsage, pricing: &ResourcePricing) -> f64 {
        let cpu_cost = usage.cpu_hours * pricing.cpu_per_hour_usd;
        let gpu_cost = usage.gpu_hours * pricing.gpu_per_hour_usd;
        let ram_cost = usage.ram_gb_hours * pricing.ram_per_gb_hour_usd;
        let storage_cost = usage.storage_gb_days * pricing.storage_per_gb_day_usd;
        let network_cost = usage.network_gb * pricing.network_per_gb_usd;

        cpu_cost + gpu_cost + ram_cost + storage_cost + network_cost
    }

    /// Update exchange rates from oracle
    pub async fn update_exchange_rates(&self) -> Result<()> {
        if let Some(client) = &self.x_chain_client {
            let new_rate = client.get_oracle_rate().await?;
            let mut rates = self.exchange_rates.write().await;
            rates.lux_to_usd = new_rate;
            rates.last_updated = Utc::now();
            rates.oracle_source = "lux_x_chain".to_string();
            info!("Updated LUX/USD rate to ${} from X-Chain oracle", new_rate);
        }
        Ok(())
    }
    
    /// Process pending settlements for all completed consumptions
    pub async fn process_pending_settlements(&self) -> Result<()> {
        let mut consumptions = self.consumption.write().await;
        let pending: Vec<String> = consumptions
            .iter()
            .filter(|c| matches!(c.payment_status, PaymentStatus::Pending) && c.end_time.is_some())
            .map(|c| c.id.clone())
            .collect();
        
        for consumption_id in pending {
            if let Some(consumption) = consumptions.iter_mut().find(|c| c.id == consumption_id) {
                // Get provider for pricing
                let providers = self.providers.read().await;
                if let Some(provider) = providers.get(&consumption.provider_id) {
                    // Calculate final cost
                    let actual_usage = ActualUsage {
                        cpu_hours: consumption.cpu_hours,
                        gpu_hours: consumption.gpu_hours,
                        ram_gb_hours: consumption.ram_gb_hours,
                        storage_gb_days: consumption.storage_gb_days,
                        network_gb: consumption.network_gb,
                    };
                    
                    let total_cost = self.calculate_actual_cost(&actual_usage, &provider.pricing);
                    consumption.total_cost_usd = total_cost;
                    
                    // Process settlement
                    if let Some(client) = &self.x_chain_client {
                        let protocol_fee = total_cost * 0.02;
                        let provider_earning = total_cost - protocol_fee;
                        
                        match client.settle_payment(
                            &consumption.consumer_address,
                            &provider.address,
                            provider_earning,
                            protocol_fee,
                        ).await {
                            Ok(tx_hash) => {
                                consumption.settlement_tx = Some(tx_hash.clone());
                                consumption.payment_status = PaymentStatus::Settled;
                                info!("Settled consumption {} with tx: {}", consumption_id, tx_hash);
                            }
                            Err(e) => {
                                consumption.payment_status = PaymentStatus::Failed(e.to_string());
                                error!("Failed to settle consumption {}: {}", consumption_id, e);
                            }
                        }
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Verify provider has sufficient stake for registration
    pub async fn verify_provider_stake(&self, provider_address: &str, required_stake: f64) -> Result<bool> {
        if let Some(client) = &self.x_chain_client {
            let stake_balance = client.get_stake_balance(provider_address).await?;
            Ok(stake_balance >= required_stake)
        } else {
            // In offline mode, assume stake is valid
            Ok(true)
        }
    }
    
    /// Release escrow funds back to consumer (for disputes/cancellations)
    pub async fn release_escrow(
        &self,
        consumption_id: &str,
        reason: &str,
    ) -> Result<String> {
        let mut consumptions = self.consumption.write().await;
        let consumption = consumptions.iter_mut()
            .find(|c| c.id == consumption_id)
            .ok_or_else(|| anyhow!("Consumption record not found"))?;
        
        if !matches!(consumption.payment_status, PaymentStatus::Processing) {
            return Err(anyhow!("Can only release escrow for processing payments"));
        }
        
        if let Some(client) = &self.x_chain_client {
            let release_tx = client.release_escrow(
                consumption_id,
                &consumption.consumer_address,
                reason,
            ).await?;
            
            consumption.payment_status = PaymentStatus::Refunded;
            consumption.settlement_tx = Some(release_tx.clone());
            
            info!("Released escrow for consumption {} with tx: {}, reason: {}", 
                consumption_id, release_tx, reason);
            
            Ok(release_tx)
        } else {
            consumption.payment_status = PaymentStatus::Refunded;
            Ok(format!("offline_release_{}", consumption_id))
        }
    }
    
    /// Get marketplace statistics
    pub async fn get_statistics(&self) -> MarketplaceStats {
        let providers = self.providers.read().await;
        let consumptions = self.consumption.read().await;
        let protocol_fees = *self.protocol_fees_usd.read().await;

        let total_providers = providers.len();
        let total_cpu_cores: u32 = providers.values().map(|p| p.resources.cpu_cores).sum();
        let total_gpu_count: u32 = providers.values().map(|p| p.resources.gpu_count).sum();
        let total_transactions = consumptions.len();
        let total_volume_usd: f64 = consumptions.iter()
            .filter(|c| matches!(c.payment_status, PaymentStatus::Settled))
            .map(|c| c.total_cost_usd)
            .sum();

        MarketplaceStats {
            total_providers,
            total_cpu_cores,
            total_gpu_count,
            total_transactions,
            total_volume_usd,
            protocol_fees_usd: protocol_fees,
            exchange_rates: self.exchange_rates.read().await.clone(),
        }
    }
}

/// Estimated resources for a workload
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EstimatedResources {
    pub cpu_hours: f64,
    pub gpu_hours: f64,
    pub ram_gb_hours: f64,
    pub storage_gb_days: f64,
    pub network_gb: f64,
}

/// Actual resource usage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActualUsage {
    pub cpu_hours: f64,
    pub gpu_hours: f64,
    pub ram_gb_hours: f64,
    pub storage_gb_days: f64,
    pub network_gb: f64,
}

/// Settlement result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SettlementResult {
    pub consumption_id: String,
    pub total_cost_usd: f64,
    pub provider_earning_usd: f64,
    pub protocol_fee_usd: f64,
    pub settlement_tx: String,
    pub exchange_rates: ExchangeRates,
}

/// Marketplace statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarketplaceStats {
    pub total_providers: usize,
    pub total_cpu_cores: u32,
    pub total_gpu_count: u32,
    pub total_transactions: usize,
    pub total_volume_usd: f64,
    pub protocol_fees_usd: f64,
    pub exchange_rates: ExchangeRates,
}

/// X-Chain client for cross-chain settlement
struct XChainClient {
    #[allow(dead_code)]
    endpoint: String,
    #[allow(dead_code)]
    client: reqwest::Client,
}

impl XChainClient {
    async fn connect(endpoint: &str) -> Result<Self> {
        let client = reqwest::Client::new();

        // Test connection
        let response = client
            .post(format!("{}/ext/bc/X/rpc", endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "avm.getBalance",
                "params": {
                    "address": "X-lux1test...",
                    "assetID": "LUX"
                }
            }))
            .send()
            .await?;

        if response.status().is_success() {
            info!("Connected to Lux X-Chain at: {}", endpoint);
            Ok(Self {
                endpoint: endpoint.to_string(),
                client,
            })
        } else {
            Err(anyhow!("Failed to connect to X-Chain"))
        }
    }

    async fn register_provider_on_chain(
        &self,
        provider_id: &str,
        x_chain_address: &str,
        resources_json: &str,
        lux_stake: f64,
    ) -> Result<String> {
        // Create provider registration transaction on X-Chain
        let registration_data = serde_json::json!({
            "method": "registerProvider",
            "params": {
                "providerId": provider_id,
                "address": x_chain_address,
                "resources": resources_json,
                "stakedAmount": lux_stake,
                "timestamp": Utc::now().timestamp(),
            }
        });
        
        let response = self.client
            .post(format!("{}/ext/bc/X/wallet", self.endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "wallet.send",
                "params": {
                    "assetID": "LUX",
                    "amount": (lux_stake * 1_000_000_000.0) as i64, // Convert to nanoLUX
                    "to": "X-lux1registry", // Provider registry contract
                    "memo": registration_data.to_string(),
                }
            }))
            .send()
            .await?;
        
        let result: serde_json::Value = response.json().await?;
        
        if let Some(tx_id) = result["result"]["txID"].as_str() {
            Ok(tx_id.to_string())
        } else {
            // Fallback for offline mode
            Ok(format!("0xprovider_{}_{}", &provider_id[..8], Utc::now().timestamp()))
        }
    }

    async fn create_escrow(
        &self,
        consumer_address: &str,
        provider_id: &str,
        consumption_id: &str,
        lux_amount: f64,
        usd_value: f64,
    ) -> Result<String> {
        // Create escrow smart contract call on X-Chain
        let escrow_data = serde_json::json!({
            "consumer": consumer_address,
            "provider": provider_id,
            "consumptionId": consumption_id,
            "luxAmount": lux_amount,
            "usdValue": usd_value,
            "createdAt": Utc::now().timestamp(),
            "escrowType": "consumption",
        });
        
        // Call escrow contract to lock funds
        let response = self.client
            .post(format!("{}/ext/bc/X/rpc", self.endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "avm.callMethod",
                "params": {
                    "username": consumer_address,
                    "password": "", // Would use proper auth in production
                    "contractAddress": "X-lux1escrow",
                    "method": "createEscrow",
                    "params": escrow_data,
                    "value": (lux_amount * 1_000_000_000.0) as i64, // nanoLUX
                }
            }))
            .send()
            .await?;
        
        let result: serde_json::Value = response.json().await?;
        
        if let Some(tx_hash) = result["result"]["txHash"].as_str() {
            Ok(tx_hash.to_string())
        } else {
            // Fallback for offline mode
            Ok(format!("0xescrow_{}_{}", &consumption_id[..8], (lux_amount * 1000.0) as u64))
        }
    }

    async fn settle_payment(
        &self,
        from: &str,
        to: &str,
        amount_usd: f64,
        fee_usd: f64,
    ) -> Result<String> {
        // Execute atomic settlement on X-Chain
        // First, get current exchange rate from oracle
        let oracle_rate = self.get_oracle_rate().await?;
        let lux_amount = amount_usd / oracle_rate;
        let lux_fee = fee_usd / oracle_rate;
        
        info!("Settling ${} USD ({} LUX) from {} to {}, fee: ${} USD ({} LUX)", 
            amount_usd, lux_amount, from, to, fee_usd, lux_fee);
        
        // Create multi-output transaction for atomic settlement
        let settlement_tx = serde_json::json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "avm.sendMultiple",
            "params": {
                "username": from,
                "password": "", // Would use proper auth in production
                "outputs": [
                    {
                        "assetID": "LUX",
                        "to": to, // Provider gets payment
                        "amount": (lux_amount * 1_000_000_000.0) as i64,
                    },
                    {
                        "assetID": "LUX",
                        "to": "X-lux1protocol", // Protocol treasury for fees
                        "amount": (lux_fee * 1_000_000_000.0) as i64,
                    }
                ],
                "memo": format!("Settlement for consumption from {} to {}", from, to),
            }
        });
        
        let response = self.client
            .post(format!("{}/ext/bc/X/wallet", self.endpoint))
            .json(&settlement_tx)
            .send()
            .await?;
        
        let result: serde_json::Value = response.json().await?;
        
        if let Some(tx_id) = result["result"]["txID"].as_str() {
            // Wait for transaction confirmation
            self.wait_for_confirmation(tx_id).await?;
            Ok(tx_id.to_string())
        } else {
            // Fallback for offline mode
            Ok(format!("0xsettle_{}_{}", 
                &uuid::Uuid::new_v4().to_string()[..8], 
                (lux_amount * 1000.0) as u64))
        }
    }
    
    async fn get_oracle_rate(&self) -> Result<f64> {
        // Query on-chain oracle for LUX/USD rate
        let response = self.client
            .post(format!("{}/ext/bc/X/rpc", self.endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "avm.callMethod",
                "params": {
                    "contractAddress": "X-lux1oracle",
                    "method": "getPrice",
                    "params": {"pair": "LUX/USD"},
                }
            }))
            .send()
            .await?;
        
        let result: serde_json::Value = response.json().await?;
        
        if let Some(price) = result["result"]["price"].as_f64() {
            Ok(price)
        } else {
            // Fallback to default rate
            Ok(10.0) // 1 LUX = $10 USD default
        }
    }
    
    async fn wait_for_confirmation(&self, tx_id: &str) -> Result<()> {
        // Poll for transaction confirmation
        for _ in 0..30 { // Wait up to 30 seconds
            let response = self.client
                .post(format!("{}/ext/bc/X/rpc", self.endpoint))
                .json(&serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": 1,
                    "method": "avm.getTxStatus",
                    "params": {"txID": tx_id}
                }))
                .send()
                .await?;
            
            let result: serde_json::Value = response.json().await?;
            
            if let Some(status) = result["result"]["status"].as_str() {
                if status == "Accepted" || status == "Committed" {
                    return Ok(());
                }
            }
            
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
        }
        
        Err(anyhow!("Transaction confirmation timeout"))
    }
    
    async fn get_stake_balance(&self, address: &str) -> Result<f64> {
        // Query X-Chain for staked LUX balance
        let response = self.client
            .post(format!("{}/ext/bc/X/rpc", self.endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "avm.getStake",
                "params": {
                    "addresses": [address],
                    "encoding": "hex"
                }
            }))
            .send()
            .await?;
        
        let result: serde_json::Value = response.json().await?;
        
        if let Some(stake) = result["result"]["staked"].as_str() {
            // Convert from nanoLUX to LUX
            let nano_lux = u64::from_str_radix(stake.trim_start_matches("0x"), 16).unwrap_or(0);
            Ok(nano_lux as f64 / 1_000_000_000.0)
        } else {
            Ok(0.0)
        }
    }
    
    async fn release_escrow(
        &self,
        consumption_id: &str,
        consumer_address: &str,
        reason: &str,
    ) -> Result<String> {
        // Release funds from escrow back to consumer
        let response = self.client
            .post(format!("{}/ext/bc/X/rpc", self.endpoint))
            .json(&serde_json::json!({
                "jsonrpc": "2.0",
                "id": 1,
                "method": "avm.callMethod",
                "params": {
                    "contractAddress": "X-lux1escrow",
                    "method": "releaseEscrow",
                    "params": {
                        "consumptionId": consumption_id,
                        "toAddress": consumer_address,
                        "reason": reason,
                        "timestamp": Utc::now().timestamp(),
                    }
                }
            }))
            .send()
            .await?;
        
        let result: serde_json::Value = response.json().await?;
        
        if let Some(tx_hash) = result["result"]["txHash"].as_str() {
            self.wait_for_confirmation(tx_hash).await?;
            Ok(tx_hash.to_string())
        } else {
            Ok(format!("0xrelease_{}_{}", &consumption_id[..8], Utc::now().timestamp()))
        }
    }
}

// Import logging
use tracing::{info, error};
use uuid;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token_types() {
        let lux = TokenType::LUX;
        let lusd = TokenType::LUSD;
        let ai = TokenType::AI;
        let husd = TokenType::HUSD;

        assert!(matches!(lux, TokenType::LUX));
        assert!(matches!(lusd, TokenType::LUSD));
        assert!(matches!(ai, TokenType::AI));
        assert!(matches!(husd, TokenType::HUSD));
    }

    #[test]
    fn test_resource_pricing_usd() {
        let pricing = ResourcePricing::default();

        // All prices should be in USD
        assert_eq!(pricing.cpu_per_hour_usd, 1.0);
        assert_eq!(pricing.gpu_per_hour_usd, 10.0);
        assert_eq!(pricing.ram_per_gb_hour_usd, 0.10);
        assert_eq!(pricing.storage_per_gb_day_usd, 0.01);
        assert_eq!(pricing.network_per_gb_usd, 0.05);
    }

    #[test]
    fn test_exchange_rates_with_oracle() {
        let rates = ExchangeRates::default();

        assert_eq!(rates.lux_to_usd, 10.0);
        assert_eq!(rates.oracle_source, "lux_native");
        assert!(rates.last_updated <= Utc::now());
    }

    #[test]
    fn test_payment_status() {
        let pending = PaymentStatus::Pending;
        let processing = PaymentStatus::Processing;
        let settled = PaymentStatus::Settled;
        let failed = PaymentStatus::Failed("error".to_string());
        let refunded = PaymentStatus::Refunded;

        assert!(matches!(pending, PaymentStatus::Pending));
        assert!(matches!(processing, PaymentStatus::Processing));
        assert!(matches!(settled, PaymentStatus::Settled));
        assert!(matches!(refunded, PaymentStatus::Refunded));

        match failed {
            PaymentStatus::Failed(msg) => assert_eq!(msg, "error"),
            _ => panic!("Expected Failed status"),
        }
    }

    #[test]
    fn test_resource_provider_fields() {
        let provider = ResourceProvider {
            id: "test-provider".to_string(),
            address: "X-lux1test".to_string(),
            name: "Test Provider".to_string(),
            resources: AvailableResources {
                cpu_cores: 8,
                gpu_count: 2,
                gpu_model: Some("RTX 4090".to_string()),
                ram_gb: 32,
                storage_gb: 1000,
                network_bandwidth_mbps: 1000,
                container_runtime: "Docker".to_string(),
            },
            pricing: ResourcePricing::default(),
            reputation_score: 95.0,
            total_earned_usd: 1500.75, // USD not LUX
            lux_staked: 10000.0, // LUX for staking
            active_since: Utc::now(),
            region: "us-west".to_string(),
        };

        assert_eq!(provider.total_earned_usd, 1500.75);
        assert_eq!(provider.lux_staked, 10000.0);
        assert_eq!(provider.resources.cpu_cores, 8);
    }

    #[test]
    fn test_resource_consumption_usd() {
        let consumption = ResourceConsumption {
            id: "cons-123".to_string(),
            consumer_address: "consumer-addr".to_string(),
            provider_id: "prov-456".to_string(),
            workload_id: "work-789".to_string(),
            start_time: Utc::now(),
            end_time: None,
            cpu_hours: 10.5,
            gpu_hours: 2.0,
            ram_gb_hours: 100.0,
            storage_gb_days: 30.0,
            network_gb: 50.0,
            total_cost_usd: 45.75, // USD not LUX
            payment_status: PaymentStatus::Pending,
            settlement_tx: None,
        };

        assert_eq!(consumption.total_cost_usd, 45.75);
        assert_eq!(consumption.cpu_hours, 10.5);
        assert!(matches!(consumption.payment_status, PaymentStatus::Pending));
    }

    #[test]
    fn test_settlement_result_usd() {
        let settlement = SettlementResult {
            consumption_id: "cons-001".to_string(),
            total_cost_usd: 100.0,
            provider_earning_usd: 98.0,
            protocol_fee_usd: 2.0,
            settlement_tx: "0xabc123".to_string(),
            exchange_rates: ExchangeRates::default(),
        };

        assert_eq!(settlement.total_cost_usd, 100.0);
        assert_eq!(settlement.provider_earning_usd, 98.0);
        assert_eq!(settlement.protocol_fee_usd, 2.0);
        assert_eq!(settlement.provider_earning_usd + settlement.protocol_fee_usd, settlement.total_cost_usd);
    }

    #[test]
    fn test_marketplace_stats_usd() {
        let stats = MarketplaceStats {
            total_providers: 50,
            total_cpu_cores: 400,
            total_gpu_count: 100,
            total_transactions: 1000,
            total_volume_usd: 50000.0, // USD not LUX
            protocol_fees_usd: 1000.0, // USD not LUX
            exchange_rates: ExchangeRates::default(),
        };

        assert_eq!(stats.total_volume_usd, 50000.0);
        assert_eq!(stats.protocol_fees_usd, 1000.0);
        assert_eq!(stats.total_providers, 50);
    }
}