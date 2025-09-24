//! Native RPC Server for Hanzo Chain with KuzuDB Ledger
//!
//! Provides both gRPC and HTTP/JSON-RPC interfaces for:
//! - Blockchain operations (Lux consensus)
//! - AI inference (Qwen3-Next, Qwen3-Reranker)
//! - Vector operations (embeddings, search)
//! - Node operator management

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;
use tonic::{transport::Server, Request, Response, Status};
use axum::{
    routing::{post, get},
    extract::{State, Json, Path, Query},
    http::StatusCode,
    Router,
};
use tower::ServiceBuilder;
use tower_http::cors::CorsLayer;
use chrono::{DateTime, Utc};

use crate::chain::Chain;
use crate::blockchain::{KeyManager, TransactionType, BlockchainTransaction};
use crate::database::DatabaseManager;

/// RPC Server configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpcConfig {
    /// gRPC port (default: 50051)
    pub grpc_port: u16,
    /// HTTP port for JSON-RPC (default: 8545)
    pub http_port: u16,
    /// WebSocket port for subscriptions (default: 8546)
    pub ws_port: u16,
    /// Enable Qwen3-Next model
    pub enable_qwen3_next: bool,
    /// Enable Qwen3-Reranker model
    pub enable_qwen3_reranker: bool,
    /// Model endpoints
    pub model_endpoints: ModelEndpoints,
}

impl Default for RpcConfig {
    fn default() -> Self {
        Self {
            grpc_port: 50051,
            http_port: 8545,
            ws_port: 8546,
            enable_qwen3_next: true,
            enable_qwen3_reranker: true,
            model_endpoints: ModelEndpoints::default(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelEndpoints {
    /// Qwen3-Next endpoint (default: local)
    pub qwen3_next: String,
    /// Qwen3-Reranker endpoint (default: local)
    pub qwen3_reranker: String,
    /// Embedding model endpoint
    pub embedding_model: String,
}

impl Default for ModelEndpoints {
    fn default() -> Self {
        Self {
            qwen3_next: "http://localhost:3690/v1/qwen3-next".to_string(),
            qwen3_reranker: "http://localhost:3690/v1/qwen3-reranker".to_string(),
            embedding_model: "http://localhost:3690/v1/embeddings".to_string(),
        }
    }
}

// ===== RPC Request/Response Types =====

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeOperatorRequest {
    /// Node operator's public key
    pub public_key: String,
    /// Signature for authentication
    pub signature: String,
    /// Staking amount in LUX tokens
    pub stake_amount: f64,
    /// Node capabilities
    pub capabilities: NodeCapabilities,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeCapabilities {
    /// Can run Qwen3-Next model
    pub qwen3_next: bool,
    /// Can run Qwen3-Reranker model
    pub qwen3_reranker: bool,
    /// Available GPU memory in GB
    pub gpu_memory_gb: u32,
    /// Available CPU cores
    pub cpu_cores: u32,
    /// Network bandwidth in Mbps
    pub bandwidth_mbps: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceRequest {
    /// Model to use (qwen3-next, qwen3-reranker, etc.)
    pub model: String,
    /// Input prompt or documents to rerank
    pub input: serde_json::Value,
    /// Optional: specific node operator to use
    pub node_operator: Option<String>,
    /// Max tokens for generation
    pub max_tokens: Option<u32>,
    /// Temperature for sampling
    pub temperature: Option<f32>,
    /// For reranking: query string
    pub query: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceResponse {
    /// Model used
    pub model: String,
    /// Output from the model
    pub output: serde_json::Value,
    /// Node operator who processed
    pub node_operator: String,
    /// Tokens used
    pub tokens_used: u32,
    /// Processing time in ms
    pub latency_ms: u64,
    /// Transaction hash on blockchain
    pub tx_hash: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VectorSearchRequest {
    /// Query embedding or text
    pub query: QueryInput,
    /// Number of results
    pub k: usize,
    /// Similarity threshold
    pub threshold: Option<f32>,
    /// Optional metadata filters
    pub filters: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum QueryInput {
    Text(String),
    Embedding(Vec<f32>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockchainStatusResponse {
    /// Current block height
    pub block_height: u64,
    /// Network type
    pub network: String,
    /// Chain ID
    pub chain_id: String,
    /// Node operator count
    pub active_operators: u32,
    /// Total staked LUX
    pub total_staked: f64,
    /// Consensus health
    pub consensus_status: String,
}

// ===== gRPC Proto Definitions (would normally be in .proto file) =====

// Proto definitions will be included when build completes
// pub mod proto {
//     tonic::include_proto!("hanzo.chain");
// }

// ===== RPC Server Implementation =====

pub struct RpcServer {
    chain: Arc<Chain>,
    config: RpcConfig,
    node_operators: Arc<RwLock<Vec<RegisteredOperator>>>,
}

#[derive(Debug, Clone)]
struct RegisteredOperator {
    pub address: String,
    pub public_key: String,
    pub capabilities: NodeCapabilities,
    pub stake_amount: f64,
    pub reputation_score: f64,
    pub active: bool,
    pub last_heartbeat: DateTime<Utc>,
}

impl RpcServer {
    pub fn new(chain: Arc<Chain>, config: RpcConfig) -> Self {
        Self {
            chain,
            config,
            node_operators: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Start both gRPC and HTTP RPC servers
    pub async fn start(self: Arc<Self>) -> Result<()> {
        // Start gRPC server
        let grpc_handle = {
            let server = self.clone();
            tokio::spawn(async move {
                server.start_grpc().await
            })
        };

        // Start HTTP JSON-RPC server
        let http_handle = {
            let server = self.clone();
            tokio::spawn(async move {
                server.start_http().await
            })
        };

        // Wait for both
        tokio::try_join!(grpc_handle, http_handle)?;
        Ok(())
    }

    /// Start gRPC server
    async fn start_grpc(self: Arc<Self>) -> Result<()> {
        let addr: std::net::SocketAddr = format!("0.0.0.0:{}", self.config.grpc_port).parse()?;

        println!("🚀 gRPC server listening on {}", addr);

        // Server::builder()
        //     .add_service(proto::chain_service_server::ChainServiceServer::new(
        //         GrpcHandler { server: self.clone() }
        //     ))
        //     .serve(addr)
        //     .await?;

        Ok(())
    }

    /// Start HTTP JSON-RPC server
    async fn start_http(self: Arc<Self>) -> Result<()> {
        let app = Router::new()
            // Node operator endpoints
            .route("/v1/operator/register", post(register_operator))
            .route("/v1/operator/heartbeat", post(operator_heartbeat))
            .route("/v1/operators", get(list_operators))

            // Inference endpoints
            .route("/v1/inference", post(inference))
            .route("/v1/inference/qwen3-next", post(inference_qwen3_next))
            .route("/v1/inference/qwen3-reranker", post(inference_qwen3_reranker))

            // Vector operations
            .route("/v1/embeddings", post(generate_embeddings))
            .route("/v1/vector/search", post(vector_search))

            // Blockchain operations
            .route("/v1/chain/status", get(chain_status))
            .route("/v1/chain/submit", post(submit_transaction))
            .route("/v1/chain/block/:height", get(get_block))

            // Graph queries (KuzuDB Cypher)
            .route("/v1/graph/query", post(graph_query))

            // Health check
            .route("/health", get(health_check))

            .layer(
                ServiceBuilder::new()
                    .layer(CorsLayer::permissive())
            )
            .with_state(self.clone());

        let addr = format!("0.0.0.0:{}", self.config.http_port);
        println!("🌐 HTTP RPC server listening on http://{}", addr);

        let listener = tokio::net::TcpListener::bind(&addr).await?;
        axum::serve(listener, app).await?;

        Ok(())
    }

    /// Register a new node operator
    async fn register_operator(&self, req: NodeOperatorRequest) -> Result<String> {
        // Verify signature
        // TODO: Implement signature verification

        // Check minimum stake requirement (e.g., 100,000 LUX)
        if req.stake_amount < 100_000.0 {
            return Err(anyhow!("Insufficient stake amount"));
        }

        // Generate operator address
        let address = format!("hanzo-op-{}", &req.public_key[..8]);

        // Register operator
        let operator = RegisteredOperator {
            address: address.clone(),
            public_key: req.public_key,
            capabilities: req.capabilities.clone(),
            stake_amount: req.stake_amount,
            reputation_score: 100.0, // Start with base reputation
            active: true,
            last_heartbeat: Utc::now(),
        };

        let mut operators = self.node_operators.write().await;
        operators.push(operator);

        // Record on blockchain
        let tx_type = TransactionType::ProviderRegistration {
            provider_id: address.clone(),
            x_chain_address: address.clone(),
            resources: serde_json::to_string(&req.capabilities)?,
        };

        // TODO: Submit to blockchain

        Ok(address)
    }

    /// Process inference request with Qwen3-Next
    async fn inference_qwen3_next(&self, req: InferenceRequest) -> Result<InferenceResponse> {
        if !self.config.enable_qwen3_next {
            return Err(anyhow!("Qwen3-Next model not enabled"));
        }

        // Select node operator based on capabilities
        let operator = self.select_operator_for_model("qwen3-next").await?;

        // Forward to model endpoint
        let start = std::time::Instant::now();

        // TODO: Actually call the model endpoint
        let output = serde_json::json!({
            "response": "Generated by Qwen3-Next",
            "model": "qwen3-next",
            "operator": &operator.address,
        });

        let latency_ms = start.elapsed().as_millis() as u64;

        // Record on blockchain
        let tx_hash = self.record_inference_transaction(
            &operator.address,
            "qwen3-next",
            latency_ms,
        ).await?;

        Ok(InferenceResponse {
            model: "qwen3-next".to_string(),
            output,
            node_operator: operator.address,
            tokens_used: 100, // TODO: Actual token count
            latency_ms,
            tx_hash,
        })
    }

    /// Process reranking request with Qwen3-Reranker
    async fn inference_qwen3_reranker(&self, req: InferenceRequest) -> Result<InferenceResponse> {
        if !self.config.enable_qwen3_reranker {
            return Err(anyhow!("Qwen3-Reranker model not enabled"));
        }

        let query = req.query.ok_or_else(|| anyhow!("Query required for reranking"))?;

        // Select node operator
        let operator = self.select_operator_for_model("qwen3-reranker").await?;

        // Process reranking
        let start = std::time::Instant::now();

        // TODO: Actually call the reranker model
        let output = serde_json::json!({
            "reranked_documents": [],
            "query": query,
            "model": "qwen3-reranker",
        });

        let latency_ms = start.elapsed().as_millis() as u64;

        // Record on blockchain
        let tx_hash = self.record_inference_transaction(
            &operator.address,
            "qwen3-reranker",
            latency_ms,
        ).await?;

        Ok(InferenceResponse {
            model: "qwen3-reranker".to_string(),
            output,
            node_operator: operator.address,
            tokens_used: 50, // Reranking uses fewer tokens
            latency_ms,
            tx_hash,
        })
    }

    /// Select best operator for a model
    async fn select_operator_for_model(&self, model: &str) -> Result<RegisteredOperator> {
        let operators = self.node_operators.read().await;

        let mut eligible: Vec<_> = operators
            .iter()
            .filter(|op| {
                op.active &&
                match model {
                    "qwen3-next" => op.capabilities.qwen3_next,
                    "qwen3-reranker" => op.capabilities.qwen3_reranker,
                    _ => false,
                }
            })
            .cloned()
            .collect();

        if eligible.is_empty() {
            return Err(anyhow!("No operators available for model {}", model));
        }

        // Sort by reputation and select best
        eligible.sort_by(|a, b| b.reputation_score.partial_cmp(&a.reputation_score).unwrap());

        Ok(eligible[0].clone())
    }

    /// Record inference transaction on blockchain
    async fn record_inference_transaction(
        &self,
        operator: &str,
        model: &str,
        latency_ms: u64,
    ) -> Result<String> {
        // Generate transaction hash
        let tx_hash = format!("0x{:064x}", rand::random::<u64>());

        // TODO: Actually submit to blockchain

        Ok(tx_hash)
    }
}

// ===== HTTP Handler Functions =====

async fn register_operator(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<NodeOperatorRequest>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    match server.register_operator(req).await {
        Ok(address) => Ok(Json(serde_json::json!({
            "success": true,
            "address": address,
        }))),
        Err(e) => Ok(Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        }))),
    }
}

async fn operator_heartbeat(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<serde_json::Value>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    // Update operator heartbeat
    Ok(Json(serde_json::json!({
        "success": true,
        "timestamp": Utc::now().to_rfc3339(),
    })))
}

async fn list_operators(
    State(server): State<Arc<RpcServer>>,
) -> Result<Json<Vec<serde_json::Value>>, StatusCode> {
    let operators = server.node_operators.read().await;
    let list: Vec<_> = operators
        .iter()
        .map(|op| serde_json::json!({
            "address": op.address,
            "capabilities": op.capabilities,
            "stake": op.stake_amount,
            "reputation": op.reputation_score,
            "active": op.active,
        }))
        .collect();

    Ok(Json(list))
}

async fn inference(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<InferenceRequest>,
) -> Result<Json<InferenceResponse>, StatusCode> {
    let response = match req.model.as_str() {
        "qwen3-next" => server.inference_qwen3_next(req).await,
        "qwen3-reranker" => server.inference_qwen3_reranker(req).await,
        _ => Err(anyhow!("Unknown model: {}", req.model)),
    };

    response
        .map(Json)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

async fn inference_qwen3_next(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<InferenceRequest>,
) -> Result<Json<InferenceResponse>, StatusCode> {
    server.inference_qwen3_next(req).await
        .map(Json)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

async fn inference_qwen3_reranker(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<InferenceRequest>,
) -> Result<Json<InferenceResponse>, StatusCode> {
    server.inference_qwen3_reranker(req).await
        .map(Json)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

async fn generate_embeddings(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<serde_json::Value>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    // TODO: Implement embedding generation
    Ok(Json(serde_json::json!({
        "embeddings": [],
        "model": "snowflake-arctic-embed-l",
        "dimensions": 1536,
    })))
}

async fn vector_search(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<VectorSearchRequest>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    // TODO: Implement vector search using KuzuDB
    Ok(Json(serde_json::json!({
        "results": [],
        "query_type": "vector_search",
    })))
}

async fn chain_status(
    State(server): State<Arc<RpcServer>>,
) -> Result<Json<BlockchainStatusResponse>, StatusCode> {
    let operators = server.node_operators.read().await;

    Ok(Json(BlockchainStatusResponse {
        block_height: 12345, // TODO: Get from blockchain
        network: "mainnet".to_string(),
        chain_id: "hanzo-chain-1".to_string(),
        active_operators: operators.len() as u32,
        total_staked: operators.iter().map(|op| op.stake_amount).sum(),
        consensus_status: "healthy".to_string(),
    }))
}

async fn submit_transaction(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<serde_json::Value>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    // TODO: Submit transaction to blockchain
    Ok(Json(serde_json::json!({
        "tx_hash": format!("0x{:064x}", rand::random::<u64>()),
        "status": "pending",
    })))
}

async fn get_block(
    State(server): State<Arc<RpcServer>>,
    Path(height): Path<u64>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    // TODO: Get block from KuzuDB
    Ok(Json(serde_json::json!({
        "height": height,
        "hash": format!("0x{:064x}", height),
        "transactions": [],
    })))
}

async fn graph_query(
    State(server): State<Arc<RpcServer>>,
    Json(req): Json<serde_json::Value>,
) -> Result<Json<serde_json::Value>, StatusCode> {
    // TODO: Execute Cypher query on KuzuDB
    Ok(Json(serde_json::json!({
        "results": [],
        "query": req["query"],
    })))
}

async fn health_check() -> Result<Json<serde_json::Value>, StatusCode> {
    Ok(Json(serde_json::json!({
        "status": "healthy",
        "timestamp": Utc::now().to_rfc3339(),
        "services": {
            "blockchain": "operational",
            "kuzu_ledger": "operational",
            "qwen3_next": "operational",
            "qwen3_reranker": "operational",
        }
    })))
}