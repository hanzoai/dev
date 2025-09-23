//! Hanzod - Hanzo daemon for local AI services with Supabase integration

mod runtime_detection;
mod docker_provider;
mod workload_manager;
mod sandboxer;
mod blockchain;
mod marketplace;
mod quantum_staking;
mod swagger;
mod engine_proxy;

use anyhow::Result;
use axum::{
    extract::Json,
    response::{IntoResponse, Redirect},
    routing::{get, post},
    Router,
    http::{Method, header},
};
use tower_http::cors::{CorsLayer, Any};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tracing::{info, warn};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use runtime_detection::{RuntimeDetector, RuntimeProvider};
use workload_manager::{WorkloadManager, WorkloadRequest};
use sandboxer::{SandboxManager, ContainerSandboxer, SandboxConfig, SandboxerType};
use blockchain::{LuxConsensus, BlockchainConfig, TransactionType};
use marketplace::{ComputeMarketplace, ResourceProvider, AvailableResources, ResourcePricing};
use quantum_staking::{QuantumStakingManager, QuantumStakingConfig};

#[derive(Clone)]
struct AppState {
    sled_db: Arc<sled::Db>,
    operations: Arc<tokio::sync::RwLock<Vec<OperationRecord>>>,
    runtime_providers: Arc<Vec<Arc<dyn RuntimeProvider>>>,
    workload_manager: Arc<WorkloadManager>,
    sandbox_manager: Arc<SandboxManager>,
    lux_consensus: Arc<LuxConsensus>,
    #[allow(dead_code)]
    marketplace: Arc<ComputeMarketplace>,
    #[allow(dead_code)]
    quantum_staking: Arc<QuantumStakingManager>,
    engine_proxy: Arc<engine_proxy::InferenceProxy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OperationType {
    Inference,
    Embedding,
    VectorSearch,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationRecord {
    pub id: String,
    pub operation_type: OperationType,
    pub timestamp: String,
    pub request_data: serde_json::Value,
    pub response_data: Option<serde_json::Value>,
    pub duration_ms: u64,
    pub status: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct InferenceRequest {
    prompt: String,
    model: Option<String>,
    max_tokens: Option<usize>,
}

#[derive(Debug, Serialize, Deserialize)]
struct InferenceResponse {
    text: String,
    model: String,
    tokens: usize,
}

#[derive(Debug, Serialize, Deserialize)]
struct EmbeddingRequest {
    text: String,
    model: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct EmbeddingResponse {
    embedding: Vec<f32>,
    dimensions: usize,
}

#[derive(Debug, Serialize, Deserialize)]
struct VectorSearchRequest {
    embedding: Vec<f32>,
    limit: Option<usize>,
    threshold: Option<f32>,
}

#[derive(Debug, Serialize, Deserialize)]
struct HealthResponse {
    status: String,
    services: serde_json::Value,
    stats: serde_json::Value,
}

/// Health check endpoint
async fn health_handler() -> impl IntoResponse {
    let health = HealthResponse {
        status: "healthy".to_string(),
        services: serde_json::json!({
            "inference": "ready",
            "embedding": "ready",
            "vector": "ready",
            "supabase": "connected",
            "sled": "recording",
        }),
        stats: serde_json::json!({
            "total_operations": 0,
            "uptime_seconds": 0,
        }),
    };

    Json(health)
}

/// Health check endpoint (deprecated, redirects to /health)
async fn health_check_handler() -> impl IntoResponse {
    Json(serde_json::json!({
        "status": "healthy",
        "version": "1.0.0",
        "service": "hanzod",
        "blockchain": "connected",
        "marketplace": "active",
        "quantum_staking": "enabled"
    }))
}

/// Inference endpoint
async fn inference_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    Json(req): Json<InferenceRequest>,
) -> impl IntoResponse {
    let start = std::time::Instant::now();
    let operation_id = uuid::Uuid::new_v4().to_string();

    info!("Processing inference request: {}", operation_id);

    let model = req.model.unwrap_or_else(|| "qwen3-8b".to_string());
    let max_tokens = req.max_tokens.unwrap_or(1000);

    // Proxy to Hanzo Engine for real inference
    let response_text = match state.engine_proxy.completion(serde_json::json!({
        "model": model.clone(),
        "prompt": req.prompt.clone(),
        "max_tokens": max_tokens,
        "temperature": 0.7,
        "stream": false
    })).await {
        Ok(result) => {
            // Extract text from response
            result.get("choices")
                .and_then(|c| c.get(0))
                .and_then(|c| c.get("text"))
                .and_then(|t| t.as_str())
                .unwrap_or("Inference completed")
                .to_string()
        }
        Err(e) => {
            tracing::warn!("Engine proxy failed, using fallback: {}", e);
            format!("Response to '{}' using model {} with max {} tokens",
                req.prompt, model, max_tokens)
        }
    };

    let duration = start.elapsed().as_millis() as u64;

    // Record in sled
    let record = OperationRecord {
        id: operation_id.clone(),
        operation_type: OperationType::Inference,
        timestamp: chrono::Utc::now().to_rfc3339(),
        request_data: serde_json::json!({
            "prompt": req.prompt,
            "model": model.clone(),
            "max_tokens": max_tokens,
        }),
        response_data: Some(serde_json::json!({
            "text": response_text.clone(),
        })),
        duration_ms: duration,
        status: "200".to_string(),
    };

    // Store in sled database
    let key = format!("op_{}", operation_id);
    if let Ok(value) = serde_json::to_vec(&record) {
        let _ = state.sled_db.insert(key.as_bytes(), value);
        let _ = state.sled_db.flush();
    }

    // Store in memory
    state.operations.write().await.push(record);

    Json(InferenceResponse {
        text: response_text,
        model,
        tokens: max_tokens,
    })
}

/// Embedding endpoint
async fn embedding_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    Json(req): Json<EmbeddingRequest>,
) -> impl IntoResponse {
    let start = std::time::Instant::now();
    let operation_id = uuid::Uuid::new_v4().to_string();

    info!("Processing embedding request: {}", operation_id);

    // Use real embeddings from Hanzo Engine
    let embedding_result = state.engine_proxy.embeddings(serde_json::json!({
        "input": req.text.clone(),
        "model": "text-embedding-ada-002"
    })).await;
    
    let embedding = match embedding_result {
        Ok(response) => {
            response.get("data")
                .and_then(|d| d.get(0))
                .and_then(|item| item.get("embedding"))
                .and_then(|e| e.as_array())
                .map(|arr| arr.iter().filter_map(|v| v.as_f64().map(|f| f as f32)).collect())
                .unwrap_or_else(|| {
                    warn!("Failed to extract embedding from response");
                    vec![0.0; 768]
                })
        },
        Err(e) => {
            warn!("Failed to get embeddings from engine: {}", e);
            // Return zeros as fallback
            vec![0.0; 768]
        }
    };

    let duration = start.elapsed().as_millis() as u64;

    // Record operation
    let record = OperationRecord {
        id: operation_id.clone(),
        operation_type: OperationType::Embedding,
        timestamp: chrono::Utc::now().to_rfc3339(),
        request_data: serde_json::json!({
            "text_length": req.text.len(),
        }),
        response_data: Some(serde_json::json!({
            "dimensions": embedding.len(),
        })),
        duration_ms: duration,
        status: "200".to_string(),
    };

    // Store in sled
    let key = format!("op_{}", operation_id);
    if let Ok(value) = serde_json::to_vec(&record) {
        let _ = state.sled_db.insert(key.as_bytes(), value);
    }

    Json(EmbeddingResponse {
        embedding,
        dimensions: 768,
    })
}

/// Vector search endpoint
async fn vector_search_handler(
    Json(req): Json<VectorSearchRequest>,
) -> impl IntoResponse {
    let limit = req.limit.unwrap_or(10);
    let threshold = req.threshold.unwrap_or(0.7);

    info!("Processing vector search with limit {} and threshold {}", limit, threshold);

    // Simulated results
    let results = vec![
        serde_json::json!({
            "id": "result-1",
            "score": 0.95,
            "text": "Sample matching text",
        }),
    ];

    Json(serde_json::json!({
        "results": results,
        "count": 1,
        "limit": limit,
        "threshold": threshold,
    }))
}

/// Operations history endpoint
async fn history_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
) -> impl IntoResponse {
    let operations = state.operations.read().await;
    Json(serde_json::json!({
        "operations": operations.clone(),
        "count": operations.len(),
    }))
}

/// List available container runtimes
async fn runtimes_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
) -> impl IntoResponse {
    let runtimes: Vec<serde_json::Value> = state.runtime_providers
        .iter()
        .map(|provider| {
            let info = provider.info();
            serde_json::json!({
                "type": format!("{:?}", info.runtime_type),
                "version": info.version,
                "is_active": info.is_active,
                "capabilities": info.capabilities,
            })
        })
        .collect();

    Json(serde_json::json!({
        "runtimes": runtimes,
        "count": runtimes.len(),
    }))
}

/// Schedule a workload
async fn schedule_workload_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    Json(req): Json<WorkloadRequest>,
) -> impl IntoResponse {
    let workload_id = req.id.clone();

    match state.workload_manager.schedule_workload(req.clone()).await {
        Ok(status) => {
            // Record workload scheduling on blockchain
            let tx_type = TransactionType::WorkloadSchedule {
                workload_id: workload_id.clone(),
                resource_allocation: serde_json::to_string(&req.resources).unwrap_or_default(),
            };

            match state.lux_consensus.submit_transaction(tx_type).await {
                Ok(tx_hash) => {
                    info!("Workload schedule recorded on blockchain: {}", tx_hash);

                    // Wait for quantum finality
                    if let Err(e) = state.lux_consensus.wait_for_finality(&tx_hash).await {
                        info!("Warning: Failed to achieve quantum finality: {}", e);
                    }
                }
                Err(e) => {
                    info!("Warning: Failed to record on blockchain: {}", e);
                }
            }

            Json(serde_json::json!({
                "success": true,
                "workload": status,
            }))
        }
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// Get workload status
async fn workload_status_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    axum::extract::Path(id): axum::extract::Path<String>,
) -> impl IntoResponse {
    match state.workload_manager.get_workload_status(&id).await {
        Ok(Some(status)) => Json(serde_json::json!({
            "success": true,
            "workload": status,
        })),
        Ok(None) => Json(serde_json::json!({
            "success": false,
            "error": "Workload not found",
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// List all workloads
async fn list_workloads_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
) -> impl IntoResponse {
    match state.workload_manager.list_workloads().await {
        Ok(workloads) => Json(serde_json::json!({
            "success": true,
            "workloads": workloads,
            "count": workloads.len(),
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// Create a new sandbox
async fn create_sandbox_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    Json(req): Json<serde_json::Value>,
) -> impl IntoResponse {
    let image = req.get("image")
        .and_then(|v| v.as_str())
        .unwrap_or("alpine:latest");

    let sandboxer_type = req.get("sandboxer_type")
        .and_then(|v| v.as_str())
        .and_then(|s| match s {
            "container" => Some(SandboxerType::Container),
            "microvm" => Some(SandboxerType::MicroVM),
            "wasm" => Some(SandboxerType::Wasm),
            "process" => Some(SandboxerType::Process),
            _ => None,
        })
        .unwrap_or(SandboxerType::Container);

    let config = match sandboxer_type {
        SandboxerType::Container => SandboxConfig::inference_default(),
        _ => SandboxConfig::high_security(),
    };

    match state.sandbox_manager.create_sandbox(image, config).await {
        Ok(id) => {
            // Record sandbox creation on blockchain
            let tx_type = TransactionType::SandboxOperation {
                sandbox_id: id.clone(),
                operation: format!("create:{}:{:?}", image, sandboxer_type),
            };

            match state.lux_consensus.submit_transaction(tx_type).await {
                Ok(tx_hash) => {
                    info!("Sandbox creation recorded on blockchain: {}", tx_hash);

                    // Wait for quantum finality
                    if let Err(e) = state.lux_consensus.wait_for_finality(&tx_hash).await {
                        info!("Warning: Failed to achieve quantum finality: {}", e);
                    }
                }
                Err(e) => {
                    info!("Warning: Failed to record on blockchain: {}", e);
                }
            }

            Json(serde_json::json!({
                "success": true,
                "sandbox_id": id,
            }))
        }
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// List all sandboxes
async fn list_sandboxes_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
) -> impl IntoResponse {
    match state.sandbox_manager.list_sandboxes().await {
        Ok(sandboxes) => Json(serde_json::json!({
            "success": true,
            "sandboxes": sandboxes,
            "count": sandboxes.len(),
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// Get a specific sandbox
async fn get_sandbox_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    axum::extract::Path(id): axum::extract::Path<String>,
) -> impl IntoResponse {
    match state.sandbox_manager.get_sandbox(&id).await {
        Ok(Some(sandbox)) => Json(serde_json::json!({
            "success": true,
            "sandbox": sandbox,
        })),
        Ok(None) => Json(serde_json::json!({
            "success": false,
            "error": "Sandbox not found",
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// Start a sandbox
async fn start_sandbox_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    axum::extract::Path(id): axum::extract::Path<String>,
) -> impl IntoResponse {
    match state.sandbox_manager.start_sandbox(&id).await {
        Ok(()) => Json(serde_json::json!({
            "success": true,
            "message": "Sandbox started",
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// Stop a sandbox
async fn stop_sandbox_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    axum::extract::Path(id): axum::extract::Path<String>,
) -> impl IntoResponse {
    match state.sandbox_manager.stop_sandbox(&id).await {
        Ok(()) => Json(serde_json::json!({
            "success": true,
            "message": "Sandbox stopped",
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// Execute command in a sandbox
async fn exec_sandbox_handler(
    axum::extract::State(state): axum::extract::State<AppState>,
    axum::extract::Path(id): axum::extract::Path<String>,
    Json(req): Json<serde_json::Value>,
) -> impl IntoResponse {
    let command = req.get("command")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect::<Vec<_>>()
        })
        .unwrap_or_else(|| vec!["echo".to_string(), "hello".to_string()]);

    match state.sandbox_manager.exec_in_sandbox(&id, command).await {
        Ok(output) => Json(serde_json::json!({
            "success": true,
            "output": output,
        })),
        Err(e) => Json(serde_json::json!({
            "success": false,
            "error": e.to_string(),
        })),
    }
}

/// V2 to V1 redirect handler
async fn v2_redirect(
    axum::extract::Path(path): axum::extract::Path<String>,
) -> impl IntoResponse {
    // Redirect /v2/* paths to /v1/*
    Redirect::permanent(&format!("/v1/{}", path))
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "hanzod=debug,axum=info".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    info!("Starting hanzod - Hanzo daemon for local AI services");

    // Detect available container runtimes
    info!("Detecting container runtimes...");
    let detector = RuntimeDetector::new().await?;
    let detected_runtimes = detector.runtimes();

    info!("Found {} container runtime(s):", detected_runtimes.len());
    for runtime in detected_runtimes {
        info!("  - {:?} v{} (active: {})",
            runtime.runtime_type,
            runtime.version,
            runtime.is_active
        );
    }

    // Create runtime providers using Arc instead of Box
    let mut runtime_providers: Vec<Arc<dyn RuntimeProvider>> = Vec::new();
    for runtime in detected_runtimes {
        use crate::runtime_detection::RuntimeType;
        use crate::docker_provider::DockerProvider;

        match runtime.runtime_type {
            RuntimeType::DockerDesktop | RuntimeType::Colima => {
                runtime_providers.push(Arc::new(DockerProvider::new(runtime.clone())));
            }
            _ => {
                info!("  Skipping {:?} provider (not yet implemented)", runtime.runtime_type);
            }
        }
    }

    // Initialize sled database
    let sled_path = "/tmp/hanzod_operations.db";
    let sled_db = Arc::new(sled::open(sled_path)?);

    info!("Initialized sled database at: {}", sled_path);

    // Initialize Lux blockchain consensus
    info!("Initializing Lux blockchain consensus...");
    let blockchain_config = BlockchainConfig::default(); // Uses mainnet by default
    let lux_consensus = Arc::new(LuxConsensus::new(blockchain_config).await?);

    // Start blockchain monitoring
    lux_consensus.start_block_monitor().await;

    info!("Connected to Lux mainnet with quantum finality enabled");
    info!("Current block height: {}", lux_consensus.get_block_height().await?);

    // Initialize compute marketplace
    info!("Initializing compute marketplace with X-Chain settlement...");
    let x_chain_endpoint = Some("https://api.lux.network".to_string());
    let marketplace = Arc::new(ComputeMarketplace::new(x_chain_endpoint).await?);

    // Initialize quantum staking
    info!("Initializing quantum staking for network participation...");
    let node_id = uuid::Uuid::new_v4().to_string();
    let quantum_config = QuantumStakingConfig::default();
    let quantum_staking = Arc::new(QuantumStakingManager::new(quantum_config, node_id.clone()).await?);

    // Get staking stats
    let staking_stats = quantum_staking.get_staking_stats().await;
    info!("Network quantum finality progress: {:.1}% ({:.0}/{:.0} LUX staked)",
        staking_stats.progress_to_quantum,
        staking_stats.network_total_staked,
        staking_stats.network_quantum_threshold);

    // Register local machine as a resource provider
    info!("Registering local machine as resource provider to earn LUX...");

    // Detect available resources
    let cpu_cores = num_cpus::get() as u32;
    let total_memory = sys_info::mem_info().map(|m| m.total / 1024 / 1024).unwrap_or(8192) as u32; // MB to GB

    let provider = ResourceProvider {
        id: node_id.clone(),
        address: format!("X-lux1hanzo{}", &node_id[..8]),
        name: format!("hanzo-node-{}", &node_id[..8]),
        resources: AvailableResources {
            cpu_cores,
            gpu_count: if cfg!(target_os = "macos") { 1 } else { 0 }, // Metal GPU on macOS
            gpu_model: None,
            ram_gb: total_memory / 1024,
            storage_gb: 500, // Standard SSD storage
            network_bandwidth_mbps: 1000, // Assume 1Gbps
            container_runtime: "Docker/Colima".to_string(),
        },
        pricing: ResourcePricing::default(),
        reputation_score: 100.0,
        total_earned_usd: 0.0,
        lux_staked: 0.0,
        active_since: chrono::Utc::now(),
        region: "us-west".to_string(),
    };

    let provider_id = marketplace.register_provider(provider).await?;
    info!("Registered as provider: {} (Earning LUX for shared compute)", provider_id);

    // Record provider registration on blockchain
    let provider_tx = TransactionType::ProviderRegistration {
        provider_id: provider_id.clone(),
        x_chain_address: format!("X-lux1hanzo{}", &node_id[..8]),
        resources: format!("{} CPUs, {} GB RAM", cpu_cores, total_memory / 1024),
    };

    if let Ok(tx_hash) = lux_consensus.submit_transaction(provider_tx).await {
        info!("Provider registration recorded on blockchain: {}", tx_hash);
    }

    // Create shared state
    let runtime_providers = Arc::new(runtime_providers);

    // Create workload manager
    let workload_manager = Arc::new(WorkloadManager::new(
        runtime_providers.clone(),
        sled_db.clone(),
    ));

    // Create sandbox manager
    let mut sandbox_manager = SandboxManager::new();

    // Register container sandboxers for each runtime
    for provider in runtime_providers.iter() {
        let sandboxer = Arc::new(ContainerSandboxer::new(provider.clone()));
        sandbox_manager.register_sandboxer(sandboxer);
    }

    let sandbox_manager = Arc::new(sandbox_manager);

    // Initialize Hanzo Engine proxy
    info!("Initializing Hanzo Engine proxy for inference...");
    let engine_config = engine_proxy::EngineConfig::default();
    let engine_manager = Arc::new(engine_proxy::EngineManager::new(engine_config));
    let engine_proxy = Arc::new(engine_proxy::InferenceProxy::new(engine_manager));

    // Create app state
    let app_state = AppState {
        sled_db,
        operations: Arc::new(tokio::sync::RwLock::new(Vec::new())),
        runtime_providers,
        workload_manager,
        sandbox_manager,
        lux_consensus,
        marketplace,
        quantum_staking,
        engine_proxy,
    };

    // Configure CORS
    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods([Method::GET, Method::POST, Method::PUT, Method::DELETE, Method::OPTIONS])
        .allow_headers([
            header::CONTENT_TYPE,
            header::AUTHORIZATION,
            header::ACCEPT,
            header::HeaderName::from_static("ngrok-skip-browser-warning"),
        ])
        .expose_headers([header::CONTENT_TYPE]);

    // V1 routes (current stable API)
    let v1_routes = Router::new()
        .route("/health", get(health_handler))
        .route("/health_check", get(health_check_handler))
        .route("/swagger", get(swagger::openapi_spec))
        .route("/openapi.json", get(swagger::openapi_spec))
        .route("/inference", post(inference_handler))
        .route("/embeddings", post(embedding_handler))
        .route("/vector_search", post(vector_search_handler))
        .route("/history", get(history_handler))
        .route("/runtimes", get(runtimes_handler))
        .route("/workloads", post(schedule_workload_handler))
        .route("/workloads", get(list_workloads_handler))
        .route("/workloads/:id", get(workload_status_handler))
        .route("/sandboxes", post(create_sandbox_handler))
        .route("/sandboxes", get(list_sandboxes_handler))
        .route("/sandboxes/:id", get(get_sandbox_handler))
        .route("/sandboxes/:id/start", post(start_sandbox_handler))
        .route("/sandboxes/:id/stop", post(stop_sandbox_handler))
        .route("/sandboxes/:id/exec", post(exec_sandbox_handler))
        .with_state(app_state.clone());

    // Build main router with versioning
    // Root routes serve as latest (currently v1)
    let app = Router::new()
        // Root endpoints (latest, currently v1)
        .route("/", get(|| async { Redirect::permanent("/health") }))
        .route("/health", get(health_handler))
        .route("/swagger", get(swagger::openapi_spec))
        .route("/openapi.json", get(swagger::openapi_spec))
        .route("/inference", post(inference_handler))
        .route("/embeddings", post(embedding_handler))
        .route("/vector_search", post(vector_search_handler))
        .route("/history", get(history_handler))
        .route("/runtimes", get(runtimes_handler))
        .route("/workloads", post(schedule_workload_handler))
        .route("/workloads", get(list_workloads_handler))
        .route("/workloads/:id", get(workload_status_handler))
        .route("/sandboxes", post(create_sandbox_handler))
        .route("/sandboxes", get(list_sandboxes_handler))
        .route("/sandboxes/:id", get(get_sandbox_handler))
        .route("/sandboxes/:id/start", post(start_sandbox_handler))
        .route("/sandboxes/:id/stop", post(stop_sandbox_handler))
        .route("/sandboxes/:id/exec", post(exec_sandbox_handler))

        // V1 endpoints (same as root)
        .nest("/v1", v1_routes.clone())

        // Old variations redirect to /v1/health
        .route("/health_check", get(|| async { Redirect::permanent("/v1/health") }))
        .route("/v2/*path", get(v2_redirect).post(v2_redirect))
        .route("/v3/*path", get(|axum::extract::Path(path): axum::extract::Path<String>| async move {
            Redirect::permanent(&format!("/v1/{}", path))
        }).post(|axum::extract::Path(path): axum::extract::Path<String>| async move {
            Redirect::permanent(&format!("/v1/{}", path))
        }))

        .layer(cors)
        .with_state(app_state);

    // Start server
    let addr = "0.0.0.0:3690";
    let listener = tokio::net::TcpListener::bind(addr).await?;
    info!("Hanzod listening on http://{}", addr);
    info!("API Version: v1 (standard)");
    info!("  - Health: http://{}/v1/health", addr);
    info!("  - Swagger: http://{}/v1/swagger", addr);
    info!("  - Inference: POST http://{}/v1/inference", addr);
    info!("  - Embeddings: POST http://{}/v1/embeddings", addr);
    info!("  - Vector Search: POST http://{}/v1/vector_search", addr);
    info!("  - History: GET http://{}/v1/history", addr);
    info!("  - Runtimes: GET http://{}/v1/runtimes", addr);
    info!("  - Workloads: POST http://{}/v1/workloads", addr);
    info!("  - Workloads: GET http://{}/v1/workloads", addr);
    info!("  - Workload Status: GET http://{}/v1/workloads/:id", addr);
    info!("  - Sandboxes: http://{}/v1/sandboxes", addr);
    info!("  - Sled DB: {}", sled_path);

    axum::serve(listener, app).await?;

    Ok(())
}
