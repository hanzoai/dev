//! Hanzo Node - RPC-based compute node for Hanzo Platform
//!
//! This crate provides:
//! - P2P networking via libp2p for node discovery and communication
//! - gRPC server for compute pool operations
//! - Storage layer for deployment state
//! - Compute orchestration for container management
//! - IAM (Identity and Access Management) for authentication and authorization

#![deny(clippy::expect_used)]
#![deny(clippy::unwrap_used)]

pub mod compute;
pub mod consensus;
pub mod iam;
pub mod p2p;
pub mod rpc;
pub mod storage;

mod error;

pub use error::{Error, Result};
pub use iam::{IamConfig, IamService};

use std::sync::Arc;
use tokio::sync::RwLock;

/// Default bind address for the embedded canonical engine HTTP API.
/// Matches the `node` provider contract used by hanzo-dev.
pub const DEFAULT_ENGINE_API_ADDR: &str = "0.0.0.0:36900";

/// Configuration for the optional in-process canonical inference engine.
///
/// When `enabled` and a model is configured (only meaningful with the `engine`
/// feature compiled in), the node serves the engine's OpenAI/Anthropic/Responses
/// HTTP API on `engine_api_addr`, concurrently with the gRPC server. When the
/// `engine` feature is off, or `enabled` is false, this is inert and the node
/// behaves identically to before.
#[derive(Debug, Clone)]
pub struct EngineConfig {
    /// Serve the embedded engine HTTP API. No-op unless the `engine` feature is built.
    pub enabled: bool,
    /// Force CPU-only inference.
    pub cpu: bool,
    /// Bind address for the engine HTTP API.
    pub engine_api_addr: String,
    /// Model to load via the auto loader. Required for the engine to actually start.
    #[cfg(feature = "engine")]
    pub model: Option<hanzo_engine::ModelSelected>,
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            cpu: false,
            engine_api_addr: DEFAULT_ENGINE_API_ADDR.to_string(),
            #[cfg(feature = "engine")]
            model: None,
        }
    }
}

/// Shared canonical-engine state, re-exported for downstream callers.
#[cfg(feature = "engine")]
pub use hanzo_server_core::types::SharedHanzoState;

/// Node configuration
#[derive(Debug, Clone)]
pub struct NodeConfig {
    /// Unique node identifier
    pub node_id: String,
    /// P2P listen addresses
    pub p2p_listen_addrs: Vec<String>,
    /// gRPC server bind address
    pub grpc_addr: String,
    /// HTTP health check address
    pub http_addr: String,
    /// Data directory for storage
    pub data_dir: String,
    /// Bootstrap peers for P2P network
    pub bootstrap_peers: Vec<String>,
    /// Network ID (43114 mainnet, 43113 testnet, 1337 local)
    pub network_id: String,
    /// Operator wallet address
    pub operator_address: Option<String>,
    /// Enable MLX acceleration (macOS only)
    pub mlx_enabled: bool,
    /// IAM configuration for authentication/authorization
    pub iam: IamConfig,
    /// Lux node endpoint for consensus replication (e.g., "http://localhost:9650")
    pub lux_endpoint: Option<String>,
    /// Embedded canonical inference engine configuration (default-off).
    pub engine: EngineConfig,
}

impl Default for NodeConfig {
    fn default() -> Self {
        Self {
            node_id: uuid::Uuid::new_v4().to_string(),
            p2p_listen_addrs: vec!["/ip4/0.0.0.0/tcp/9000".to_string()],
            grpc_addr: "0.0.0.0:50051".to_string(),
            http_addr: "0.0.0.0:8080".to_string(),
            data_dir: "./data".to_string(),
            bootstrap_peers: vec![],
            network_id: "1337".to_string(),
            operator_address: None,
            mlx_enabled: false,
            iam: IamConfig::default(),
            lux_endpoint: None,
            engine: EngineConfig::default(),
        }
    }
}

/// Node state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeState {
    Starting,
    Ready,
    Draining,
    Stopped,
}

/// The main Hanzo Node instance
pub struct HanzoNode {
    config: NodeConfig,
    state: Arc<RwLock<NodeState>>,
    storage: Arc<storage::Storage>,
    p2p: Arc<p2p::P2PNetwork>,
    compute: Arc<compute::ComputeManager>,
    iam: Arc<IamService>,
    start_time: std::time::Instant,
    #[cfg(feature = "engine")]
    hanzo_state: Option<SharedHanzoState>,
}

impl HanzoNode {
    /// Create a new Hanzo Node instance
    pub async fn new(config: NodeConfig) -> Result<Self> {
        tracing::info!(node_id = %config.node_id, "Initializing Hanzo Node");

        // Initialize storage
        let storage = Arc::new(storage::Storage::new(&config.data_dir)?);

        // Initialize P2P network
        let p2p = Arc::new(p2p::P2PNetwork::new(&config).await?);

        // Initialize compute manager
        let compute = Arc::new(compute::ComputeManager::new(storage.clone())?);

        // Initialize IAM service
        let iam = Arc::new(
            IamService::new(config.iam.clone())
                .await
                .map_err(|e| Error::Config(format!("IAM initialization failed: {e}")))?,
        );

        if iam.is_enabled() {
            tracing::info!("IAM authentication enabled");
        } else {
            tracing::warn!("IAM authentication disabled - all requests will be allowed");
        }

        #[cfg(feature = "engine")]
        let hanzo_state = Self::build_engine_state(&config.engine).await?;

        Ok(Self {
            config,
            state: Arc::new(RwLock::new(NodeState::Starting)),
            storage,
            p2p,
            compute,
            iam,
            start_time: std::time::Instant::now(),
            #[cfg(feature = "engine")]
            hanzo_state,
        })
    }

    /// Build the embedded canonical-engine state when enabled and a model is set.
    /// Returns `Ok(None)` (and the node serves gRPC only) when the engine is
    /// disabled or no model is configured.
    #[cfg(feature = "engine")]
    async fn build_engine_state(cfg: &EngineConfig) -> Result<Option<SharedHanzoState>> {
        use hanzo_server_core::hanzo_for_server_builder::HanzoForServerBuilder;

        if !cfg.enabled {
            return Ok(None);
        }
        let Some(model) = cfg.model.clone() else {
            tracing::warn!(
                "Embedded engine enabled but no model configured; engine HTTP API will not start. \
                 Pass --model/HANZO_ENGINE_MODEL to load one."
            );
            return Ok(None);
        };

        tracing::info!(cpu = cfg.cpu, "Loading embedded canonical engine model");
        let state = HanzoForServerBuilder::new()
            .with_model(model)
            .with_cpu(cfg.cpu)
            .build()
            .await
            .map_err(|e| Error::Config(format!("Failed to build embedded engine: {e}")))?;
        Ok(Some(state))
    }

    /// Start the node
    pub async fn start(&self) -> Result<()> {
        tracing::info!("Starting Hanzo Node");

        // Update state
        {
            let mut state = self.state.write().await;
            *state = NodeState::Starting;
        }

        // Start P2P network
        self.p2p.start().await?;

        // Start gRPC server
        let rpc_server = rpc::RpcServer::new(
            self.config.clone(),
            self.state.clone(),
            self.storage.clone(),
            self.compute.clone(),
            self.start_time,
        );

        // Update state to ready
        {
            let mut state = self.state.write().await;
            *state = NodeState::Ready;
        }

        tracing::info!(
            grpc_addr = %self.config.grpc_addr,
            http_addr = %self.config.http_addr,
            "Hanzo Node is ready"
        );

        // When the embedded engine is built, serve its HTTP API concurrently with
        // gRPC. Otherwise keep the original behavior: gRPC blocks until shutdown.
        #[cfg(feature = "engine")]
        if let Some(state) = self.hanzo_state.clone() {
            return self.run_with_engine(rpc_server, state).await;
        }

        // Run the gRPC server (this blocks until shutdown)
        rpc_server.run().await?;

        Ok(())
    }

    /// Serve the gRPC server and the embedded engine HTTP API concurrently.
    #[cfg(feature = "engine")]
    async fn run_with_engine(
        &self,
        rpc_server: rpc::RpcServer,
        state: SharedHanzoState,
    ) -> Result<()> {
        use hanzo_server_core::hanzo_server_router_builder::HanzoServerRouterBuilder;

        // hanzo-server-core is pulled with default-features = false, so its
        // `swagger-ui` feature is off and the /docs + /api-doc routes are compiled
        // out. The router therefore exposes only the API surface (chat/completions,
        // responses, messages, models, ...), which is what we want for the node.
        let app = HanzoServerRouterBuilder::new()
            .with_hanzo(state)
            .build()
            .await
            .map_err(|e| Error::Config(format!("Failed to build engine router: {e}")))?;

        let addr = self.config.engine.engine_api_addr.clone();
        let listener = tokio::net::TcpListener::bind(&addr).await?;
        tracing::info!(engine_api = %addr, "Embedded engine HTTP API listening");

        let grpc = tokio::spawn(async move { rpc_server.run().await });
        let engine = tokio::spawn(async move {
            axum::serve(listener, app)
                .await
                .map_err(Error::from)
        });

        let (grpc_res, engine_res) = tokio::join!(grpc, engine);
        grpc_res.map_err(|e| Error::Internal(format!("gRPC task panicked: {e}")))??;
        engine_res.map_err(|e| Error::Internal(format!("engine task panicked: {e}")))??;
        Ok(())
    }

    /// Graceful shutdown
    pub async fn shutdown(&self) -> Result<()> {
        tracing::info!("Shutting down Hanzo Node");

        // Set state to draining
        {
            let mut state = self.state.write().await;
            *state = NodeState::Draining;
        }

        // Stop accepting new work
        self.compute.drain().await?;

        // Stop P2P network
        self.p2p.stop().await?;

        // Set state to stopped
        {
            let mut state = self.state.write().await;
            *state = NodeState::Stopped;
        }

        tracing::info!("Hanzo Node shutdown complete");
        Ok(())
    }

    /// Get current node state
    pub async fn state(&self) -> NodeState {
        *self.state.read().await
    }

    /// Get node configuration
    pub fn config(&self) -> &NodeConfig {
        &self.config
    }

    /// Get uptime in seconds
    pub fn uptime_seconds(&self) -> u64 {
        self.start_time.elapsed().as_secs()
    }

    /// Get the IAM service
    pub fn iam(&self) -> &Arc<IamService> {
        &self.iam
    }
}

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const PKG_NAME: &str = env!("CARGO_PKG_NAME");
