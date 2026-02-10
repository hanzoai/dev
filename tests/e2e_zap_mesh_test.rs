//! E2E ZAP Mesh Test: Agent mesh communication using ZAP protocol
//!
//! ZAP: Zero-copy Agent Protocol v0.3.0
//! Profile: zap-1
//! Namespace: ai.hanzo.zap
//!
//! This test demonstrates:
//! 1. ZAP message routing between agents via hanzod mesh
//! 2. MeshCoordinator peer discovery and topology
//! 3. Agent task delegation with effect/scope annotations
//! 4. Tool invocation through ZAP gateway
//! 5. Zero-copy message formats (wire = memory)
//!
//! Run with: cargo test --test e2e_zap_mesh_test -- --nocapture

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{mpsc, RwLock};
use tokio::time::timeout;
use serde::{Deserialize, Serialize};

/// ZAP Protocol Version
pub const ZAP_VERSION: &str = "0.3.0";
/// ZAP Profile
pub const ZAP_PROFILE: &str = "zap-1";
/// ZAP Namespace
pub const ZAP_NAMESPACE: &str = "ai.hanzo.zap";

// ============================================================================
// ZAP Core Types (zap.zap schema v0.3.0)
// ============================================================================

/// Effect annotation for tool calls
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Effect {
    Pure,              // No side effects, deterministic, cacheable
    Deterministic,     // Side effects, but reproducible given same input
    Nondeterministic,  // May vary between calls
}

/// Scope of operation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Scope {
    Span,       // Single operation
    File,       // File-level
    Repo,       // Repository-level
    Workspace,  // Workspace-level
    Node,       // Node-level
    Chain,      // Blockchain/consensus chain
    Global,     // Global scope
}

/// Task state matching ZAP TaskState enum
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum TaskState {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
}

/// Witness level for audit logging
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum WitnessLevel {
    None,       // No witness logging
    Minimal,    // Basic operation logging
    Full,       // Full witness with inputs/outputs
}

/// Cost model for metering
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum CostModel {
    Free,       // No cost
    Metered,    // Metered usage
    Gas,        // Gas-based (blockchain)
    Quota,      // Quota-based
}

/// Tool stability level
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Stability {
    Experimental,
    Beta,
    Stable,
    Deprecated,
}

/// Metadata for tracing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Meta {
    pub timestamp: u64,
    pub trace_id: String,
    pub span_id: String,
}

impl Default for Meta {
    fn default() -> Self {
        Self {
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
            trace_id: uuid::Uuid::new_v4().to_string(),
            span_id: uuid::Uuid::new_v4().to_string(),
        }
    }
}

/// Call context shared by all tool calls
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallContext {
    pub trace_id: String,
    pub span_id: String,
    pub timeout_ms: u64,
}

impl Default for CallContext {
    fn default() -> Self {
        Self {
            trace_id: uuid::Uuid::new_v4().to_string(),
            span_id: uuid::Uuid::new_v4().to_string(),
            timeout_ms: 30000,
        }
    }
}

// ============================================================================
// Agent Types (matching zap.zap Agent interface)
// ============================================================================

/// Agent configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfig {
    pub name: String,
    pub model: String,
    pub system_prompt: String,
    pub tools: Vec<String>,
    pub max_turns: u32,
    pub timeout_ms: u64,
}

impl Default for AgentConfig {
    fn default() -> Self {
        Self {
            name: "agent".to_string(),
            model: "haiku".to_string(),
            system_prompt: "You are a helpful assistant.".to_string(),
            tools: vec![],
            max_turns: 10,
            timeout_ms: 60000,
        }
    }
}

/// Agent info for listing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentInfo {
    pub id: String,
    pub name: String,
    pub status: TaskState,
    pub turns: u32,
    pub started_at: u64,
}

/// Agent role in the mesh
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentRole {
    CTO,        // Chief Technical Officer - architect
    Developer,  // Implements features
    Reviewer,   // Reviews code
    Coordinator, // Orchestrates other agents
}

// ============================================================================
// Mesh Types (matching zap.zap Mesh interface)
// ============================================================================

/// Peer information in the mesh
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PeerInfo {
    pub id: String,
    pub endpoint: String,
    pub public_key: Vec<u8>,
    pub tools: Vec<String>,
    pub load: f64,
    pub latency_ms: u32,
}

/// Mesh topology snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MeshTopology {
    pub peers: Vec<PeerInfo>,
    pub version: u64,
    pub timestamp: u64,
}

// ============================================================================
// ZAP Catalog Types
// ============================================================================

/// Tool identifier in the ZAP catalog
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ToolId {
    pub namespace: String,  // e.g. "native", "mcp.github", "mcp.stripe"
    pub name: String,       // e.g. "fs.read", "shell.exec"
    pub version: String,    // semver
}

impl ToolId {
    pub fn native(name: &str) -> Self {
        Self {
            namespace: "native".to_string(),
            name: name.to_string(),
            version: ZAP_VERSION.to_string(),
        }
    }

    pub fn mcp(server: &str, name: &str) -> Self {
        Self {
            namespace: format!("mcp.{}", server),
            name: name.to_string(),
            version: "1.0.0".to_string(),
        }
    }
}

/// Tool information in the catalog
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolInfo {
    pub id: ToolId,
    pub description: String,
    pub effect: Effect,
    pub idempotent: bool,
    pub stability: Stability,
}

// ============================================================================
// ZAP Message Types
// ============================================================================

/// ZAP message content types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ZapContent {
    /// Simple text message
    Text(String),

    /// Agent spawn request (Agent::spawn)
    AgentSpawn {
        prompt: String,
        config: AgentConfig,
    },

    /// Task request with ZAP annotations
    TaskRequest {
        description: String,
        priority: u8,
        scope: Scope,
        effect: Effect,
        witness: WitnessLevel,
    },

    /// Task response with status
    TaskResponse {
        task_id: String,
        result: String,
        status: TaskState,
    },

    /// Tool invocation via ZAP gateway (Catalog::invoke)
    ToolCall {
        tool_id: ToolId,
        args: serde_json::Value,
        cost_model: CostModel,
    },

    /// Tool result with effect classification
    ToolResult {
        call_id: String,
        result: serde_json::Value,
        effect: Effect,
    },

    /// Mesh ping (Mesh::ping)
    Ping,
    /// Mesh pong response
    Pong,

    /// Catalog list request (Catalog::listTools)
    CatalogList {
        certified_only: bool,
    },

    /// Catalog response
    CatalogSnapshot {
        version: u64,
        tools: Vec<ToolInfo>,
    },
}

/// A ZAP message with full metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZapMessage {
    pub id: String,
    pub from: String,
    pub to: String,
    pub content: ZapContent,
    pub meta: Meta,
    pub ctx: CallContext,
}

impl ZapMessage {
    pub fn new(from: &str, to: &str, content: ZapContent) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            from: from.to_string(),
            to: to.to_string(),
            content,
            meta: Meta::default(),
            ctx: CallContext::default(),
        }
    }
}

// ============================================================================
// Mesh Coordinator (simulates hanzod)
// ============================================================================

/// Mesh coordinator that routes messages between agents
pub struct MeshCoordinator {
    agents: Arc<RwLock<HashMap<String, mpsc::Sender<ZapMessage>>>>,
    topology: Arc<RwLock<MeshTopology>>,
    message_log: Arc<RwLock<Vec<ZapMessage>>>,
}

impl MeshCoordinator {
    pub fn new() -> Self {
        Self {
            agents: Arc::new(RwLock::new(HashMap::new())),
            topology: Arc::new(RwLock::new(MeshTopology {
                peers: vec![],
                version: 0,
                timestamp: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs(),
            })),
            message_log: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Register an agent with the mesh
    pub async fn register_agent(&self, peer: PeerInfo) -> mpsc::Receiver<ZapMessage> {
        let (tx, rx) = mpsc::channel(100);

        // Add to agents map
        let mut agents = self.agents.write().await;
        agents.insert(peer.id.clone(), tx);

        // Update topology
        let mut topology = self.topology.write().await;
        topology.peers.push(peer.clone());
        topology.version += 1;
        topology.timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs();

        println!("✅ Mesh: Agent '{}' registered (peers: {})", peer.id, topology.peers.len());
        rx
    }

    /// Route a ZAP message to the destination agent
    pub async fn route(&self, msg: ZapMessage) -> Result<(), String> {
        // Log the message
        {
            let mut log = self.message_log.write().await;
            log.push(msg.clone());
        }

        // Find destination agent
        let agents = self.agents.read().await;
        if let Some(tx) = agents.get(&msg.to) {
            tx.send(msg.clone()).await.map_err(|e| e.to_string())?;
            println!("📨 Mesh: Routed {} -> {} ({:?})",
                     msg.from, msg.to,
                     match &msg.content {
                         ZapContent::Text(_) => "Text",
                         ZapContent::TaskRequest { .. } => "TaskRequest",
                         ZapContent::TaskResponse { .. } => "TaskResponse",
                         ZapContent::ToolCall { .. } => "ToolCall",
                         ZapContent::ToolResult { .. } => "ToolResult",
                         ZapContent::AgentSpawn { .. } => "AgentSpawn",
                         ZapContent::Ping => "Ping",
                         ZapContent::Pong => "Pong",
                         ZapContent::CatalogList { .. } => "CatalogList",
                         ZapContent::CatalogSnapshot { .. } => "CatalogSnapshot",
                     });
            Ok(())
        } else {
            Err(format!("Agent '{}' not found in mesh", msg.to))
        }
    }

    /// Get current mesh topology
    pub async fn topology(&self) -> MeshTopology {
        self.topology.read().await.clone()
    }

    /// Get all routed messages
    pub async fn get_messages(&self) -> Vec<ZapMessage> {
        self.message_log.read().await.clone()
    }

    /// Ping an agent and measure latency
    pub async fn ping(&self, peer_id: &str) -> Result<u64, String> {
        let start = std::time::Instant::now();

        let agents = self.agents.read().await;
        if agents.contains_key(peer_id) {
            // Simulate ping response
            tokio::time::sleep(Duration::from_millis(1)).await;
            Ok(start.elapsed().as_nanos() as u64)
        } else {
            Err(format!("Agent '{}' not found", peer_id))
        }
    }
}

// ============================================================================
// ZAP Agent
// ============================================================================

/// A light agent that connects to the mesh via ZAP
pub struct ZapAgent {
    pub id: String,
    pub name: String,
    pub role: AgentRole,
    pub config: AgentConfig,
    inbox: mpsc::Receiver<ZapMessage>,
    outbox: mpsc::Sender<ZapMessage>,
}

impl ZapAgent {
    /// Connect to the mesh coordinator
    pub async fn connect(
        coordinator: Arc<MeshCoordinator>,
        name: &str,
        role: AgentRole,
        config: AgentConfig,
    ) -> Self {
        let id = format!("agent-{}", uuid::Uuid::new_v4());

        let peer = PeerInfo {
            id: id.clone(),
            endpoint: format!("zap://{}", id),
            public_key: vec![0u8; 32], // Placeholder PQ key
            tools: config.tools.clone(),
            load: 0.0,
            latency_ms: 0,
        };

        let inbox = coordinator.register_agent(peer).await;
        let (outbox, mut outbox_rx) = mpsc::channel::<ZapMessage>(100);

        // Spawn message sender task
        let coord = coordinator.clone();
        tokio::spawn(async move {
            while let Some(msg) = outbox_rx.recv().await {
                if let Err(e) = coord.route(msg).await {
                    eprintln!("Failed to route message: {}", e);
                }
            }
        });

        println!("🤖 Agent '{}' ({:?}) connected to mesh", name, role);

        Self {
            id,
            name: name.to_string(),
            role,
            config,
            inbox,
            outbox,
        }
    }

    /// Send a ZAP message to another agent
    pub async fn send(&self, to: &str, content: ZapContent) -> Result<(), String> {
        let msg = ZapMessage::new(&self.id, to, content);
        self.outbox.send(msg).await.map_err(|e| e.to_string())
    }

    /// Receive a message (with timeout)
    pub async fn recv(&mut self, timeout_ms: u64) -> Option<ZapMessage> {
        timeout(Duration::from_millis(timeout_ms), self.inbox.recv())
            .await
            .ok()
            .flatten()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    /// Test: ZAP-style ping-pong between agents
    #[tokio::test]
    async fn test_zap_ping_pong() {
        println!("\n🚀 ZAP E2E Test: Ping-Pong\n");

        let coordinator = Arc::new(MeshCoordinator::new());
        println!("📡 Mesh coordinator started");

        let mut cto = ZapAgent::connect(
            coordinator.clone(),
            "CTO",
            AgentRole::CTO,
            AgentConfig {
                model: "haiku".to_string(),
                ..Default::default()
            },
        ).await;

        let mut dev = ZapAgent::connect(
            coordinator.clone(),
            "Developer",
            AgentRole::Developer,
            AgentConfig {
                model: "haiku".to_string(),
                tools: vec!["fs".to_string(), "shell".to_string()],
                ..Default::default()
            },
        ).await;

        let dev_id = dev.id.clone();
        let cto_id = cto.id.clone();

        // CTO sends Ping
        println!("\n📤 CTO sending Ping to Developer");
        cto.send(&dev_id, ZapContent::Ping).await.unwrap();

        tokio::time::sleep(Duration::from_millis(50)).await;

        // Developer receives Ping
        let msg = dev.recv(1000).await;
        assert!(msg.is_some(), "Developer should receive Ping");
        assert!(matches!(msg.unwrap().content, ZapContent::Ping));
        println!("📥 Developer received Ping");

        // Developer sends Pong
        println!("📤 Developer sending Pong to CTO");
        dev.send(&cto_id, ZapContent::Pong).await.unwrap();

        tokio::time::sleep(Duration::from_millis(50)).await;

        // CTO receives Pong
        let msg = cto.recv(1000).await;
        assert!(msg.is_some(), "CTO should receive Pong");
        assert!(matches!(msg.unwrap().content, ZapContent::Pong));
        println!("📥 CTO received Pong");

        // Verify topology
        let topology = coordinator.topology().await;
        assert_eq!(topology.peers.len(), 2, "Should have 2 peers in mesh");

        // Verify message log
        let messages = coordinator.get_messages().await;
        assert_eq!(messages.len(), 2, "Should have 2 messages in log");

        println!("\n✅ ZAP Ping-Pong test passed!\n");
    }

    /// Test: ZAP task delegation with scope and effect annotations
    #[tokio::test]
    async fn test_zap_task_delegation() {
        println!("\n🚀 ZAP E2E Test: Task Delegation\n");

        let coordinator = Arc::new(MeshCoordinator::new());

        let mut cto = ZapAgent::connect(
            coordinator.clone(),
            "CTO",
            AgentRole::CTO,
            AgentConfig::default(),
        ).await;

        let mut dev = ZapAgent::connect(
            coordinator.clone(),
            "Developer",
            AgentRole::Developer,
            AgentConfig {
                tools: vec!["fs".to_string(), "shell".to_string(), "code".to_string()],
                ..Default::default()
            },
        ).await;

        let dev_id = dev.id.clone();
        let cto_id = cto.id.clone();

        // CTO assigns a task with ZAP metadata
        println!("📋 CTO assigning task: 'Implement user authentication'");
        cto.send(&dev_id, ZapContent::TaskRequest {
            description: "Implement user authentication with JWT tokens".to_string(),
            priority: 1,
            scope: Scope::Repo,
            effect: Effect::Nondeterministic,
            witness: WitnessLevel::Full,
        }).await.unwrap();

        tokio::time::sleep(Duration::from_millis(50)).await;

        // Developer receives and processes task
        let msg = dev.recv(1000).await.unwrap();
        match &msg.content {
            ZapContent::TaskRequest { description, priority, scope, effect, witness } => {
                println!("📥 Developer received task:");
                println!("   Description: {}", description);
                println!("   Priority: {}", priority);
                println!("   Scope: {:?}", scope);
                println!("   Effect: {:?}", effect);
                println!("   Witness: {:?}", witness);
                assert_eq!(*scope, Scope::Repo);
            }
            _ => panic!("Expected TaskRequest"),
        }

        // Developer completes and responds
        println!("✅ Developer completed task, sending response");
        dev.send(&cto_id, ZapContent::TaskResponse {
            task_id: msg.id.clone(),
            result: "Auth implemented with JWT + refresh tokens, tests passing".to_string(),
            status: TaskState::Completed,
        }).await.unwrap();

        tokio::time::sleep(Duration::from_millis(50)).await;

        // CTO receives completion
        let response = cto.recv(1000).await.unwrap();
        match &response.content {
            ZapContent::TaskResponse { task_id, result, status } => {
                println!("📥 CTO received response:");
                println!("   Task ID: {}", task_id);
                println!("   Result: {}", result);
                println!("   Status: {:?}", status);
                assert_eq!(*status, TaskState::Completed);
            }
            _ => panic!("Expected TaskResponse"),
        }

        println!("\n✅ ZAP Task Delegation test passed!\n");
    }

    /// Test: Tool invocation through mesh
    #[tokio::test]
    async fn test_zap_tool_invocation() {
        println!("\n🚀 ZAP E2E Test: Tool Invocation\n");

        let coordinator = Arc::new(MeshCoordinator::new());

        let mut orchestrator = ZapAgent::connect(
            coordinator.clone(),
            "Orchestrator",
            AgentRole::Coordinator,
            AgentConfig::default(),
        ).await;

        let mut tool_provider = ZapAgent::connect(
            coordinator.clone(),
            "ToolProvider",
            AgentRole::Developer,
            AgentConfig {
                tools: vec!["fs".to_string(), "shell".to_string()],
                ..Default::default()
            },
        ).await;

        let provider_id = tool_provider.id.clone();
        let orchestrator_id = orchestrator.id.clone();

        // Orchestrator requests tool invocation via ZAP gateway
        let call_id = uuid::Uuid::new_v4().to_string();
        println!("🔧 Orchestrator invoking fs.read tool via ZAP");
        orchestrator.send(&provider_id, ZapContent::ToolCall {
            tool_id: ToolId::native("fs.read"),
            args: serde_json::json!({
                "path": "/etc/hosts",
                "limit": 10
            }),
            cost_model: CostModel::Free,
        }).await.unwrap();

        tokio::time::sleep(Duration::from_millis(50)).await;

        // Tool provider receives and executes
        let msg = tool_provider.recv(1000).await.unwrap();
        match &msg.content {
            ZapContent::ToolCall { tool_id, args, cost_model } => {
                println!("📥 ToolProvider received ZAP call:");
                println!("   Tool: {}.{} v{}", tool_id.namespace, tool_id.name, tool_id.version);
                println!("   Args: {}", args);
                println!("   CostModel: {:?}", cost_model);

                // Simulate tool execution
                tool_provider.send(&orchestrator_id, ZapContent::ToolResult {
                    call_id: call_id.clone(),
                    result: serde_json::json!({
                        "content": "127.0.0.1 localhost\n::1 localhost",
                        "lines": 2
                    }),
                    effect: Effect::Deterministic,
                }).await.unwrap();
            }
            _ => panic!("Expected ToolCall"),
        }

        tokio::time::sleep(Duration::from_millis(50)).await;

        // Orchestrator receives result
        let result = orchestrator.recv(1000).await.unwrap();
        match &result.content {
            ZapContent::ToolResult { call_id: _, result, effect } => {
                println!("📥 Orchestrator received result:");
                println!("   Result: {}", result);
                println!("   Effect: {:?}", effect);
                assert_eq!(*effect, Effect::Deterministic);
            }
            _ => panic!("Expected ToolResult"),
        }

        println!("\n✅ ZAP Tool Invocation test passed!\n");
    }

    /// Test: Multi-agent mesh with parallel communication
    #[tokio::test]
    async fn test_zap_multi_agent_mesh() {
        println!("\n🚀 ZAP E2E Test: Multi-Agent Mesh\n");

        let coordinator = Arc::new(MeshCoordinator::new());

        // Create team of agents
        let roles = [
            ("CTO", AgentRole::CTO),
            ("Frontend-Dev", AgentRole::Developer),
            ("Backend-Dev", AgentRole::Developer),
            ("Reviewer", AgentRole::Reviewer),
        ];

        let mut agents = Vec::new();
        for (name, role) in roles {
            let agent = ZapAgent::connect(
                coordinator.clone(),
                name,
                role,
                AgentConfig::default(),
            ).await;
            agents.push(agent);
        }

        // Get topology
        let topology = coordinator.topology().await;
        println!("📡 Mesh topology: {} peers", topology.peers.len());
        for peer in &topology.peers {
            println!("   - {} (endpoint: {})", peer.id, peer.endpoint);
        }

        // Collect agent IDs
        let ids: Vec<String> = agents.iter().map(|a| a.id.clone()).collect();

        // CTO broadcasts task to all developers via ZAP
        println!("\n📢 CTO broadcasting task to team via ZAP");
        for i in 1..3 { // Frontend-Dev and Backend-Dev
            agents[0].send(&ids[i], ZapContent::TaskRequest {
                description: format!("Implement {} component", if i == 1 { "UI" } else { "API" }),
                priority: 1,
                scope: Scope::Repo,
                effect: Effect::Nondeterministic,
                witness: WitnessLevel::Minimal,
            }).await.unwrap();
        }

        tokio::time::sleep(Duration::from_millis(100)).await;

        // Both developers receive tasks
        for i in 1..3 {
            let msg = agents[i].recv(1000).await;
            assert!(msg.is_some(), "Agent {} should receive task", i);
            println!("📥 {} received task", agents[i].name);
        }

        // Both respond to CTO
        for i in 1..3 {
            agents[i].send(&ids[0], ZapContent::TaskResponse {
                task_id: uuid::Uuid::new_v4().to_string(),
                result: format!("{} component implemented", if i == 1 { "UI" } else { "API" }),
                status: TaskState::Completed,
            }).await.unwrap();
        }

        tokio::time::sleep(Duration::from_millis(100)).await;

        // CTO receives both responses
        for _ in 0..2 {
            let msg = agents[0].recv(1000).await;
            assert!(msg.is_some(), "CTO should receive response");
            println!("📥 CTO received response");
        }

        // Verify message log
        let messages = coordinator.get_messages().await;
        assert_eq!(messages.len(), 4, "Should have 4 messages (2 tasks + 2 responses)");

        println!("\n✅ ZAP Multi-Agent Mesh test passed!\n");
    }

    /// Test: Mesh peer ping
    #[tokio::test]
    async fn test_zap_mesh_ping() {
        println!("\n🚀 ZAP E2E Test: Mesh Peer Ping\n");

        let coordinator = Arc::new(MeshCoordinator::new());

        let agent = ZapAgent::connect(
            coordinator.clone(),
            "TestAgent",
            AgentRole::Developer,
            AgentConfig::default(),
        ).await;

        // Ping the agent
        let latency = coordinator.ping(&agent.id).await;
        assert!(latency.is_ok(), "Ping should succeed");
        println!("📡 Ping latency: {}ns", latency.unwrap());

        // Try to ping non-existent agent
        let result = coordinator.ping("non-existent").await;
        assert!(result.is_err(), "Ping to non-existent agent should fail");

        println!("\n✅ ZAP Mesh Ping test passed!\n");
    }
}
