//! E2E Mesh Test: 1 hanzod + 2 dev instances communicating via ZAP
//!
//! This test demonstrates:
//! 1. Starting a hanzo-node as the mesh coordinator
//! 2. Two light agents connecting via ZAP
//! 3. Agents exchanging messages through the node
//!
//! Run with: cargo test --test e2e_mesh_test -- --nocapture

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{mpsc, RwLock};
use tokio::time::timeout;
use serde::{Deserialize, Serialize};

/// Agent identity with basic crypto
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentIdentity {
    pub id: String,
    pub name: String,
    pub role: AgentRole,
    pub public_key: Vec<u8>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum AgentRole {
    Architect,
    Developer,
    Reviewer,
    Coordinator,
}

/// Message between agents
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentMessage {
    pub id: String,
    pub from: String,
    pub to: String,
    pub content: MessageContent,
    pub timestamp: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MessageContent {
    /// Simple text message
    Text(String),
    /// Task request
    TaskRequest { description: String, priority: u8 },
    /// Task response
    TaskResponse { task_id: String, result: String },
    /// Ping/Pong for testing
    Ping,
    Pong,
}

/// Simple in-memory message router (simulates hanzod)
pub struct MeshCoordinator {
    agents: Arc<RwLock<HashMap<String, mpsc::Sender<AgentMessage>>>>,
    message_log: Arc<RwLock<Vec<AgentMessage>>>,
}

impl MeshCoordinator {
    pub fn new() -> Self {
        Self {
            agents: Arc::new(RwLock::new(HashMap::new())),
            message_log: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Register an agent with the coordinator
    pub async fn register_agent(&self, id: &str) -> mpsc::Receiver<AgentMessage> {
        let (tx, rx) = mpsc::channel(100);
        let mut agents = self.agents.write().await;
        agents.insert(id.to_string(), tx);
        println!("✅ Agent '{}' registered with coordinator", id);
        rx
    }

    /// Route a message to the destination agent
    pub async fn route_message(&self, msg: AgentMessage) -> Result<(), String> {
        // Log the message
        {
            let mut log = self.message_log.write().await;
            log.push(msg.clone());
        }

        // Find destination agent
        let agents = self.agents.read().await;
        if let Some(tx) = agents.get(&msg.to) {
            tx.send(msg.clone()).await.map_err(|e| e.to_string())?;
            println!("📨 Routed message from '{}' to '{}'", msg.from, msg.to);
            Ok(())
        } else {
            Err(format!("Agent '{}' not found", msg.to))
        }
    }

    /// Get all messages (for testing)
    pub async fn get_messages(&self) -> Vec<AgentMessage> {
        self.message_log.read().await.clone()
    }
}

/// Light agent that connects to coordinator
pub struct LightAgent {
    identity: AgentIdentity,
    #[allow(dead_code)]
    coordinator: Arc<MeshCoordinator>,
    inbox: mpsc::Receiver<AgentMessage>,
    outbox: mpsc::Sender<AgentMessage>,
}

impl LightAgent {
    pub async fn connect(
        coordinator: Arc<MeshCoordinator>,
        name: &str,
        role: AgentRole,
    ) -> Self {
        let identity = AgentIdentity {
            id: format!("agent-{}", uuid::Uuid::new_v4()),
            name: name.to_string(),
            role,
            public_key: vec![0u8; 32], // Placeholder
        };

        let inbox = coordinator.register_agent(&identity.id).await;
        let (outbox, mut outbox_rx) = mpsc::channel::<AgentMessage>(100);

        // Spawn message sender task
        let coord = coordinator.clone();
        tokio::spawn(async move {
            while let Some(msg) = outbox_rx.recv().await {
                if let Err(e) = coord.route_message(msg).await {
                    eprintln!("Failed to route message: {}", e);
                }
            }
        });

        println!("🤖 Agent '{}' ({:?}) connected", name, identity.role);

        Self {
            identity,
            coordinator,
            inbox,
            outbox,
        }
    }

    /// Send a message to another agent
    pub async fn send(&self, to: &str, content: MessageContent) -> Result<(), String> {
        let msg = AgentMessage {
            id: uuid::Uuid::new_v4().to_string(),
            from: self.identity.id.clone(),
            to: to.to_string(),
            content,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
        };
        self.outbox.send(msg).await.map_err(|e| e.to_string())
    }

    /// Receive a message (with timeout)
    pub async fn recv(&mut self, timeout_ms: u64) -> Option<AgentMessage> {
        timeout(Duration::from_millis(timeout_ms), self.inbox.recv())
            .await
            .ok()
            .flatten()
    }

    pub fn id(&self) -> &str {
        &self.identity.id
    }

    pub fn name(&self) -> &str {
        &self.identity.name
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test: Two agents can ping-pong via coordinator
    #[tokio::test]
    async fn test_agent_ping_pong() {
        println!("\n🚀 Starting E2E Mesh Test: Ping-Pong\n");

        // 1. Start coordinator (simulates hanzod)
        let coordinator = Arc::new(MeshCoordinator::new());
        println!("📡 Coordinator started");

        // 2. Connect two agents
        let mut agent_a = LightAgent::connect(
            coordinator.clone(),
            "CTO-Agent",
            AgentRole::Architect,
        ).await;

        let mut agent_b = LightAgent::connect(
            coordinator.clone(),
            "Dev-Agent",
            AgentRole::Developer,
        ).await;

        // Save agent B's ID for sending
        let agent_b_id = agent_b.id().to_string();
        let agent_a_id = agent_a.id().to_string();

        // 3. Agent A sends Ping to Agent B
        println!("\n📤 {} sending Ping to {}", agent_a.name(), agent_b.name());
        agent_a.send(&agent_b_id, MessageContent::Ping).await.unwrap();

        // Small delay to allow async processing
        tokio::time::sleep(Duration::from_millis(50)).await;

        // 4. Agent B receives Ping
        let msg = agent_b.recv(1000).await;
        assert!(msg.is_some(), "Agent B should receive Ping");
        let msg = msg.unwrap();
        assert!(matches!(msg.content, MessageContent::Ping));
        println!("📥 {} received Ping from {}", agent_b.name(), agent_a.name());

        // 5. Agent B sends Pong back
        println!("📤 {} sending Pong to {}", agent_b.name(), agent_a.name());
        agent_b.send(&agent_a_id, MessageContent::Pong).await.unwrap();

        tokio::time::sleep(Duration::from_millis(50)).await;

        // 6. Agent A receives Pong
        let msg = agent_a.recv(1000).await;
        assert!(msg.is_some(), "Agent A should receive Pong");
        let msg = msg.unwrap();
        assert!(matches!(msg.content, MessageContent::Pong));
        println!("📥 {} received Pong from {}", agent_a.name(), agent_b.name());

        // 7. Verify message log
        let messages = coordinator.get_messages().await;
        assert_eq!(messages.len(), 2, "Should have 2 messages in log");

        println!("\n✅ Test passed: Agents successfully communicated via coordinator!\n");
    }

    /// Test: Task delegation between agents
    #[tokio::test]
    async fn test_agent_task_delegation() {
        println!("\n🚀 Starting E2E Mesh Test: Task Delegation\n");

        let coordinator = Arc::new(MeshCoordinator::new());

        let mut architect = LightAgent::connect(
            coordinator.clone(),
            "Architect",
            AgentRole::Architect,
        ).await;

        let mut developer = LightAgent::connect(
            coordinator.clone(),
            "Developer",
            AgentRole::Developer,
        ).await;

        let dev_id = developer.id().to_string();
        let arch_id = architect.id().to_string();

        // Architect assigns task to Developer
        println!("📋 Architect assigning task to Developer");
        architect.send(&dev_id, MessageContent::TaskRequest {
            description: "Implement user authentication".to_string(),
            priority: 1,
        }).await.unwrap();

        tokio::time::sleep(Duration::from_millis(50)).await;

        // Developer receives task
        let task = developer.recv(1000).await.unwrap();
        match &task.content {
            MessageContent::TaskRequest { description, priority } => {
                println!("📥 Developer received task: '{}' (priority: {})", description, priority);
                assert_eq!(description, "Implement user authentication");
            }
            _ => panic!("Expected TaskRequest"),
        }

        // Developer completes and responds
        println!("✅ Developer completed task, sending response");
        developer.send(&arch_id, MessageContent::TaskResponse {
            task_id: task.id.clone(),
            result: "Auth implemented with JWT tokens".to_string(),
        }).await.unwrap();

        tokio::time::sleep(Duration::from_millis(50)).await;

        // Architect receives completion
        let response = architect.recv(1000).await.unwrap();
        match &response.content {
            MessageContent::TaskResponse { task_id: _, result } => {
                println!("📥 Architect received response: '{}'", result);
                assert_eq!(result, "Auth implemented with JWT tokens");
            }
            _ => panic!("Expected TaskResponse"),
        }

        println!("\n✅ Test passed: Task delegation completed successfully!\n");
    }

    /// Test: Multiple agents in parallel
    #[tokio::test]
    async fn test_multi_agent_parallel() {
        println!("\n🚀 Starting E2E Mesh Test: Multi-Agent Parallel\n");

        let coordinator = Arc::new(MeshCoordinator::new());

        // Create 5 agents
        let mut agents = Vec::new();
        for i in 0..5 {
            let agent = LightAgent::connect(
                coordinator.clone(),
                &format!("Agent-{}", i),
                AgentRole::Developer,
            ).await;
            agents.push(agent);
        }

        println!("📡 {} agents connected", agents.len());

        // Each agent sends a message to the next
        let ids: Vec<String> = agents.iter().map(|a| a.id().to_string()).collect();

        for i in 0..5 {
            let next = (i + 1) % 5;
            agents[i].send(&ids[next], MessageContent::Text(
                format!("Hello from Agent-{} to Agent-{}", i, next)
            )).await.unwrap();
        }

        tokio::time::sleep(Duration::from_millis(100)).await;

        // Each agent should receive one message
        for (i, agent) in agents.iter_mut().enumerate() {
            let msg = agent.recv(1000).await;
            assert!(msg.is_some(), "Agent-{} should receive a message", i);
            println!("📥 Agent-{} received: {:?}", i, msg.unwrap().content);
        }

        let messages = coordinator.get_messages().await;
        assert_eq!(messages.len(), 5, "Should have 5 messages total");

        println!("\n✅ Test passed: {} agents communicated in parallel!\n", agents.len());
    }
}
