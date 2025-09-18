//! Agent orchestration - matches Python's agents module

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use crate::Result;

/// Agent context for decision making
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Context {
    pub messages: Vec<crate::Message>,
    pub state: serde_json::Value,
    pub tools_available: Vec<String>,
}

/// Thought produced by agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Thought {
    pub reasoning: String,
    pub next_action: String,
    pub confidence: f32,
}

/// Action to be executed
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Action {
    pub action_type: String,
    pub parameters: serde_json::Value,
    pub expected_outcome: String,
}

/// Agent trait - matches Python's Agent class
#[async_trait]
pub trait Agent: Send + Sync {
    /// Think about the context and plan next steps
    async fn think(&self, context: Context) -> Result<Thought>;

    /// Execute an action based on thought
    async fn act(&self, thought: Thought) -> Result<Action>;

    /// Get agent metadata
    fn metadata(&self) -> AgentMetadata;
}

/// Agent metadata
#[derive(Debug, Clone)]
pub struct AgentMetadata {
    pub name: String,
    pub description: String,
    pub capabilities: Vec<String>,
}

/// Agent orchestrator for multi-agent systems
pub struct Orchestrator {
    agents: Vec<Box<dyn Agent>>,
}

impl Orchestrator {
    pub fn new() -> Self {
        Self {
            agents: Vec::new(),
        }
    }

    /// Register an agent
    pub fn register(&mut self, agent: Box<dyn Agent>) {
        self.agents.push(agent);
    }

    /// Select best agent for a context
    pub async fn select_agent(&self, context: &Context) -> Option<&Box<dyn Agent>> {
        // Simple selection - in real impl would use more sophisticated logic
        self.agents.first()
    }

    /// Run agent loop
    pub async fn run(&self, initial_context: Context) -> Result<Vec<Action>> {
        let mut actions = Vec::new();
        let mut context = initial_context;

        // Run up to 10 iterations
        for _ in 0..10 {
            if let Some(agent) = self.select_agent(&context).await {
                let thought = agent.think(context.clone()).await?;
                let action = agent.act(thought).await?;

                actions.push(action.clone());

                // Update context based on action
                // In real impl, would execute action and update context
                if action.action_type == "complete" {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(actions)
    }
}

impl Default for Orchestrator {
    fn default() -> Self {
        Self::new()
    }
}