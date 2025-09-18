//! Model Context Protocol implementation - matches Python's mcp module

use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use crate::Result;

/// Tool trait - matches Python's Tool Protocol
#[async_trait]
pub trait Tool: Send + Sync {
    /// Execute the tool with parameters
    async fn execute(&self, params: Value) -> Result<Value>;

    /// Get tool metadata
    fn metadata(&self) -> ToolMetadata;
}

/// Tool metadata
#[derive(Debug, Clone)]
pub struct ToolMetadata {
    pub name: String,
    pub description: String,
    pub parameters: Value,
}

/// MCP Bridge - matches Python's Bridge pattern
pub struct Bridge {
    tools: HashMap<String, Box<dyn Tool>>,
}

impl Bridge {
    pub fn new() -> Self {
        Self {
            tools: HashMap::new(),
        }
    }

    /// Register a tool
    pub fn register(&mut self, tool: Box<dyn Tool>) {
        let metadata = tool.metadata();
        self.tools.insert(metadata.name.clone(), tool);
    }

    /// Execute a tool by name
    pub async fn execute(&self, name: &str, params: Value) -> Result<Value> {
        let tool = self.tools
            .get(name)
            .ok_or_else(|| crate::HanzoError::InvalidRequest(format!("Tool {} not found", name)))?;

        tool.execute(params).await
    }

    /// List all available tools
    pub fn list_tools(&self) -> Vec<ToolMetadata> {
        self.tools
            .values()
            .map(|t| t.metadata())
            .collect()
    }
}

impl Default for Bridge {
    fn default() -> Self {
        Self::new()
    }
}