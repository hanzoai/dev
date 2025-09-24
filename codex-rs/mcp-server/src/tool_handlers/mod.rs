// Simple tool types for codex-rs - independent of external hanzo-tools
use std::collections::HashMap;
use serde_json::Value;

pub type ToolResult = Result<Value, ToolError>;

#[derive(Debug, Clone)]
pub struct ToolInfo {
    pub name: String,
    pub description: String,
    pub parameters: Value,
}

#[derive(Debug, thiserror::Error)]
pub enum ToolError {
    #[error("Tool execution failed: {0}")]
    ExecutionError(String),
    #[error("Tool not found: {0}")]
    NotFound(String),
    #[error("Invalid parameters: {0}")]
    InvalidParameters(String),
}

pub struct ToolRegistry {
    tools: HashMap<String, ToolInfo>,
}

impl ToolRegistry {
    pub fn new() -> Self {
        Self {
            tools: HashMap::new(),
        }
    }

    pub fn register_tool(&mut self, info: ToolInfo) {
        self.tools.insert(info.name.clone(), info);
    }

    pub fn get_tool(&self, name: &str) -> Option<&ToolInfo> {
        self.tools.get(name)
    }

    pub fn list_tools(&self) -> Vec<&ToolInfo> {
        self.tools.values().collect()
    }
}