/// Unified tool handler for all MCP tools
/// This consolidates all tool functionality in one place

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ToolError {
    #[error("Tool not found: {0}")]
    NotFound(String),
    
    #[error("Invalid parameters: {0}")]
    InvalidParams(String),
    
    #[error("Execution failed: {0}")]
    ExecutionFailed(String),
    
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type ToolResult = Result<Value, ToolError>;

/// Tool information for discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolInfo {
    pub name: String,
    pub description: String,
    pub category: String,
    pub schema: Value,
}

/// Registry of all available tools
pub struct ToolRegistry {
    pub tools: HashMap<String, ToolInfo>,
}

impl ToolRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            tools: HashMap::new(),
        };
        registry.register_all();
        registry
    }
    
    fn register_all(&mut self) {
        // Register file tools
        self.register_file_tools();
        self.register_search_tools();
        self.register_shell_tools();
        self.register_edit_tools();
        self.register_git_tools();
        self.register_ast_tools();
        self.register_browser_tools();
        self.register_ai_tools();
        self.register_project_tools();
    }
    
    fn register_file_tools(&mut self) {
        self.tools.insert("read_file".to_string(), ToolInfo {
            name: "read_file".to_string(),
            description: "Read contents of a file".to_string(),
            category: "file".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "encoding": { "type": "string", "default": "utf-8" }
                },
                "required": ["path"]
            }),
        });
        
        self.tools.insert("write_file".to_string(), ToolInfo {
            name: "write_file".to_string(),
            description: "Write content to a file".to_string(),
            category: "file".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "content": { "type": "string" },
                    "overwrite": { "type": "boolean", "default": false }
                },
                "required": ["path", "content"]
            }),
        });
        
        // TODO: Add remaining file tools
    }
    
    fn register_search_tools(&mut self) {
        self.tools.insert("grep".to_string(), ToolInfo {
            name: "grep".to_string(),
            description: "Search for patterns in files".to_string(),
            category: "search".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "pattern": { "type": "string" },
                    "path": { "type": "string" },
                    "recursive": { "type": "boolean", "default": false }
                },
                "required": ["pattern", "path"]
            }),
        });
        
        // TODO: Add remaining search tools
    }
    
    fn register_shell_tools(&mut self) {
        self.tools.insert("bash".to_string(), ToolInfo {
            name: "bash".to_string(),
            description: "Execute a bash command".to_string(),
            category: "shell".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "command": { "type": "string" },
                    "timeout": { "type": "number", "default": 120000 }
                },
                "required": ["command"]
            }),
        });
        
        // TODO: Add remaining shell tools
    }
    
    fn register_edit_tools(&mut self) {
        // TODO: Implement edit tools
    }
    
    fn register_git_tools(&mut self) {
        // TODO: Implement git tools
    }
    
    fn register_ast_tools(&mut self) {
        // TODO: Implement AST/code intelligence tools
    }
    
    fn register_browser_tools(&mut self) {
        // TODO: Implement browser tools
    }
    
    fn register_ai_tools(&mut self) {
        // TODO: Implement AI orchestration tools
    }
    
    fn register_project_tools(&mut self) {
        // TODO: Implement project intelligence tools
    }
    
    pub fn list_tools(&self) -> Vec<ToolInfo> {
        let mut tools: Vec<_> = self.tools.values().cloned().collect();
        tools.sort_by(|a, b| a.category.cmp(&b.category).then(a.name.cmp(&b.name)));
        tools
    }
    
    pub fn get_tool(&self, name: &str) -> Option<&ToolInfo> {
        self.tools.get(name)
    }
    
    pub async fn execute(&self, name: &str, params: Value) -> ToolResult {
        // Route to appropriate tool implementation
        match name {
            "read_file" => self.execute_read_file(params).await,
            "write_file" => self.execute_write_file(params).await,
            "grep" => self.execute_grep(params).await,
            "bash" => self.execute_bash(params).await,
            _ => Err(ToolError::NotFound(name.to_string())),
        }
    }
    
    async fn execute_read_file(&self, params: Value) -> ToolResult {
        let path = params["path"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("path is required".to_string()))?;
        
        let content = std::fs::read_to_string(path)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        Ok(json!({
            "content": content,
            "path": path
        }))
    }
    
    async fn execute_write_file(&self, params: Value) -> ToolResult {
        let path = params["path"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("path is required".to_string()))?;
        let content = params["content"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("content is required".to_string()))?;
        
        std::fs::write(path, content)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        Ok(json!({
            "success": true,
            "path": path,
            "bytes_written": content.len()
        }))
    }
    
    async fn execute_grep(&self, _params: Value) -> ToolResult {
        // TODO: Implement grep using existing file-search crate
        Ok(json!({ "message": "grep not yet implemented" }))
    }
    
    async fn execute_bash(&self, params: Value) -> ToolResult {
        // Use existing shell execution from core
        // TODO: Bridge to existing shell implementation
        let command = params["command"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("command is required".to_string()))?;
        
        Ok(json!({
            "message": format!("Would execute: {}", command)
        }))
    }
}