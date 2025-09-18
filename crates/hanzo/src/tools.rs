//! Tool implementations for common operations

use async_trait::async_trait;
use hanzoai::mcp::{Tool, ToolMetadata};
use hanzoai::Result;
use serde_json::{json, Value};
use std::process::Command;

/// Calculator tool
pub struct Calculator;

#[async_trait]
impl Tool for Calculator {
    async fn execute(&self, params: Value) -> Result<Value> {
        let expression = params["expression"]
            .as_str()
            .ok_or_else(|| hanzoai::HanzoError::InvalidRequest("Missing expression".into()))?;

        // Simple eval using bc command (safe for basic math)
        let output = Command::new("echo")
            .arg(expression)
            .pipe(Command::new("bc").arg("-l"))
            .output()
            .map_err(|e| hanzoai::HanzoError::Unknown(e.to_string()))?;

        let result = String::from_utf8_lossy(&output.stdout).trim().to_string();

        Ok(json!({
            "result": result
        }))
    }

    fn metadata(&self) -> ToolMetadata {
        ToolMetadata {
            name: "calculator".to_string(),
            description: "Perform mathematical calculations".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "expression": {
                        "type": "string",
                        "description": "Mathematical expression to evaluate"
                    }
                },
                "required": ["expression"]
            }),
        }
    }
}

/// Web search tool
pub struct WebSearch {
    api_key: Option<String>,
}

impl WebSearch {
    pub fn new() -> Self {
        Self {
            api_key: std::env::var("SEARCH_API_KEY").ok(),
        }
    }
}

#[async_trait]
impl Tool for WebSearch {
    async fn execute(&self, params: Value) -> Result<Value> {
        let query = params["query"]
            .as_str()
            .ok_or_else(|| hanzoai::HanzoError::InvalidRequest("Missing query".into()))?;

        // In real impl, would call search API
        Ok(json!({
            "results": [
                {
                    "title": format!("Search result for: {}", query),
                    "url": "https://example.com",
                    "snippet": "This is a mock search result"
                }
            ]
        }))
    }

    fn metadata(&self) -> ToolMetadata {
        ToolMetadata {
            name: "web_search".to_string(),
            description: "Search the web for information".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "query": {
                        "type": "string",
                        "description": "Search query"
                    }
                },
                "required": ["query"]
            }),
        }
    }
}

/// File reader tool
pub struct FileReader;

#[async_trait]
impl Tool for FileReader {
    async fn execute(&self, params: Value) -> Result<Value> {
        let path = params["path"]
            .as_str()
            .ok_or_else(|| hanzoai::HanzoError::InvalidRequest("Missing path".into()))?;

        let content = tokio::fs::read_to_string(path)
            .await
            .map_err(|e| hanzoai::HanzoError::Unknown(e.to_string()))?;

        Ok(json!({
            "content": content,
            "path": path,
            "size": content.len()
        }))
    }

    fn metadata(&self) -> ToolMetadata {
        ToolMetadata {
            name: "file_reader".to_string(),
            description: "Read contents of a file".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "path": {
                        "type": "string",
                        "description": "Path to the file"
                    }
                },
                "required": ["path"]
            }),
        }
    }
}

/// Shell command executor
pub struct ShellExecutor {
    allowed_commands: Vec<String>,
}

impl ShellExecutor {
    pub fn new() -> Self {
        Self {
            allowed_commands: vec![
                "ls".to_string(),
                "echo".to_string(),
                "date".to_string(),
                "pwd".to_string(),
            ],
        }
    }

    pub fn with_allowed_commands(mut self, commands: Vec<String>) -> Self {
        self.allowed_commands = commands;
        self
    }
}

#[async_trait]
impl Tool for ShellExecutor {
    async fn execute(&self, params: Value) -> Result<Value> {
        let command = params["command"]
            .as_str()
            .ok_or_else(|| hanzoai::HanzoError::InvalidRequest("Missing command".into()))?;

        // Check if command is allowed
        let cmd_name = command.split_whitespace().next().unwrap_or("");
        if !self.allowed_commands.contains(&cmd_name.to_string()) {
            return Err(hanzoai::HanzoError::InvalidRequest(
                format!("Command '{}' not allowed", cmd_name),
            ));
        }

        let output = Command::new("sh")
            .arg("-c")
            .arg(command)
            .output()
            .map_err(|e| hanzoai::HanzoError::Unknown(e.to_string()))?;

        Ok(json!({
            "stdout": String::from_utf8_lossy(&output.stdout).to_string(),
            "stderr": String::from_utf8_lossy(&output.stderr).to_string(),
            "status": output.status.code()
        }))
    }

    fn metadata(&self) -> ToolMetadata {
        ToolMetadata {
            name: "shell".to_string(),
            description: "Execute shell commands".to_string(),
            parameters: json!({
                "type": "object",
                "properties": {
                    "command": {
                        "type": "string",
                        "description": "Shell command to execute"
                    }
                },
                "required": ["command"]
            }),
        }
    }
}

/// Tool registry with all built-in tools
pub fn create_tool_registry() -> hanzoai::mcp::Bridge {
    let mut bridge = hanzoai::mcp::Bridge::new();

    bridge.register(Box::new(Calculator));
    bridge.register(Box::new(WebSearch::new()));
    bridge.register(Box::new(FileReader));
    bridge.register(Box::new(ShellExecutor::new()));

    bridge
}

// Helper trait for command piping
trait CommandPipe {
    fn pipe(self, next: Command) -> Command;
}

impl CommandPipe for Command {
    fn pipe(mut self, mut next: Command) -> Command {
        let output = self.output().expect("Failed to execute command");
        next.stdin(std::process::Stdio::piped());
        if let Some(mut stdin) = next.stdin.take() {
            use std::io::Write;
            stdin.write_all(&output.stdout).expect("Failed to pipe");
        }
        next
    }
}