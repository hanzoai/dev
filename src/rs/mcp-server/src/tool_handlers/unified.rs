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
        
        self.tools.insert("list_files".to_string(), ToolInfo {
            name: "list_files".to_string(),
            description: "List files in a directory".to_string(),
            category: "file".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "recursive": { "type": "boolean", "default": false },
                    "pattern": { "type": "string" }
                },
                "required": ["path"]
            }),
        });
        
        self.tools.insert("delete_file".to_string(), ToolInfo {
            name: "delete_file".to_string(),
            description: "Delete a file or directory".to_string(),
            category: "file".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "recursive": { "type": "boolean", "default": false }
                },
                "required": ["path"]
            }),
        });
        
        self.tools.insert("move_file".to_string(), ToolInfo {
            name: "move_file".to_string(),
            description: "Move or rename a file".to_string(),
            category: "file".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "source": { "type": "string" },
                    "destination": { "type": "string" }
                },
                "required": ["source", "destination"]
            }),
        });
        
        self.tools.insert("copy_file".to_string(), ToolInfo {
            name: "copy_file".to_string(),
            description: "Copy a file or directory".to_string(),
            category: "file".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "source": { "type": "string" },
                    "destination": { "type": "string" },
                    "recursive": { "type": "boolean", "default": false }
                },
                "required": ["source", "destination"]
            }),
        });
        
        self.tools.insert("file_info".to_string(), ToolInfo {
            name: "file_info".to_string(),
            description: "Get metadata about a file".to_string(),
            category: "file".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" }
                },
                "required": ["path"]
            }),
        });
    }
    
    fn register_search_tools(&mut self) {
        self.tools.insert("grep".to_string(), ToolInfo {
            name: "grep".to_string(),
            description: "Search for patterns in files using ripgrep".to_string(),
            category: "search".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "pattern": { "type": "string" },
                    "path": { "type": "string" },
                    "recursive": { "type": "boolean", "default": true },
                    "case_sensitive": { "type": "boolean", "default": true },
                    "whole_word": { "type": "boolean", "default": false },
                    "file_type": { "type": "string" }
                },
                "required": ["pattern", "path"]
            }),
        });
        
        self.tools.insert("find_files".to_string(), ToolInfo {
            name: "find_files".to_string(),
            description: "Find files by name pattern".to_string(),
            category: "search".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "pattern": { "type": "string" },
                    "path": { "type": "string", "default": "." },
                    "type": { "type": "string", "enum": ["file", "directory", "any"] }
                },
                "required": ["pattern"]
            }),
        });
        
        self.tools.insert("search_ast".to_string(), ToolInfo {
            name: "search_ast".to_string(),
            description: "Search code using AST patterns".to_string(),
            category: "search".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "pattern": { "type": "string" },
                    "language": { "type": "string" },
                    "path": { "type": "string" }
                },
                "required": ["pattern", "language"]
            }),
        });
        
        self.tools.insert("search_symbols".to_string(), ToolInfo {
            name: "search_symbols".to_string(),
            description: "Search for code symbols (functions, classes, etc)".to_string(),
            category: "search".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "symbol": { "type": "string" },
                    "type": { "type": "string", "enum": ["function", "class", "variable", "any"] },
                    "path": { "type": "string" }
                },
                "required": ["symbol"]
            }),
        });
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
        self.tools.insert("edit_file".to_string(), ToolInfo {
            name: "edit_file".to_string(),
            description: "Edit a file with precise replacements".to_string(),
            category: "edit".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "old_text": { "type": "string" },
                    "new_text": { "type": "string" },
                    "occurrence": { "type": "integer", "default": 1 }
                },
                "required": ["path", "old_text", "new_text"]
            }),
        });
        
        self.tools.insert("multi_edit".to_string(), ToolInfo {
            name: "multi_edit".to_string(),
            description: "Make multiple edits to a file".to_string(),
            category: "edit".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "edits": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "old_text": { "type": "string" },
                                "new_text": { "type": "string" }
                            },
                            "required": ["old_text", "new_text"]
                        }
                    }
                },
                "required": ["path", "edits"]
            }),
        });
        
        self.tools.insert("apply_patch".to_string(), ToolInfo {
            name: "apply_patch".to_string(),
            description: "Apply a unified diff patch to files".to_string(),
            category: "edit".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "patch": { "type": "string" },
                    "base_path": { "type": "string", "default": "." }
                },
                "required": ["patch"]
            }),
        });
        
        self.tools.insert("refactor_code".to_string(), ToolInfo {
            name: "refactor_code".to_string(),
            description: "Refactor code using AST transformations".to_string(),
            category: "edit".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string" },
                    "refactor_type": { 
                        "type": "string", 
                        "enum": ["rename_symbol", "extract_function", "inline_variable"]
                    },
                    "params": { "type": "object" }
                },
                "required": ["path", "refactor_type", "params"]
            }),
        });
    }
    
    fn register_git_tools(&mut self) {
        self.tools.insert("git_status".to_string(), ToolInfo {
            name: "git_status".to_string(),
            description: "Get git repository status".to_string(),
            category: "git".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string", "default": "." }
                }
            }),
        });
        
        self.tools.insert("git_diff".to_string(), ToolInfo {
            name: "git_diff".to_string(),
            description: "Show git diff for files".to_string(),
            category: "git".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string", "default": "." },
                    "staged": { "type": "boolean", "default": false },
                    "commit": { "type": "string" }
                }
            }),
        });
        
        self.tools.insert("git_commit".to_string(), ToolInfo {
            name: "git_commit".to_string(),
            description: "Create a git commit".to_string(),
            category: "git".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "message": { "type": "string" },
                    "files": { 
                        "type": "array",
                        "items": { "type": "string" }
                    },
                    "all": { "type": "boolean", "default": false }
                },
                "required": ["message"]
            }),
        });
        
        self.tools.insert("git_log".to_string(), ToolInfo {
            name: "git_log".to_string(),
            description: "Show git commit history".to_string(),
            category: "git".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "limit": { "type": "integer", "default": 10 },
                    "format": { "type": "string", "default": "oneline" },
                    "path": { "type": "string" }
                }
            }),
        });
        
        self.tools.insert("git_branch".to_string(), ToolInfo {
            name: "git_branch".to_string(),
            description: "Manage git branches".to_string(),
            category: "git".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "action": { 
                        "type": "string",
                        "enum": ["list", "create", "checkout", "delete"]
                    },
                    "name": { "type": "string" }
                },
                "required": ["action"]
            }),
        });
        
        self.tools.insert("git_push".to_string(), ToolInfo {
            name: "git_push".to_string(),
            description: "Push commits to remote repository".to_string(),
            category: "git".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "remote": { "type": "string", "default": "origin" },
                    "branch": { "type": "string" },
                    "force": { "type": "boolean", "default": false }
                }
            }),
        });
        
        self.tools.insert("git_pull".to_string(), ToolInfo {
            name: "git_pull".to_string(),
            description: "Pull commits from remote repository".to_string(),
            category: "git".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "remote": { "type": "string", "default": "origin" },
                    "branch": { "type": "string" },
                    "rebase": { "type": "boolean", "default": false }
                }
            }),
        });
    }
    
    fn register_ast_tools(&mut self) {
        // TODO: Implement AST/code intelligence tools
    }
    
    fn register_browser_tools(&mut self) {
        // TODO: Implement browser tools
    }
    
    fn register_ai_tools(&mut self) {
        self.tools.insert("llm_complete".to_string(), ToolInfo {
            name: "llm_complete".to_string(),
            description: "Get LLM completion for a prompt".to_string(),
            category: "ai".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "prompt": { "type": "string" },
                    "model": { "type": "string", "default": "gpt-4" },
                    "temperature": { "type": "number", "default": 0.7 },
                    "max_tokens": { "type": "integer", "default": 1000 }
                },
                "required": ["prompt"]
            }),
        });
        
        self.tools.insert("agent_execute".to_string(), ToolInfo {
            name: "agent_execute".to_string(),
            description: "Execute an AI agent with a task".to_string(),
            category: "ai".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "agent": { "type": "string" },
                    "task": { "type": "string" },
                    "context": { "type": "object" }
                },
                "required": ["agent", "task"]
            }),
        });
        
        self.tools.insert("embeddings_create".to_string(), ToolInfo {
            name: "embeddings_create".to_string(),
            description: "Create embeddings for text".to_string(),
            category: "ai".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "text": { "type": "string" },
                    "model": { "type": "string", "default": "text-embedding-ada-002" }
                },
                "required": ["text"]
            }),
        });
    }
    
    fn register_project_tools(&mut self) {
        self.tools.insert("project_analyze".to_string(), ToolInfo {
            name: "project_analyze".to_string(),
            description: "Analyze project structure and dependencies".to_string(),
            category: "project".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string", "default": "." },
                    "depth": { "type": "integer", "default": 3 }
                }
            }),
        });
        
        self.tools.insert("dependency_tree".to_string(), ToolInfo {
            name: "dependency_tree".to_string(),
            description: "Generate dependency tree for project".to_string(),
            category: "project".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string", "default": "." },
                    "language": { "type": "string" }
                }
            }),
        });
        
        self.tools.insert("test_run".to_string(), ToolInfo {
            name: "test_run".to_string(),
            description: "Run project tests".to_string(),
            category: "project".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string", "default": "." },
                    "pattern": { "type": "string" },
                    "coverage": { "type": "boolean", "default": false }
                }
            }),
        });
        
        self.tools.insert("build_project".to_string(), ToolInfo {
            name: "build_project".to_string(),
            description: "Build the project".to_string(),
            category: "project".to_string(),
            schema: json!({
                "type": "object",
                "properties": {
                    "path": { "type": "string", "default": "." },
                    "target": { "type": "string", "default": "release" }
                }
            }),
        });
    }
    
    pub fn list_tools(&self) -> Vec<ToolInfo> {
        let mut tools: Vec<_> = self.tools.values().cloned().collect();
        tools.sort_by(|a, b| a.category.cmp(&b.category).then(a.name.cmp(&b.name)));
        tools
    }
    
    pub async fn execute(&self, name: &str, params: Value) -> ToolResult {
        // Route to appropriate tool implementation
        match name {
            // File tools
            "read_file" => self.execute_read_file(params).await,
            "write_file" => self.execute_write_file(params).await,
            "list_files" => self.execute_list_files(params).await,
            "delete_file" => self.execute_delete_file(params).await,
            "move_file" => self.execute_move_file(params).await,
            "copy_file" => self.execute_copy_file(params).await,
            "file_info" => self.execute_file_info(params).await,
            
            // Search tools
            "grep" => self.execute_grep(params).await,
            "find_files" => self.execute_find_files(params).await,
            
            // Shell tools
            "bash" => self.execute_bash(params).await,
            
            // Edit tools
            "edit_file" => self.execute_edit_file(params).await,
            
            // Git tools
            "git_status" => self.execute_git_status(params).await,
            "git_diff" => self.execute_git_diff(params).await,
            
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
        use std::process::Command;
        
        let command = params["command"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("command is required".to_string()))?;
        
        let output = Command::new("bash")
            .arg("-c")
            .arg(command)
            .output()
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        Ok(json!({
            "stdout": String::from_utf8_lossy(&output.stdout),
            "stderr": String::from_utf8_lossy(&output.stderr),
            "exit_code": output.status.code().unwrap_or(-1)
        }))
    }
    
    async fn execute_list_files(&self, params: Value) -> ToolResult {
        use std::fs;
        
        let path = params["path"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("path is required".to_string()))?;
        let recursive = params["recursive"].as_bool().unwrap_or(false);
        
        let mut files = Vec::new();
        
        if recursive {
            fn walk_dir(dir: &std::path::Path, files: &mut Vec<String>) -> std::io::Result<()> {
                for entry in fs::read_dir(dir)? {
                    let entry = entry?;
                    let path = entry.path();
                    files.push(path.display().to_string());
                    if path.is_dir() {
                        walk_dir(&path, files)?;
                    }
                }
                Ok(())
            }
            walk_dir(std::path::Path::new(path), &mut files)
                .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        } else {
            for entry in fs::read_dir(path)
                .map_err(|e| ToolError::ExecutionFailed(e.to_string()))? {
                let entry = entry.map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
                files.push(entry.path().display().to_string());
            }
        }
        
        Ok(json!({
            "files": files,
            "count": files.len()
        }))
    }
    
    async fn execute_delete_file(&self, params: Value) -> ToolResult {
        use std::fs;
        
        let path = params["path"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("path is required".to_string()))?;
        let recursive = params["recursive"].as_bool().unwrap_or(false);
        
        let metadata = fs::metadata(path)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        if metadata.is_dir() && recursive {
            fs::remove_dir_all(path)
                .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        } else if metadata.is_dir() {
            fs::remove_dir(path)
                .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        } else {
            fs::remove_file(path)
                .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        }
        
        Ok(json!({
            "success": true,
            "path": path
        }))
    }
    
    async fn execute_move_file(&self, params: Value) -> ToolResult {
        use std::fs;
        
        let source = params["source"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("source is required".to_string()))?;
        let destination = params["destination"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("destination is required".to_string()))?;
        
        fs::rename(source, destination)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        Ok(json!({
            "success": true,
            "source": source,
            "destination": destination
        }))
    }
    
    async fn execute_copy_file(&self, params: Value) -> ToolResult {
        use std::fs;
        
        let source = params["source"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("source is required".to_string()))?;
        let destination = params["destination"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("destination is required".to_string()))?;
        
        let metadata = fs::metadata(source)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        if metadata.is_file() {
            fs::copy(source, destination)
                .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        } else {
            return Err(ToolError::ExecutionFailed("Directory copying not yet implemented".to_string()));
        }
        
        Ok(json!({
            "success": true,
            "source": source,
            "destination": destination
        }))
    }
    
    async fn execute_file_info(&self, params: Value) -> ToolResult {
        use std::fs;
        
        let path = params["path"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("path is required".to_string()))?;
        
        let metadata = fs::metadata(path)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        Ok(json!({
            "path": path,
            "size": metadata.len(),
            "is_file": metadata.is_file(),
            "is_dir": metadata.is_dir(),
            "readonly": metadata.permissions().readonly()
        }))
    }
    
    async fn execute_find_files(&self, params: Value) -> ToolResult {
        use std::process::Command;
        
        let pattern = params["pattern"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("pattern is required".to_string()))?;
        let path = params["path"].as_str().unwrap_or(".");
        
        let output = Command::new("find")
            .arg(path)
            .arg("-name")
            .arg(pattern)
            .output()
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        let files: Vec<String> = String::from_utf8_lossy(&output.stdout)
            .lines()
            .map(|s| s.to_string())
            .collect();
        
        Ok(json!({
            "files": files,
            "count": files.len()
        }))
    }
    
    async fn execute_edit_file(&self, params: Value) -> ToolResult {
        use std::fs;
        
        let path = params["path"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("path is required".to_string()))?;
        let old_text = params["old_text"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("old_text is required".to_string()))?;
        let new_text = params["new_text"].as_str()
            .ok_or_else(|| ToolError::InvalidParams("new_text is required".to_string()))?;
        
        let content = fs::read_to_string(path)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        if !content.contains(old_text) {
            return Err(ToolError::ExecutionFailed("old_text not found in file".to_string()));
        }
        
        let new_content = content.replacen(old_text, new_text, 1);
        
        fs::write(path, new_content)
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        Ok(json!({
            "success": true,
            "path": path
        }))
    }
    
    async fn execute_git_status(&self, _params: Value) -> ToolResult {
        use std::process::Command;
        
        let output = Command::new("git")
            .arg("status")
            .arg("--porcelain")
            .output()
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        let status = String::from_utf8_lossy(&output.stdout);
        
        Ok(json!({
            "status": status,
            "clean": status.is_empty()
        }))
    }
    
    async fn execute_git_diff(&self, params: Value) -> ToolResult {
        use std::process::Command;
        
        let mut cmd = Command::new("git");
        cmd.arg("diff");
        
        if params["staged"].as_bool().unwrap_or(false) {
            cmd.arg("--staged");
        }
        
        if let Some(commit) = params["commit"].as_str() {
            cmd.arg(commit);
        }
        
        let output = cmd.output()
            .map_err(|e| ToolError::ExecutionFailed(e.to_string()))?;
        
        Ok(json!({
            "diff": String::from_utf8_lossy(&output.stdout)
        }))
    }
}
