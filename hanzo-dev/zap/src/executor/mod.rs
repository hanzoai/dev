//! Tool executor trait and dispatch logic.
//!
//! This module provides the infrastructure to execute tools with typed interfaces.
//! Each tool category has an executor that implements the actual functionality.
//!
//! ## Cross-Language Support
//!
//! ZAP provides a language-agnostic protocol for tool execution. Agents can be
//! implemented in any supported language while tools maintain consistent behavior:
//!
//! - **Rust**: Native executors in this module
//! - **Python**: hanzo-tools-* packages via MCP bridge
//! - **Node.js**: @hanzo/tools via MCP bridge
//! - **Go**: hanzo-go-tools via MCP bridge
//! - **C/C++**: libzap FFI bindings
//! - **Ruby**: hanzo-ruby-tools gem
//! - **Elixir**: hanzo_tools hex package
//!
//! All implementations share the same tool schemas defined in `crate::tools`.

use crate::error::{Error, Result};
use crate::message::ToolResult;
use crate::tools::ToolCategory;
use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

mod build;
mod computer;
mod filesystem;
mod network;
pub mod permissions;
mod plan;
mod vcs;

pub use build::BuildExecutor;
pub use computer::ComputerExecutor;
pub use filesystem::FilesystemExecutor;
pub use network::NetworkExecutor;
pub use plan::PlanExecutor;
pub use vcs::VcsExecutor;

// Re-export permission types from hanzo-protocol
pub use permissions::{AskForApproval, PermissionLevel, PermissionResult, SandboxPolicy};
pub use permissions::{check_approval, is_path_writable, operation_level};

/// Context passed to tool executors.
///
/// Contains all state needed for permission checks and execution.
/// Bridges with hanzo-protocol's `AskForApproval` and `SandboxPolicy`.
#[derive(Debug, Clone)]
pub struct ExecutorContext {
    /// Working directory for filesystem operations
    pub cwd: Option<String>,
    /// Environment variables
    pub env: HashMap<String, String>,
    /// User/session ID for audit
    pub session_id: Option<String>,
    /// When to ask for approval (from hanzo-protocol)
    pub approval_policy: AskForApproval,
    /// How to sandbox execution (from hanzo-protocol)
    pub sandbox_policy: SandboxPolicy,
    /// Timeout in milliseconds
    pub timeout_ms: Option<u64>,
}

impl Default for ExecutorContext {
    fn default() -> Self {
        Self {
            cwd: None,
            env: HashMap::new(),
            session_id: None,
            approval_policy: AskForApproval::OnRequest,
            sandbox_policy: SandboxPolicy::WorkspaceWrite {
                writable_roots: vec![],
                network_access: false,
                exclude_tmpdir_env_var: false,
                exclude_slash_tmp: false,
                allow_git_writes: true,
            },
            timeout_ms: None,
        }
    }
}

impl ExecutorContext {
    /// Create a new context with specified working directory
    pub fn with_cwd(cwd: impl Into<String>) -> Self {
        Self {
            cwd: Some(cwd.into()),
            ..Default::default()
        }
    }

    /// Check if an operation should be approved
    pub fn check_approval(&self, operation: &str) -> PermissionResult {
        let level = operation_level(operation);
        check_approval(
            operation,
            level,
            &self.approval_policy,
            &self.sandbox_policy,
        )
    }

    /// Check if a path is writable under current sandbox policy
    pub fn is_path_writable(&self, path: &std::path::Path) -> bool {
        let cwd = self
            .cwd
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| PathBuf::from("."));
        is_path_writable(path, &self.sandbox_policy, &cwd)
    }

    /// Create a permissive context (for trusted environments)
    pub fn permissive() -> Self {
        Self {
            cwd: None,
            env: HashMap::new(),
            session_id: None,
            approval_policy: AskForApproval::Never,
            sandbox_policy: SandboxPolicy::DangerFullAccess,
            timeout_ms: None,
        }
    }

    /// Create a read-only context (for sandboxed agents)
    pub fn read_only() -> Self {
        Self {
            cwd: None,
            env: HashMap::new(),
            session_id: None,
            approval_policy: AskForApproval::UnlessTrusted,
            sandbox_policy: SandboxPolicy::ReadOnly,
            timeout_ms: None,
        }
    }
}

/// Trait for tool executors
#[async_trait]
pub trait ToolExecutor: Send + Sync {
    /// Execute a tool by name with JSON arguments
    async fn execute(&self, name: &str, args: Value, ctx: &ExecutorContext) -> Result<ToolResult>;

    /// List tools provided by this executor
    fn tools(&self) -> Vec<&'static str>;

    /// Get the category this executor handles
    fn category(&self) -> ToolCategory;
}

/// Tool dispatcher that routes calls to appropriate executors
pub struct ToolDispatcher {
    executors: HashMap<String, Arc<dyn ToolExecutor>>,
    category_map: HashMap<ToolCategory, Vec<String>>,
}

impl ToolDispatcher {
    /// Create a new dispatcher
    pub fn new() -> Self {
        Self {
            executors: HashMap::new(),
            category_map: HashMap::new(),
        }
    }

    /// Register an executor
    pub fn register(&mut self, executor: Arc<dyn ToolExecutor>) {
        let category = executor.category();
        for tool in executor.tools() {
            self.executors.insert(tool.to_string(), executor.clone());
            self.category_map
                .entry(category.clone())
                .or_default()
                .push(tool.to_string());
        }
    }

    /// Execute a tool by name
    pub async fn execute(
        &self,
        name: &str,
        args: Value,
        ctx: &ExecutorContext,
    ) -> Result<ToolResult> {
        let executor = self
            .executors
            .get(name)
            .ok_or_else(|| Error::ToolNotFound(name.to_string()))?;

        executor.execute(name, args, ctx).await
    }

    /// List all available tools
    pub fn list_tools(&self) -> Vec<String> {
        self.executors.keys().cloned().collect()
    }

    /// List tools by category
    pub fn tools_by_category(&self, category: &ToolCategory) -> Vec<String> {
        self.category_map.get(category).cloned().unwrap_or_default()
    }
}

impl Default for ToolDispatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Create a dispatcher with all default executors registered
pub fn default_dispatcher() -> ToolDispatcher {
    let mut dispatcher = ToolDispatcher::new();

    // Register all executors
    dispatcher.register(Arc::new(FilesystemExecutor::new()));
    dispatcher.register(Arc::new(ComputerExecutor::new()));
    dispatcher.register(Arc::new(VcsExecutor::new()));
    dispatcher.register(Arc::new(BuildExecutor::new()));
    dispatcher.register(Arc::new(NetworkExecutor::new()));
    dispatcher.register(Arc::new(PlanExecutor::new()));

    dispatcher
}
