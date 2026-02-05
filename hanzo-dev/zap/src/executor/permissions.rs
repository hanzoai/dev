//! Permission management bridging ZAP with hanzo-protocol.
//!
//! Re-exports and adapts hanzo-protocol types for ZAP executor context.
//! This ensures a single source of truth for approval and sandbox policies.

pub use hanzo_protocol::protocol::{AskForApproval, SandboxPolicy};

use std::path::Path;

/// Permission level for tool operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PermissionLevel {
    /// Read-only access (file reads, status queries)
    Read,
    /// Write access (file writes, edits)
    Write,
    /// Execute access (run commands, processes)
    Execute,
    /// Admin access (system changes, destructive operations)
    Admin,
}

/// Result of a permission check
#[derive(Debug, Clone)]
pub enum PermissionResult {
    /// Operation is allowed
    Allowed,
    /// Operation requires human approval
    RequiresApproval(String),
    /// Operation is denied
    Denied(String),
}

/// Check if an operation should be auto-approved based on policy.
///
/// Bridges hanzo-protocol's approval policy with ZAP executor operations.
pub fn check_approval(
    operation: &str,
    level: PermissionLevel,
    policy: &AskForApproval,
    _sandbox: &SandboxPolicy,
) -> PermissionResult {
    match policy {
        AskForApproval::Never => {
            // Never ask - auto-approve everything
            PermissionResult::Allowed
        }
        AskForApproval::OnFailure => {
            // Auto-approve, sandbox will catch issues
            PermissionResult::Allowed
        }
        AskForApproval::OnRequest => {
            // Model decides - default to allowed for read, require approval for write/exec
            match level {
                PermissionLevel::Read => PermissionResult::Allowed,
                _ => PermissionResult::RequiresApproval(format!(
                    "Operation '{}' may require approval",
                    operation
                )),
            }
        }
        AskForApproval::UnlessTrusted => {
            // Only auto-approve known safe read operations
            match level {
                PermissionLevel::Read => PermissionResult::Allowed,
                _ => PermissionResult::RequiresApproval(format!(
                    "Operation '{}' requires approval (untrusted mode)",
                    operation
                )),
            }
        }
    }
}

/// Check if a path is writable under the given sandbox policy.
pub fn is_path_writable(path: &Path, sandbox: &SandboxPolicy, cwd: &Path) -> bool {
    match sandbox {
        SandboxPolicy::DangerFullAccess => true,
        SandboxPolicy::ReadOnly => false,
        SandboxPolicy::WorkspaceWrite { writable_roots, .. } => {
            // Check if under cwd
            if path.starts_with(cwd) {
                return true;
            }
            // Check additional writable roots
            writable_roots.iter().any(|root| path.starts_with(root))
        }
    }
}

/// Tool operation categories for permission mapping
pub fn operation_level(tool_name: &str) -> PermissionLevel {
    match tool_name {
        // Read operations
        "read_file" | "glob" | "grep" | "list_dir" | "git_status" | "git_diff" | "git_log"
        | "git_blame" | "list_processes" | "get_env" | "fetch_url" | "port_check"
        | "dns_lookup" | "cache_lookup" => PermissionLevel::Read,

        // Write operations
        "write_file" | "edit_file" | "git_commit" | "git_branch" => PermissionLevel::Write,

        // Execute operations
        "exec" | "build" | "test" | "lint" | "typecheck" | "http_request" => {
            PermissionLevel::Execute
        }

        // Admin operations
        "kill_process" => PermissionLevel::Admin,

        // Planning tools (generally safe)
        "plan_intent" | "plan_route" | "plan_compose" | "audit_log" => PermissionLevel::Read,

        // Default to execute (requires approval)
        _ => PermissionLevel::Execute,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_operation_levels() {
        assert_eq!(operation_level("read_file"), PermissionLevel::Read);
        assert_eq!(operation_level("write_file"), PermissionLevel::Write);
        assert_eq!(operation_level("exec"), PermissionLevel::Execute);
        assert_eq!(operation_level("kill_process"), PermissionLevel::Admin);
    }

    #[test]
    fn test_path_writable_workspace() {
        let cwd = PathBuf::from("/project");
        let sandbox = SandboxPolicy::WorkspaceWrite {
            writable_roots: vec![],
            network_access: false,
            exclude_tmpdir_env_var: false,
            exclude_slash_tmp: false,
            allow_git_writes: true,
        };

        assert!(is_path_writable(
            &PathBuf::from("/project/src/main.rs"),
            &sandbox,
            &cwd
        ));
        assert!(!is_path_writable(
            &PathBuf::from("/etc/passwd"),
            &sandbox,
            &cwd
        ));
    }
}
