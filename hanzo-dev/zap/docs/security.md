# Security Model

ZAP provides a comprehensive security model with human-in-the-loop approval policies and sandboxing. This ensures AI agents operate within defined boundaries while remaining productive.

## Overview

ZAP security has two orthogonal dimensions:

1. **AskForApproval**: When to pause and ask a human for permission
2. **SandboxPolicy**: What operations are physically allowed

```
                    AskForApproval
                    |
    Never -------- OnFailure -------- OnRequest -------- UnlessTrusted
                    |
                    |
    SandboxPolicy   |
    ----------------+---------------------------------->
    |               |
    DangerFull      WorkspaceWrite      ReadOnly
```

## AskForApproval Policies

The `AskForApproval` enum determines when the agent should pause and request human confirmation:

### Never

```rust
AskForApproval::Never
```

- **Behavior**: Never ask for approval
- **Use case**: Fully trusted automation, CI/CD pipelines
- **Risk**: High - agent has full autonomy

```rust
let ctx = ExecutorContext {
    approval_policy: AskForApproval::Never,
    ..Default::default()
};
```

### OnFailure

```rust
AskForApproval::OnFailure
```

- **Behavior**: Only ask when an operation fails
- **Use case**: Semi-autonomous operation with human recovery
- **Risk**: Medium - failures trigger review

### OnRequest

```rust
AskForApproval::OnRequest
```

- **Behavior**: Model decides when to ask based on operation risk
- **Use case**: Interactive development with AI assistance
- **Risk**: Low-medium - risky operations flagged

```rust
// Read operations auto-approved
ctx.check_approval("read_file")  // -> Allowed

// Write operations may require approval
ctx.check_approval("write_file") // -> RequiresApproval("Operation 'write_file' may require approval")
```

### UnlessTrusted (Default)

```rust
AskForApproval::UnlessTrusted
```

- **Behavior**: Ask for everything except explicitly trusted operations
- **Use case**: Maximum safety for untrusted code or sensitive environments
- **Risk**: Lowest - human approves most actions

## SandboxPolicy Modes

The `SandboxPolicy` enum determines what operations are physically permitted:

### DangerFullAccess

```rust
SandboxPolicy::DangerFullAccess
```

- **Filesystem**: Full read/write to entire system
- **Network**: Unrestricted
- **Processes**: Can spawn any process
- **Use case**: Only for fully trusted environments

**Warning**: This bypasses all filesystem restrictions.

### ReadOnly

```rust
SandboxPolicy::ReadOnly
```

- **Filesystem**: Read anywhere, write nowhere
- **Network**: Typically blocked
- **Processes**: Limited to query-only commands
- **Use case**: Safe code exploration, documentation

```rust
let ctx = ExecutorContext {
    sandbox_policy: SandboxPolicy::ReadOnly,
    ..Default::default()
};

// Allowed
ctx.is_path_writable(&Path::new("/etc/passwd")); // false
dispatcher.execute("read_file", json!({"path": "/etc/passwd"}), &ctx).await; // OK

// Blocked
dispatcher.execute("write_file", json!({"path": "/tmp/test.txt", "content": "x"}), &ctx).await;
// Error: Permission denied
```

### WorkspaceWrite (Default)

```rust
SandboxPolicy::WorkspaceWrite {
    writable_roots: vec![PathBuf::from("/home/user/project")],
    network_access: true,
    exclude_tmpdir_env_var: false,
    exclude_slash_tmp: false,
    allow_git_writes: true,
}
```

- **Filesystem**: Read anywhere, write only to specified directories
- **Network**: Configurable
- **Processes**: Allowed with restrictions
- **Use case**: Typical development workflow

```rust
let ctx = ExecutorContext {
    cwd: Some("/home/user/project".to_string()),
    sandbox_policy: SandboxPolicy::WorkspaceWrite {
        writable_roots: vec![],
        network_access: true,
        exclude_tmpdir_env_var: false,
        exclude_slash_tmp: false,
        allow_git_writes: true,
    },
    ..Default::default()
};

// Allowed - within workspace
ctx.is_path_writable(&Path::new("/home/user/project/src/main.rs")); // true

// Blocked - outside workspace
ctx.is_path_writable(&Path::new("/etc/passwd")); // false
```

## Permission Levels

ZAP classifies operations into permission levels:

| Level | Description | Examples |
|-------|-------------|----------|
| `Read` | Information retrieval | `read_file`, `git_status`, `glob` |
| `Write` | Data modification | `write_file`, `edit_file`, `git_commit` |
| `Execute` | Process execution | `exec`, `build`, `test` |
| `Admin` | System changes | `kill_process`, destructive operations |

### Automatic Classification

```rust
// From permissions.rs
pub fn operation_level(tool_name: &str) -> PermissionLevel {
    match tool_name {
        // Read operations
        "read_file" | "glob" | "grep" | "list_dir" | "git_status" | "git_diff"
        | "git_log" | "git_blame" | "list_processes" | "get_env" | "fetch_url"
        | "port_check" | "dns_lookup" | "cache_lookup" => PermissionLevel::Read,

        // Write operations
        "write_file" | "edit_file" | "git_commit" | "git_branch" => PermissionLevel::Write,

        // Execute operations
        "exec" | "build" | "test" | "lint" | "typecheck" | "http_request" => PermissionLevel::Execute,

        // Admin operations
        "kill_process" => PermissionLevel::Admin,

        // Planning tools (generally safe)
        "plan_intent" | "plan_route" | "plan_compose" | "audit_log" => PermissionLevel::Read,

        // Default to execute (requires approval)
        _ => PermissionLevel::Execute,
    }
}
```

## Permission Check Flow

```
                                +----------------+
                                |  Tool Call     |
                                +----------------+
                                        |
                                        v
                          +-------------------------+
                          |  Get Permission Level   |
                          +-------------------------+
                                        |
                                        v
                          +-------------------------+
                          |  Check AskForApproval   |
                          +-------------------------+
                                        |
              +-------------------------+-------------------------+
              |                         |                         |
              v                         v                         v
        +-----------+           +-----------+           +-----------+
        |  Never    |           | OnRequest |           |UnlessTrust|
        +-----------+           +-----------+           +-----------+
              |                         |                         |
              v                         v                         v
        +-----------+           +-----------+           +-----------+
        |  Allowed  |           |Read=Allow |           |Read=Allow |
        +-----------+           |Write=Ask  |           |Other=Ask  |
                                +-----------+           +-----------+
                                        |                         |
                                        v                         v
                          +-------------------------+
                          |  Check SandboxPolicy    |
                          +-------------------------+
                                        |
              +-------------------------+-------------------------+
              |                         |                         |
              v                         v                         v
        +-----------+           +-----------+           +-----------+
        | FullAccess|           |WorkspaceW |           | ReadOnly  |
        +-----------+           +-----------+           +-----------+
              |                         |                         |
              v                         v                         v
        +-----------+           +-----------+           +-----------+
        |  Execute  |           |Path check |           |Read only  |
        +-----------+           +-----------+           +-----------+
```

## Implementing Approval Requests

When an operation requires approval, the agent should pause and request human confirmation:

### Example: Human-in-the-Loop Approval

```rust
use hanzo_zap::{ExecutorContext, PermissionResult};

async fn execute_with_approval(
    dispatcher: &ToolDispatcher,
    tool: &str,
    args: Value,
    ctx: &ExecutorContext,
) -> Result<ToolResult> {
    // Check if approval is needed
    match ctx.check_approval(tool) {
        PermissionResult::Allowed => {
            // Execute directly
            dispatcher.execute(tool, args, ctx).await
        }
        PermissionResult::RequiresApproval(reason) => {
            // Show approval dialog
            if request_human_approval(tool, &args, &reason).await? {
                dispatcher.execute(tool, args, ctx).await
            } else {
                Err(Error::PermissionDenied(format!(
                    "User denied approval for {}", tool
                )))
            }
        }
        PermissionResult::Denied(reason) => {
            Err(Error::PermissionDenied(reason))
        }
    }
}

async fn request_human_approval(tool: &str, args: &Value, reason: &str) -> Result<bool> {
    println!("\n=== Approval Required ===");
    println!("Tool: {}", tool);
    println!("Arguments: {}", serde_json::to_string_pretty(args)?);
    println!("Reason: {}", reason);
    println!("\nApprove? [y/N]: ");

    let mut input = String::new();
    std::io::stdin().read_line(&mut input)?;

    Ok(input.trim().eq_ignore_ascii_case("y"))
}
```

### Example: GUI Approval Dialog

```python
import tkinter as tk
from tkinter import messagebox

def request_approval(tool: str, args: dict, reason: str) -> bool:
    """Show a GUI dialog for approval."""
    root = tk.Tk()
    root.withdraw()

    message = f"""
Tool: {tool}
Arguments: {json.dumps(args, indent=2)}
Reason: {reason}

Do you approve this operation?
"""

    result = messagebox.askyesno("Approval Required", message)
    root.destroy()
    return result
```

## Path Validation

ZAP validates paths against the sandbox policy:

```rust
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
```

### Path Traversal Protection

ZAP prevents path traversal attacks:

```rust
// These are blocked:
"/home/user/project/../../../etc/passwd"  // Canonicalizes to /etc/passwd
"../../../etc/passwd"                      // Outside workspace
"./../../etc/passwd"                       // Outside workspace

// Implementation
fn validate_path(path: &str, ctx: &ExecutorContext) -> Result<PathBuf> {
    let canonical = path.canonicalize()?;
    if !ctx.is_path_writable(&canonical) {
        return Err(Error::PermissionDenied(format!(
            "Path {} is outside writable roots", canonical.display()
        )));
    }
    Ok(canonical)
}
```

## Network Access Control

The `WorkspaceWrite` policy includes network access control:

```rust
SandboxPolicy::WorkspaceWrite {
    network_access: false,  // Block all network operations
    ..
}
```

When `network_access` is `false`:
- HTTP requests are blocked
- SSH connections are blocked
- Port scanning is blocked
- DNS lookups may be allowed (for error messages)

## Command Execution Security

Command execution (`exec` tool) has additional security layers:

### Allowed Commands (Read-only context)

```rust
const SAFE_READ_COMMANDS: &[&str] = &[
    "ls", "cat", "head", "tail", "wc",
    "git status", "git log", "git diff",
    "cargo check", "cargo clippy",
    "npm list", "pip list",
];
```

### Blocked Commands (Always)

```rust
const DANGEROUS_COMMANDS: &[&str] = &[
    "rm -rf /",
    "dd if=/dev/zero",
    ":(){ :|:& };:",  // Fork bomb
    "mkfs",
    "chmod -R 777 /",
];
```

### Command Validation

```rust
fn validate_command(command: &str, ctx: &ExecutorContext) -> Result<()> {
    // Check for shell injection
    if contains_shell_metacharacters(command) {
        return Err(Error::SecurityViolation("Shell metacharacters not allowed"));
    }

    // Check sandbox policy
    match &ctx.sandbox_policy {
        SandboxPolicy::ReadOnly => {
            if !is_read_only_command(command) {
                return Err(Error::PermissionDenied("Only read-only commands allowed"));
            }
        }
        SandboxPolicy::WorkspaceWrite { network_access, .. } => {
            if !network_access && is_network_command(command) {
                return Err(Error::PermissionDenied("Network access not allowed"));
            }
        }
        _ => {}
    }

    Ok(())
}
```

## Audit Logging

ZAP logs all security-relevant events:

```rust
#[derive(Debug, Serialize)]
pub struct AuditEvent {
    pub timestamp: DateTime<Utc>,
    pub session_id: String,
    pub operation: String,
    pub tool: String,
    pub args_hash: String,  // Hash of arguments (not full args for privacy)
    pub permission_level: PermissionLevel,
    pub result: AuditResult,
    pub approval: Option<ApprovalInfo>,
}

#[derive(Debug, Serialize)]
pub enum AuditResult {
    Allowed,
    Denied { reason: String },
    Approved { by: String },
    Failed { error: String },
}
```

### Audit Log Example

```json
[
  {
    "timestamp": "2025-01-15T10:30:00Z",
    "session_id": "agent-001",
    "operation": "call_tool",
    "tool": "read_file",
    "args_hash": "sha256:abc123...",
    "permission_level": "read",
    "result": "allowed"
  },
  {
    "timestamp": "2025-01-15T10:30:01Z",
    "session_id": "agent-001",
    "operation": "call_tool",
    "tool": "write_file",
    "args_hash": "sha256:def456...",
    "permission_level": "write",
    "result": {
      "approved": {
        "by": "user@example.com"
      }
    }
  }
]
```

## Best Practices

### 1. Use the Least Permissive Policy

```rust
// Good - minimal permissions
let ctx = ExecutorContext {
    approval_policy: AskForApproval::UnlessTrusted,
    sandbox_policy: SandboxPolicy::WorkspaceWrite {
        writable_roots: vec![],
        network_access: false,
        ..Default::default()
    },
    ..Default::default()
};

// Bad - too permissive
let ctx = ExecutorContext {
    approval_policy: AskForApproval::Never,
    sandbox_policy: SandboxPolicy::DangerFullAccess,
    ..Default::default()
};
```

### 2. Validate All Paths

```rust
async fn read_file(path: &str, ctx: &ExecutorContext) -> Result<String> {
    // Always validate paths before operations
    let canonical = validate_path(path, ctx)?;
    tokio::fs::read_to_string(canonical).await
}
```

### 3. Log Security Events

```rust
fn log_security_event(event: &AuditEvent) {
    // Always log to persistent storage
    audit_logger.log(event);

    // Alert on denied operations
    if matches!(event.result, AuditResult::Denied { .. }) {
        alert_security_team(event);
    }
}
```

### 4. Implement Timeouts

```rust
// Prevent resource exhaustion
let result = tokio::time::timeout(
    Duration::from_secs(30),
    dispatcher.execute(tool, args, ctx)
).await??;
```

### 5. Sanitize Error Messages

```rust
// Don't leak sensitive paths in errors
fn sanitize_error(e: Error, ctx: &ExecutorContext) -> Error {
    match e {
        Error::Io(io_err) => {
            // Remove absolute paths from error message
            Error::Io(std::io::Error::new(
                io_err.kind(),
                "File operation failed"
            ))
        }
        _ => e
    }
}
```

## Next Steps

- [Language Bindings](languages.md): Implement security in your language
- [Tool Reference](tools.md): Security annotations per tool
