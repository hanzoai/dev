//! ZAP Agent Example - Rust
//!
//! Demonstrates a complete ZAP agent with tool execution in Rust.
//!
//! ```bash
//! cargo run --example rust_agent
//! ```

use hanzo_zap::{
    default_dispatcher, ExecutorContext, AskForApproval, SandboxPolicy,
    tools::filesystem::ReadFileArgs,
};
use serde_json::json;
use std::collections::HashMap;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize dispatcher with all default executors
    let dispatcher = default_dispatcher();

    // Create execution context with workspace-write sandbox
    let ctx = ExecutorContext {
        cwd: Some(".".to_string()),
        env: HashMap::new(),
        session_id: Some("rust-agent-001".to_string()),
        approval_policy: AskForApproval::OnRequest,
        sandbox_policy: SandboxPolicy::WorkspaceWrite {
            writable_roots: vec![],
            network_access: true,
            exclude_tmpdir_env_var: false,
            exclude_slash_tmp: false,
            allow_git_writes: false,
        },
        timeout_ms: Some(30_000),
    };

    println!("ZAP Rust Agent Example");
    println!("======================");
    println!();

    // List available tools
    let tools = dispatcher.list_tools();
    println!("Available tools ({}):", tools.len());
    for tool in &tools {
        println!("  - {}", tool);
    }
    println!();

    // Example 1: Read a file
    println!("Example 1: Read file");
    let result = dispatcher.execute(
        "read_file",
        json!({ "path": "Cargo.toml" }),
        &ctx,
    ).await?;
    println!("  Result: {} bytes read",
        result.content.as_str().map(|s| s.len()).unwrap_or(0)
    );
    println!();

    // Example 2: List directory
    println!("Example 2: List directory");
    let result = dispatcher.execute(
        "list_dir",
        json!({ "path": ".", "show_hidden": false }),
        &ctx,
    ).await?;
    println!("  Result: {:?}", result.content);
    println!();

    // Example 3: Git status
    println!("Example 3: Git status");
    let result = dispatcher.execute(
        "git_status",
        json!({}),
        &ctx,
    ).await?;
    println!("  Result: {:?}", result.content);
    println!();

    // Example 4: Check permission before write
    println!("Example 4: Permission check");
    let approval = ctx.check_approval("write_file");
    println!("  Write file approval: {:?}", approval);

    Ok(())
}
