use std::collections::HashMap;
use std::path::PathBuf;

use anyhow::Result;
use codex_core::config_types::McpServerConfig;
use codex_core::mcp_connection_manager::McpConnectionManager;

fn server_bin_path() -> PathBuf {
    let mut root = std::env::current_dir().unwrap();
    // Ensure we run from workspace root `codex-rs` for relative path to work.
    if root.ends_with("codex-rs") {
        // ok
    } else if root.ends_with("code") {
        root.push("codex-rs");
    }
    let mut p = root;
    p.push("target");
    p.push("debug");
    #[cfg(windows)]
    p.push("codex-mcp-test-server.exe");
    #[cfg(not(windows))]
    p.push("codex-mcp-test-server");
    p
}

#[tokio::main]
async fn main() -> Result<()> {
    let server = server_bin_path();
    if !server.exists() {
        eprintln!("Build the test server first: cargo build -p codex-mcp-test-server");
        std::process::exit(2);
    }

    // Fast server
    let fast = McpServerConfig {
        transport: McpServerTransportConfig::Stdio {
            command: server.to_string_lossy().to_string(),
            args: vec![],
            env: None,
        },
        startup_timeout_sec: Some(Duration::from_millis(500)),
        tool_timeout_sec: None,
    };
    // Slow-one: 2s but we allow 3s
    let slow_ok = McpServerConfig {
        transport: McpServerTransportConfig::Stdio {
            command: "bash".to_string(),
            args: vec![
                "-lc".to_string(),
                format!("SLOW_INIT_MS=500 SLOW_LIST_MS=2000 {}", server.display()),
            ],
            env: None,
        },
        startup_timeout_sec: Some(Duration::from_millis(3000)),
        tool_timeout_sec: None,
    };
    // Slow-two: 3s but we allow 1s (should fail)
    let slow_fail = McpServerConfig {
        transport: McpServerTransportConfig::Stdio {
            command: "bash".to_string(),
            args: vec![
                "-lc".to_string(),
                format!("SLOW_INIT_MS=500 SLOW_LIST_MS=3000 {}", server.display()),
            ],
            env: None,
        },
        startup_timeout_sec: Some(Duration::from_millis(1000)),
        tool_timeout_sec: None,
    };

    let mut servers = HashMap::new();
    servers.insert("fast".to_string(), fast);
    servers.insert("slow_ok".to_string(), slow_ok);
    servers.insert("slow_fail".to_string(), slow_fail);

    let (mgr, errs) = McpConnectionManager::new(servers, false, std::collections::HashSet::new()).await?;
    println!("Errors: {}", errs.len());
    for (name, e) in &errs {
        println!("  {}: {}", name, e);
    }
    let tools = mgr.list_all_tools();
    println!("Tools ({}):", tools.len());
    for k in tools.keys() {
        println!("  {}", k);
    }
    Ok(())
}
