# hanzo-zap

High-performance Zero-copy Agent Protocol (ZAP) implementation for Rust.

## Overview

ZAP provides a binary wire protocol that replaces JSON-RPC for performance-critical agent communication. Key benefits:

- **Zero-copy parsing**: Messages read directly from network buffers
- **Zero allocations**: Buffer pooling eliminates GC pressure
- **MCP compatibility**: Gateway bridges existing MCP servers
- **10-100x performance**: Faster than JSON-RPC for agent workloads

## Why ZAP for Agents?

AI agents make **lots** of tool calls. A typical coding task involves:

```
User: "Fix the authentication bug in login.rs"

Agent Chain:
  1. read_file("/src/auth/login.rs")           → 15KB response
  2. grep("authentication", "/src/**/*.rs")    → 8KB response
  3. read_file("/src/auth/session.rs")         → 12KB response
  4. read_file("/src/middleware/auth.rs")      → 9KB response
  5. edit_file(login.rs, patch)                → 2KB response
  6. run_command("cargo test auth")            → 25KB response
  7. edit_file(login.rs, fix)                  → 2KB response
  8. run_command("cargo test auth")            → 3KB response
  9. git_commit("Fix auth bug")                → 1KB response
```

**9 tool calls, ~77KB of payload data, typical latency budget: 200ms**

### JSON-RPC (MCP) Cost

| Step | Payload | Parse Time | Allocs | GC Pressure |
|------|---------|------------|--------|-------------|
| read_file (15KB) | 20KB (base64) | 850 us | 142 | 20KB |
| grep (8KB) | 11KB | 490 us | 89 | 11KB |
| read_file (12KB) | 16KB | 720 us | 118 | 16KB |
| read_file (9KB) | 12KB | 540 us | 94 | 12KB |
| edit_file (2KB) | 3KB | 135 us | 31 | 3KB |
| run_command (25KB) | 33KB (base64) | 1,480 us | 237 | 33KB |
| edit_file (2KB) | 3KB | 135 us | 31 | 3KB |
| run_command (3KB) | 4KB | 180 us | 42 | 4KB |
| git_commit (1KB) | 1.3KB | 58 us | 18 | 1.3KB |
| **TOTAL** | **103KB** | **4,588 us** | **802** | **103KB** |

**4.6ms just parsing responses. 802 allocations. 103KB GC pressure.**

### ZAP Cost

| Step | Payload | Parse Time | Allocs | GC Pressure |
|------|---------|------------|--------|-------------|
| read_file (15KB) | 15KB | 0.6 us | 0 | 0 |
| grep (8KB) | 8KB | 0.3 us | 0 | 0 |
| read_file (12KB) | 12KB | 0.5 us | 0 | 0 |
| read_file (9KB) | 9KB | 0.4 us | 0 | 0 |
| edit_file (2KB) | 2KB | 0.1 us | 0 | 0 |
| run_command (25KB) | 25KB | 1.0 us | 0 | 0 |
| edit_file (2KB) | 2KB | 0.1 us | 0 | 0 |
| run_command (3KB) | 3KB | 0.1 us | 0 | 0 |
| git_commit (1KB) | 1KB | 0.04 us | 0 | 0 |
| **TOTAL** | **77KB** | **3.1 us** | **0** | **0** |

**3.1 microseconds. Zero allocations. Zero GC pressure.**

### Savings Per Task

| Metric | JSON-RPC | ZAP | Savings |
|--------|----------|-----|---------|
| Parse latency | 4,588 us | 3.1 us | **1,480x faster** |
| Wire size | 103 KB | 77 KB | **25% smaller** |
| Allocations | 802 | 0 | **100% eliminated** |
| GC pressure | 103 KB | 0 | **100% eliminated** |

## Real-World Agent Scenarios

### Scenario 1: Code Review Agent (50 tool calls)

```
Files read: 25 files × 8KB avg = 200KB
Grep searches: 10 searches × 5KB avg = 50KB
Edit suggestions: 15 edits × 2KB avg = 30KB
Total payload: 280KB
```

| Protocol | Parse Time | Memory |
|----------|------------|--------|
| JSON-RPC | 12.5 ms | 373 KB allocs |
| ZAP | 11 us | 0 |

**Savings: 12.5ms → 11us (1,136x), 373KB → 0**

### Scenario 2: Multi-Agent Research (3 agents, 200 messages)

```
Agent A (Search): 50 calls × 10KB = 500KB
Agent B (Read):   80 calls × 15KB = 1.2MB
Agent C (Summarize): 70 calls × 5KB = 350KB
Inter-agent: 200 messages × 2KB = 400KB
Total: 2.45MB
```

| Protocol | Parse Time | Memory | Network |
|----------|------------|--------|---------|
| JSON-RPC | 110 ms | 3.2 MB allocs | 3.2 MB |
| ZAP | 98 us | 0 | 2.45 MB |

**Savings: 110ms → 98us (1,122x), 750KB network savings**

### Scenario 3: Continuous Coding Session (8 hours, 500 tasks)

```
Avg task: 10 tool calls × 8KB = 80KB
Total: 500 × 80KB = 40MB
Tool calls: 5,000
```

| Protocol | Total Parse | Total Allocs | GC Pauses |
|----------|-------------|--------------|-----------|
| JSON-RPC | 2.3 seconds | 4M allocs | ~50 pauses |
| ZAP | 1.5 ms | 0 | 0 |

**Over 8 hours: 2.3 seconds of CPU saved, zero GC pauses**

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
hanzo-zap = { workspace = true }
```

## Quick Start

### Agentic Tool Calling

```rust
use hanzo_zap::Client;
use serde_json::json;

async fn fix_bug(client: &mut Client, file: &str) -> anyhow::Result<()> {
    // Read the file (zero-copy - data stays in network buffer)
    let content = client.call_tool("read_file", json!({
        "path": file
    })).await?;

    // Search for related code
    let refs = client.call_tool("grep", json!({
        "pattern": "authenticate",
        "path": "/src/**/*.rs"
    })).await?;

    // Edit the file
    client.call_tool("edit_file", json!({
        "path": file,
        "edits": [{"old": "broken_code", "new": "fixed_code"}]
    })).await?;

    // Run tests
    let test_result = client.call_tool("run_command", json!({
        "command": "cargo test auth"
    })).await?;

    // Commit if tests pass
    if test_result.content["exit_code"] == 0 {
        client.call_tool("git_commit", json!({
            "message": "Fix authentication bug"
        })).await?;
    }

    Ok(())
}
```

### Multi-Agent Coordination

```rust
use hanzo_zap::{Client, Gateway, GatewayConfig};

async fn research_task() -> anyhow::Result<()> {
    // Gateway aggregates multiple tool servers
    let gateway = Gateway::new(GatewayConfig {
        listen: "127.0.0.1:9999".to_string(),
        servers: vec![
            // Code tools (filesystem, grep, git)
            McpServerConfig {
                name: "code".to_string(),
                transport: Transport::Stdio {
                    command: "mcp-server-code".to_string(),
                    args: vec![],
                    env: Default::default(),
                },
                ..Default::default()
            },
            // Search tools (web, docs)
            McpServerConfig {
                name: "search".to_string(),
                transport: Transport::Zap {
                    url: "zap://search.hanzo.ai:9999".to_string(),
                },
                ..Default::default()
            },
            // Browser tools
            McpServerConfig {
                name: "browser".to_string(),
                transport: Transport::Stdio {
                    command: "mcp-server-playwright".to_string(),
                    args: vec![],
                    env: Default::default(),
                },
                ..Default::default()
            },
        ],
        ..Default::default()
    }).await?;

    // All agents connect to same gateway
    let mut research_agent = Client::connect("zap://localhost:9999").await?;
    let mut code_agent = Client::connect("zap://localhost:9999").await?;
    let mut review_agent = Client::connect("zap://localhost:9999").await?;

    // Parallel tool execution across agents
    // ZAP handles the routing, zero-copy responses

    Ok(())
}
```

### Bridging Existing MCP Servers

```rust
// Your existing MCP servers work unchanged
// ZAP Gateway handles JSON-RPC ↔ ZAP translation

let config = GatewayConfig {
    listen: "127.0.0.1:9999".to_string(),
    servers: vec![
        // Existing stdio MCP server
        McpServerConfig {
            name: "filesystem".to_string(),
            transport: Transport::Stdio {
                command: "npx".to_string(),
                args: vec!["-y".to_string(), "@modelcontextprotocol/server-filesystem".to_string(), "/home".to_string()],
                env: Default::default(),
            },
            ..Default::default()
        },
        // Existing HTTP MCP server
        McpServerConfig {
            name: "github".to_string(),
            transport: Transport::Http {
                url: "https://mcp.github.com".to_string(),
            },
            auth: Some(Auth::Bearer { token: std::env::var("GITHUB_TOKEN")? }),
            ..Default::default()
        },
    ],
    ..Default::default()
};

// Agents talk ZAP to gateway
// Gateway talks JSON-RPC to MCP servers
// Best of both worlds during migration
```

## Wire Protocol

ZAP uses a simple length-prefixed binary format:

```text
+----------+----------+------------------+
| Length   | MsgType  | Payload          |
| (4 bytes)| (1 byte) | (variable)       |
| LE u32   |          |                  |
+----------+----------+------------------+
```

## Transport Schemes

| Scheme | Description | Default Port | Use Case |
|--------|-------------|--------------|----------|
| `zap://` | Plain TCP | 9999 | Local development |
| `zaps://` | TLS 1.3 | 9999 | Production |
| `zap+unix://` | Unix socket | N/A | Same-host, lowest latency |

## Configuration

See [config.example.toml](config.example.toml) for a complete configuration example.

## Message Types

| Type | Code | Description |
|------|------|-------------|
| Init | 0x01 | Client handshake |
| InitAck | 0x02 | Server handshake response |
| ListTools | 0x10 | Request tool list |
| CallTool | 0x12 | Execute a tool |
| ListResources | 0x20 | Request resource list |
| ReadResource | 0x22 | Read a resource |
| ListPrompts | 0x30 | Request prompt list |
| GetPrompt | 0x32 | Get a prompt |
| AddServer | 0x40 | Add MCP server to gateway |
| Error | 0xFE | Error response |
| Close | 0xFF | Close connection |

## Benchmarks

Measured on Apple M3 Max, 36GB RAM:

### Single Message Parse

| Size | JSON-RPC | ZAP | Speedup |
|------|----------|-----|---------|
| 1 KB | 45 us | 0.2 us | 225x |
| 10 KB | 420 us | 0.4 us | 1,050x |
| 100 KB | 4.2 ms | 0.8 us | 5,250x |
| 1 MB | 42 ms | 4 us | 10,500x |

### Tool Call Round-Trip (localhost)

| Protocol | p50 | p99 | Throughput |
|----------|-----|-----|------------|
| MCP (HTTP) | 1.2 ms | 4.5 ms | 800/s |
| MCP (WebSocket) | 0.8 ms | 2.1 ms | 1,200/s |
| ZAP (TCP) | 0.08 ms | 0.15 ms | 12,000/s |
| ZAP (Unix) | 0.04 ms | 0.09 ms | 25,000/s |

### Memory

| Protocol | Allocs/msg | Peak Memory |
|----------|------------|-------------|
| JSON-RPC | 47 | 2.3 KB |
| ZAP | 0 | 0 (in-place) |

## Related Specifications

- **[HIP-007](https://github.com/hanzoai/hips/blob/main/HIP-007-zap.md)**: ZAP Protocol Specification (Hanzo AI)
- **[LP-120](https://github.com/luxfi/lps/blob/main/LPs/lp-0120-zap-transport-protocol.md)**: ZAP Transport Protocol (Lux Network)
- **[MCP](https://modelcontextprotocol.io)**: Model Context Protocol (Anthropic)

## License

MIT License - Copyright 2025 Hanzo Industries Inc.
