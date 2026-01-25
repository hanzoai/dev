# ZAP: Zero-copy Agent Protocol

ZAP is a high-performance binary protocol designed for AI agent communication. It replaces JSON-RPC with zero-copy serialization, eliminating parsing overhead and memory allocations while maintaining full compatibility with the Model Context Protocol (MCP) ecosystem.

## Why ZAP?

AI agents make **lots** of tool calls. A typical coding task involves dozens of file reads, searches, edits, and command executions. Each tool call requires serialization, network transfer, and deserialization. With JSON-RPC, this overhead accumulates quickly.

### The Problem with JSON-RPC

Consider a simple bug fix task:

```
Agent Chain:
  1. read_file("/src/auth/login.rs")           -> 15KB response
  2. grep("authentication", "/src/**/*.rs")    -> 8KB response
  3. read_file("/src/auth/session.rs")         -> 12KB response
  4. edit_file(login.rs, patch)                -> 2KB response
  5. run_command("cargo test auth")            -> 25KB response
  6. git_commit("Fix auth bug")                -> 1KB response
```

**6 tool calls, ~63KB of payload data**

With JSON-RPC (MCP):
- **4.6ms** just parsing responses
- **802 allocations** per chain
- **103KB** memory pressure (base64 inflation)

With ZAP:
- **3.1 microseconds** parsing (1,480x faster)
- **0 allocations** (zero-copy)
- **63KB** on wire (no inflation)

### Key Benefits

| Feature | Description |
|---------|-------------|
| **Zero-copy parsing** | Messages read directly from network buffers |
| **Zero allocations** | Buffer pooling eliminates GC pressure |
| **MCP compatibility** | Gateway bridges existing MCP servers seamlessly |
| **10-100x performance** | Measured speedups on real agent workloads |
| **Polyglot support** | Rust, Python, Go, Node.js, C/C++, Ruby, Elixir |
| **Unified tool schemas** | Same tool definitions across all languages |

## Protocol Overview

ZAP uses a simple length-prefixed binary format:

```
+----------+----------+------------------+
| Length   | MsgType  | Payload          |
| (4 bytes)| (1 byte) | (variable)       |
| LE u32   |          |                  |
+----------+----------+------------------+
```

This design enables:
- **Streaming**: Process messages before fully received
- **Pipelining**: Send multiple requests without waiting
- **Zero-copy**: Read strings directly from network buffer

## Transport Schemes

| Scheme | Description | Default Port | Use Case |
|--------|-------------|--------------|----------|
| `zap://` | Plain TCP | 9999 | Local development |
| `zaps://` | TLS 1.3 | 9999 | Production |
| `zap+unix://` | Unix socket | N/A | Same-host, lowest latency |

## Architecture

```
+------------------+     +------------------+     +------------------+
|   AI Agent       |     |   ZAP Gateway    |     |   MCP Servers    |
|   (any language) |<--->|   (aggregator)   |<--->|   (existing)     |
+------------------+     +------------------+     +------------------+
        |                        |                        |
        | ZAP Protocol           | ZAP or JSON-RPC        | JSON-RPC
        | (binary)               | (bridged)              | (stdio/HTTP)
```

The ZAP Gateway serves as a protocol bridge, allowing agents to communicate via ZAP while transparently connecting to existing MCP servers. This enables gradual migration without breaking existing infrastructure.

## Comparison with Alternatives

### vs JSON-RPC (MCP)

| Aspect | JSON-RPC | ZAP |
|--------|----------|-----|
| Parse latency (10KB) | 420 us | 0.4 us |
| Wire size | +33% (base64) | Native binary |
| Allocations | 47/msg | 0 |
| Streaming | Limited | Native |
| Ecosystem | Large | MCP-compatible |

### vs gRPC

| Aspect | gRPC | ZAP |
|--------|------|-----|
| Schema language | Protobuf | Native types |
| Code generation | Required | Optional |
| Zero-copy | No | Yes |
| Simplicity | Complex | Simple |
| HTTP/2 required | Yes | No |

### vs Cap'n Proto

| Aspect | Cap'n Proto | ZAP |
|--------|-------------|-----|
| Zero-copy | Yes | Yes |
| Schema complexity | High | Low |
| Agent-specific | No | Yes |
| MCP compatibility | No | Yes |

## Quick Example

```rust
use hanzo_zap::Client;
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Connect to ZAP gateway
    let mut client = Client::connect("zap://localhost:9999").await?;

    // List available tools
    let tools = client.list_tools().await?;
    println!("Available tools: {}", tools.len());

    // Call a tool (zero-copy response)
    let result = client.call_tool("read_file", json!({
        "path": "/src/main.rs"
    })).await?;

    println!("Content: {}", result.content);
    Ok(())
}
```

## Related Specifications

- **[HIP-007](https://github.com/hanzoai/hips/blob/main/HIP-007-zap.md)**: ZAP Protocol Specification (Hanzo AI)
- **[LP-120](https://github.com/luxfi/lps/blob/main/LPs/lp-0120-zap-transport-protocol.md)**: ZAP Transport Protocol (Lux Network)
- **[MCP](https://modelcontextprotocol.io)**: Model Context Protocol (Anthropic)

## Next Steps

- [Getting Started](getting-started.md): Install and run your first agent
- [Schema Language](schema.md): Define tool schemas
- [Wire Format](encoding.md): Understand the binary protocol
- [RPC Protocol](rpc.md): Request/response patterns
- [Security](security.md): Permission and sandbox policies
- [Language Bindings](languages.md): Use ZAP in your language
- [Tool Reference](tools.md): Complete API documentation

## License

MIT License - Copyright 2025 Hanzo Industries Inc.
