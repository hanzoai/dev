# hanzo-mcp-client

[![Crates.io](https://img.shields.io/crates/v/hanzo-mcp-client.svg)](https://crates.io/crates/hanzo-mcp-client)
[![Documentation](https://docs.rs/hanzo-mcp-client/badge.svg)](https://docs.rs/hanzo-mcp-client)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Async MCP (Model Context Protocol) client for Rust.

## Overview

This crate provides an async client for communicating with MCP servers. It handles the JSON-RPC transport layer, message framing, and provides a clean API for tool discovery and execution.

## Installation

```toml
[dependencies]
hanzo-mcp-client = "0.6"
tokio = { version = "1", features = ["full"] }
```

## Quick Start

```rust
use hanzo_mcp_client::McpClient;
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Spawn an MCP server and connect
    let client = McpClient::spawn(
        "npx",
        &["-y", "@modelcontextprotocol/server-filesystem", "/workspace"],
    ).await?;

    // List available tools
    let tools = client.list_tools().await?;
    println!("Available tools: {:?}", tools.iter().map(|t| &t.name).collect::<Vec<_>>());

    // Call a tool
    let result = client.call_tool("read_file", json!({
        "path": "README.md"
    })).await?;

    println!("Result: {:?}", result);
    Ok(())
}
```

## Features

- **Async/await** - Built on Tokio for high-performance async I/O
- **Subprocess management** - Spawn and manage MCP server processes
- **JSON-RPC** - Full implementation of MCP's JSON-RPC protocol
- **Type-safe** - Strongly typed requests and responses via `hanzo-mcp-types`
- **Error handling** - Comprehensive error types with context

## API Reference

### McpClient

```rust
impl McpClient {
    /// Spawn an MCP server subprocess and connect
    pub async fn spawn(command: &str, args: &[&str]) -> Result<Self>;

    /// Connect to an existing MCP server via stdio
    pub async fn connect(stdin: ChildStdin, stdout: ChildStdout) -> Result<Self>;

    /// Initialize the MCP connection
    pub async fn initialize(&self) -> Result<InitializeResult>;

    /// List available tools
    pub async fn list_tools(&self) -> Result<Vec<Tool>>;

    /// Call a tool with arguments
    pub async fn call_tool(&self, name: &str, arguments: Value) -> Result<CallToolResult>;

    /// List available resources
    pub async fn list_resources(&self) -> Result<Vec<Resource>>;

    /// Read a resource
    pub async fn read_resource(&self, uri: &str) -> Result<ReadResourceResult>;

    /// List available prompts
    pub async fn list_prompts(&self) -> Result<Vec<Prompt>>;

    /// Get a prompt with arguments
    pub async fn get_prompt(&self, name: &str, arguments: Value) -> Result<GetPromptResult>;
}
```

## Example: Multiple Servers

```rust
use hanzo_mcp_client::McpClient;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Connect to multiple MCP servers
    let filesystem = McpClient::spawn(
        "npx", &["-y", "@modelcontextprotocol/server-filesystem", "/workspace"]
    ).await?;

    let github = McpClient::spawn(
        "npx", &["-y", "@modelcontextprotocol/server-github"]
    ).await?;

    // Use tools from different servers
    let files = filesystem.call_tool("list_directory", json!({"path": "."})).await?;
    let issues = github.call_tool("list_issues", json!({"repo": "hanzoai/dev"})).await?;

    Ok(())
}
```

## Error Handling

```rust
use hanzo_mcp_client::{McpClient, McpError};

match client.call_tool("unknown_tool", json!({})).await {
    Ok(result) => println!("Success: {:?}", result),
    Err(McpError::ToolNotFound(name)) => println!("Tool not found: {}", name),
    Err(McpError::InvalidParams(msg)) => println!("Invalid params: {}", msg),
    Err(McpError::Transport(e)) => println!("Transport error: {}", e),
    Err(e) => println!("Other error: {}", e),
}
```

## Related Crates

- [`hanzo-mcp-types`](https://crates.io/crates/hanzo-mcp-types) - MCP type definitions
- [`hanzo-protocol`](https://crates.io/crates/hanzo-protocol) - Core protocol types
- [`hanzo-zap`](https://crates.io/crates/hanzo-zap) - Zero-copy Agent Protocol (1000x faster)

## License

MIT License - Copyright 2025 Hanzo AI Inc.
