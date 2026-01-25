# hanzo-mcp-types

[![Crates.io](https://img.shields.io/crates/v/hanzo-mcp-types.svg)](https://crates.io/crates/hanzo-mcp-types)
[![Documentation](https://docs.rs/hanzo-mcp-types/badge.svg)](https://docs.rs/hanzo-mcp-types)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Rust type definitions for the [Model Context Protocol (MCP)](https://modelcontextprotocol.io).

## Overview

This crate provides strongly-typed Rust definitions for all MCP protocol messages, enabling type-safe communication between AI agents and tool servers.

## Installation

```toml
[dependencies]
hanzo-mcp-types = "0.6"
```

## Usage

```rust
use mcp_types::{
    CallToolRequest, CallToolResult, ListToolsRequest, ListToolsResult,
    Tool, ToolInfo, Content, TextContent,
};

// Define a tool
let tool = Tool {
    name: "read_file".to_string(),
    description: Some("Read contents of a file".to_string()),
    input_schema: serde_json::json!({
        "type": "object",
        "properties": {
            "path": {"type": "string"}
        },
        "required": ["path"]
    }),
};

// Create a tool call request
let request = CallToolRequest {
    name: "read_file".to_string(),
    arguments: Some(serde_json::json!({"path": "src/main.rs"})),
};
```

## Key Types

| Type | Description |
|------|-------------|
| `Tool` | Tool definition with name, description, and JSON schema |
| `CallToolRequest` | Request to execute a tool |
| `CallToolResult` | Result from tool execution |
| `ListToolsRequest` | Request to list available tools |
| `ListToolsResult` | List of available tools |
| `Content` | Tool output content (text, image, resource) |
| `Resource` | External resource reference |
| `Prompt` | Prompt template definition |

## Protocol Version

This crate implements MCP specification version `2025-06-18`.

- TypeScript schema (source of truth): [schema.ts](https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/schema/2025-06-18/schema.ts)
- JSON schema: [schema.json](https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/schema/2025-06-18/schema.json)

## Related Crates

- [`hanzo-mcp-client`](https://crates.io/crates/hanzo-mcp-client) - Async MCP client
- [`hanzo-protocol`](https://crates.io/crates/hanzo-protocol) - Core protocol types
- [`hanzo-zap`](https://crates.io/crates/hanzo-zap) - Zero-copy Agent Protocol

## License

MIT License - Copyright 2025 Hanzo AI Inc.
