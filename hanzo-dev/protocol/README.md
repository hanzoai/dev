# hanzo-protocol

[![Crates.io](https://img.shields.io/crates/v/hanzo-protocol.svg)](https://crates.io/crates/hanzo-protocol)
[![Documentation](https://docs.rs/hanzo-protocol/badge.svg)](https://docs.rs/hanzo-protocol)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Core protocol types for Hanzo AI agents.

## Overview

This crate defines the fundamental types used throughout the Hanzo AI ecosystem, including:

- **AskForApproval** - Human-in-the-loop approval policies
- **SandboxPolicy** - Filesystem and execution sandboxing
- **Protocol messages** - Internal communication types
- **Configuration** - Agent configuration schemas

## Installation

```toml
[dependencies]
hanzo-protocol = "0.6"
```

## Key Types

### AskForApproval

Controls when the agent pauses to request human confirmation:

```rust
use hanzo_protocol::AskForApproval;

let policy = AskForApproval::OnRequest; // Model decides when to ask

match policy {
    AskForApproval::Never => { /* Full autonomy */ }
    AskForApproval::OnFailure => { /* Ask only on errors */ }
    AskForApproval::OnRequest => { /* Model decides */ }
    AskForApproval::UnlessTrusted => { /* Ask for risky operations */ }
}
```

| Policy | Description |
|--------|-------------|
| `Never` | Full autonomy - never ask |
| `OnFailure` | Ask only when operations fail |
| `OnRequest` | Model decides based on risk assessment |
| `UnlessTrusted` | Ask unless operation is known-safe |

### SandboxPolicy

Controls what operations are physically allowed:

```rust
use hanzo_protocol::SandboxPolicy;

let policy = SandboxPolicy::WorkspaceWrite {
    writable_roots: vec![],
    network_access: true,
    exclude_tmpdir_env_var: false,
    exclude_slash_tmp: false,
    allow_git_writes: true,
};

match policy {
    SandboxPolicy::DangerFullAccess => { /* No restrictions */ }
    SandboxPolicy::ReadOnly => { /* Read-only filesystem */ }
    SandboxPolicy::WorkspaceWrite { .. } => { /* Write to workspace only */ }
}
```

| Policy | Filesystem | Network | Processes |
|--------|------------|---------|-----------|
| `DangerFullAccess` | Full | Full | Full |
| `WorkspaceWrite` | Read all, write workspace | Configurable | Allowed |
| `ReadOnly` | Read only | Blocked | Limited |

## Modules

| Module | Description |
|--------|-------------|
| `config_types` | Configuration schemas |
| `protocol` | Core protocol definitions |
| `models` | Data models |
| `mcp_protocol` | MCP-specific types |
| `responses` | Response structures |
| `tool_config` | Tool configuration |

## Example: Agent Configuration

```rust
use hanzo_protocol::{AskForApproval, SandboxPolicy, AgentConfig};

let config = AgentConfig {
    approval_policy: AskForApproval::OnRequest,
    sandbox_policy: SandboxPolicy::WorkspaceWrite {
        writable_roots: vec!["/home/user/project".into()],
        network_access: true,
        exclude_tmpdir_env_var: false,
        exclude_slash_tmp: false,
        allow_git_writes: true,
    },
    timeout_ms: 30000,
    ..Default::default()
};
```

## TypeScript Bindings

This crate uses `ts-rs` to generate TypeScript type definitions:

```bash
cargo test --features ts-rs
# Generates TypeScript bindings in ./bindings/
```

## Related Crates

- [`hanzo-mcp-types`](https://crates.io/crates/hanzo-mcp-types) - MCP type definitions
- [`hanzo-mcp-client`](https://crates.io/crates/hanzo-mcp-client) - Async MCP client
- [`hanzo-zap`](https://crates.io/crates/hanzo-zap) - Zero-copy Agent Protocol

## License

MIT License - Copyright 2025 Hanzo AI Inc.
