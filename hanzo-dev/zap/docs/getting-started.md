# Getting Started with ZAP

This guide will help you install ZAP and build your first agent in under 5 minutes.

## Installation

### Rust

Add to your `Cargo.toml`:

```toml
[dependencies]
hanzo-zap = "0.1"
tokio = { version = "1", features = ["full"] }
serde_json = "1"
```

### Python

```bash
pip install hanzo-zap

# Or with specific tool packages:
pip install hanzo-tools-fs hanzo-tools-vcs hanzo-tools-shell
```

### Go

```bash
go get github.com/hanzoai/zap-go
```

### Node.js

```bash
npm install @hanzo/zap
# or
pnpm add @hanzo/zap
```

### Other Languages

| Language | Package | Install |
|----------|---------|---------|
| Ruby | `hanzo-zap` | `gem install hanzo-zap` |
| Elixir | `hanzo_zap` | `mix deps.get` (add to mix.exs) |
| C/C++ | `libzap` | See [FFI Guide](languages.md#cc) |

## Starting a Gateway

The ZAP Gateway bridges existing MCP servers and exposes them via ZAP protocol.

### Using the CLI

```bash
# Install the gateway
cargo install hanzo-zap-gateway

# Start with default config
zap-gateway

# Start with custom config
zap-gateway --config config.toml
```

### Using Docker

```bash
docker run -p 9999:9999 hanzoai/zap-gateway
```

### Example Configuration

Create `config.toml`:

```toml
[gateway]
listen = "127.0.0.1:9999"
max_message_size = 16777216  # 16MB
timeout_ms = 30000

[[servers]]
name = "filesystem"
[servers.transport]
type = "stdio"
command = "mcp-server-filesystem"
args = ["/home/user"]

[[servers]]
name = "github"
[servers.transport]
type = "http"
url = "https://mcp.github.com"
[servers.auth]
type = "bearer"
token = "${GITHUB_TOKEN}"

[[servers]]
name = "search"
[servers.transport]
type = "zap"
url = "zaps://search.hanzo.ai:9999"
```

## Hello World: Rust

```rust
use hanzo_zap::Client;
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Connect to the gateway
    let mut client = Client::connect("zap://localhost:9999").await?;

    println!("Connected to ZAP gateway");

    // List all available tools
    let tools = client.list_tools().await?;
    println!("Available tools ({}):", tools.len());
    for tool in &tools {
        println!("  - {}: {}", tool.name, tool.description);
    }

    // Call the read_file tool
    let result = client.call_tool("read_file", json!({
        "path": "README.md"
    })).await?;

    if let Some(error) = &result.error {
        eprintln!("Error: {}", error);
    } else {
        println!("File content:\n{}", result.content);
    }

    // Clean up
    client.close().await?;
    Ok(())
}
```

## Hello World: Python

```python
#!/usr/bin/env python3
import asyncio
from hanzo_zap import Client

async def main():
    # Connect to the gateway
    client = await Client.connect("zap://localhost:9999")
    print("Connected to ZAP gateway")

    # List all available tools
    tools = await client.list_tools()
    print(f"Available tools ({len(tools)}):")
    for tool in tools:
        print(f"  - {tool.name}: {tool.description}")

    # Call the read_file tool
    result = await client.call_tool("read_file", {"path": "README.md"})

    if result.error:
        print(f"Error: {result.error}")
    else:
        print(f"File content:\n{result.content}")

    # Clean up
    await client.close()

if __name__ == "__main__":
    asyncio.run(main())
```

## Hello World: Go

```go
package main

import (
    "context"
    "fmt"
    "log"

    zap "github.com/hanzoai/zap-go"
)

func main() {
    ctx := context.Background()

    // Connect to the gateway
    client, err := zap.Connect(ctx, "zap://localhost:9999")
    if err != nil {
        log.Fatal(err)
    }
    defer client.Close()

    fmt.Println("Connected to ZAP gateway")

    // List all available tools
    tools, err := client.ListTools(ctx)
    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("Available tools (%d):\n", len(tools))
    for _, tool := range tools {
        fmt.Printf("  - %s: %s\n", tool.Name, tool.Description)
    }

    // Call the read_file tool
    result, err := client.CallTool(ctx, "read_file", map[string]interface{}{
        "path": "README.md",
    })
    if err != nil {
        log.Fatal(err)
    }

    if result.Error != "" {
        fmt.Printf("Error: %s\n", result.Error)
    } else {
        fmt.Printf("File content:\n%s\n", result.Content)
    }
}
```

## Hello World: Node.js

```javascript
import { Client } from '@hanzo/zap';

async function main() {
    // Connect to the gateway
    const client = await Client.connect('zap://localhost:9999');
    console.log('Connected to ZAP gateway');

    // List all available tools
    const tools = await client.listTools();
    console.log(`Available tools (${tools.length}):`);
    for (const tool of tools) {
        console.log(`  - ${tool.name}: ${tool.description}`);
    }

    // Call the read_file tool
    const result = await client.callTool('read_file', { path: 'README.md' });

    if (result.error) {
        console.error(`Error: ${result.error}`);
    } else {
        console.log(`File content:\n${result.content}`);
    }

    // Clean up
    await client.close();
}

main().catch(console.error);
```

## Your First Agent

Here's a complete agent that can fix a bug:

```rust
use hanzo_zap::Client;
use serde_json::json;

async fn fix_bug(client: &mut Client, file: &str) -> anyhow::Result<()> {
    println!("Fixing bug in {}", file);

    // Step 1: Read the file
    let content = client.call_tool("read_file", json!({
        "path": file
    })).await?;
    println!("Read {} bytes", content.content.as_str().unwrap_or("").len());

    // Step 2: Search for related code
    let refs = client.call_tool("grep", json!({
        "pattern": "authenticate",
        "path": "src/**/*.rs",
        "recursive": true
    })).await?;
    println!("Found references: {:?}", refs.content);

    // Step 3: Make the fix
    client.call_tool("edit_file", json!({
        "path": file,
        "edits": [{
            "old_text": "if password == stored_hash",
            "new_text": "if verify_password(password, stored_hash)"
        }]
    })).await?;
    println!("Applied fix");

    // Step 4: Run tests
    let test_result = client.call_tool("exec", json!({
        "command": "cargo test auth"
    })).await?;

    let exit_code = test_result.content.get("exit_code")
        .and_then(|v| v.as_i64())
        .unwrap_or(-1);

    if exit_code == 0 {
        // Step 5: Commit if tests pass
        client.call_tool("git_commit", json!({
            "message": "Fix password verification bug"
        })).await?;
        println!("Committed fix");
    } else {
        println!("Tests failed, not committing");
    }

    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let mut client = Client::connect("zap://localhost:9999").await?;
    fix_bug(&mut client, "src/auth/login.rs").await?;
    client.close().await
}
```

## Running Without a Gateway

For simple use cases, you can use the built-in tool executors directly:

```rust
use hanzo_zap::{default_dispatcher, ExecutorContext};
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Create a dispatcher with all default executors
    let dispatcher = default_dispatcher();

    // Create an execution context
    let ctx = ExecutorContext::with_cwd(".");

    // Execute tools directly
    let result = dispatcher.execute(
        "read_file",
        json!({ "path": "README.md" }),
        &ctx
    ).await?;

    println!("{}", result.content);
    Ok(())
}
```

## Next Steps

- [Schema Language](schema.md): Define custom tool schemas
- [Wire Format](encoding.md): Understand the binary protocol
- [Security](security.md): Configure permissions and sandboxing
- [Tool Reference](tools.md): Explore all 100+ built-in tools
