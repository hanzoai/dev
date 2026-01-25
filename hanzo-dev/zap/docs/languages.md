# Language Bindings

ZAP provides native bindings for multiple programming languages, enabling agents to be written in whatever language is most appropriate for the task.

## Supported Languages

| Language | Package | Status | Native Tools | MCP Bridge |
|----------|---------|--------|--------------|------------|
| Rust | `hanzo-zap` | Stable | Yes | Yes |
| Python | `hanzo-zap` | Stable | Yes | Yes |
| Go | `github.com/hanzoai/zap-go` | Stable | Yes | Yes |
| Node.js | `@hanzo/zap` | Stable | Yes | Yes |
| C/C++ | `libzap` | Beta | FFI | Yes |
| Ruby | `hanzo-zap` | Beta | Via FFI | Yes |
| Elixir | `hanzo_zap` | Beta | Via NIF | Yes |

## Rust

The reference implementation with full zero-copy support.

### Installation

```toml
[dependencies]
hanzo-zap = "0.1"
tokio = { version = "1", features = ["full"] }
serde_json = "1"
```

### Client Usage

```rust
use hanzo_zap::Client;
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let mut client = Client::connect("zap://localhost:9999").await?;

    // List tools
    let tools = client.list_tools().await?;
    for tool in &tools {
        println!("{}: {}", tool.name, tool.description);
    }

    // Call a tool
    let result = client.call_tool("read_file", json!({
        "path": "src/main.rs"
    })).await?;

    println!("{}", result.content);

    client.close().await?;
    Ok(())
}
```

### Gateway Usage

```rust
use hanzo_zap::{Gateway, GatewayConfig, McpServerConfig, Transport};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let config = GatewayConfig {
        listen: "0.0.0.0:9999".to_string(),
        servers: vec![
            McpServerConfig {
                name: "filesystem".to_string(),
                transport: Transport::Stdio {
                    command: "mcp-server-filesystem".to_string(),
                    args: vec!["/home".to_string()],
                    env: Default::default(),
                },
                auth: None,
                timeout_ms: None,
            },
        ],
        ..Default::default()
    };

    let gateway = Gateway::new(config).await?;
    gateway.serve().await
}
```

### Direct Tool Execution

```rust
use hanzo_zap::{default_dispatcher, ExecutorContext};
use serde_json::json;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let dispatcher = default_dispatcher();
    let ctx = ExecutorContext::with_cwd(".");

    let result = dispatcher.execute(
        "read_file",
        json!({ "path": "README.md" }),
        &ctx
    ).await?;

    println!("{}", result.content);
    Ok(())
}
```

## Python

Pythonic async/await interface with type hints.

### Installation

```bash
pip install hanzo-zap

# With specific tool packages
pip install hanzo-tools-fs hanzo-tools-vcs hanzo-tools-shell
```

### Client Usage

```python
import asyncio
from hanzo_zap import Client

async def main():
    client = await Client.connect("zap://localhost:9999")

    # List tools
    tools = await client.list_tools()
    for tool in tools:
        print(f"{tool.name}: {tool.description}")

    # Call a tool
    result = await client.call_tool("read_file", {"path": "src/main.py"})
    print(result.content)

    await client.close()

if __name__ == "__main__":
    asyncio.run(main())
```

### Direct Tool Execution

```python
from hanzo_tools.core import BaseTool, PermissionManager
from hanzo_tools.fs import FsTool
from hanzo_tools.vcs import VcsTool

async def main():
    # Create permission manager
    pm = PermissionManager(
        allowed_paths=["."],
        deny_patterns=[".env", "*.pem"]
    )

    # Create tools
    fs = FsTool(permission_manager=pm)
    vcs = VcsTool(cwd=".")

    # Execute
    content = await fs.call(ctx=None, action="read", path="README.md")
    status = await vcs.call(ctx=None, action="status")

    print(content)
    print(status)
```

### Type Hints

```python
from dataclasses import dataclass
from typing import Optional, List

@dataclass
class ReadFileArgs:
    path: str
    offset: Optional[int] = None
    limit: Optional[int] = None

@dataclass
class ReadFileResult:
    content: str
    mime_type: str
    size: int

async def read_file(client: Client, args: ReadFileArgs) -> ReadFileResult:
    result = await client.call_tool("read_file", {
        "path": args.path,
        "offset": args.offset,
        "limit": args.limit,
    })
    return ReadFileResult(
        content=result.content,
        mime_type=result.metadata.get("mime_type", "text/plain"),
        size=int(result.metadata.get("size", "0"))
    )
```

## Go

Idiomatic Go with context support and error handling.

### Installation

```bash
go get github.com/hanzoai/zap-go
```

### Client Usage

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

    client, err := zap.Connect(ctx, "zap://localhost:9999")
    if err != nil {
        log.Fatal(err)
    }
    defer client.Close()

    // List tools
    tools, err := client.ListTools(ctx)
    if err != nil {
        log.Fatal(err)
    }

    for _, tool := range tools {
        fmt.Printf("%s: %s\n", tool.Name, tool.Description)
    }

    // Call a tool
    result, err := client.CallTool(ctx, "read_file", map[string]interface{}{
        "path": "main.go",
    })
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(result.Content)
}
```

### Typed Tool Calls

```go
package main

import (
    "context"

    zap "github.com/hanzoai/zap-go"
    "github.com/hanzoai/zap-go/tools/filesystem"
)

func main() {
    ctx := context.Background()
    client, _ := zap.Connect(ctx, "zap://localhost:9999")
    defer client.Close()

    // Typed arguments
    args := filesystem.ReadFileArgs{
        Path:   "main.go",
        Offset: nil,
        Limit:  nil,
    }

    // Typed result
    var result filesystem.ReadFileResult
    err := client.CallToolTyped(ctx, "read_file", args, &result)
    if err != nil {
        log.Fatal(err)
    }

    fmt.Printf("Read %d bytes\n", result.Size)
}
```

### Executor Context

```go
package main

import (
    "github.com/hanzoai/zap-go/executor"
)

func main() {
    ctx := executor.NewContext(
        executor.WithCwd("."),
        executor.WithApprovalPolicy(executor.OnRequest),
        executor.WithSandboxPolicy(executor.WorkspaceWrite{
            WritableRoots: []string{},
            NetworkAccess: true,
        }),
    )

    dispatcher := executor.NewDispatcher()

    result, err := dispatcher.Execute(ctx, "read_file", map[string]interface{}{
        "path": "main.go",
    })
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(result.Content)
}
```

## Node.js / TypeScript

Modern async/await with full TypeScript support.

### Installation

```bash
npm install @hanzo/zap
# or
pnpm add @hanzo/zap
```

### Client Usage

```typescript
import { Client } from '@hanzo/zap';

async function main() {
    const client = await Client.connect('zap://localhost:9999');

    // List tools
    const tools = await client.listTools();
    for (const tool of tools) {
        console.log(`${tool.name}: ${tool.description}`);
    }

    // Call a tool
    const result = await client.callTool('read_file', {
        path: 'src/index.ts'
    });

    console.log(result.content);

    await client.close();
}

main().catch(console.error);
```

### TypeScript Types

```typescript
import { Client, Tool, ToolResult } from '@hanzo/zap';

interface ReadFileArgs {
    path: string;
    offset?: number;
    limit?: number;
}

interface ReadFileResult {
    content: string;
    mimeType: string;
    size: number;
}

async function readFile(
    client: Client,
    args: ReadFileArgs
): Promise<ReadFileResult> {
    const result = await client.callTool<ReadFileResult>('read_file', args);
    return result.content as ReadFileResult;
}
```

### MCP Bridge

```typescript
import { Gateway, McpServerConfig } from '@hanzo/zap';

async function main() {
    const gateway = new Gateway({
        listen: '0.0.0.0:9999',
        servers: [
            {
                name: 'filesystem',
                transport: {
                    type: 'stdio',
                    command: 'npx',
                    args: ['-y', '@modelcontextprotocol/server-filesystem', '/home'],
                },
            },
        ],
    });

    await gateway.serve();
}
```

## C/C++ (FFI)

Low-level bindings for performance-critical applications.

### Installation

```bash
# Build from source
git clone https://github.com/hanzoai/zap
cd zap
cargo build --release --features c-ffi

# Copy library
cp target/release/libzap.{so,dylib,dll} /usr/local/lib/
cp zap/include/zap.h /usr/local/include/
```

### Header File

```c
// zap.h
#ifndef ZAP_H
#define ZAP_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct ZapClient ZapClient;
typedef struct ZapToolResult ZapToolResult;

// Connection
ZapClient* zap_connect(const char* url);
void zap_close(ZapClient* client);

// Tool operations
char** zap_list_tools(ZapClient* client, size_t* count);
ZapToolResult* zap_call_tool(ZapClient* client, const char* name, const char* args_json);

// Result access
const char* zap_result_content(ZapToolResult* result);
const char* zap_result_error(ZapToolResult* result);
void zap_result_free(ZapToolResult* result);

// Error handling
const char* zap_last_error(void);

#ifdef __cplusplus
}
#endif

#endif // ZAP_H
```

### C Usage

```c
#include <stdio.h>
#include "zap.h"

int main() {
    // Connect
    ZapClient* client = zap_connect("zap://localhost:9999");
    if (!client) {
        fprintf(stderr, "Failed to connect: %s\n", zap_last_error());
        return 1;
    }

    // List tools
    size_t count;
    char** tools = zap_list_tools(client, &count);
    printf("Available tools (%zu):\n", count);
    for (size_t i = 0; i < count; i++) {
        printf("  - %s\n", tools[i]);
    }

    // Call tool
    ZapToolResult* result = zap_call_tool(client, "read_file",
        "{\"path\": \"README.md\"}");

    const char* error = zap_result_error(result);
    if (error) {
        fprintf(stderr, "Error: %s\n", error);
    } else {
        printf("Content:\n%s\n", zap_result_content(result));
    }

    // Cleanup
    zap_result_free(result);
    zap_close(client);
    return 0;
}
```

### C++ Wrapper

```cpp
#include <string>
#include <vector>
#include <stdexcept>
#include "zap.h"

namespace zap {

class Client {
public:
    explicit Client(const std::string& url) {
        client_ = zap_connect(url.c_str());
        if (!client_) {
            throw std::runtime_error(zap_last_error());
        }
    }

    ~Client() {
        if (client_) {
            zap_close(client_);
        }
    }

    std::vector<std::string> list_tools() {
        size_t count;
        char** tools = zap_list_tools(client_, &count);
        std::vector<std::string> result(tools, tools + count);
        return result;
    }

    std::string call_tool(const std::string& name, const std::string& args) {
        ZapToolResult* result = zap_call_tool(client_, name.c_str(), args.c_str());

        const char* error = zap_result_error(result);
        if (error) {
            std::string err(error);
            zap_result_free(result);
            throw std::runtime_error(err);
        }

        std::string content(zap_result_content(result));
        zap_result_free(result);
        return content;
    }

private:
    ZapClient* client_;
};

} // namespace zap
```

## Ruby

Ruby gem with idiomatic interface.

### Installation

```bash
gem install hanzo-zap
```

### Usage

```ruby
require 'hanzo/zap'

# Connect
client = Hanzo::Zap::Client.connect('zap://localhost:9999')

# List tools
client.list_tools.each do |tool|
  puts "#{tool.name}: #{tool.description}"
end

# Call tool
result = client.call_tool('read_file', path: 'README.md')
puts result.content

# Close
client.close
```

### Block Style

```ruby
Hanzo::Zap::Client.connect('zap://localhost:9999') do |client|
  result = client.call_tool('read_file', path: 'README.md')
  puts result.content
end  # Automatically closed
```

## Elixir

Elixir package with OTP integration.

### Installation

```elixir
# mix.exs
defp deps do
  [
    {:hanzo_zap, "~> 0.1"}
  ]
end
```

### Usage

```elixir
defmodule MyAgent do
  use GenServer
  alias HanzoZap.Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def init(_opts) do
    {:ok, client} = Client.connect("zap://localhost:9999")
    {:ok, %{client: client}}
  end

  def handle_call({:call_tool, name, args}, _from, %{client: client} = state) do
    result = Client.call_tool(client, name, args)
    {:reply, result, state}
  end

  def handle_cast(:close, %{client: client} = state) do
    Client.close(client)
    {:stop, :normal, state}
  end
end

# Usage
{:ok, _pid} = MyAgent.start_link([])
result = GenServer.call(MyAgent, {:call_tool, "read_file", %{path: "README.md"}})
IO.puts(result.content)
```

## MCP Bridge

All language bindings support bridging existing MCP servers:

### Architecture

```
+------------------+     +------------------+     +------------------+
|   ZAP Client     |     |   ZAP Gateway    |     |   MCP Server     |
|   (any language) |<--->|   (Rust)         |<--->|   (any language) |
+------------------+     +------------------+     +------------------+
        |                        |                        |
        | ZAP binary             | JSON-RPC translation   | JSON-RPC
        | protocol               |                        | (stdio/HTTP)
```

### Supported MCP Transports

| Transport | ZAP Config | Description |
|-----------|------------|-------------|
| Stdio | `type: "stdio"` | Subprocess with stdin/stdout |
| HTTP | `type: "http"` | HTTP/SSE endpoint |
| WebSocket | `type: "websocket"` | WebSocket endpoint |
| ZAP | `type: "zap"` | Native ZAP server |

### Example Configuration

```toml
[[servers]]
name = "filesystem"
[servers.transport]
type = "stdio"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/home"]

[[servers]]
name = "github"
[servers.transport]
type = "http"
url = "https://mcp.github.com"
[servers.auth]
type = "bearer"
token = "${GITHUB_TOKEN}"

[[servers]]
name = "internal-search"
[servers.transport]
type = "zap"
url = "zaps://search.internal:9999"
```

## Best Practices

### 1. Use Connection Pooling

```rust
// Rust - Use Arc for shared client
let client = Arc::new(Mutex::new(Client::connect(url).await?));

// Python - Use connection pool
pool = ConnectionPool("zap://localhost:9999", max_connections=10)
async with pool.acquire() as client:
    result = await client.call_tool(...)

// Go - Use sync.Pool
var clientPool = sync.Pool{
    New: func() interface{} {
        client, _ := zap.Connect(context.Background(), url)
        return client
    },
}
```

### 2. Handle Reconnection

```python
async def resilient_call(tool: str, args: dict, max_retries: int = 3):
    for attempt in range(max_retries):
        try:
            return await client.call_tool(tool, args)
        except ConnectionError:
            if attempt == max_retries - 1:
                raise
            await asyncio.sleep(2 ** attempt)  # Exponential backoff
            await client.reconnect()
```

### 3. Use Typed Interfaces

```typescript
// TypeScript - Define interfaces
interface GrepArgs {
    pattern: string;
    path: string;
    ignoreCase?: boolean;
}

interface GrepMatch {
    path: string;
    lineNumber: number;
    line: string;
}

const matches = await client.callTool<GrepMatch[]>('grep', {
    pattern: 'TODO',
    path: 'src/**/*.ts',
} as GrepArgs);
```

## Next Steps

- [Tool Reference](tools.md): Complete API documentation for all tools
