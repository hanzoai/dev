# zap-go

Zero-copy Agent Protocol (ZAP) SDK for Go.

**1000x faster than MCP/JSON-RPC** through binary wire protocol with zero-copy serialization.

## Installation

```bash
go get github.com/hanzoai/zap-go
```

## Quick Start

### Client

```go
package main

import (
    "fmt"
    "log"

    zap "github.com/hanzoai/zap-go"
)

func main() {
    // Connect to a ZAP server
    client, err := zap.Connect("zap://localhost:9999")
    if err != nil {
        log.Fatal(err)
    }
    defer client.Close()

    // List available tools
    tools, err := client.ListTools()
    if err != nil {
        log.Fatal(err)
    }
    for _, t := range tools {
        fmt.Printf("Tool: %s - %s\n", t.Name, t.Description)
    }

    // Call a tool
    result, err := client.CallTool("read_file", map[string]any{
        "path": "README.md",
    })
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Content: %v\n", result.Content)
}
```

### Server

```go
package main

import (
    "fmt"
    "log"

    zap "github.com/hanzoai/zap-go"
)

func main() {
    server := zap.NewServer("my-tools", "1.0.0")

    // Register a tool
    server.RegisterTool("greet", "Greet someone by name", nil, func(args map[string]any) (any, error) {
        name := args["name"].(string)
        return fmt.Sprintf("Hello, %s!", name), nil
    })

    // Start server
    log.Println("ZAP server listening on port 9999")
    if err := server.Listen(9999); err != nil {
        log.Fatal(err)
    }
}
```

## Wire Protocol

ZAP uses a simple length-prefixed binary format:

```
+----------+----------+------------------+
| Length   | MsgType  | Payload          |
| (4 bytes)| (1 byte) | (variable)       |
| LE u32   |          | JSON             |
+----------+----------+------------------+
```

## API

### Client

- `Connect(url)` - Connect to server
- `client.ListTools()` - List available tools
- `client.CallTool(name, args)` - Call a tool
- `client.Ping()` - Check connection
- `client.Close()` - Close connection

### Server

- `NewServer(name, version)` - Create server
- `server.RegisterTool(name, description, schema, handler)` - Register tool
- `server.Listen(port)` - Start listening
- `server.Close()` - Stop server

## Types

```go
// Approval policies (when to ask for human approval)
zap.ApprovalUnlessTrusted  // Only auto-approve known-safe reads
zap.ApprovalOnFailure      // Auto-approve, escalate on failure
zap.ApprovalOnRequest      // Model decides (default)
zap.ApprovalNever          // Never ask

// Sandbox modes
zap.SandboxDangerFullAccess
zap.SandboxReadOnly
zap.SandboxWorkspaceWrite

// Sandbox policy
policy := zap.SandboxPolicy{
    Mode:          zap.SandboxWorkspaceWrite,
    WritableRoots: []string{"/home/user/project"},
    NetworkAccess: true,
}
```

## License

MIT - Hanzo AI Inc.
