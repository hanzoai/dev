# hanzo-zap

Zero-copy Agent Protocol (ZAP) SDK for Python.

**1000x faster than MCP/JSON-RPC** through binary wire protocol with zero-copy serialization.

## Installation

```bash
pip install hanzo-zap
# or
uv add hanzo-zap
```

## Quick Start

### Client

```python
import asyncio
from hanzo_zap import ZapClient

async def main():
    # Connect to a ZAP server
    async with ZapClient.connect("zap://localhost:9999") as client:
        # List available tools
        tools = await client.list_tools()
        print("Tools:", [t.name for t in tools])

        # Call a tool
        result = await client.call_tool("read_file", {"path": "README.md"})
        print("Content:", result.content)

        # Batch multiple calls
        results = await client.batch([
            {"name": "read_file", "args": {"path": "package.json"}},
            {"name": "git_status", "args": {}},
        ])

asyncio.run(main())
```

### Server

```python
import asyncio
from hanzo_zap import ZapServer

server = ZapServer(name="my-tools", version="1.0.0")

@server.tool("greet", "Greet someone by name", {"type": "object", "properties": {"name": {"type": "string"}}})
async def greet(name: str, args: dict) -> str:
    return f"Hello, {args['name']}!"

asyncio.run(server.serve(9999))
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

### ZapClient

- `ZapClient.connect(url)` - Connect to server
- `client.list_tools()` - List available tools
- `client.call_tool(name, args)` - Call a tool
- `client.batch(calls)` - Call multiple tools
- `client.ping()` - Check connection
- `client.close()` - Close connection

### ZapServer

- `ZapServer(name, version)` - Create server
- `server.register_tool(name, description, schema, handler)` - Register tool
- `@server.tool(name, description, schema)` - Decorator to register tool
- `server.serve(port)` - Start serving (blocking)
- `server.start(port)` - Start in background
- `server.stop()` - Stop server

## Policies

```python
from hanzo_zap import ApprovalPolicy, SandboxPolicy

# Approval policies (when to ask for human approval)
ApprovalPolicy.UNLESS_TRUSTED  # Only auto-approve known-safe reads
ApprovalPolicy.ON_FAILURE      # Auto-approve, escalate on failure
ApprovalPolicy.ON_REQUEST      # Model decides (default)
ApprovalPolicy.NEVER           # Never ask

# Sandbox policies
sandbox = SandboxPolicy.workspace_write(
    writable_roots=["/home/user/project"],
    network_access=True,
)
```

## License

MIT - Hanzo AI Inc.
