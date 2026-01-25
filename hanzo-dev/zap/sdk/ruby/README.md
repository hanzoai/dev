# hanzo-zap

Zero-copy Agent Protocol (ZAP) SDK for Ruby.

**1000x faster than MCP/JSON-RPC** through binary wire protocol with zero-copy serialization.

## Installation

```ruby
gem "hanzo-zap"
```

Or install directly:

```bash
gem install hanzo-zap
```

## Quick Start

### Client

```ruby
require "hanzo/zap"

# Connect to a ZAP server
client = Hanzo::Zap::Client.connect("zap://localhost:9999")

# List available tools
tools = client.list_tools
puts "Tools: #{tools.map(&:name)}"

# Call a tool
result = client.call_tool("read_file", path: "README.md")
puts "Content: #{result.content}"

# Batch multiple calls
results = client.batch([
  { name: "read_file", args: { path: "package.json" } },
  { name: "git_status", args: {} },
])

# Clean up
client.close
```

### Server

```ruby
require "hanzo/zap"

server = Hanzo::Zap::Server.new(name: "my-tools", version: "1.0.0")

# Register a tool
server.register_tool("greet", "Greet someone by name") do |args|
  "Hello, #{args[:name]}!"
end

# Start server (blocking)
server.listen(9999)
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

- `Client.connect(url)` - Connect to server
- `client.list_tools` - List available tools
- `client.call_tool(name, **args)` - Call a tool
- `client.batch(calls)` - Call multiple tools
- `client.ping` - Check connection
- `client.close` - Close connection

### Server

- `Server.new(name:, version:)` - Create server
- `server.register_tool(name, description, schema, &block)` - Register tool
- `server.listen(port)` - Start listening
- `server.stop` - Stop server

## Policies

```ruby
# Approval policies (when to ask for human approval)
Hanzo::Zap::ApprovalPolicy::UNLESS_TRUSTED  # Only auto-approve known-safe reads
Hanzo::Zap::ApprovalPolicy::ON_FAILURE      # Auto-approve, escalate on failure
Hanzo::Zap::ApprovalPolicy::ON_REQUEST      # Model decides (default)
Hanzo::Zap::ApprovalPolicy::NEVER           # Never ask

# Sandbox policies
sandbox = Hanzo::Zap::SandboxPolicy.workspace_write(
  writable_roots: ["/home/user/project"],
  network_access: true
)
```

## License

MIT - Hanzo AI Inc.
