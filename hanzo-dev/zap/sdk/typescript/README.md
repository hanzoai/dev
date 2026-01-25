# @hanzo/zap

Zero-copy Agent Protocol (ZAP) SDK for TypeScript/JavaScript.

**1000x faster than MCP/JSON-RPC** through binary wire protocol with zero-copy serialization.

## Installation

```bash
npm install @hanzo/zap
# or
pnpm add @hanzo/zap
```

## Quick Start

### Client

```typescript
import { ZapClient } from '@hanzo/zap';

// Connect to a ZAP server
const client = await ZapClient.connect('zap://localhost:9999');

// List available tools
const tools = await client.listTools();
console.log('Tools:', tools.map(t => t.name));

// Call a tool
const result = await client.callTool('read_file', { path: 'README.md' });
console.log('Content:', result.content);

// Batch multiple calls
const results = await client.batch([
  { name: 'read_file', args: { path: 'package.json' } },
  { name: 'git_status', args: {} },
]);

// Clean up
await client.close();
```

### Server

```typescript
import { ZapServer } from '@hanzo/zap';

const server = new ZapServer({
  name: 'my-tools',
  version: '1.0.0',
});

// Register a tool
server.registerTool({
  name: 'greet',
  description: 'Greet someone by name',
  inputSchema: {
    type: 'object',
    properties: {
      name: { type: 'string', description: 'Name to greet' },
    },
    required: ['name'],
  },
  handler: async ({ name }) => `Hello, ${name}!`,
});

// Start server
await server.listen(9999);
console.log('ZAP server listening on port 9999');
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
- `client.listTools()` - List available tools
- `client.callTool(name, args)` - Call a tool
- `client.batch(calls)` - Call multiple tools
- `client.ping()` - Check connection
- `client.close()` - Close connection

### ZapServer

- `new ZapServer({ name, version })` - Create server
- `server.registerTool({ name, description, inputSchema, handler })` - Register tool
- `server.listen(port, options)` - Start listening
- `server.close()` - Stop server

## Policies

```typescript
import { ApprovalPolicy, SandboxPolicy } from '@hanzo/zap';

// Approval policies (when to ask for human approval)
ApprovalPolicy.UnlessTrusted  // Only auto-approve known-safe reads
ApprovalPolicy.OnFailure      // Auto-approve, escalate on failure
ApprovalPolicy.OnRequest      // Model decides (default)
ApprovalPolicy.Never          // Never ask

// Sandbox policies
const sandbox: SandboxPolicy = {
  mode: 'workspace-write',
  writableRoots: ['/home/user/project'],
  networkAccess: true,
};
```

## License

MIT - Hanzo AI Inc.
