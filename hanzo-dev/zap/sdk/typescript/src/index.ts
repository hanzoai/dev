/**
 * @hanzo/zap - Zero-copy Agent Protocol SDK for TypeScript
 *
 * 1000x faster than MCP/JSON-RPC through binary wire protocol.
 *
 * @example
 * ```typescript
 * import { ZapClient } from '@hanzo/zap';
 *
 * const client = await ZapClient.connect('zap://localhost:9999');
 * const tools = await client.listTools();
 * const result = await client.callTool('read_file', { path: 'README.md' });
 * await client.close();
 * ```
 */

export { ZapClient } from './client.js';
export { ZapServer, type ToolHandler } from './server.js';
export {
  ApprovalPolicy,
  SandboxPolicy,
  MessageType,
  type ToolCall,
  type ToolResult,
  type Tool,
  type ServerInfo,
  type ClientInfo,
} from './types.js';
