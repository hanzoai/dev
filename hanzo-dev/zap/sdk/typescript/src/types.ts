/**
 * ZAP type definitions matching the Rust implementation.
 */

/**
 * Approval policy for tool execution (from hanzo-protocol).
 */
export enum ApprovalPolicy {
  /** Only auto-approve known-safe read operations */
  UnlessTrusted = 'unless-trusted',
  /** Auto-approve, escalate on failure */
  OnFailure = 'on-failure',
  /** Model decides when to ask (default) */
  OnRequest = 'on-request',
  /** Never ask, return failures to model */
  Never = 'never',
}

/**
 * Sandbox policy for tool execution (from hanzo-protocol).
 */
export type SandboxPolicy =
  | { mode: 'danger-full-access' }
  | { mode: 'read-only' }
  | {
      mode: 'workspace-write';
      writableRoots?: string[];
      networkAccess?: boolean;
      allowGitWrites?: boolean;
    };

/**
 * Wire protocol message types.
 */
export enum MessageType {
  // Handshake
  Init = 0x01,
  InitAck = 0x02,

  // Tools
  ListTools = 0x10,
  ListToolsResponse = 0x11,
  CallTool = 0x12,
  CallToolResponse = 0x13,

  // Resources
  ListResources = 0x20,
  ListResourcesResponse = 0x21,
  ReadResource = 0x22,
  ReadResourceResponse = 0x23,

  // Prompts
  ListPrompts = 0x30,
  ListPromptsResponse = 0x31,
  GetPrompt = 0x32,
  GetPromptResponse = 0x33,

  // Control
  Ping = 0xf0,
  Pong = 0xf1,
  Error = 0xff,
}

/**
 * Tool definition.
 */
export interface Tool {
  name: string;
  description: string;
  inputSchema: Record<string, unknown>;
}

/**
 * Tool call request.
 */
export interface ToolCall {
  id: string;
  name: string;
  args: Record<string, unknown>;
  metadata?: Record<string, unknown>;
}

/**
 * Tool execution result.
 */
export interface ToolResult {
  id: string;
  content: unknown;
  error?: string;
  metadata?: Record<string, unknown>;
}

/**
 * Server capabilities and info.
 */
export interface ServerInfo {
  name: string;
  version: string;
  capabilities: {
    tools: boolean;
    resources: boolean;
    prompts: boolean;
  };
}

/**
 * Client info sent during handshake.
 */
export interface ClientInfo {
  name: string;
  version: string;
}

/**
 * Resource definition.
 */
export interface Resource {
  uri: string;
  name: string;
  description?: string;
  mimeType?: string;
}

/**
 * Prompt definition.
 */
export interface Prompt {
  name: string;
  description?: string;
  arguments?: Array<{
    name: string;
    description?: string;
    required?: boolean;
  }>;
}
