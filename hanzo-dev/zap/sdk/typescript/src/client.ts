/**
 * ZAP Client implementation.
 */

import { createConnection, Socket } from 'node:net';
import { connect as tlsConnect, TLSSocket } from 'node:tls';
import {
  MessageType,
  type Tool,
  type ToolCall,
  type ToolResult,
  type ServerInfo,
  type ClientInfo,
  type Resource,
  type Prompt,
} from './types.js';

const MAX_MESSAGE_SIZE = 16 * 1024 * 1024; // 16MB

/**
 * ZAP Client for connecting to ZAP servers.
 *
 * @example
 * ```typescript
 * const client = await ZapClient.connect('zap://localhost:9999');
 * const tools = await client.listTools();
 * const result = await client.callTool('read_file', { path: 'README.md' });
 * await client.close();
 * ```
 */
export class ZapClient {
  private socket: Socket | TLSSocket;
  private serverInfo?: ServerInfo;
  private requestId = 0;
  private pendingRequests = new Map<
    number,
    { resolve: (value: unknown) => void; reject: (error: Error) => void }
  >();
  private receiveBuffer = Buffer.alloc(0);

  private constructor(socket: Socket | TLSSocket) {
    this.socket = socket;
    this.socket.on('data', (data) => this.onData(data));
    this.socket.on('error', (err) => this.onError(err));
  }

  /**
   * Connect to a ZAP server.
   *
   * @param url - Server URL (zap:// or zaps:// for TLS)
   */
  static async connect(url: string): Promise<ZapClient> {
    const parsed = new URL(url);
    const useTls = parsed.protocol === 'zaps:';
    const host = parsed.hostname;
    const port = parseInt(parsed.port || '9999', 10);

    const socket = await new Promise<Socket | TLSSocket>((resolve, reject) => {
      const s = useTls
        ? tlsConnect({ host, port, rejectUnauthorized: false }, () => resolve(s))
        : createConnection({ host, port }, () => resolve(s));
      s.on('error', reject);
    });

    const client = new ZapClient(socket);
    await client.handshake();
    return client;
  }

  /**
   * Perform protocol handshake.
   */
  private async handshake(): Promise<void> {
    const clientInfo: ClientInfo = {
      name: '@hanzo/zap',
      version: '0.6.0',
    };

    this.send(MessageType.Init, JSON.stringify(clientInfo));
    const response = await this.recv<ServerInfo>(MessageType.InitAck);
    this.serverInfo = response;
  }

  /**
   * Get server info from handshake.
   */
  getServerInfo(): ServerInfo | undefined {
    return this.serverInfo;
  }

  /**
   * List available tools.
   */
  async listTools(): Promise<Tool[]> {
    this.send(MessageType.ListTools, '');
    return this.recv<Tool[]>(MessageType.ListToolsResponse);
  }

  /**
   * Call a tool by name.
   */
  async callTool(name: string, args: Record<string, unknown>): Promise<ToolResult> {
    const call: ToolCall = {
      id: `req-${++this.requestId}`,
      name,
      args,
    };
    this.send(MessageType.CallTool, JSON.stringify(call));
    return this.recv<ToolResult>(MessageType.CallToolResponse);
  }

  /**
   * Call multiple tools in a batch.
   */
  async batch(calls: Array<{ name: string; args: Record<string, unknown> }>): Promise<ToolResult[]> {
    return Promise.all(calls.map((c) => this.callTool(c.name, c.args)));
  }

  /**
   * List available resources.
   */
  async listResources(): Promise<Resource[]> {
    this.send(MessageType.ListResources, '');
    return this.recv<Resource[]>(MessageType.ListResourcesResponse);
  }

  /**
   * Read a resource by URI.
   */
  async readResource(uri: string): Promise<{ uri: string; content: string; mimeType?: string }> {
    this.send(MessageType.ReadResource, JSON.stringify({ uri }));
    return this.recv(MessageType.ReadResourceResponse);
  }

  /**
   * List available prompts.
   */
  async listPrompts(): Promise<Prompt[]> {
    this.send(MessageType.ListPrompts, '');
    return this.recv<Prompt[]>(MessageType.ListPromptsResponse);
  }

  /**
   * Send ping to check connection.
   */
  async ping(): Promise<void> {
    this.send(MessageType.Ping, '');
    await this.recv(MessageType.Pong);
  }

  /**
   * Close the connection.
   */
  async close(): Promise<void> {
    return new Promise((resolve) => {
      this.socket.end(() => resolve());
    });
  }

  /**
   * Send a message with ZAP wire format.
   */
  private send(type: MessageType, payload: string): void {
    const payloadBuf = Buffer.from(payload, 'utf-8');
    const totalLen = 1 + payloadBuf.length;

    const header = Buffer.alloc(5);
    header.writeUInt32LE(totalLen, 0);
    header.writeUInt8(type, 4);

    this.socket.write(Buffer.concat([header, payloadBuf]));
  }

  /**
   * Receive and parse a message.
   */
  private recv<T>(expectedType: MessageType): Promise<T> {
    return new Promise((resolve, reject) => {
      const id = ++this.requestId;
      this.pendingRequests.set(id, {
        resolve: (value) => {
          this.pendingRequests.delete(id);
          resolve(value as T);
        },
        reject: (err) => {
          this.pendingRequests.delete(id);
          reject(err);
        },
      });

      // Process any buffered data
      this.processBuffer(expectedType, id);
    });
  }

  /**
   * Handle incoming data.
   */
  private onData(data: Buffer): void {
    this.receiveBuffer = Buffer.concat([this.receiveBuffer, data]);

    // Process all complete messages
    while (this.receiveBuffer.length >= 5) {
      const totalLen = this.receiveBuffer.readUInt32LE(0);

      if (totalLen > MAX_MESSAGE_SIZE) {
        this.onError(new Error(`Message too large: ${totalLen}`));
        return;
      }

      if (this.receiveBuffer.length < 4 + totalLen) {
        break; // Wait for more data
      }

      const msgType = this.receiveBuffer.readUInt8(4);
      const payload = this.receiveBuffer.subarray(5, 4 + totalLen);
      this.receiveBuffer = this.receiveBuffer.subarray(4 + totalLen);

      // Resolve pending request
      const pending = this.pendingRequests.entries().next().value;
      if (pending) {
        const [, handler] = pending;
        if (msgType === MessageType.Error) {
          const error = JSON.parse(payload.toString('utf-8'));
          handler.reject(new Error(error.message || 'Server error'));
        } else {
          const result = payload.length > 0 ? JSON.parse(payload.toString('utf-8')) : null;
          handler.resolve(result);
        }
      }
    }
  }

  /**
   * Process buffered data for a specific request.
   */
  private processBuffer(_expectedType: MessageType, _requestId: number): void {
    // Data will be processed in onData
  }

  /**
   * Handle socket errors.
   */
  private onError(err: Error): void {
    for (const [, handler] of this.pendingRequests) {
      handler.reject(err);
    }
    this.pendingRequests.clear();
  }
}
