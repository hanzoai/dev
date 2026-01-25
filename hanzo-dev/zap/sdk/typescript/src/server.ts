/**
 * ZAP Server implementation.
 */

import { createServer, Server, Socket } from 'node:net';
import { createServer as createTlsServer, TlsOptions } from 'node:tls';
import {
  MessageType,
  type Tool,
  type ToolResult,
  type ServerInfo,
  type ClientInfo,
} from './types.js';

/**
 * Tool handler function type.
 */
export type ToolHandler = (
  name: string,
  args: Record<string, unknown>
) => Promise<unknown> | unknown;

/**
 * ZAP Server for hosting tools.
 *
 * @example
 * ```typescript
 * const server = new ZapServer({
 *   name: 'my-tools',
 *   version: '1.0.0',
 * });
 *
 * server.registerTool({
 *   name: 'greet',
 *   description: 'Greet someone',
 *   inputSchema: { type: 'object', properties: { name: { type: 'string' } } },
 *   handler: async ({ name }) => `Hello, ${name}!`,
 * });
 *
 * await server.listen(9999);
 * ```
 */
export class ZapServer {
  private server?: Server;
  private tools = new Map<string, { tool: Tool; handler: ToolHandler }>();
  private info: ServerInfo;
  private clients = new Set<Socket>();

  constructor(options: { name: string; version: string }) {
    this.info = {
      name: options.name,
      version: options.version,
      capabilities: {
        tools: true,
        resources: false,
        prompts: false,
      },
    };
  }

  /**
   * Register a tool.
   */
  registerTool(options: {
    name: string;
    description: string;
    inputSchema: Record<string, unknown>;
    handler: ToolHandler;
  }): void {
    const tool: Tool = {
      name: options.name,
      description: options.description,
      inputSchema: options.inputSchema,
    };
    this.tools.set(options.name, { tool, handler: options.handler });
  }

  /**
   * Start listening for connections.
   */
  async listen(port: number, options?: { host?: string; tls?: TlsOptions }): Promise<void> {
    return new Promise((resolve) => {
      const handler = (socket: Socket) => this.handleConnection(socket);

      this.server = options?.tls
        ? createTlsServer(options.tls, handler)
        : createServer(handler);

      this.server.listen(port, options?.host || '0.0.0.0', () => {
        resolve();
      });
    });
  }

  /**
   * Stop the server.
   */
  async close(): Promise<void> {
    for (const client of this.clients) {
      client.destroy();
    }
    this.clients.clear();

    return new Promise((resolve) => {
      if (this.server) {
        this.server.close(() => resolve());
      } else {
        resolve();
      }
    });
  }

  /**
   * Handle a new client connection.
   */
  private handleConnection(socket: Socket): void {
    this.clients.add(socket);
    let receiveBuffer = Buffer.alloc(0);

    socket.on('data', async (data) => {
      receiveBuffer = Buffer.concat([receiveBuffer, data]);

      while (receiveBuffer.length >= 5) {
        const totalLen = receiveBuffer.readUInt32LE(0);
        if (receiveBuffer.length < 4 + totalLen) break;

        const msgType = receiveBuffer.readUInt8(4) as MessageType;
        const payload = receiveBuffer.subarray(5, 4 + totalLen);
        receiveBuffer = receiveBuffer.subarray(4 + totalLen);

        await this.handleMessage(socket, msgType, payload);
      }
    });

    socket.on('close', () => {
      this.clients.delete(socket);
    });

    socket.on('error', () => {
      this.clients.delete(socket);
    });
  }

  /**
   * Handle a message from a client.
   */
  private async handleMessage(
    socket: Socket,
    type: MessageType,
    payload: Buffer
  ): Promise<void> {
    try {
      switch (type) {
        case MessageType.Init: {
          const _clientInfo: ClientInfo = JSON.parse(payload.toString('utf-8'));
          this.send(socket, MessageType.InitAck, JSON.stringify(this.info));
          break;
        }

        case MessageType.ListTools: {
          const tools = Array.from(this.tools.values()).map((t) => t.tool);
          this.send(socket, MessageType.ListToolsResponse, JSON.stringify(tools));
          break;
        }

        case MessageType.CallTool: {
          const call = JSON.parse(payload.toString('utf-8'));
          const entry = this.tools.get(call.name);

          if (!entry) {
            const result: ToolResult = {
              id: call.id,
              content: null,
              error: `Unknown tool: ${call.name}`,
            };
            this.send(socket, MessageType.CallToolResponse, JSON.stringify(result));
            return;
          }

          try {
            const content = await entry.handler(call.name, call.args);
            const result: ToolResult = { id: call.id, content };
            this.send(socket, MessageType.CallToolResponse, JSON.stringify(result));
          } catch (err) {
            const result: ToolResult = {
              id: call.id,
              content: null,
              error: err instanceof Error ? err.message : String(err),
            };
            this.send(socket, MessageType.CallToolResponse, JSON.stringify(result));
          }
          break;
        }

        case MessageType.Ping: {
          this.send(socket, MessageType.Pong, '');
          break;
        }

        default: {
          this.send(
            socket,
            MessageType.Error,
            JSON.stringify({ message: `Unknown message type: ${type}` })
          );
        }
      }
    } catch (err) {
      this.send(
        socket,
        MessageType.Error,
        JSON.stringify({ message: err instanceof Error ? err.message : String(err) })
      );
    }
  }

  /**
   * Send a message to a client.
   */
  private send(socket: Socket, type: MessageType, payload: string): void {
    const payloadBuf = Buffer.from(payload, 'utf-8');
    const totalLen = 1 + payloadBuf.length;

    const header = Buffer.alloc(5);
    header.writeUInt32LE(totalLen, 0);
    header.writeUInt8(type, 4);

    socket.write(Buffer.concat([header, payloadBuf]));
  }
}
