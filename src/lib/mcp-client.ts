import { spawn, ChildProcess } from 'child_process';
import WebSocket from 'ws';
import { EventEmitter } from 'events';

export interface MCPTool {
  name: string;
  description: string;
  inputSchema: any;
}

export interface MCPSession {
  id: string;
  transport: 'stdio' | 'websocket';
  tools: MCPTool[];
  client: MCPClient;
}

export interface MCPServerConfig {
  name: string;
  command?: string;
  args?: string[];
  env?: Record<string, string>;
  url?: string; // For websocket connections
}

export class MCPClient extends EventEmitter {
  private sessions: Map<string, MCPSession> = new Map();
  private processes: Map<string, ChildProcess> = new Map();
  private websockets: Map<string, WebSocket> = new Map();

  async connect(config: MCPServerConfig): Promise<MCPSession> {
    const sessionId = this.generateSessionId();
    
    if (config.url) {
      // WebSocket connection
      return this.connectWebSocket(sessionId, config);
    } else if (config.command) {
      // Stdio connection
      return this.connectStdio(sessionId, config);
    } else {
      throw new Error('Either url or command must be specified');
    }
  }

  private async connectStdio(sessionId: string, config: MCPServerConfig): Promise<MCPSession> {
    return new Promise((resolve, reject) => {
      const proc = spawn(config.command!, config.args || [], {
        env: { ...process.env, ...config.env },
        stdio: ['pipe', 'pipe', 'pipe']
      });

      this.processes.set(sessionId, proc);

      let buffer = '';
      let tools: MCPTool[] = [];

      proc.stdout?.on('data', (data) => {
        buffer += data.toString();
        const lines = buffer.split('\n');
        buffer = lines.pop() || '';

        for (const line of lines) {
          if (line.trim()) {
            try {
              const msg = JSON.parse(line);
              if (msg.type === 'tools') {
                tools = msg.tools;
                const session: MCPSession = {
                  id: sessionId,
                  transport: 'stdio',
                  tools,
                  client: this
                };
                this.sessions.set(sessionId, session);
                resolve(session);
              } else if (msg.type === 'result') {
                this.emit(`result:${sessionId}`, msg);
              } else if (msg.type === 'error') {
                this.emit(`error:${sessionId}`, msg);
              }
            } catch (e) {
              // Ignore parse errors
            }
          }
        }
      });

      proc.stderr?.on('data', (data) => {
        console.error(`MCP stderr: ${data}`);
      });

      proc.on('error', (error) => {
        reject(error);
      });

      proc.on('exit', (code) => {
        this.processes.delete(sessionId);
        this.sessions.delete(sessionId);
        this.emit(`close:${sessionId}`, code);
      });

      // Request tools list
      proc.stdin?.write(JSON.stringify({ type: 'list_tools' }) + '\n');
    });
  }

  private async connectWebSocket(sessionId: string, config: MCPServerConfig): Promise<MCPSession> {
    return new Promise((resolve, reject) => {
      const ws = new WebSocket(config.url!);
      this.websockets.set(sessionId, ws);

      let tools: MCPTool[] = [];

      ws.on('open', () => {
        // Request tools list
        ws.send(JSON.stringify({ type: 'list_tools' }));
      });

      ws.on('message', (data) => {
        try {
          const msg = JSON.parse(data.toString());
          if (msg.type === 'tools') {
            tools = msg.tools;
            const session: MCPSession = {
              id: sessionId,
              transport: 'websocket',
              tools,
              client: this
            };
            this.sessions.set(sessionId, session);
            resolve(session);
          } else if (msg.type === 'result') {
            this.emit(`result:${sessionId}`, msg);
          } else if (msg.type === 'error') {
            this.emit(`error:${sessionId}`, msg);
          }
        } catch (e) {
          console.error('Failed to parse MCP message:', e);
        }
      });

      ws.on('error', (error) => {
        reject(error);
      });

      ws.on('close', () => {
        this.websockets.delete(sessionId);
        this.sessions.delete(sessionId);
        this.emit(`close:${sessionId}`);
      });
    });
  }

  async callTool(sessionId: string, toolName: string, args: any): Promise<any> {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error(`Session ${sessionId} not found`);
    }

    const requestId = this.generateRequestId();
    const request = {
      type: 'tool_call',
      id: requestId,
      tool: toolName,
      arguments: args
    };

    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        reject(new Error('Tool call timeout'));
      }, 30000);

      const resultHandler = (msg: any) => {
        if (msg.id === requestId) {
          clearTimeout(timeout);
          this.removeListener(`result:${sessionId}`, resultHandler);
          this.removeListener(`error:${sessionId}`, errorHandler);
          resolve(msg.result);
        }
      };

      const errorHandler = (msg: any) => {
        if (msg.id === requestId) {
          clearTimeout(timeout);
          this.removeListener(`result:${sessionId}`, resultHandler);
          this.removeListener(`error:${sessionId}`, errorHandler);
          reject(new Error(msg.error));
        }
      };

      this.on(`result:${sessionId}`, resultHandler);
      this.on(`error:${sessionId}`, errorHandler);

      if (session.transport === 'stdio') {
        const proc = this.processes.get(sessionId);
        proc?.stdin?.write(JSON.stringify(request) + '\n');
      } else {
        const ws = this.websockets.get(sessionId);
        ws?.send(JSON.stringify(request));
      }
    });
  }

  async disconnect(sessionId: string): Promise<void> {
    const session = this.sessions.get(sessionId);
    if (!session) return;

    if (session.transport === 'stdio') {
      const proc = this.processes.get(sessionId);
      proc?.kill();
    } else {
      const ws = this.websockets.get(sessionId);
      ws?.close();
    }

    this.sessions.delete(sessionId);
  }

  getSession(sessionId: string): MCPSession | undefined {
    return this.sessions.get(sessionId);
  }

  getAllSessions(): MCPSession[] {
    return Array.from(this.sessions.values());
  }

  private generateSessionId(): string {
    return `session-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  }

  private generateRequestId(): string {
    return `req-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  }
}

// Default MCP server configurations
export const DEFAULT_MCP_SERVERS: MCPServerConfig[] = [
  {
    name: 'filesystem',
    command: 'npx',
    args: ['@modelcontextprotocol/server-filesystem'],
    env: {
      MCP_ALLOWED_PATHS: process.cwd()
    }
  },
  {
    name: 'git',
    command: 'npx',
    args: ['@modelcontextprotocol/server-git'],
    env: {
      MCP_GIT_REPO: process.cwd()
    }
  },
  {
    name: 'search',
    command: 'npx',
    args: ['@modelcontextprotocol/server-search']
  }
];