import { describe, test, expect, beforeEach, afterEach, vi } from 'vitest';
import { MCPClient, MCPSession, MCPServerConfig } from '../src/lib/mcp-client';
import { EventEmitter } from 'events';
import * as child_process from 'child_process';

// Mock child_process
vi.mock('child_process');

describe('MCPClient', () => {
  let client: MCPClient;
  let mockProcess: any;

  beforeEach(() => {
    client = new MCPClient();
    
    // Mock spawn to return a fake process
    mockProcess = new EventEmitter();
    mockProcess.stdin = { write: vi.fn() };
    mockProcess.stdout = new EventEmitter();
    mockProcess.stderr = new EventEmitter();
    mockProcess.kill = vi.fn();

    vi.mocked(child_process.spawn).mockReturnValue(mockProcess as any);
  });

  afterEach(() => {
    vi.clearAllMocks();
  });

  describe('stdio transport', () => {
    test('should connect to MCP server via stdio', async () => {
      const config: MCPServerConfig = {
        name: 'test-server',
        transport: 'stdio',
        command: 'test-mcp-server',
        args: ['--test']
      };

      // Start connection in background
      const connectPromise = client.connect(config);

      // Simulate server sending initialization response
      setTimeout(() => {
        mockProcess.stdout.emit('data', JSON.stringify({
          jsonrpc: '2.0',
          id: 1,
          result: {
            protocolVersion: '1.0',
            serverInfo: { name: 'test-server', version: '1.0.0' }
          }
        }) + '\n');

        // Simulate tools list response
        mockProcess.stdout.emit('data', JSON.stringify({
          jsonrpc: '2.0',
          id: 2,
          result: {
            tools: [
              {
                name: 'test_tool',
                description: 'A test tool',
                parameters: {
                  type: 'object',
                  properties: {
                    input: { type: 'string' }
                  }
                }
              }
            ]
          }
        }) + '\n');
      }, 10);

      const session = await connectPromise;
      expect(session).toBeDefined();
      expect(session.tools).toHaveLength(1);
      expect(session.tools[0].name).toBe('test_tool');
    });

    test('should handle server errors', async () => {
      const config: MCPServerConfig = {
        name: 'error-server',
        transport: 'stdio',
        command: 'failing-server'
      };

      const connectPromise = client.connect(config);

      // Simulate process error
      setTimeout(() => {
        mockProcess.emit('error', new Error('Failed to start'));
      }, 10);

      await expect(connectPromise).rejects.toThrow('Failed to start');
    });
  });

  describe('tool calling', () => {
    test('should call tool on MCP server', async () => {
      const session: MCPSession = {
        serverName: 'test-server',
        tools: [{
          name: 'echo',
          description: 'Echo input',
          parameters: {
            type: 'object',
            properties: {
              message: { type: 'string' }
            }
          }
        }],
        prompts: [],
        resources: []
      };

      // Mock session in client
      (client as any).sessions.set('test-server', session);
      (client as any).processes.set('test-server', mockProcess);

      // Start tool call
      const callPromise = client.callTool('test-server', 'echo', { message: 'Hello' });

      // Simulate server response
      setTimeout(() => {
        // Find the request that was sent
        const writeCall = mockProcess.stdin.write.mock.calls[0];
        const request = JSON.parse(writeCall[0]);
        
        // Send response with same ID
        mockProcess.stdout.emit('data', JSON.stringify({
          jsonrpc: '2.0',
          id: request.id,
          result: {
            output: 'Echo: Hello'
          }
        }) + '\n');
      }, 10);

      const result = await callPromise;
      expect(result.output).toBe('Echo: Hello');
    });
  });

  describe('session management', () => {
    test('should list connected sessions', async () => {
      // Mock two sessions
      (client as any).sessions.set('server1', {
        serverName: 'server1',
        tools: [],
        prompts: [],
        resources: []
      });
      (client as any).sessions.set('server2', {
        serverName: 'server2',
        tools: [],
        prompts: [],
        resources: []
      });

      const sessions = client.listSessions();
      expect(sessions).toHaveLength(2);
      expect(sessions.map(s => s.serverName)).toContain('server1');
      expect(sessions.map(s => s.serverName)).toContain('server2');
    });

    test('should disconnect from server', async () => {
      const serverName = 'test-server';
      
      // Mock session and process
      (client as any).sessions.set(serverName, {
        serverName,
        tools: [],
        prompts: [],
        resources: []
      });
      (client as any).processes.set(serverName, mockProcess);

      await client.disconnect(serverName);

      expect(mockProcess.kill).toHaveBeenCalled();
      expect(client.listSessions()).toHaveLength(0);
    });
  });

  describe('error handling', () => {
    test('should handle JSON-RPC errors', async () => {
      const session: MCPSession = {
        serverName: 'test-server',
        tools: [{
          name: 'failing_tool',
          description: 'A tool that fails',
          parameters: { type: 'object' }
        }],
        prompts: [],
        resources: []
      };

      (client as any).sessions.set('test-server', session);
      (client as any).processes.set('test-server', mockProcess);

      const callPromise = client.callTool('test-server', 'failing_tool', {});

      setTimeout(() => {
        const writeCall = mockProcess.stdin.write.mock.calls[0];
        const request = JSON.parse(writeCall[0]);
        
        // Send error response
        mockProcess.stdout.emit('data', JSON.stringify({
          jsonrpc: '2.0',
          id: request.id,
          error: {
            code: -32601,
            message: 'Method not found'
          }
        }) + '\n');
      }, 10);

      await expect(callPromise).rejects.toThrow('Method not found');
    });

    test('should handle malformed responses', async () => {
      const config: MCPServerConfig = {
        name: 'malformed-server',
        transport: 'stdio',
        command: 'test-server'
      };

      const connectPromise = client.connect(config);

      setTimeout(() => {
        // Send malformed JSON
        mockProcess.stdout.emit('data', 'not valid json\n');
      }, 10);

      await expect(connectPromise).rejects.toThrow();
    });
  });
});