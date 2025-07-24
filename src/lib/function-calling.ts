import { FileEditor, EditCommand } from './editor';
import { MCPClient, MCPSession } from './mcp-client';
import { spawn } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';

export interface Tool {
  name: string;
  description: string;
  parameters: any; // JSON Schema
  handler: (args: any) => Promise<any>;
}

export interface FunctionCall {
  id: string;
  name: string;
  arguments: any;
}

export interface ToolCallResult {
  id: string;
  result?: any;
  error?: string;
}

export class FunctionCallingSystem {
  private tools: Map<string, Tool> = new Map();
  private fileEditor: FileEditor;
  private mcpClient: MCPClient;
  private mcpSessions: Map<string, MCPSession> = new Map();

  constructor() {
    this.fileEditor = new FileEditor();
    this.mcpClient = new MCPClient();
    this.registerBuiltinTools();
  }

  private registerBuiltinTools() {
    // File editing tools
    this.registerTool({
      name: 'view_file',
      description: 'View contents of a file with optional line range',
      parameters: {
        type: 'object',
        properties: {
          path: { type: 'string', description: 'File path' },
          start_line: { type: 'number', description: 'Start line (optional)' },
          end_line: { type: 'number', description: 'End line (optional)' }
        },
        required: ['path']
      },
      handler: async (args) => {
        const result = await this.fileEditor.execute({
          command: 'view',
          path: args.path,
          startLine: args.start_line,
          endLine: args.end_line
        });
        return result;
      }
    });

    this.registerTool({
      name: 'create_file',
      description: 'Create a new file with content',
      parameters: {
        type: 'object',
        properties: {
          path: { type: 'string', description: 'File path' },
          content: { type: 'string', description: 'File content' }
        },
        required: ['path', 'content']
      },
      handler: async (args) => {
        const result = await this.fileEditor.execute({
          command: 'create',
          path: args.path,
          content: args.content
        });
        return result;
      }
    });

    this.registerTool({
      name: 'str_replace',
      description: 'Replace exact string match in file',
      parameters: {
        type: 'object',
        properties: {
          path: { type: 'string', description: 'File path' },
          old_str: { type: 'string', description: 'String to replace' },
          new_str: { type: 'string', description: 'Replacement string' }
        },
        required: ['path', 'old_str', 'new_str']
      },
      handler: async (args) => {
        const result = await this.fileEditor.execute({
          command: 'str_replace',
          path: args.path,
          oldStr: args.old_str,
          newStr: args.new_str
        });
        return result;
      }
    });

    // Command execution
    this.registerTool({
      name: 'run_command',
      description: 'Execute a shell command',
      parameters: {
        type: 'object',
        properties: {
          command: { type: 'string', description: 'Command to execute' },
          cwd: { type: 'string', description: 'Working directory (optional)' },
          timeout: { type: 'number', description: 'Timeout in ms (optional)' }
        },
        required: ['command']
      },
      handler: async (args) => {
        return this.executeCommand(args.command, args.cwd, args.timeout);
      }
    });

    // File system tools
    this.registerTool({
      name: 'list_directory',
      description: 'List contents of a directory',
      parameters: {
        type: 'object',
        properties: {
          path: { type: 'string', description: 'Directory path' }
        },
        required: ['path']
      },
      handler: async (args) => {
        try {
          const files = fs.readdirSync(args.path);
          const details = files.map(file => {
            const fullPath = path.join(args.path, file);
            const stats = fs.statSync(fullPath);
            return {
              name: file,
              type: stats.isDirectory() ? 'directory' : 'file',
              size: stats.size,
              modified: stats.mtime
            };
          });
          return { success: true, files: details };
        } catch (error) {
          return { success: false, error: error.toString() };
        }
      }
    });

    this.registerTool({
      name: 'search_files',
      description: 'Search for files matching a pattern',
      parameters: {
        type: 'object',
        properties: {
          pattern: { type: 'string', description: 'Search pattern' },
          path: { type: 'string', description: 'Directory to search in' },
          regex: { type: 'boolean', description: 'Use regex matching' }
        },
        required: ['pattern']
      },
      handler: async (args) => {
        const searchPath = args.path || process.cwd();
        return this.searchFiles(searchPath, args.pattern, args.regex);
      }
    });
  }

  registerTool(tool: Tool): void {
    this.tools.set(tool.name, tool);
  }

  registerFunction(
    name: string, 
    handler: (args: any) => Promise<any>,
    description: string,
    parameters: any
  ): void {
    this.registerTool({
      name,
      description,
      parameters,
      handler
    });
  }

  async registerMCPServer(name: string, session: MCPSession): Promise<void> {
    this.mcpSessions.set(name, session);
    
    // Register MCP tools as function calling tools
    for (const tool of session.tools) {
      this.registerTool({
        name: `${name}.${tool.name}`,
        description: tool.description,
        parameters: tool.inputSchema,
        handler: async (args) => {
          return this.mcpClient.callTool(session.id, tool.name, args);
        }
      });
    }
  }

  async callFunction(call: FunctionCall): Promise<ToolCallResult> {
    const tool = this.tools.get(call.name);
    if (!tool) {
      return {
        id: call.id,
        error: `Tool '${call.name}' not found`
      };
    }

    try {
      const result = await tool.handler(call.arguments);
      return {
        id: call.id,
        result
      };
    } catch (error) {
      return {
        id: call.id,
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }

  async callFunctions(calls: FunctionCall[]): Promise<ToolCallResult[]> {
    return Promise.all(calls.map(call => this.callFunction(call)));
  }

  getAvailableTools(): Tool[] {
    return Array.from(this.tools.values());
  }

  getToolSchema(name: string): any {
    const tool = this.tools.get(name);
    if (!tool) return null;
    
    return {
      type: 'function',
      function: {
        name: tool.name,
        description: tool.description,
        parameters: tool.parameters
      }
    };
  }

  getAllToolSchemas(): any[] {
    return this.getAvailableTools().map(tool => this.getToolSchema(tool.name));
  }

  private async executeCommand(command: string, cwd?: string, timeout?: number): Promise<any> {
    return new Promise((resolve, reject) => {
      const options: any = {
        shell: true,
        cwd: cwd || process.cwd()
      };

      const proc = spawn(command, [], options);
      let stdout = '';
      let stderr = '';

      const timer = timeout ? setTimeout(() => {
        proc.kill();
        reject(new Error(`Command timeout after ${timeout}ms`));
      }, timeout) : null;

      proc.stdout?.on('data', (data) => {
        stdout += data.toString();
      });

      proc.stderr?.on('data', (data) => {
        stderr += data.toString();
      });

      proc.on('close', (code) => {
        if (timer) clearTimeout(timer);
        
        resolve({
          success: code === 0,
          code,
          stdout,
          stderr
        });
      });

      proc.on('error', (error) => {
        if (timer) clearTimeout(timer);
        reject(error);
      });
    });
  }

  private async searchFiles(searchPath: string, pattern: string, useRegex: boolean = false): Promise<any> {
    const results: string[] = [];
    const regex = useRegex ? new RegExp(pattern) : null;

    const walkDir = (dir: string) => {
      try {
        const files = fs.readdirSync(dir);
        for (const file of files) {
          const fullPath = path.join(dir, file);
          const stats = fs.statSync(fullPath);
          
          if (stats.isDirectory() && !file.startsWith('.') && file !== 'node_modules') {
            walkDir(fullPath);
          } else if (stats.isFile()) {
            if (regex ? regex.test(file) : file.includes(pattern)) {
              results.push(fullPath);
            }
          }
        }
      } catch (error) {
        // Ignore permission errors
      }
    };

    walkDir(searchPath);
    
    return {
      success: true,
      matches: results.slice(0, 100), // Limit results
      total: results.length
    };
  }
}