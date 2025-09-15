/**
 * MCP Bridge - Bridge between TypeScript and Rust MCP implementation
 */

import { Tool, ToolParams } from './types';

export class McpBridge {
  private rustBridge: any = null;
  private fallbackMode: boolean = false;

  constructor() {
    this.initializeBridge();
  }

  /**
   * Initialize the bridge to Rust or fallback to JS
   */
  private initializeBridge(): void {
    try {
      // Try to load the native Rust bridge
      // @ts-ignore
      this.rustBridge = require('../native/mcp.node');
      console.log('MCP: Using native Rust bridge');
    } catch (error) {
      // Fallback to JavaScript implementation
      console.log('MCP: Using JavaScript fallback');
      this.fallbackMode = true;
    }
  }

  /**
   * List all available tools
   */
  listTools(): Tool[] {
    if (!this.fallbackMode && this.rustBridge) {
      return this.rustBridge.listTools();
    }
    
    // JavaScript fallback - return basic tool definitions
    return this.getFallbackTools();
  }

  /**
   * Execute a tool
   */
  async execute(name: string, params: ToolParams): Promise<any> {
    if (!this.fallbackMode && this.rustBridge) {
      return this.rustBridge.execute(name, JSON.stringify(params));
    }
    
    // JavaScript fallback implementation
    return this.executeFallback(name, params);
  }

  /**
   * Get fallback tool definitions
   */
  private getFallbackTools(): Tool[] {
    return [
      {
        name: 'read_file',
        description: 'Read contents of a file',
        category: 'file',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'File path to read' }
          },
          required: ['path']
        }
      },
      {
        name: 'write_file',
        description: 'Write content to a file',
        category: 'file',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'File path to write' },
            content: { type: 'string', description: 'Content to write' }
          },
          required: ['path', 'content']
        }
      },
      {
        name: 'list_files',
        description: 'List files in a directory',
        category: 'file',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'Directory path' },
            recursive: { type: 'boolean', description: 'List recursively' }
          },
          required: ['path']
        }
      },
      {
        name: 'grep',
        description: 'Search for pattern in files',
        category: 'search',
        schema: {
          type: 'object',
          properties: {
            pattern: { type: 'string', description: 'Search pattern' },
            path: { type: 'string', description: 'Path to search' }
          },
          required: ['pattern']
        }
      },
      {
        name: 'bash',
        description: 'Execute bash command',
        category: 'shell',
        schema: {
          type: 'object',
          properties: {
            command: { type: 'string', description: 'Command to execute' }
          },
          required: ['command']
        }
      }
    ];
  }

  /**
   * Execute tool in JavaScript fallback mode
   */
  private async executeFallback(name: string, params: ToolParams): Promise<any> {
    const fs = require('fs').promises;
    const path = require('path');
    const { exec } = require('child_process');
    const { promisify } = require('util');
    const execAsync = promisify(exec);

    switch (name) {
      case 'read_file':
        return { content: await fs.readFile(params.path, 'utf8') };
      
      case 'write_file':
        await fs.writeFile(params.path, params.content);
        return { success: true };
      
      case 'list_files':
        const files = await fs.readdir(params.path, { withFileTypes: true });
        return {
          files: files.map((f: any) => ({
            name: f.name,
            path: path.join(params.path, f.name),
            is_file: f.isFile(),
            is_directory: f.isDirectory()
          })),
          count: files.length
        };
      
      case 'bash':
        const { stdout, stderr } = await execAsync(params.command);
        return { stdout, stderr, exit_code: 0 };
      
      case 'grep':
        // Simple grep implementation
        const grepCmd = `grep -r "${params.pattern}" ${params.path || '.'}`;
        const result = await execAsync(grepCmd).catch((e: any) => ({ stdout: '', stderr: e.message }));
        return { matches: result.stdout.split('\n').filter(Boolean) };
      
      default:
        throw new Error(`Tool '${name}' not implemented in fallback mode`);
    }
  }
}