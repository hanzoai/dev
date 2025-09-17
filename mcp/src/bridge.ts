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
      // File operations
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
        name: 'edit_file',
        description: 'Edit a file by replacing text',
        category: 'file',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'File path' },
            old_text: { type: 'string', description: 'Text to replace' },
            new_text: { type: 'string', description: 'Replacement text' }
          },
          required: ['path', 'old_text', 'new_text']
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
        name: 'create_directory',
        description: 'Create a directory',
        category: 'file',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'Directory path to create' },
            recursive: { type: 'boolean', description: 'Create parent directories' }
          },
          required: ['path']
        }
      },
      {
        name: 'delete_file',
        description: 'Delete a file or directory',
        category: 'file',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'Path to delete' }
          },
          required: ['path']
        }
      },
      {
        name: 'move_file',
        description: 'Move or rename a file',
        category: 'file',
        schema: {
          type: 'object',
          properties: {
            source: { type: 'string', description: 'Source path' },
            destination: { type: 'string', description: 'Destination path' }
          },
          required: ['source', 'destination']
        }
      },
      {
        name: 'copy_file',
        description: 'Copy a file',
        category: 'file',
        schema: {
          type: 'object',
          properties: {
            source: { type: 'string', description: 'Source path' },
            destination: { type: 'string', description: 'Destination path' }
          },
          required: ['source', 'destination']
        }
      },

      // Search operations
      {
        name: 'grep',
        description: 'Search for pattern in files',
        category: 'search',
        schema: {
          type: 'object',
          properties: {
            pattern: { type: 'string', description: 'Search pattern' },
            path: { type: 'string', description: 'Path to search' },
            case_sensitive: { type: 'boolean', description: 'Case sensitive search' }
          },
          required: ['pattern']
        }
      },
      {
        name: 'find_files',
        description: 'Find files by name pattern',
        category: 'search',
        schema: {
          type: 'object',
          properties: {
            pattern: { type: 'string', description: 'File name pattern' },
            path: { type: 'string', description: 'Starting directory' }
          },
          required: ['pattern']
        }
      },
      {
        name: 'ripgrep',
        description: 'Fast search using ripgrep',
        category: 'search',
        schema: {
          type: 'object',
          properties: {
            pattern: { type: 'string', description: 'Search pattern' },
            path: { type: 'string', description: 'Path to search' },
            file_type: { type: 'string', description: 'File type filter' }
          },
          required: ['pattern']
        }
      },

      // Shell operations
      {
        name: 'bash',
        description: 'Execute bash command',
        category: 'shell',
        schema: {
          type: 'object',
          properties: {
            command: { type: 'string', description: 'Command to execute' },
            cwd: { type: 'string', description: 'Working directory' }
          },
          required: ['command']
        }
      },
      {
        name: 'shell',
        description: 'Execute shell command',
        category: 'shell',
        schema: {
          type: 'object',
          properties: {
            command: { type: 'string', description: 'Command to execute' }
          },
          required: ['command']
        }
      },
      {
        name: 'python',
        description: 'Execute Python code',
        category: 'shell',
        schema: {
          type: 'object',
          properties: {
            code: { type: 'string', description: 'Python code to execute' }
          },
          required: ['code']
        }
      },

      // Git operations
      {
        name: 'git_status',
        description: 'Get git status',
        category: 'git',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'Repository path' }
          }
        }
      },
      {
        name: 'git_diff',
        description: 'Get git diff',
        category: 'git',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'Repository path' },
            staged: { type: 'boolean', description: 'Show staged changes' }
          }
        }
      },
      {
        name: 'git_log',
        description: 'Get git log',
        category: 'git',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'Repository path' },
            limit: { type: 'number', description: 'Number of commits' }
          }
        }
      },
      {
        name: 'git_commit',
        description: 'Create git commit',
        category: 'git',
        schema: {
          type: 'object',
          properties: {
            message: { type: 'string', description: 'Commit message' },
            path: { type: 'string', description: 'Repository path' }
          },
          required: ['message']
        }
      },

      // System operations
      {
        name: 'get_env',
        description: 'Get environment variable',
        category: 'system',
        schema: {
          type: 'object',
          properties: {
            name: { type: 'string', description: 'Variable name' }
          },
          required: ['name']
        }
      },
      {
        name: 'set_env',
        description: 'Set environment variable',
        category: 'system',
        schema: {
          type: 'object',
          properties: {
            name: { type: 'string', description: 'Variable name' },
            value: { type: 'string', description: 'Variable value' }
          },
          required: ['name', 'value']
        }
      },
      {
        name: 'get_cwd',
        description: 'Get current working directory',
        category: 'system',
        schema: {
          type: 'object',
          properties: {}
        }
      },
      {
        name: 'set_cwd',
        description: 'Set current working directory',
        category: 'system',
        schema: {
          type: 'object',
          properties: {
            path: { type: 'string', description: 'New working directory' }
          },
          required: ['path']
        }
      },

      // Web operations
      {
        name: 'http_get',
        description: 'Make HTTP GET request',
        category: 'web',
        schema: {
          type: 'object',
          properties: {
            url: { type: 'string', description: 'URL to fetch' },
            headers: { type: 'object', description: 'Request headers' }
          },
          required: ['url']
        }
      },
      {
        name: 'http_post',
        description: 'Make HTTP POST request',
        category: 'web',
        schema: {
          type: 'object',
          properties: {
            url: { type: 'string', description: 'URL to post to' },
            data: { type: 'object', description: 'Request body' },
            headers: { type: 'object', description: 'Request headers' }
          },
          required: ['url']
        }
      }
    ];
  }

  /**
   * Execute tool in JavaScript fallback mode
   */
  private async executeFallback(name: string, params: ToolParams): Promise<any> {
    const fs = require('fs').promises;
    const fsSync = require('fs');
    const path = require('path');
    const { exec } = require('child_process');
    const { promisify } = require('util');
    const execAsync = promisify(exec);
    const https = require('https');
    const http = require('http');

    switch (name) {
      // File operations
      case 'read_file':
        return { content: await fs.readFile(params.path, 'utf8') };

      case 'write_file':
        await fs.writeFile(params.path, params.content);
        return { success: true };

      case 'edit_file': {
        const content = await fs.readFile(params.path, 'utf8');
        const newContent = content.replace(params.old_text, params.new_text);
        await fs.writeFile(params.path, newContent);
        return { success: true, lines_changed: content !== newContent ? 1 : 0 };
      }

      case 'list_files': {
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
      }

      case 'create_directory':
        await fs.mkdir(params.path, { recursive: params.recursive || false });
        return { success: true };

      case 'delete_file':
        const stats = await fs.stat(params.path);
        if (stats.isDirectory()) {
          await fs.rmdir(params.path, { recursive: true });
        } else {
          await fs.unlink(params.path);
        }
        return { success: true };

      case 'move_file':
        await fs.rename(params.source, params.destination);
        return { success: true };

      case 'copy_file':
        await fs.copyFile(params.source, params.destination);
        return { success: true };

      // Search operations
      case 'grep': {
        const flags = params.case_sensitive ? '' : '-i';
        const grepCmd = `grep ${flags} -r "${params.pattern}" ${params.path || '.'}`;
        const result = await execAsync(grepCmd).catch((e: any) => ({ stdout: '', stderr: e.message }));
        return { matches: result.stdout.split('\n').filter(Boolean) };
      }

      case 'find_files': {
        const findCmd = `find ${params.path || '.'} -name "${params.pattern}"`;
        const result = await execAsync(findCmd);
        return { files: result.stdout.split('\n').filter(Boolean) };
      }

      case 'ripgrep': {
        let rgCmd = `rg "${params.pattern}" ${params.path || '.'}`;
        if (params.file_type) {
          rgCmd += ` --type ${params.file_type}`;
        }
        const result = await execAsync(rgCmd).catch((e: any) => ({ stdout: '', stderr: e.message }));
        return { matches: result.stdout.split('\n').filter(Boolean) };
      }

      // Shell operations
      case 'bash':
      case 'shell': {
        const options = params.cwd ? { cwd: params.cwd } : {};
        const { stdout, stderr } = await execAsync(params.command, options);
        return { stdout, stderr, exit_code: 0 };
      }

      case 'python': {
        const { stdout, stderr } = await execAsync(`python3 -c "${params.code}"`);
        return { stdout, stderr, exit_code: 0 };
      }

      // Git operations
      case 'git_status': {
        const { stdout } = await execAsync('git status --porcelain', { cwd: params.path || '.' });
        return { output: stdout };
      }

      case 'git_diff': {
        const cmd = params.staged ? 'git diff --staged' : 'git diff';
        const { stdout } = await execAsync(cmd, { cwd: params.path || '.' });
        return { diff: stdout };
      }

      case 'git_log': {
        const limit = params.limit || 10;
        const { stdout } = await execAsync(`git log --oneline -${limit}`, { cwd: params.path || '.' });
        return { commits: stdout.split('\n').filter(Boolean) };
      }

      case 'git_commit': {
        const { stdout } = await execAsync(`git commit -m "${params.message}"`, { cwd: params.path || '.' });
        return { output: stdout };
      }

      // System operations
      case 'get_env':
        return { value: process.env[params.name] || null };

      case 'set_env':
        process.env[params.name] = params.value;
        return { success: true };

      case 'get_cwd':
        return { path: process.cwd() };

      case 'set_cwd':
        process.chdir(params.path);
        return { success: true };

      // Web operations
      case 'http_get': {
        return new Promise((resolve, reject) => {
          const url = new URL(params.url);
          const client = url.protocol === 'https:' ? https : http;
          client.get(params.url, { headers: params.headers || {} }, (res: any) => {
            let data = '';
            res.on('data', (chunk: any) => data += chunk);
            res.on('end', () => resolve({
              status: res.statusCode,
              headers: res.headers,
              body: data
            }));
          }).on('error', reject);
        });
      }

      case 'http_post': {
        return new Promise((resolve, reject) => {
          const url = new URL(params.url);
          const client = url.protocol === 'https:' ? https : http;
          const postData = JSON.stringify(params.data || {});
          const options = {
            method: 'POST',
            headers: {
              'Content-Type': 'application/json',
              'Content-Length': Buffer.byteLength(postData),
              ...(params.headers || {})
            }
          };
          const req = client.request(params.url, options, (res: any) => {
            let data = '';
            res.on('data', (chunk: any) => data += chunk);
            res.on('end', () => resolve({
              status: res.statusCode,
              headers: res.headers,
              body: data
            }));
          });
          req.on('error', reject);
          req.write(postData);
          req.end();
        });
      }

      default:
        throw new Error(`Tool '${name}' not implemented in fallback mode`);
    }
  }
}