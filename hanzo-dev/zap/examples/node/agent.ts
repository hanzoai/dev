/**
 * ZAP Agent Example - Node.js/TypeScript
 *
 * Demonstrates a complete ZAP agent with tool execution in TypeScript.
 *
 * Usage:
 *   npm install @hanzo/tools
 *   npx ts-node agent.ts
 */

import { readFile, readdir, stat } from 'fs/promises';
import { execSync } from 'child_process';
import { join, resolve } from 'path';

// Approval policy (matches hanzo-protocol)
type AskForApproval = 'untrusted' | 'on-failure' | 'on-request' | 'never';

// Sandbox policy (matches hanzo-protocol)
type SandboxPolicy =
  | { mode: 'danger-full-access' }
  | { mode: 'read-only' }
  | {
      mode: 'workspace-write';
      writableRoots?: string[];
      networkAccess?: boolean;
      allowGitWrites?: boolean;
    };

interface ExecutorContext {
  cwd: string;
  env: Record<string, string>;
  sessionId?: string;
  approvalPolicy: AskForApproval;
  sandboxPolicy: SandboxPolicy;
  timeoutMs?: number;
}

interface ToolResult {
  content: unknown;
  error?: string;
}

/**
 * ZAP-compatible agent for Node.js
 */
class ZapAgent {
  private ctx: ExecutorContext;

  constructor(cwd: string = '.') {
    this.ctx = {
      cwd: resolve(cwd),
      env: process.env as Record<string, string>,
      sessionId: `node-agent-${Date.now()}`,
      approvalPolicy: 'on-request',
      sandboxPolicy: {
        mode: 'workspace-write',
        writableRoots: [],
        networkAccess: true,
        allowGitWrites: false,
      },
      timeoutMs: 30000,
    };
  }

  /**
   * Execute a tool by name
   */
  async execute(
    name: string,
    args: Record<string, unknown>
  ): Promise<ToolResult> {
    switch (name) {
      case 'read_file':
        return this.readFile(args.path as string);
      case 'list_dir':
        return this.listDir(args.path as string);
      case 'git_status':
        return this.gitStatus();
      case 'git_log':
        return this.gitLog(args.limit as number);
      case 'exec':
        return this.exec(args.command as string);
      default:
        return { content: null, error: `Unknown tool: ${name}` };
    }
  }

  private async readFile(path: string): Promise<ToolResult> {
    try {
      const fullPath = join(this.ctx.cwd, path);
      const content = await readFile(fullPath, 'utf-8');
      return { content };
    } catch (e) {
      return { content: null, error: String(e) };
    }
  }

  private async listDir(path: string): Promise<ToolResult> {
    try {
      const fullPath = join(this.ctx.cwd, path);
      const entries = await readdir(fullPath, { withFileTypes: true });
      const result = entries.map((e) => ({
        name: e.name,
        isDir: e.isDirectory(),
      }));
      return { content: result };
    } catch (e) {
      return { content: null, error: String(e) };
    }
  }

  private gitStatus(): ToolResult {
    try {
      const output = execSync('git status --porcelain=v2 --branch', {
        cwd: this.ctx.cwd,
        encoding: 'utf-8',
      });
      return { content: output };
    } catch (e) {
      return { content: null, error: String(e) };
    }
  }

  private gitLog(limit: number = 10): ToolResult {
    try {
      const output = execSync(
        `git log --format="%H|%an|%s" -${limit}`,
        { cwd: this.ctx.cwd, encoding: 'utf-8' }
      );
      return { content: output };
    } catch (e) {
      return { content: null, error: String(e) };
    }
  }

  private exec(command: string): ToolResult {
    // Check approval policy
    if (this.ctx.approvalPolicy === 'untrusted') {
      return {
        content: null,
        error: 'Command execution requires approval in untrusted mode',
      };
    }

    try {
      const output = execSync(command, {
        cwd: this.ctx.cwd,
        encoding: 'utf-8',
        timeout: this.ctx.timeoutMs,
      });
      return { content: output };
    } catch (e) {
      return { content: null, error: String(e) };
    }
  }

  listTools(): string[] {
    return [
      'read_file',
      'list_dir',
      'git_status',
      'git_log',
      'exec',
    ];
  }
}

async function main() {
  console.log('ZAP Node.js Agent Example');
  console.log('=========================\n');

  const agent = new ZapAgent('.');

  // List available tools
  const tools = agent.listTools();
  console.log(`Available tools (${tools.length}):`);
  tools.forEach((t) => console.log(`  - ${t}`));
  console.log();

  // Example 1: Read a file
  console.log('Example 1: Read file');
  const readResult = await agent.execute('read_file', { path: 'Cargo.toml' });
  if (readResult.error) {
    console.log(`  Error: ${readResult.error}`);
  } else {
    const content = readResult.content as string;
    console.log(`  Result: ${content.length} chars read`);
  }
  console.log();

  // Example 2: List directory
  console.log('Example 2: List directory');
  const listResult = await agent.execute('list_dir', { path: '.' });
  console.log(`  Result: ${JSON.stringify(listResult.content).slice(0, 100)}...`);
  console.log();

  // Example 3: Git status
  console.log('Example 3: Git status');
  const statusResult = await agent.execute('git_status', {});
  if (statusResult.error) {
    console.log(`  Error: ${statusResult.error}`);
  } else {
    const status = (statusResult.content as string).slice(0, 100);
    console.log(`  Result: ${status}...`);
  }
}

main().catch(console.error);
