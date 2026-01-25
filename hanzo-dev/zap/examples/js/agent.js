/**
 * ZAP Agent Example - Plain JavaScript (ES Modules)
 *
 * A minimal ZAP-compatible agent demonstrating tool execution.
 *
 * Usage:
 *   node agent.js
 */

import { readFile, readdir } from 'node:fs/promises';
import { execSync } from 'node:child_process';
import { join, resolve } from 'node:path';

// ============================================================================
// Constants
// ============================================================================

/**
 * Approval policy for tool execution.
 * @type {'untrusted' | 'on-failure' | 'on-request' | 'never'}
 */
const APPROVAL_POLICY = 'on-request';

/**
 * Sandbox mode configuration.
 * @type {'danger-full-access' | 'read-only' | 'workspace-write'}
 */
const SANDBOX_MODE = 'workspace-write';

// ============================================================================
// ZapAgent Class
// ============================================================================

/**
 * ZAP-compatible agent for executing tools.
 */
class ZapAgent {
  /**
   * Create a new ZapAgent instance.
   * @param {string} [cwd='.'] - Working directory for the agent.
   * @param {object} [options={}] - Agent options.
   * @param {number} [options.timeoutMs=30000] - Command timeout in milliseconds.
   * @param {string[]} [options.writableRoots=[]] - Writable directories in workspace-write mode.
   * @param {boolean} [options.networkAccess=true] - Allow network access.
   */
  constructor(cwd = '.', options = {}) {
    const { timeoutMs = 30000, writableRoots = [], networkAccess = true } = options;

    /** @type {object} */
    this.ctx = {
      cwd: resolve(cwd),
      env: process.env,
      sessionId: `js-agent-${Date.now()}`,
      approvalPolicy: APPROVAL_POLICY,
      sandboxMode: SANDBOX_MODE,
      writableRoots,
      networkAccess,
      timeoutMs,
    };
  }

  /**
   * Execute a tool by name.
   * @param {string} name - Tool name.
   * @param {object} [args={}] - Tool arguments.
   * @returns {Promise<{content: unknown, error?: string}>} Tool result.
   */
  async execute(name, args = {}) {
    const tools = {
      read_file: () => this.#readFile(args.path),
      list_dir: () => this.#listDir(args.path),
      git_status: () => this.#gitStatus(),
      exec: () => this.#exec(args.command),
    };

    const tool = tools[name];
    if (!tool) {
      return { content: null, error: `Unknown tool: ${name}` };
    }

    return tool();
  }

  /**
   * Read file contents.
   * @param {string} path - Relative path to file.
   * @returns {Promise<{content: string|null, error?: string}>}
   */
  async #readFile(path) {
    try {
      const fullPath = join(this.ctx.cwd, path);
      const content = await readFile(fullPath, 'utf-8');
      return { content };
    } catch (err) {
      return { content: null, error: err.message };
    }
  }

  /**
   * List directory contents.
   * @param {string} path - Relative path to directory.
   * @returns {Promise<{content: Array|null, error?: string}>}
   */
  async #listDir(path) {
    try {
      const fullPath = join(this.ctx.cwd, path);
      const entries = await readdir(fullPath, { withFileTypes: true });
      const result = entries.map((entry) => ({
        name: entry.name,
        isDir: entry.isDirectory(),
      }));
      return { content: result };
    } catch (err) {
      return { content: null, error: err.message };
    }
  }

  /**
   * Get git status.
   * @returns {{content: string|null, error?: string}}
   */
  #gitStatus() {
    try {
      const output = execSync('git status --porcelain=v2 --branch', {
        cwd: this.ctx.cwd,
        encoding: 'utf-8',
      });
      return { content: output };
    } catch (err) {
      return { content: null, error: err.message };
    }
  }

  /**
   * Execute a shell command.
   * @param {string} command - Command to execute.
   * @returns {{content: string|null, error?: string}}
   */
  #exec(command) {
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
    } catch (err) {
      return { content: null, error: err.message };
    }
  }

  /**
   * List all available tools.
   * @returns {string[]} Array of tool names.
   */
  listTools() {
    return ['read_file', 'list_dir', 'git_status', 'exec'];
  }

  /**
   * Get agent context info.
   * @returns {object} Context information.
   */
  getContext() {
    return {
      cwd: this.ctx.cwd,
      sessionId: this.ctx.sessionId,
      approvalPolicy: this.ctx.approvalPolicy,
      sandboxMode: this.ctx.sandboxMode,
    };
  }
}

// ============================================================================
// Main
// ============================================================================

async function main() {
  console.log('ZAP JavaScript Agent Example');
  console.log('============================\n');

  const agent = new ZapAgent('.');

  // Show agent context
  const ctx = agent.getContext();
  console.log('Agent Context:');
  console.log(`  Session: ${ctx.sessionId}`);
  console.log(`  CWD: ${ctx.cwd}`);
  console.log(`  Approval: ${ctx.approvalPolicy}`);
  console.log(`  Sandbox: ${ctx.sandboxMode}\n`);

  // List available tools
  const tools = agent.listTools();
  console.log(`Available tools (${tools.length}):`);
  for (const tool of tools) {
    console.log(`  - ${tool}`);
  }
  console.log();

  // Example 1: Read a file
  console.log('Example 1: Read file (package.json)');
  const readResult = await agent.execute('read_file', { path: 'package.json' });
  if (readResult.error) {
    console.log(`  Error: ${readResult.error}`);
  } else {
    console.log(`  Result: ${readResult.content.length} chars read`);
  }
  console.log();

  // Example 2: List directory
  console.log('Example 2: List directory');
  const listResult = await agent.execute('list_dir', { path: '.' });
  if (listResult.error) {
    console.log(`  Error: ${listResult.error}`);
  } else {
    const { content } = listResult;
    console.log(`  Found ${content.length} entries:`);
    for (const entry of content.slice(0, 5)) {
      const type = entry.isDir ? '[dir]' : '[file]';
      console.log(`    ${type} ${entry.name}`);
    }
    if (content.length > 5) {
      console.log(`    ... and ${content.length - 5} more`);
    }
  }
  console.log();

  // Example 3: Git status
  console.log('Example 3: Git status');
  const statusResult = await agent.execute('git_status', {});
  if (statusResult.error) {
    console.log(`  Error: ${statusResult.error}`);
  } else {
    const lines = statusResult.content.trim().split('\n');
    console.log(`  ${lines.length} lines of status`);
    if (lines.length > 0) {
      console.log(`  First line: ${lines[0]}`);
    }
  }
  console.log();

  // Example 4: Execute command
  console.log('Example 4: Execute command (echo)');
  const execResult = await agent.execute('exec', { command: 'echo "Hello from ZAP!"' });
  if (execResult.error) {
    console.log(`  Error: ${execResult.error}`);
  } else {
    console.log(`  Output: ${execResult.content.trim()}`);
  }
}

main().catch(console.error);

export { ZapAgent, APPROVAL_POLICY, SANDBOX_MODE };
