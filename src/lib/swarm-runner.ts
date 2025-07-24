import * as fs from 'fs';
import * as path from 'path';
import { glob, globSync } from 'glob';
import chalk from 'chalk';
import ora from 'ora';
import { spawn, ChildProcess } from 'child_process';
import { EventEmitter } from 'events';

export interface SwarmOptions {
  provider: 'claude' | 'openai' | 'gemini' | 'grok' | 'local';
  count: number;
  prompt: string;
  cwd?: string;
  pattern?: string;
  autoLogin?: boolean;
}

export interface SwarmAgent {
  id: string;
  process?: ChildProcess;
  file?: string;
  status: 'idle' | 'busy' | 'done' | 'error';
  result?: string;
  error?: string;
}

export class SwarmRunner extends EventEmitter {
  private agents: Map<string, SwarmAgent> = new Map();
  private fileQueue: string[] = [];
  private options: SwarmOptions;
  private activeCount: number = 0;

  constructor(options: SwarmOptions) {
    super();
    this.options = {
      cwd: process.cwd(),
      pattern: '**/*',
      autoLogin: true,
      ...options
    };
  }

  async run(): Promise<void> {
    const spinner = ora(`Initializing swarm with ${this.options.count} agents...`).start();

    try {
      // Find files to process
      spinner.text = `Searching for files in ${this.options.cwd || process.cwd()}...`;
      this.fileQueue = await this.findFiles();
      spinner.succeed(`Found ${this.fileQueue.length} files to process`);

      if (this.fileQueue.length === 0) {
        console.log(chalk.yellow('No files found matching pattern'));
        return;
      }

      // Initialize agent pool
      const agentCount = Math.min(this.options.count, this.fileQueue.length);
      spinner.start(`Spawning ${agentCount} agents...`);
      
      for (let i = 0; i < agentCount; i++) {
        const agent: SwarmAgent = {
          id: `agent-${i}`,
          status: 'idle'
        };
        this.agents.set(agent.id, agent);
      }
      
      spinner.succeed(`Spawned ${agentCount} agents`);

      // Process files in parallel
      spinner.start('Processing files...');
      const startTime = Date.now();
      
      // Start processing
      await this.processFiles();
      
      const duration = (Date.now() - startTime) / 1000;
      spinner.succeed(`Completed in ${duration.toFixed(1)}s`);

      // Show results
      this.showResults();

    } catch (error) {
      spinner.fail(`Swarm error: ${error}`);
      throw error;
    }
  }

  private async findFiles(): Promise<string[]> {
    try {
      const options = {
        cwd: this.options.cwd || process.cwd(),
        nodir: true,
        ignore: [
          '**/node_modules/**',
          '**/.git/**',
          '**/dist/**',
          '**/build/**',
          '**/*.min.js',
          '**/*.map',
          '**/.swarm-tmp/**'
        ],
        absolute: false
      };

      const pattern = this.options.pattern || '**/*';
      console.log(chalk.gray(`Searching with pattern: ${pattern} in ${options.cwd}`));
      
      // Use sync version for reliability
      const files = globSync(pattern, options);
      console.log(chalk.gray(`Found ${files.length} total files`));
      
      // Filter to only editable files
      const editableFiles = files.filter(file => {
        const ext = path.extname(file);
        return ['.js', '.ts', '.jsx', '.tsx', '.py', '.java', '.cpp', '.c', '.h', '.go', '.rs', '.rb', '.php', '.swift', '.kt', '.scala', '.r', '.m', '.mm', '.md', '.txt', '.json', '.xml', '.yaml', '.yml', '.toml', '.ini', '.conf', '.sh', '.bash', '.zsh', '.fish', '.ps1', '.bat', '.cmd'].includes(ext);
      });
      
      console.log(chalk.gray(`Filtered to ${editableFiles.length} editable files`));
      return editableFiles;
    } catch (error) {
      console.error(chalk.red('Error finding files:'), error);
      return [];
    }
  }

  private async processFiles(): Promise<void> {
    const promises: Promise<void>[] = [];

    // Start initial batch of work
    for (const [id, agent] of this.agents) {
      if (this.fileQueue.length > 0) {
        promises.push(this.processNextFile(agent));
      }
    }

    // Wait for all agents to complete
    await Promise.all(promises);
  }

  private async processNextFile(agent: SwarmAgent): Promise<void> {
    while (this.fileQueue.length > 0) {
      const file = this.fileQueue.shift();
      if (!file) break;

      agent.file = file;
      agent.status = 'busy';
      this.activeCount++;

      try {
        await this.processFile(agent, file);
        agent.status = 'done';
      } catch (error) {
        agent.status = 'error';
        agent.error = error instanceof Error ? error.message : String(error);
      } finally {
        this.activeCount--;
      }
    }
  }

  private async processFile(agent: SwarmAgent, file: string): Promise<void> {
    const fullPath = path.join(this.options.cwd!, file);
    
    // Build command based on provider
    const command = this.buildCommand(file);
    
    return new Promise((resolve, reject) => {
      const child = spawn(command.cmd, command.args, {
        cwd: this.options.cwd,
        env: {
          ...process.env,
          ANTHROPIC_API_KEY: process.env.ANTHROPIC_API_KEY,
          OPENAI_API_KEY: process.env.OPENAI_API_KEY,
          GOOGLE_API_KEY: process.env.GOOGLE_API_KEY,
          GROK_API_KEY: process.env.GROK_API_KEY,
          // Auto-accept edits for non-interactive mode
          CLAUDE_CODE_PERMISSION_MODE: 'acceptEdits'
        }
      });

      agent.process = child;
      
      let output = '';
      let error = '';

      child.stdout?.on('data', (data) => {
        output += data.toString();
      });

      child.stderr?.on('data', (data) => {
        error += data.toString();
      });

      child.on('close', (code) => {
        if (code === 0) {
          agent.result = output;
          resolve();
        } else {
          reject(new Error(`Process exited with code ${code}: ${error}`));
        }
      });

      child.on('error', (err) => {
        reject(err);
      });
    });
  }

  private buildCommand(file: string): { cmd: string, args: string[] } {
    const fullPath = path.join(this.options.cwd!, file);
    const filePrompt = `${this.options.prompt}\n\nFile: ${file}`;

    switch (this.options.provider) {
      case 'claude':
        return {
          cmd: 'claude',
          args: [
            '-p',
            filePrompt,
            '--max-turns', '5',
            '--allowedTools', 'Read,Write,Edit',
            '--permission-mode', 'acceptEdits'
          ]
        };

      case 'openai':
        return {
          cmd: 'openai',
          args: [
            'chat',
            '--prompt', filePrompt,
            '--file', fullPath,
            '--edit'
          ]
        };

      case 'gemini':
        return {
          cmd: 'gemini',
          args: [
            'edit',
            fullPath,
            '--prompt', filePrompt
          ]
        };

      case 'grok':
        return {
          cmd: 'grok',
          args: [
            '--edit',
            fullPath,
            '--prompt', filePrompt
          ]
        };

      case 'local':
        return {
          cmd: 'dev',
          args: [
            'agent',
            filePrompt
          ]
        };

      default:
        throw new Error(`Unknown provider: ${this.options.provider}`);
    }
  }

  private showResults(): void {
    console.log(chalk.bold.cyan('\n📊 Swarm Results\n'));

    let successful = 0;
    let failed = 0;

    for (const [id, agent] of this.agents) {
      if (agent.status === 'done') {
        successful++;
        console.log(chalk.green(`✓ ${agent.file || id}`));
      } else if (agent.status === 'error') {
        failed++;
        console.log(chalk.red(`✗ ${agent.file || id}: ${agent.error}`));
      }
    }

    console.log(chalk.gray('\n─────────────────'));
    console.log(chalk.white('Total files:'), this.fileQueue.length + successful + failed);
    console.log(chalk.green('Successful:'), successful);
    if (failed > 0) {
      console.log(chalk.red('Failed:'), failed);
    }
  }

  async ensureProviderAuth(): Promise<boolean> {
    switch (this.options.provider) {
      case 'claude':
        return this.ensureClaudeAuth();
      case 'openai':
        return !!process.env.OPENAI_API_KEY;
      case 'gemini':
        return !!process.env.GOOGLE_API_KEY || !!process.env.GEMINI_API_KEY;
      case 'grok':
        return !!process.env.GROK_API_KEY;
      case 'local':
        return true;
      default:
        return false;
    }
  }

  private async ensureClaudeAuth(): Promise<boolean> {
    // Check if already authenticated
    try {
      const testResult = await new Promise<boolean>((resolve) => {
        const child = spawn('claude', ['-p', 'test', '--max-turns', '1'], {
          env: process.env
        });

        let hasError = false;
        let resolved = false;
        
        const cleanup = () => {
          if (!resolved) {
            resolved = true;
            clearTimeout(timeout);
            child.kill();
          }
        };

        child.stderr?.on('data', (data) => {
          const output = data.toString();
          if (output.includes('not authenticated') || output.includes('API key')) {
            hasError = true;
          }
        });

        child.on('close', () => {
          cleanup();
          resolve(!hasError);
        });

        // Timeout after 5 seconds
        const timeout = setTimeout(() => {
          cleanup();
          resolve(!hasError);
        }, 5000);
      });

      if (testResult) {
        return true;
      }

      // Try to login automatically if we have API key
      if (process.env.ANTHROPIC_API_KEY && this.options.autoLogin) {
        console.log(chalk.yellow('Attempting automatic Claude login...'));
        
        const loginResult = await new Promise<boolean>((resolve) => {
          const child = spawn('claude', ['login'], {
            env: {
              ...process.env,
              ANTHROPIC_API_KEY: process.env.ANTHROPIC_API_KEY
            },
            stdio: 'inherit'
          });

          child.on('close', (code) => {
            resolve(code === 0);
          });
        });

        if (loginResult) {
          console.log(chalk.green('✓ Claude login successful'));
          return true;
        }
      }

      return false;
    } catch (error) {
      return false;
    }
  }
}