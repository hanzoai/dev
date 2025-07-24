import * as readline from 'readline';
import * as fs from 'fs';
import * as path from 'path';
import chalk from 'chalk';
import { ConfigurableAgentLoop, AgentLoopConfig, LLMProvider } from './agent-loop';
import { SwarmTool } from './swarm-tool';
import { v4 as uuidv4 } from 'uuid';

export interface Session {
  id: string;
  messages: any[];
  context: any;
  createdAt: Date;
  updatedAt: Date;
}

export class InteractiveAgent {
  private rl: readline.Interface;
  private agent: ConfigurableAgentLoop;
  private sessionId: string;
  private sessionDir: string;
  private swarmTool: SwarmTool;
  private swarmEnabled: boolean = false;
  private swarmCount: number = 5;

  constructor(provider?: LLMProvider, swarmCount?: number) {
    this.sessionId = uuidv4();
    this.sessionDir = path.join(process.cwd(), '.dev-sessions');
    this.swarmTool = new SwarmTool();
    
    if (swarmCount) {
      this.swarmEnabled = true;
      this.swarmCount = swarmCount;
    }

    // Ensure session directory exists
    if (!fs.existsSync(this.sessionDir)) {
      fs.mkdirSync(this.sessionDir, { recursive: true });
    }

    // Create readline interface
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      prompt: chalk.cyan('dev> ')
    });

    // Initialize agent with config
    const config: AgentLoopConfig = {
      provider: provider || this.getDefaultProvider(),
      maxIterations: 10,
      enableMCP: true,
      enableBrowser: false,
      enableSwarm: this.swarmEnabled,
      streamOutput: true,
      confirmActions: false
    };

    this.agent = new ConfigurableAgentLoop(config);
    this.setupAgentTools();
  }

  private getDefaultProvider(): LLMProvider {
    // Check for available providers
    const providers = ConfigurableAgentLoop.getAvailableProviders();
    return providers[0] || {
      name: 'Local',
      type: 'local',
      model: 'dev-agent',
      supportsTools: true,
      supportsStreaming: false
    };
  }

  private setupAgentTools(): void {
    // Add swarm tool if enabled
    if (this.swarmEnabled) {
      this.agent.addTool({
        name: 'distribute_work',
        description: 'Distribute work across multiple parallel agents',
        parameters: {
          files: { type: 'array', description: 'Files to process' },
          task: { type: 'string', description: 'Task to perform on files' },
          batchSize: { type: 'number', description: 'Files per worker', default: 5 }
        },
        execute: async (params: any) => {
          const tasks = this.swarmTool.createTaskBatches(
            params.files,
            params.batchSize || 5,
            params.task,
            this.agent.config.provider.type
          );
          
          const results = await this.swarmTool.executeParallelTasks(tasks);
          return {
            success: true,
            results: results,
            summary: `Processed ${params.files.length} files across ${tasks.length} workers`
          };
        }
      });
    }

    // Add session management tools
    this.agent.addTool({
      name: 'save_session',
      description: 'Save the current session',
      parameters: {
        name: { type: 'string', description: 'Session name', optional: true }
      },
      execute: async (params: any) => {
        const sessionName = params.name || `session-${Date.now()}`;
        await this.saveSession(sessionName);
        return { success: true, message: `Session saved as: ${sessionName}` };
      }
    });

    this.agent.addTool({
      name: 'load_session',
      description: 'Load a previous session',
      parameters: {
        name: { type: 'string', description: 'Session name or ID' }
      },
      execute: async (params: any) => {
        const loaded = await this.loadSession(params.name);
        return { 
          success: loaded, 
          message: loaded ? `Session loaded: ${params.name}` : 'Session not found' 
        };
      }
    });
  }

  async start(initialPrompt?: string): Promise<void> {
    console.log(chalk.bold.cyan('\n🤖 Hanzo Dev Interactive Mode\n'));
    console.log(chalk.gray('Type your commands or questions. Use /help for available commands.\n'));

    if (this.swarmEnabled) {
      console.log(chalk.yellow(`🐝 Swarm mode enabled with ${this.swarmCount} workers\n`));
    }

    // Handle initial prompt if provided
    if (initialPrompt) {
      await this.handleInput(initialPrompt);
    }

    // Show prompt
    this.rl.prompt();

    // Handle line input
    this.rl.on('line', async (line) => {
      const input = line.trim();
      
      if (!input) {
        this.rl.prompt();
        return;
      }

      // Handle special commands
      if (input.startsWith('/')) {
        await this.handleCommand(input);
      } else {
        await this.handleInput(input);
      }

      this.rl.prompt();
    });

    // Handle close
    this.rl.on('close', () => {
      console.log(chalk.gray('\nGoodbye! 👋'));
      this.cleanup();
      process.exit(0);
    });
  }

  private async handleCommand(command: string): Promise<void> {
    const [cmd, ...args] = command.split(' ');
    
    switch (cmd) {
      case '/help':
        this.showHelp();
        break;
        
      case '/save':
        await this.saveSession(args[0] || `session-${Date.now()}`);
        console.log(chalk.green('Session saved!'));
        break;
        
      case '/load':
        if (args[0]) {
          const loaded = await this.loadSession(args[0]);
          if (loaded) {
            console.log(chalk.green('Session loaded!'));
          } else {
            console.log(chalk.red('Session not found!'));
          }
        } else {
          await this.listSessions();
        }
        break;
        
      case '/clear':
        console.clear();
        break;
        
      case '/exit':
      case '/quit':
        this.rl.close();
        break;
        
      case '/swarm':
        if (args[0] === 'on') {
          this.swarmEnabled = true;
          this.setupAgentTools();
          console.log(chalk.yellow('Swarm mode enabled'));
        } else if (args[0] === 'off') {
          this.swarmEnabled = false;
          console.log(chalk.gray('Swarm mode disabled'));
        } else {
          console.log(chalk.gray(`Swarm mode: ${this.swarmEnabled ? 'ON' : 'OFF'}`));
        }
        break;
        
      default:
        console.log(chalk.red(`Unknown command: ${cmd}`));
        console.log(chalk.gray('Use /help for available commands'));
    }
  }

  private async handleInput(input: string): Promise<void> {
    try {
      // Send to agent
      const response = await this.agent.processMessage(input);
      
      // Response is streamed by the agent if streaming is enabled
      if (!this.agent.config.streamOutput) {
        console.log(response);
      }
    } catch (error) {
      console.error(chalk.red(`Error: ${error}`));
    }
  }

  private showHelp(): void {
    console.log(chalk.bold('\nAvailable Commands:'));
    console.log(chalk.gray('  /help          - Show this help message'));
    console.log(chalk.gray('  /save [name]   - Save current session'));
    console.log(chalk.gray('  /load [name]   - Load a previous session (or list if no name)'));
    console.log(chalk.gray('  /clear         - Clear the screen'));
    console.log(chalk.gray('  /swarm on/off  - Toggle swarm mode'));
    console.log(chalk.gray('  /exit          - Exit interactive mode'));
    console.log(chalk.gray('\nJust type normally to chat with the agent!\n'));
  }

  private async saveSession(name: string): Promise<void> {
    const session: Session = {
      id: this.sessionId,
      messages: this.agent.getMessages(),
      context: this.agent.getContext(),
      createdAt: new Date(),
      updatedAt: new Date()
    };

    const sessionPath = path.join(this.sessionDir, `${name}.json`);
    fs.writeFileSync(sessionPath, JSON.stringify(session, null, 2));
  }

  private async loadSession(name: string): Promise<boolean> {
    const sessionPath = path.join(this.sessionDir, `${name}.json`);
    
    if (!fs.existsSync(sessionPath)) {
      // Try with .json extension if not provided
      const altPath = path.join(this.sessionDir, `${name}.json`);
      if (!fs.existsSync(altPath)) {
        return false;
      }
    }

    try {
      const sessionData = JSON.parse(fs.readFileSync(sessionPath, 'utf-8'));
      this.sessionId = sessionData.id;
      this.agent.loadSession(sessionData.messages, sessionData.context);
      return true;
    } catch (error) {
      console.error(chalk.red(`Error loading session: ${error}`));
      return false;
    }
  }

  private async listSessions(): Promise<void> {
    const files = fs.readdirSync(this.sessionDir)
      .filter(f => f.endsWith('.json'))
      .map(f => f.replace('.json', ''));

    if (files.length === 0) {
      console.log(chalk.gray('No saved sessions found.'));
    } else {
      console.log(chalk.bold('\nSaved Sessions:'));
      files.forEach(f => {
        console.log(chalk.gray(`  - ${f}`));
      });
      console.log(chalk.gray('\nUse /load <name> to load a session\n'));
    }
  }

  private cleanup(): void {
    this.swarmTool.cleanup();
  }
}