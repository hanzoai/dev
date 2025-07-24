import { EventEmitter } from 'events';
import { spawn, ChildProcess } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';
import chalk from 'chalk';
import { FileEditor } from './editor';
import { CodeActAgent } from './code-act-agent';
import { FunctionCallingSystem } from './function-calling';
import { MCPClient, MCPSession } from './mcp-client';

export interface WorkspacePane {
  id: string;
  type: 'shell' | 'editor' | 'browser' | 'planner' | 'output';
  title: string;
  content?: string;
  active: boolean;
}

export interface ShellSession {
  id: string;
  process: ChildProcess;
  cwd: string;
  history: string[];
  output: string;
}

export class UnifiedWorkspace extends EventEmitter {
  private panes: Map<string, WorkspacePane> = new Map();
  private shellSessions: Map<string, ShellSession> = new Map();
  private editor: FileEditor;
  private agent: CodeActAgent;
  private functionCalling: FunctionCallingSystem;
  private mcpClient: MCPClient;
  private activePane: string = '';
  private browserUrl: string = '';

  constructor() {
    super();
    this.editor = new FileEditor();
    this.agent = new CodeActAgent();
    this.functionCalling = new FunctionCallingSystem();
    this.mcpClient = new MCPClient();
    
    // Initialize default panes
    this.initializeDefaultPanes();
  }

  private initializeDefaultPanes(): void {
    // Shell pane
    this.createPane('shell', 'Shell', '');
    
    // Editor pane
    this.createPane('editor', 'Editor', 'No file open');
    
    // Browser pane
    this.createPane('browser', 'Browser', 'Browser: Ready');
    
    // Planner pane
    this.createPane('planner', 'Planner', 'Task planner ready');
    
    // Output pane
    this.createPane('output', 'Output', '');
    
    // Set shell as active by default
    this.setActivePane('shell');
  }

  private createPane(type: WorkspacePane['type'], title: string, content: string): void {
    const id = `${type}-${Date.now()}`;
    const pane: WorkspacePane = {
      id,
      type,
      title,
      content,
      active: false
    };
    this.panes.set(id, pane);
  }

  setActivePane(type: WorkspacePane['type']): void {
    // Find pane by type
    for (const [id, pane] of this.panes) {
      if (pane.type === type) {
        this.activePane = id;
        pane.active = true;
      } else {
        pane.active = false;
      }
    }
    this.emit('pane-changed', type);
  }

  // Shell operations
  async executeShellCommand(command: string): Promise<void> {
    const shellPane = this.getPane('shell');
    if (!shellPane) return;

    // Get or create shell session
    let session = this.getOrCreateShellSession();
    
    // Add to history
    session.history.push(command);
    
    // Execute command
    this.appendToPane('shell', `\n$ ${command}\n`);
    
    try {
      const result = await this.functionCalling.callFunction({
        id: Date.now().toString(),
        name: 'run_command',
        arguments: { command, cwd: session.cwd }
      });
      
      if (result.result?.stdout) {
        this.appendToPane('shell', result.result.stdout);
      }
      if (result.result?.stderr) {
        this.appendToPane('shell', chalk.red(result.result.stderr));
      }
      
      // Update cwd if cd command
      if (command.startsWith('cd ')) {
        const newDir = command.substring(3).trim();
        session.cwd = path.resolve(session.cwd, newDir);
      }
    } catch (error) {
      this.appendToPane('shell', chalk.red(`Error: ${error}`));
    }
  }

  private getOrCreateShellSession(): ShellSession {
    const sessionId = 'main';
    if (!this.shellSessions.has(sessionId)) {
      const session: ShellSession = {
        id: sessionId,
        process: spawn('bash', [], { cwd: process.cwd() }),
        cwd: process.cwd(),
        history: [],
        output: ''
      };
      this.shellSessions.set(sessionId, session);
    }
    return this.shellSessions.get(sessionId)!;
  }

  // Editor operations
  async openFile(filePath: string): Promise<void> {
    const result = await this.editor.execute({
      command: 'view',
      path: filePath
    });
    
    if (result.success) {
      this.updatePane('editor', result.content || '');
      this.updatePaneTitle('editor', `Editor - ${path.basename(filePath)}`);
      this.setActivePane('editor');
    } else {
      this.appendToPane('output', chalk.red(`Failed to open file: ${result.message}`));
    }
  }

  async saveFile(filePath: string, content: string): Promise<void> {
    fs.writeFileSync(filePath, content);
    this.appendToPane('output', chalk.green(`✓ Saved ${filePath}`));
  }

  // Browser operations
  async navigateBrowser(url: string): Promise<void> {
    this.browserUrl = url;
    this.updatePane('browser', `Browser: ${url}`);
    this.appendToPane('output', `Navigated to ${url}`);
    
    // In a real implementation, this would use a headless browser
    // For now, we'll just simulate
    try {
      const response = await fetch(url);
      const text = await response.text();
      const preview = text.substring(0, 500) + '...';
      this.updatePane('browser', `URL: ${url}\n\n${preview}`);
    } catch (error) {
      this.updatePane('browser', `Failed to load ${url}: ${error}`);
    }
  }

  // Planner operations
  async planTask(description: string): Promise<void> {
    this.updatePane('planner', `Planning: ${description}\n\nGenerating execution plan...`);
    this.setActivePane('planner');
    
    // Use the agent to plan
    const plan = await this.generatePlan(description);
    
    let planContent = `Task: ${description}\n\nExecution Plan:\n`;
    plan.steps.forEach((step, i) => {
      const parallel = plan.parallelizable[i] ? ' [can run in parallel]' : '';
      planContent += `${i + 1}. ${step}${parallel}\n`;
    });
    
    this.updatePane('planner', planContent);
  }

  private async generatePlan(description: string): Promise<{
    steps: string[];
    parallelizable: boolean[];
  }> {
    // Simplified planning logic
    const steps: string[] = [];
    const parallelizable: boolean[] = [];
    
    if (description.includes('debug')) {
      steps.push('Reproduce the issue');
      parallelizable.push(false);
      steps.push('Analyze error logs');
      parallelizable.push(false);
      steps.push('Identify root cause');
      parallelizable.push(false);
      steps.push('Implement fix');
      parallelizable.push(false);
      steps.push('Test the fix');
      parallelizable.push(false);
    } else if (description.includes('feature')) {
      steps.push('Analyze requirements');
      parallelizable.push(false);
      steps.push('Design implementation');
      parallelizable.push(false);
      steps.push('Write tests');
      parallelizable.push(true);
      steps.push('Implement feature');
      parallelizable.push(true);
      steps.push('Run tests');
      parallelizable.push(false);
      steps.push('Update documentation');
      parallelizable.push(true);
    } else {
      steps.push('Analyze task');
      parallelizable.push(false);
      steps.push('Execute task');
      parallelizable.push(false);
      steps.push('Verify results');
      parallelizable.push(false);
    }
    
    return { steps, parallelizable };
  }

  // Execute planned task
  async executePlan(): Promise<void> {
    const plannerPane = this.getPane('planner');
    if (!plannerPane || !plannerPane.content) return;
    
    // Extract task from planner
    const lines = plannerPane.content.split('\n');
    const taskLine = lines.find(l => l.startsWith('Task:'));
    if (!taskLine) return;
    
    const task = taskLine.substring(5).trim();
    this.appendToPane('output', chalk.cyan(`\nExecuting task: ${task}\n`));
    
    // Execute using agent
    await this.agent.executeTask(task);
  }

  // Pane management
  private getPane(type: WorkspacePane['type']): WorkspacePane | undefined {
    for (const pane of this.panes.values()) {
      if (pane.type === type) return pane;
    }
    return undefined;
  }

  private updatePane(type: WorkspacePane['type'], content: string): void {
    const pane = this.getPane(type);
    if (pane) {
      pane.content = content;
      this.emit('pane-updated', type, content);
    }
  }

  private appendToPane(type: WorkspacePane['type'], content: string): void {
    const pane = this.getPane(type);
    if (pane) {
      pane.content = (pane.content || '') + content;
      this.emit('pane-updated', type, pane.content);
    }
  }

  private updatePaneTitle(type: WorkspacePane['type'], title: string): void {
    const pane = this.getPane(type);
    if (pane) {
      pane.title = title;
      this.emit('pane-title-updated', type, title);
    }
  }

  // Display workspace (simplified for CLI)
  displayWorkspace(): void {
    console.clear();
    console.log(chalk.bold.cyan('╔══════════════════════════════════════════════════════════════╗'));
    console.log(chalk.bold.cyan('║                    🚀 Hanzo Dev Workspace                    ║'));
    console.log(chalk.bold.cyan('╚══════════════════════════════════════════════════════════════╝'));
    console.log();
    
    // Display pane tabs
    const tabs: string[] = [];
    for (const pane of this.panes.values()) {
      const isActive = pane.id === this.activePane;
      const tab = isActive 
        ? chalk.bold.yellow(`[${pane.title}]`)
        : chalk.gray(`[${pane.title}]`);
      tabs.push(tab);
    }
    console.log(tabs.join(' '));
    console.log(chalk.gray('─'.repeat(64)));
    
    // Display active pane content
    const activePane = this.panes.get(this.activePane);
    if (activePane && activePane.content) {
      const lines = activePane.content.split('\n');
      const maxLines = 20;
      const displayLines = lines.slice(-maxLines);
      console.log(displayLines.join('\n'));
    }
    
    console.log(chalk.gray('─'.repeat(64)));
  }

  // Cleanup
  async cleanup(): Promise<void> {
    // Close shell sessions
    for (const session of this.shellSessions.values()) {
      session.process.kill();
    }
    
    // Disconnect MCP sessions
    const sessions = this.mcpClient.getAllSessions();
    for (const session of sessions) {
      await this.mcpClient.disconnect(session.id);
    }
  }
}

// Interactive workspace session
export class WorkspaceSession {
  private workspace: UnifiedWorkspace;
  private running: boolean = true;

  constructor() {
    this.workspace = new UnifiedWorkspace();
  }

  async start(): Promise<void> {
    console.log(chalk.bold.cyan('\n🎯 Starting Unified Workspace...\n'));
    
    // Set up event listeners
    this.workspace.on('pane-updated', () => {
      if (this.running) {
        this.workspace.displayWorkspace();
      }
    });
    
    // Initial display
    this.workspace.displayWorkspace();
    
    // Start interactive loop
    const readline = require('readline');
    const rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });
    
    console.log(chalk.gray('\nCommands: shell <cmd>, edit <file>, browse <url>, plan <task>, execute, switch <pane>, exit\n'));
    
    const prompt = () => {
      rl.question(chalk.green('workspace> '), async (input) => {
        if (!this.running) return;
        
        const [cmd, ...args] = input.trim().split(' ');
        const arg = args.join(' ');
        
        try {
          switch (cmd) {
            case 'shell':
            case 'sh':
              await this.workspace.executeShellCommand(arg);
              break;
              
            case 'edit':
            case 'e':
              await this.workspace.openFile(arg);
              break;
              
            case 'browse':
            case 'b':
              await this.workspace.navigateBrowser(arg);
              break;
              
            case 'plan':
            case 'p':
              await this.workspace.planTask(arg);
              break;
              
            case 'execute':
            case 'x':
              await this.workspace.executePlan();
              break;
              
            case 'switch':
            case 's':
              this.workspace.setActivePane(arg as any);
              this.workspace.displayWorkspace();
              break;
              
            case 'exit':
            case 'quit':
              this.running = false;
              await this.workspace.cleanup();
              rl.close();
              return;
              
            default:
              console.log(chalk.red(`Unknown command: ${cmd}`));
          }
        } catch (error) {
          console.log(chalk.red(`Error: ${error}`));
        }
        
        if (this.running) {
          prompt();
        }
      });
    };
    
    prompt();
  }
}