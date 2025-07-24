import { EventEmitter } from 'events';
import { spawn, ChildProcess } from 'child_process';
import { MCPClient, MCPSession, MCPServerConfig } from './mcp-client';
import { FunctionCallingSystem } from './function-calling';
import chalk from 'chalk';
import * as fs from 'fs';
import * as path from 'path';

export interface AgentConfig {
  id: string;
  name: string;
  type: 'claude-code' | 'aider' | 'openhands' | 'custom';
  command?: string;
  args?: string[];
  env?: Record<string, string>;
  capabilities: string[];
  assignedFiles?: string[];
  mcpEndpoint?: string;
}

export interface AgentInstance {
  config: AgentConfig;
  process?: ChildProcess;
  mcpSession?: MCPSession;
  status: 'idle' | 'busy' | 'error';
  currentTask?: string;
  metrics: {
    tasksCompleted: number;
    errors: number;
    averageTime: number;
  };
}

export class PeerAgentNetwork extends EventEmitter {
  private agents: Map<string, AgentInstance> = new Map();
  private mcpClient: MCPClient;
  private functionCalling: FunctionCallingSystem;
  private networkTopology: Map<string, string[]> = new Map(); // agent -> connected agents

  constructor() {
    super();
    this.mcpClient = new MCPClient();
    this.functionCalling = new FunctionCallingSystem();
  }

  // Spawn multiple agents for a codebase
  async spawnAgentsForCodebase(
    basePath: string,
    agentType: AgentConfig['type'] = 'claude-code',
    strategy: 'one-per-file' | 'one-per-directory' | 'by-complexity' = 'one-per-file'
  ): Promise<void> {
    console.log(chalk.cyan(`\n🌐 Spawning agent network for ${basePath}...\n`));
    
    const files = await this.discoverFiles(basePath);
    const assignments = this.assignFilesToAgents(files, strategy);
    
    console.log(chalk.yellow(`Creating ${assignments.length} agents for ${files.length} files`));
    
    // Spawn agents
    for (const assignment of assignments) {
      const agentId = `${agentType}-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
      const config: AgentConfig = {
        id: agentId,
        name: `${agentType} (${assignment.files.length} files)`,
        type: agentType,
        capabilities: this.getAgentCapabilities(agentType),
        assignedFiles: assignment.files
      };
      
      await this.spawnAgent(config);
    }
    
    // Connect all agents to each other
    await this.establishPeerConnections();
    
    console.log(chalk.green(`\n✅ Agent network ready with ${this.agents.size} agents\n`));
  }

  // Spawn a single agent
  async spawnAgent(config: AgentConfig): Promise<AgentInstance> {
    const instance: AgentInstance = {
      config,
      status: 'idle',
      metrics: {
        tasksCompleted: 0,
        errors: 0,
        averageTime: 0
      }
    };
    
    // Set up command based on agent type
    switch (config.type) {
      case 'claude-code':
        config.command = 'npx';
        config.args = ['claude-code', '--mcp-server', '--port', String(this.getNextPort())];
        break;
      case 'aider':
        config.command = 'aider';
        config.args = ['--no-interactive', '--mcp-mode'];
        break;
      case 'openhands':
        config.command = hasUvx() ? 'uvx' : 'python';
        config.args = hasUvx() ? ['hanzo-dev', '--mcp'] : ['-m', 'hanzo_dev', '--mcp'];
        break;
    }
    
    // Start MCP server for this agent
    if (config.command) {
      try {
        const mcpConfig: MCPServerConfig = {
          name: config.id,
          command: config.command,
          args: config.args,
          env: {
            ...process.env,
            ...config.env,
            AGENT_ID: config.id,
            ASSIGNED_FILES: JSON.stringify(config.assignedFiles || [])
          }
        };
        
        instance.mcpSession = await this.mcpClient.connect(mcpConfig);
        instance.mcpEndpoint = `mcp://${config.id}`;
        
        // Register this agent's tools with the function calling system
        await this.functionCalling.registerMCPServer(config.id, instance.mcpSession);
        
        console.log(chalk.green(`  ✓ Spawned ${config.name}`));
      } catch (error) {
        console.error(chalk.red(`  ✗ Failed to spawn ${config.name}: ${error}`));
        instance.status = 'error';
      }
    }
    
    this.agents.set(config.id, instance);
    return instance;
  }

  // Establish peer connections between all agents
  async establishPeerConnections(): Promise<void> {
    console.log(chalk.yellow('\n🔗 Establishing peer connections...\n'));
    
    const agentIds = Array.from(this.agents.keys());
    
    for (const agentId of agentIds) {
      const connections: string[] = [];
      
      // Connect to all other agents
      for (const peerId of agentIds) {
        if (agentId !== peerId) {
          await this.connectAgents(agentId, peerId);
          connections.push(peerId);
        }
      }
      
      this.networkTopology.set(agentId, connections);
    }
    
    console.log(chalk.green(`  ✓ Established ${this.calculateTotalConnections()} peer connections`));
  }

  // Connect two agents via MCP
  private async connectAgents(agentId: string, peerId: string): Promise<void> {
    const agent = this.agents.get(agentId);
    const peer = this.agents.get(peerId);
    
    if (!agent || !peer || !agent.mcpSession || !peer.mcpSession) return;
    
    // Register peer's tools with agent
    const peerTools = peer.mcpSession.tools.map(tool => ({
      name: `peer_${peerId}_${tool.name}`,
      description: `[Via ${peer.config.name}] ${tool.description}`,
      inputSchema: tool.inputSchema,
      handler: async (args: any) => {
        return this.mcpClient.callTool(peer.mcpSession!.id, tool.name, args);
      }
    }));
    
    // Add to agent's available tools
    for (const tool of peerTools) {
      this.functionCalling.registerTool(tool);
    }
  }

  // Delegate task to best agent
  async delegateTask(task: string, context?: any): Promise<any> {
    console.log(chalk.cyan(`\n📋 Delegating task: ${task}\n`));
    
    // Find best agent for task
    const agent = await this.selectBestAgent(task, context);
    if (!agent) {
      throw new Error('No suitable agent available');
    }
    
    console.log(chalk.gray(`  → Assigned to ${agent.config.name}`));
    
    // Execute task
    agent.status = 'busy';
    agent.currentTask = task;
    const startTime = Date.now();
    
    try {
      const result = await this.executeAgentTask(agent, task, context);
      
      // Update metrics
      agent.metrics.tasksCompleted++;
      const duration = Date.now() - startTime;
      agent.metrics.averageTime = 
        (agent.metrics.averageTime * (agent.metrics.tasksCompleted - 1) + duration) / 
        agent.metrics.tasksCompleted;
      
      agent.status = 'idle';
      agent.currentTask = undefined;
      
      return result;
    } catch (error) {
      agent.metrics.errors++;
      agent.status = 'idle';
      agent.currentTask = undefined;
      throw error;
    }
  }

  // Select best agent for a task
  private async selectBestAgent(task: string, context?: any): Promise<AgentInstance | null> {
    // Score agents based on:
    // 1. Current status (idle preferred)
    // 2. Relevant files assigned
    // 3. Capabilities match
    // 4. Past performance metrics
    
    let bestAgent: AgentInstance | null = null;
    let bestScore = -1;
    
    for (const agent of this.agents.values()) {
      let score = 0;
      
      // Status score
      if (agent.status === 'idle') score += 10;
      else if (agent.status === 'busy') score -= 5;
      else continue; // Skip error agents
      
      // File relevance score
      if (context?.file && agent.config.assignedFiles?.includes(context.file)) {
        score += 20;
      }
      
      // Performance score
      if (agent.metrics.tasksCompleted > 0) {
        const successRate = 1 - (agent.metrics.errors / agent.metrics.tasksCompleted);
        score += successRate * 10;
      }
      
      // Capability match (simplified)
      if (task.includes('refactor') && agent.config.capabilities.includes('refactoring')) {
        score += 15;
      }
      if (task.includes('test') && agent.config.capabilities.includes('testing')) {
        score += 15;
      }
      
      if (score > bestScore) {
        bestScore = score;
        bestAgent = agent;
      }
    }
    
    return bestAgent;
  }

  // Execute task on specific agent
  private async executeAgentTask(
    agent: AgentInstance, 
    task: string, 
    context?: any
  ): Promise<any> {
    if (!agent.mcpSession) {
      throw new Error('Agent has no MCP session');
    }
    
    // Call the agent's main task execution tool
    return this.mcpClient.callTool(
      agent.mcpSession.id,
      'execute_task',
      {
        task,
        context,
        assigned_files: agent.config.assignedFiles
      }
    );
  }

  // Parallel task execution across multiple agents
  async executeParallelTasks(tasks: Array<{task: string, context?: any}>): Promise<any[]> {
    console.log(chalk.cyan(`\n⚡ Executing ${tasks.length} tasks in parallel...\n`));
    
    const promises = tasks.map(({task, context}) => 
      this.delegateTask(task, context).catch(error => ({
        error: error.message,
        task
      }))
    );
    
    const results = await Promise.all(promises);
    
    const successful = results.filter(r => !r.error).length;
    console.log(chalk.green(`\n✅ Completed ${successful}/${tasks.length} tasks successfully\n`));
    
    return results;
  }

  // Swarm coordination for complex tasks
  async coordinateSwarm(
    masterTask: string,
    decompositionStrategy: 'auto' | 'by-file' | 'by-feature' = 'auto'
  ): Promise<void> {
    console.log(chalk.bold.cyan(`\n🐝 Coordinating agent swarm for: ${masterTask}\n`));
    
    // Decompose master task into subtasks
    const subtasks = await this.decomposeTask(masterTask, decompositionStrategy);
    console.log(chalk.yellow(`Decomposed into ${subtasks.length} subtasks`));
    
    // Create execution plan
    const plan = this.createSwarmExecutionPlan(subtasks);
    
    // Execute plan
    for (const phase of plan.phases) {
      console.log(chalk.blue(`\n▶ Phase ${phase.id}: ${phase.description}`));
      
      if (phase.parallel) {
        await this.executeParallelTasks(phase.tasks);
      } else {
        for (const task of phase.tasks) {
          await this.delegateTask(task.task, task.context);
        }
      }
    }
    
    console.log(chalk.bold.green(`\n✅ Swarm task completed!\n`));
  }

  // Task decomposition
  private async decomposeTask(
    task: string, 
    strategy: string
  ): Promise<Array<{task: string, context?: any}>> {
    // In real implementation, would use LLM for intelligent decomposition
    const subtasks: Array<{task: string, context?: any}> = [];
    
    if (strategy === 'by-file' || task.includes('refactor all')) {
      // Create subtask for each file
      for (const agent of this.agents.values()) {
        if (agent.config.assignedFiles) {
          for (const file of agent.config.assignedFiles) {
            subtasks.push({
              task: `${task} in ${file}`,
              context: { file }
            });
          }
        }
      }
    } else {
      // Simple decomposition
      subtasks.push(
        { task: `Analyze requirements for: ${task}` },
        { task: `Implement: ${task}` },
        { task: `Test: ${task}` },
        { task: `Document: ${task}` }
      );
    }
    
    return subtasks;
  }

  // Create execution plan for swarm
  private createSwarmExecutionPlan(subtasks: Array<{task: string, context?: any}>): {
    phases: Array<{
      id: number;
      description: string;
      parallel: boolean;
      tasks: Array<{task: string, context?: any}>;
    }>;
  } {
    // Group tasks into phases based on dependencies
    const phases = [];
    
    // Phase 1: Analysis (sequential)
    const analysisTasks = subtasks.filter(t => t.task.includes('Analyze'));
    if (analysisTasks.length > 0) {
      phases.push({
        id: 1,
        description: 'Analysis',
        parallel: false,
        tasks: analysisTasks
      });
    }
    
    // Phase 2: Implementation (parallel)
    const implementTasks = subtasks.filter(t => 
      t.task.includes('Implement') || t.task.includes('refactor')
    );
    if (implementTasks.length > 0) {
      phases.push({
        id: 2,
        description: 'Implementation',
        parallel: true,
        tasks: implementTasks
      });
    }
    
    // Phase 3: Testing (parallel)
    const testTasks = subtasks.filter(t => t.task.includes('Test'));
    if (testTasks.length > 0) {
      phases.push({
        id: 3,
        description: 'Testing',
        parallel: true,
        tasks: testTasks
      });
    }
    
    // Phase 4: Documentation (parallel)
    const docTasks = subtasks.filter(t => t.task.includes('Document'));
    if (docTasks.length > 0) {
      phases.push({
        id: 4,
        description: 'Documentation',
        parallel: true,
        tasks: docTasks
      });
    }
    
    return { phases };
  }

  // Helper methods
  private async discoverFiles(basePath: string): Promise<string[]> {
    const files: string[] = [];
    
    const walkDir = (dir: string) => {
      const entries = fs.readdirSync(dir);
      for (const entry of entries) {
        const fullPath = path.join(dir, entry);
        const stats = fs.statSync(fullPath);
        
        if (stats.isDirectory() && !entry.startsWith('.') && entry !== 'node_modules') {
          walkDir(fullPath);
        } else if (stats.isFile() && this.isCodeFile(entry)) {
          files.push(fullPath);
        }
      }
    };
    
    walkDir(basePath);
    return files;
  }

  private isCodeFile(filename: string): boolean {
    const extensions = ['.ts', '.js', '.tsx', '.jsx', '.py', '.java', '.go', '.rs'];
    return extensions.some(ext => filename.endsWith(ext));
  }

  private assignFilesToAgents(
    files: string[], 
    strategy: string
  ): Array<{files: string[]}> {
    const assignments: Array<{files: string[]}> = [];
    
    if (strategy === 'one-per-file') {
      // One agent per file
      for (const file of files) {
        assignments.push({ files: [file] });
      }
    } else if (strategy === 'one-per-directory') {
      // Group by directory
      const byDir = new Map<string, string[]>();
      for (const file of files) {
        const dir = path.dirname(file);
        if (!byDir.has(dir)) byDir.set(dir, []);
        byDir.get(dir)!.push(file);
      }
      for (const files of byDir.values()) {
        assignments.push({ files });
      }
    } else {
      // By complexity - simplified: just split evenly
      const agentCount = Math.min(files.length, 10); // Max 10 agents
      const filesPerAgent = Math.ceil(files.length / agentCount);
      
      for (let i = 0; i < files.length; i += filesPerAgent) {
        assignments.push({
          files: files.slice(i, i + filesPerAgent)
        });
      }
    }
    
    return assignments;
  }

  private getAgentCapabilities(type: AgentConfig['type']): string[] {
    switch (type) {
      case 'claude-code':
        return ['editing', 'refactoring', 'analysis', 'testing', 'documentation'];
      case 'aider':
        return ['editing', 'git', 'refactoring', 'testing'];
      case 'openhands':
        return ['editing', 'execution', 'browsing', 'complex-tasks'];
      default:
        return ['editing'];
    }
  }

  private calculateTotalConnections(): number {
    const n = this.agents.size;
    return (n * (n - 1)) / 2; // Complete graph connections
  }

  private getNextPort(): number {
    // Simple port allocation starting from 9000
    return 9000 + this.agents.size;
  }

  // Get network status
  getNetworkStatus(): {
    totalAgents: number;
    activeAgents: number;
    totalConnections: number;
    taskMetrics: {
      total: number;
      successful: number;
      failed: number;
      averageTime: number;
    };
  } {
    let totalTasks = 0;
    let totalTime = 0;
    let totalErrors = 0;
    
    for (const agent of this.agents.values()) {
      totalTasks += agent.metrics.tasksCompleted;
      totalErrors += agent.metrics.errors;
      totalTime += agent.metrics.averageTime * agent.metrics.tasksCompleted;
    }
    
    return {
      totalAgents: this.agents.size,
      activeAgents: Array.from(this.agents.values()).filter(a => a.status !== 'error').length,
      totalConnections: this.calculateTotalConnections(),
      taskMetrics: {
        total: totalTasks,
        successful: totalTasks - totalErrors,
        failed: totalErrors,
        averageTime: totalTasks > 0 ? totalTime / totalTasks : 0
      }
    };
  }

  // Cleanup
  async cleanup(): Promise<void> {
    console.log(chalk.yellow('\n🧹 Cleaning up agent network...\n'));
    
    for (const agent of this.agents.values()) {
      if (agent.mcpSession) {
        await this.mcpClient.disconnect(agent.mcpSession.id);
      }
      if (agent.process) {
        agent.process.kill();
      }
    }
    
    this.agents.clear();
    this.networkTopology.clear();
  }
}

// Helper to check if uvx is available
function hasUvx(): boolean {
  try {
    require('child_process').execSync('which uvx', { stdio: 'ignore' });
    return true;
  } catch {
    return false;
  }
}