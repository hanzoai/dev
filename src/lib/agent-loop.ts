import { EventEmitter } from 'events';
import chalk from 'chalk';
import { FunctionCallingSystem, FunctionCall } from './function-calling';
import { MCPClient, MCPSession } from './mcp-client';

export interface LLMProvider {
  name: string;
  type: 'openai' | 'anthropic' | 'local' | 'hanzo-app';
  apiKey?: string;
  baseUrl?: string;
  model: string;
  supportsTools: boolean;
  supportsStreaming: boolean;
}

export interface AgentLoopConfig {
  provider: LLMProvider;
  maxIterations: number;
  enableMCP: boolean;
  enableBrowser: boolean;
  enableSwarm: boolean;
  streamOutput: boolean;
  confirmActions: boolean;
}

export interface AgentMessage {
  role: 'system' | 'user' | 'assistant' | 'tool';
  content: string;
  toolCalls?: FunctionCall[];
  toolResults?: any[];
}

export class ConfigurableAgentLoop extends EventEmitter {
  public config: AgentLoopConfig;
  private functionCalling: FunctionCallingSystem;
  private mcpClient: MCPClient;
  private messages: AgentMessage[] = [];
  private iterations: number = 0;
  private context: any = {};
  private customTools: Map<string, any> = new Map();

  constructor(config: AgentLoopConfig) {
    super();
    this.config = config;
    this.functionCalling = new FunctionCallingSystem();
    this.mcpClient = new MCPClient();
  }

  // Get available LLM providers
  static getAvailableProviders(): LLMProvider[] {
    const providers: LLMProvider[] = [];

    // Check for API keys
    if (process.env.ANTHROPIC_API_KEY) {
      providers.push({
        name: 'Claude (Anthropic)',
        type: 'anthropic',
        apiKey: process.env.ANTHROPIC_API_KEY,
        model: 'claude-3-opus-20240229',
        supportsTools: true,
        supportsStreaming: true
      });
    }

    if (process.env.OPENAI_API_KEY) {
      providers.push({
        name: 'GPT-4 (OpenAI)',
        type: 'openai',
        apiKey: process.env.OPENAI_API_KEY,
        model: 'gpt-4-turbo-preview',
        supportsTools: true,
        supportsStreaming: true
      });
    }

    // Check for local Hanzo App
    if (process.env.HANZO_APP_URL || this.isHanzoAppRunning()) {
      providers.push({
        name: 'Hanzo Local AI',
        type: 'hanzo-app',
        baseUrl: process.env.HANZO_APP_URL || 'http://localhost:8080',
        model: 'hanzo-zen',
        supportsTools: true,
        supportsStreaming: false
      });
    }

    // Check for other local models
    if (process.env.LOCAL_LLM_URL) {
      providers.push({
        name: 'Local LLM',
        type: 'local',
        baseUrl: process.env.LOCAL_LLM_URL,
        model: process.env.LOCAL_LLM_MODEL || 'llama2',
        supportsTools: false,
        supportsStreaming: true
      });
    }

    return providers;
  }

  private static isHanzoAppRunning(): boolean {
    // Check if Hanzo App is running locally
    try {
      const { execSync } = require('child_process');
      execSync('curl -s http://localhost:8080/health', { stdio: 'ignore' });
      return true;
    } catch {
      return false;
    }
  }

  // Initialize tools and connections
  async initialize(): Promise<void> {
    console.log(chalk.cyan('\n🔧 Initializing agent loop...\n'));

    // Initialize MCP tools if enabled
    if (this.config.enableMCP) {
      await this.initializeMCPTools();
    }

    // Initialize browser connection if enabled
    if (this.config.enableBrowser) {
      await this.initializeBrowserTools();
    }

    // Initialize swarm if enabled
    if (this.config.enableSwarm) {
      await this.initializeSwarmTools();
    }

    console.log(chalk.green('✓ Agent loop initialized\n'));
  }

  private async initializeMCPTools(): Promise<void> {
    console.log(chalk.gray('Loading MCP tools...'));
    
    // Load configured MCP servers from config file
    const mcpConfig = await this.loadMCPConfig();
    
    for (const server of mcpConfig.servers) {
      try {
        console.log(chalk.gray(`  Connecting to ${server.name}...`));
        const session = await this.mcpClient.connect(server);
        await this.functionCalling.registerMCPServer(server.name, session);
        console.log(chalk.green(`  ✓ Connected to ${server.name} (${session.tools.length} tools)`));
      } catch (error) {
        console.log(chalk.yellow(`  ⚠ Failed to connect to ${server.name}`));
      }
    }
  }

  private async initializeBrowserTools(): Promise<void> {
    console.log(chalk.gray('Connecting to browser...'));
    
    // Check for Hanzo Browser Extension
    if (await this.checkBrowserExtension()) {
      console.log(chalk.green('  ✓ Connected to Hanzo Browser Extension'));
      this.registerBrowserTools('extension');
    }
    // Check for Hanzo Browser
    else if (await this.checkHanzoBrowser()) {
      console.log(chalk.green('  ✓ Connected to Hanzo Browser'));
      this.registerBrowserTools('browser');
    } else {
      console.log(chalk.yellow('  ⚠ No browser connection available'));
    }
  }

  private async checkBrowserExtension(): Promise<boolean> {
    // Check if browser extension is available via WebSocket
    try {
      const ws = new (require('ws'))('ws://localhost:9222/hanzo-extension');
      return new Promise((resolve) => {
        ws.on('open', () => {
          ws.close();
          resolve(true);
        });
        ws.on('error', () => resolve(false));
        setTimeout(() => {
          ws.close();
          resolve(false);
        }, 1000);
      });
    } catch {
      return false;
    }
  }

  private async checkHanzoBrowser(): Promise<boolean> {
    // Check if Hanzo Browser is running
    try {
      const response = await fetch('http://localhost:9223/status');
      return response.ok;
    } catch {
      return false;
    }
  }

  private registerBrowserTools(type: 'extension' | 'browser'): void {
    // Register browser automation tools
    const browserTools = [
      {
        name: 'browser_navigate',
        description: 'Navigate to a URL in the browser',
        parameters: {
          type: 'object',
          properties: {
            url: { type: 'string', description: 'URL to navigate to' }
          },
          required: ['url']
        },
        handler: async (args: any) => this.browserNavigate(args.url)
      },
      {
        name: 'browser_click',
        description: 'Click on an element in the browser',
        parameters: {
          type: 'object',
          properties: {
            selector: { type: 'string', description: 'CSS selector or element ID' }
          },
          required: ['selector']
        },
        handler: async (args: any) => this.browserClick(args.selector)
      },
      {
        name: 'browser_screenshot',
        description: 'Take a screenshot of the current page',
        parameters: {
          type: 'object',
          properties: {
            fullPage: { type: 'boolean', description: 'Capture full page' }
          }
        },
        handler: async (args: any) => this.browserScreenshot(args.fullPage)
      },
      {
        name: 'browser_fill',
        description: 'Fill a form field in the browser',
        parameters: {
          type: 'object',
          properties: {
            selector: { type: 'string', description: 'CSS selector' },
            value: { type: 'string', description: 'Value to fill' }
          },
          required: ['selector', 'value']
        },
        handler: async (args: any) => this.browserFill(args.selector, args.value)
      }
    ];

    browserTools.forEach(tool => this.functionCalling.registerTool(tool));
  }

  private async initializeSwarmTools(): Promise<void> {
    console.log(chalk.gray('Initializing swarm tools...'));
    
    // Register swarm coordination tools
    this.functionCalling.registerTool({
      name: 'spawn_agent',
      description: 'Spawn a new agent for a specific task',
      parameters: {
        type: 'object',
        properties: {
          task: { type: 'string', description: 'Task for the agent' },
          agentType: { type: 'string', enum: ['claude-code', 'aider', 'openhands'] }
        },
        required: ['task']
      },
      handler: async (args: any) => this.spawnAgent(args.task, args.agentType)
    });

    this.functionCalling.registerTool({
      name: 'delegate_to_swarm',
      description: 'Delegate multiple tasks to agent swarm',
      parameters: {
        type: 'object',
        properties: {
          tasks: {
            type: 'array',
            items: { type: 'string' },
            description: 'List of tasks to delegate'
          }
        },
        required: ['tasks']
      },
      handler: async (args: any) => this.delegateToSwarm(args.tasks)
    });
  }

  // Execute agent loop
  async execute(initialPrompt: string): Promise<void> {
    console.log(chalk.cyan(`\n🤖 Starting agent loop with ${this.config.provider.name}\n`));
    
    // Add system message
    this.messages.push({
      role: 'system',
      content: this.getSystemPrompt()
    });

    // Add user message
    this.messages.push({
      role: 'user',
      content: initialPrompt
    });

    // Main agent loop
    while (this.iterations < this.config.maxIterations) {
      this.iterations++;
      console.log(chalk.blue(`\n▶ Iteration ${this.iterations}`));

      try {
        // Get LLM response
        const response = await this.callLLM();
        
        // Process response
        if (response.toolCalls && response.toolCalls.length > 0) {
          // Execute tool calls
          const results = await this.executeToolCalls(response.toolCalls);
          
          // Add tool results to messages
          this.messages.push({
            role: 'tool',
            content: JSON.stringify(results),
            toolResults: results
          });
        } else {
          // No more tool calls, task complete
          console.log(chalk.green('\n✅ Task completed'));
          break;
        }
      } catch (error) {
        console.error(chalk.red(`Error in iteration ${this.iterations}: ${error}`));
        this.emit('error', error);
        break;
      }
    }

    if (this.iterations >= this.config.maxIterations) {
      console.log(chalk.yellow('\n⚠ Maximum iterations reached'));
    }
  }

  private getSystemPrompt(): string {
    const tools = this.functionCalling.getAvailableTools();
    
    return `You are an AI assistant with access to various tools. You can:
- Edit files using view_file, create_file, str_replace
- Run commands using run_command
- Search files using search_files
- List directories using list_directory
${this.config.enableBrowser ? '- Control the browser using browser_* tools' : ''}
${this.config.enableSwarm ? '- Spawn agents and delegate tasks using swarm tools' : ''}
${this.config.enableMCP ? '- Use MCP tools for extended functionality' : ''}

Available tools: ${tools.map(t => t.name).join(', ')}

Always use tools to accomplish tasks. Think step by step.`;
  }

  private async callLLM(): Promise<AgentMessage> {
    const { provider } = this.config;
    
    switch (provider.type) {
      case 'anthropic':
        return this.callAnthropic();
      case 'openai':
        return this.callOpenAI();
      case 'hanzo-app':
        return this.callHanzoApp();
      case 'local':
        return this.callLocalLLM();
      default:
        throw new Error(`Unknown provider type: ${provider.type}`);
    }
  }

  private async callAnthropic(): Promise<AgentMessage> {
    // Implementation for Anthropic Claude
    console.log(chalk.gray('Calling Claude...'));
    
    // This is a simplified version - in production you'd use the actual API
    const tools = this.functionCalling.getAllToolSchemas();
    
    // Simulate API call
    const response = {
      role: 'assistant' as const,
      content: 'I will help you with this task.',
      toolCalls: [
        {
          id: 'call_1',
          name: 'view_file',
          arguments: { path: 'package.json' }
        }
      ]
    };
    
    this.messages.push(response);
    return response;
  }

  private async callOpenAI(): Promise<AgentMessage> {
    // Implementation for OpenAI
    console.log(chalk.gray('Calling GPT-4...'));
    
    // Similar to Anthropic but with OpenAI API format
    const response = {
      role: 'assistant' as const,
      content: 'I will analyze and complete this task.',
      toolCalls: []
    };
    
    this.messages.push(response);
    return response;
  }

  private async callHanzoApp(): Promise<AgentMessage> {
    // Implementation for local Hanzo App
    console.log(chalk.gray('Calling Hanzo Local AI...'));
    
    const response = await fetch(`${this.config.provider.baseUrl}/chat`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        messages: this.messages,
        tools: this.functionCalling.getAllToolSchemas()
      })
    });
    
    const data = await response.json();
    this.messages.push(data);
    return data;
  }

  private async callLocalLLM(): Promise<AgentMessage> {
    // Implementation for generic local LLM
    console.log(chalk.gray('Calling Local LLM...'));
    
    // Local LLMs might not support tools, so we use a different approach
    const response = {
      role: 'assistant' as const,
      content: 'Processing your request...',
      toolCalls: []
    };
    
    this.messages.push(response);
    return response;
  }

  private async executeToolCalls(calls: FunctionCall[]): Promise<any[]> {
    if (this.config.confirmActions) {
      console.log(chalk.yellow('\n⚠ Tool calls requested:'));
      calls.forEach(call => {
        console.log(`  - ${call.name}(${JSON.stringify(call.arguments)})`);
      });
      
      const { confirm } = await require('inquirer').prompt([{
        type: 'confirm',
        name: 'confirm',
        message: 'Execute these actions?',
        default: true
      }]);
      
      if (!confirm) {
        throw new Error('User cancelled tool execution');
      }
    }
    
    return this.functionCalling.callFunctions(calls);
  }

  // Browser tool implementations
  private async browserNavigate(url: string): Promise<any> {
    console.log(chalk.gray(`Navigating to ${url}...`));
    // Implementation would connect to browser
    return { success: true, url };
  }

  private async browserClick(selector: string): Promise<any> {
    console.log(chalk.gray(`Clicking ${selector}...`));
    return { success: true, selector };
  }

  private async browserScreenshot(fullPage: boolean = false): Promise<any> {
    console.log(chalk.gray(`Taking screenshot (fullPage: ${fullPage})...`));
    return { success: true, screenshot: 'base64_image_data' };
  }

  private async browserFill(selector: string, value: string): Promise<any> {
    console.log(chalk.gray(`Filling ${selector} with "${value}"...`));
    return { success: true, selector, value };
  }

  // Swarm tool implementations
  private async spawnAgent(task: string, agentType?: string): Promise<any> {
    console.log(chalk.gray(`Spawning ${agentType || 'default'} agent for: ${task}`));
    return { success: true, agentId: `agent-${Date.now()}`, task };
  }

  private async delegateToSwarm(tasks: string[]): Promise<any> {
    console.log(chalk.gray(`Delegating ${tasks.length} tasks to swarm...`));
    return { success: true, tasks, status: 'delegated' };
  }

  // Load MCP configuration
  private async loadMCPConfig(): Promise<{ servers: any[] }> {
    const fs = require('fs');
    const path = require('path');
    const os = require('os');
    
    // Check multiple locations for MCP config
    const configPaths = [
      path.join(process.cwd(), '.mcp.json'),
      path.join(os.homedir(), '.config', 'hanzo-dev', 'mcp.json'),
      path.join(os.homedir(), '.hanzo', 'mcp.json')
    ];
    
    for (const configPath of configPaths) {
      if (fs.existsSync(configPath)) {
        const content = fs.readFileSync(configPath, 'utf-8');
        return JSON.parse(content);
      }
    }
    
    // Default MCP servers
    return {
      servers: [
        {
          name: 'filesystem',
          command: 'npx',
          args: ['@modelcontextprotocol/server-filesystem'],
          env: { MCP_ALLOWED_PATHS: process.cwd() }
        },
        {
          name: 'git',
          command: 'npx',
          args: ['@modelcontextprotocol/server-git']
        }
      ]
    };
  }

  // Get current configuration
  getConfig(): AgentLoopConfig {
    return this.config;
  }

  // Update configuration
  updateConfig(updates: Partial<AgentLoopConfig>): void {
    this.config = { ...this.config, ...updates };
  }

  // Add custom tool
  addTool(tool: {
    name: string;
    description: string;
    parameters: any;
    execute: (params: any) => Promise<any>;
  }): void {
    this.customTools.set(tool.name, tool);
    this.functionCalling.registerFunction(
      tool.name,
      tool.execute,
      tool.description,
      tool.parameters
    );
  }

  // Get conversation messages
  getMessages(): AgentMessage[] {
    return [...this.messages];
  }

  // Get current context
  getContext(): any {
    return { ...this.context };
  }

  // Load session data
  loadSession(messages: AgentMessage[], context: any): void {
    this.messages = [...messages];
    this.context = { ...context };
  }

  // Process a user message
  async processMessage(content: string): Promise<string> {
    // Add user message
    const userMessage: AgentMessage = {
      role: 'user',
      content
    };
    this.messages.push(userMessage);

    // Process through agent loop
    try {
      const response = await this.executeIteration();
      return response.content;
    } catch (error) {
      console.error(chalk.red(`Error processing message: ${error}`));
      throw error;
    }
  }

  // Execute one iteration of the agent loop
  private async executeIteration(): Promise<AgentMessage> {
    this.iterations++;
    
    // Call appropriate LLM
    let response: AgentMessage;
    switch (this.config.provider.type) {
      case 'anthropic':
        response = await this.callAnthropic();
        break;
      case 'openai':
        response = await this.callOpenAI();
        break;
      case 'hanzo-app':
        response = await this.callHanzoApp();
        break;
      case 'local':
      default:
        response = await this.callLocalLLM();
        break;
    }

    // Execute any tool calls
    if (response.toolCalls && response.toolCalls.length > 0) {
      const toolResults = await this.executeToolCalls(response.toolCalls);
      response.toolResults = toolResults;
      
      // Add tool results as a message
      this.messages.push({
        role: 'tool',
        content: JSON.stringify(toolResults),
        toolResults
      });
    }

    return response;
  }
}