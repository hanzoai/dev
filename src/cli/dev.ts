#!/usr/bin/env node
import { Command } from 'commander';
import chalk from 'chalk';
import inquirer from 'inquirer';
import { spawn, execSync } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { FileEditor } from '../lib/editor';
import { MCPClient, DEFAULT_MCP_SERVERS } from '../lib/mcp-client';
import { FunctionCallingSystem } from '../lib/function-calling';
import { ConfigManager } from '../lib/config';
import { CodeActAgent } from '../lib/code-act-agent';
import { UnifiedWorkspace, WorkspaceSession } from '../lib/unified-workspace';
import { PeerAgentNetwork } from '../lib/peer-agent-network';
import { BenchmarkRunner, BenchmarkConfig } from '../lib/benchmark-runner';
import { ConfigurableAgentLoop, LLMProvider } from '../lib/agent-loop';
import { SwarmRunner, SwarmOptions } from '../lib/swarm-runner';
import { InteractiveAgent } from '../lib/interactive-agent';

const program = new Command();

// Load environment variables from .env files
function loadEnvFiles(): void {
  const envFiles = ['.env', '.env.local', '.env.development', '.env.production'];
  const cwd = process.cwd();
  
  envFiles.forEach(file => {
    const filePath = path.join(cwd, file);
    if (fs.existsSync(filePath)) {
      const content = fs.readFileSync(filePath, 'utf-8');
      content.split('\n').forEach(line => {
        const match = line.match(/^([^=]+)=(.*)$/);
        if (match) {
          const key = match[1].trim();
          const value = match[2].trim();
          if (!process.env[key]) {
            process.env[key] = value;
          }
        }
      });
    }
  });
}

// Load env files on startup
loadEnvFiles();

// Check if uvx is available
function hasUvx(): boolean {
  try {
    execSync('which uvx', { stdio: 'ignore' });
    return true;
  } catch {
    return false;
  }
}

// Available tools configuration
const TOOLS = {
  'hanzo-dev': {
    name: 'Hanzo Dev (OpenHands)',
    command: hasUvx() ? 'uvx hanzo-dev' : 'hanzo-dev',
    checkCommand: hasUvx() ? 'which uvx' : 'which hanzo-dev',
    description: 'Hanzo AI software development agent - Full featured dev environment',
    color: chalk.magenta,
    apiKeys: ['OPENAI_API_KEY', 'ANTHROPIC_API_KEY', 'LLM_API_KEY', 'HANZO_API_KEY'],
    priority: 1,
    isDefault: true
  },
  claude: {
    name: 'Claude (Anthropic)',
    command: 'claude-code',
    checkCommand: 'which claude-code',
    description: 'Claude Code - AI coding assistant',
    color: chalk.blue,
    apiKeys: ['ANTHROPIC_API_KEY', 'CLAUDE_API_KEY'],
    priority: 2
  },
  aider: {
    name: 'Aider',
    command: 'aider',
    checkCommand: 'which aider',
    description: 'AI pair programming in your terminal',
    color: chalk.green,
    apiKeys: ['OPENAI_API_KEY', 'ANTHROPIC_API_KEY', 'CLAUDE_API_KEY'],
    priority: 3
  },
  gemini: {
    name: 'Gemini (Google)',
    command: 'gemini',
    checkCommand: 'which gemini',
    description: 'Google Gemini AI assistant',
    color: chalk.yellow,
    apiKeys: ['GOOGLE_API_KEY', 'GEMINI_API_KEY'],
    priority: 4
  },
  codex: {
    name: 'OpenAI Codex',
    command: 'codex',
    checkCommand: 'which codex',
    description: 'OpenAI coding assistant',
    color: chalk.cyan,
    apiKeys: ['OPENAI_API_KEY'],
    priority: 5
  }
};

// Check if a tool has API keys configured
function hasApiKey(tool: string): boolean {
  const toolConfig = TOOLS[tool as keyof typeof TOOLS];
  if (!toolConfig || !toolConfig.apiKeys) return false;
  
  return toolConfig.apiKeys.some(key => !!process.env[key]);
}

// Check if a tool is installed
async function isToolInstalled(tool: string): Promise<boolean> {
  return new Promise((resolve) => {
    const checkCmd = TOOLS[tool as keyof typeof TOOLS]?.checkCommand || `which ${tool}`;
    const check = spawn('sh', ['-c', checkCmd]);
    check.on('close', (code) => {
      resolve(code === 0);
    });
  });
}

// Get list of available tools
async function getAvailableTools(): Promise<string[]> {
  const available: string[] = [];
  for (const toolKey of Object.keys(TOOLS)) {
    const isInstalled = await isToolInstalled(toolKey);
    const hasKey = hasApiKey(toolKey);
    if (isInstalled || hasKey) {
      available.push(toolKey);
    }
  }
  return available.sort((a, b) => {
    const priorityA = TOOLS[a as keyof typeof TOOLS].priority;
    const priorityB = TOOLS[b as keyof typeof TOOLS].priority;
    return priorityA - priorityB;
  });
}

// Get default tool
async function getDefaultTool(): Promise<string | null> {
  const availableTools = await getAvailableTools();
  if (availableTools.length === 0) return null;
  
  if (availableTools.includes('hanzo-dev')) {
    return 'hanzo-dev';
  }
  
  for (const tool of availableTools) {
    if (await isToolInstalled(tool) && hasApiKey(tool)) {
      return tool;
    }
  }
  
  return availableTools[0];
}

// Run a tool
function runTool(tool: string, args: string[] = []): void {
  const toolConfig = TOOLS[tool as keyof typeof TOOLS];
  if (!toolConfig) {
    console.error(chalk.red(`Unknown tool: ${tool}`));
    process.exit(1);
  }

  console.log(toolConfig.color(`\n🚀 Launching ${toolConfig.name}...\n`));
  
  if (tool === 'hanzo-dev' && hasUvx()) {
    console.log(chalk.gray('Using uvx to run hanzo-dev...'));
  }
  
  const child = spawn(toolConfig.command, args, {
    stdio: 'inherit',
    shell: true,
    env: process.env
  });

  child.on('error', (error) => {
    console.error(chalk.red(`Failed to start ${toolConfig.name}: ${error.message}`));
    
    if (tool === 'hanzo-dev') {
      console.log(chalk.yellow('\nTo install hanzo-dev:'));
      console.log(chalk.gray('  pip install hanzo-dev'));
      console.log(chalk.gray('  # or'));
      console.log(chalk.gray('  uvx hanzo-dev  # (if you have uv installed)'));
    }
    process.exit(1);
  });

  child.on('exit', (code) => {
    if (code !== 0) {
      console.error(chalk.red(`${toolConfig.name} exited with code ${code}`));
    }
    process.exit(code || 0);
  });
}

// Interactive editing mode using built-in editor
async function interactiveEditMode(): Promise<void> {
  const editor = new FileEditor();
  const functionCalling = new FunctionCallingSystem();
  const mcpClient = new MCPClient();
  
  console.log(chalk.bold.cyan('\n📝 Hanzo Dev Editor - Interactive Mode\n'));
  console.log(chalk.gray('Commands: view, create, str_replace, insert, undo_edit, run, list, search, mcp, help, exit\n'));

  // Connect to default MCP servers if available
  for (const serverConfig of DEFAULT_MCP_SERVERS) {
    try {
      console.log(chalk.gray(`Connecting to MCP server: ${serverConfig.name}...`));
      const session = await mcpClient.connect(serverConfig);
      await functionCalling.registerMCPServer(serverConfig.name, session);
      console.log(chalk.green(`✓ Connected to ${serverConfig.name}`));
    } catch (error) {
      console.log(chalk.yellow(`⚠ Could not connect to ${serverConfig.name}`));
    }
  }

  while (true) {
    const { command } = await inquirer.prompt([{
      type: 'input',
      name: 'command',
      message: chalk.green('editor>'),
      prefix: ''
    }]);

    if (command === 'exit' || command === 'quit') {
      break;
    }

    if (command === 'help') {
      console.log(chalk.cyan('\nAvailable commands:'));
      console.log('  view <file> [start] [end]  - View file contents');
      console.log('  create <file>              - Create new file');
      console.log('  str_replace <file>         - Replace string in file');
      console.log('  insert <file> <line>       - Insert line in file');
      console.log('  undo_edit <file>           - Undo last edit');
      console.log('  run <command>              - Run shell command');
      console.log('  list <directory>           - List directory contents');
      console.log('  search <pattern> [path]    - Search for files');
      console.log('  mcp                        - List MCP tools');
      console.log('  tools                      - List all available tools');
      console.log('  help                       - Show this help');
      console.log('  exit                       - Exit editor\n');
      continue;
    }

    if (command === 'tools') {
      const tools = functionCalling.getAvailableTools();
      console.log(chalk.cyan('\nAvailable tools:'));
      tools.forEach(tool => {
        console.log(`  ${chalk.yellow(tool.name)} - ${tool.description}`);
      });
      console.log();
      continue;
    }

    if (command === 'mcp') {
      const sessions = mcpClient.getAllSessions();
      console.log(chalk.cyan('\nMCP Sessions:'));
      sessions.forEach(session => {
        console.log(`  ${chalk.yellow(session.id)}:`);
        session.tools.forEach(tool => {
          console.log(`    - ${tool.name}: ${tool.description}`);
        });
      });
      console.log();
      continue;
    }

    // Parse and execute commands
    const parts = command.split(' ');
    const cmd = parts[0];
    
    try {
      let result;
      
      switch (cmd) {
        case 'view':
          result = await editor.execute({
            command: 'view',
            path: parts[1],
            startLine: parts[2] ? parseInt(parts[2]) : undefined,
            endLine: parts[3] ? parseInt(parts[3]) : undefined
          });
          break;
          
        case 'create':
          const { content } = await inquirer.prompt([{
            type: 'editor',
            name: 'content',
            message: 'Enter file content:'
          }]);
          result = await editor.execute({
            command: 'create',
            path: parts[1],
            content
          });
          break;
          
        case 'str_replace':
          const { oldStr } = await inquirer.prompt([{
            type: 'input',
            name: 'oldStr',
            message: 'String to replace:'
          }]);
          const { newStr } = await inquirer.prompt([{
            type: 'input',
            name: 'newStr',
            message: 'Replacement string:'
          }]);
          result = await editor.execute({
            command: 'str_replace',
            path: parts[1],
            oldStr,
            newStr
          });
          break;
          
        case 'insert':
          const { lineContent } = await inquirer.prompt([{
            type: 'input',
            name: 'lineContent',
            message: 'Line content:'
          }]);
          result = await editor.execute({
            command: 'insert',
            path: parts[1],
            lineNumber: parseInt(parts[2]),
            content: lineContent
          });
          break;
          
        case 'undo_edit':
          result = await editor.execute({
            command: 'undo_edit',
            path: parts[1]
          });
          break;
          
        case 'run':
          const runCommand = parts.slice(1).join(' ');
          result = await functionCalling.callFunction({
            id: Date.now().toString(),
            name: 'run_command',
            arguments: { command: runCommand }
          });
          break;
          
        case 'list':
          result = await functionCalling.callFunction({
            id: Date.now().toString(),
            name: 'list_directory',
            arguments: { path: parts[1] || '.' }
          });
          break;
          
        case 'search':
          result = await functionCalling.callFunction({
            id: Date.now().toString(),
            name: 'search_files',
            arguments: { 
              pattern: parts[1],
              path: parts[2] || '.',
              regex: false
            }
          });
          break;
          
        default:
          console.log(chalk.red(`Unknown command: ${cmd}`));
          continue;
      }
      
      if (result) {
        if (result.success || result.result?.success) {
          console.log(chalk.green('✓'), result.message || 'Success');
          if (result.content || result.result?.stdout) {
            console.log(result.content || result.result.stdout);
          }
          if (result.result?.files) {
            result.result.files.forEach((file: any) => {
              const icon = file.type === 'directory' ? '📁' : '📄';
              console.log(`  ${icon} ${file.name}`);
            });
          }
          if (result.result?.matches) {
            console.log(`Found ${result.result.total} matches:`);
            result.result.matches.forEach((match: string) => {
              console.log(`  📄 ${match}`);
            });
          }
        } else {
          console.log(chalk.red('✗'), result.message || result.error || 'Error');
          if (result.result?.stderr) {
            console.log(chalk.red(result.result.stderr));
          }
        }
      }
    } catch (error) {
      console.log(chalk.red('Error:'), error);
    }
  }

  console.log(chalk.gray('\nExiting editor mode...'));
}

// Interactive mode for tool selection
async function interactiveMode(): Promise<void> {
  console.log(chalk.bold.cyan('\n🤖 Hanzo Dev - AI Development Assistant\n'));
  
  const envFiles = ['.env', '.env.local', '.env.development', '.env.production'];
  const detectedEnvFiles = envFiles.filter(file => fs.existsSync(path.join(process.cwd(), file)));
  if (detectedEnvFiles.length > 0) {
    console.log(chalk.gray('📄 Detected environment files:'));
    detectedEnvFiles.forEach(file => {
      console.log(chalk.gray(`   - ${file}`));
    });
    console.log();
  }
  
  const availableTools = await getAvailableTools();
  const defaultTool = await getDefaultTool();
  
  if (availableTools.length === 0 && !hasApiKey('hanzo-dev')) {
    console.log(chalk.yellow('No AI tools available. Please either:'));
    console.log(chalk.yellow('\n1. Install hanzo-dev (recommended):'));
    console.log(chalk.gray('   pip install hanzo-dev'));
    console.log(chalk.gray('   # or'));
    console.log(chalk.gray('   uvx hanzo-dev'));
    
    console.log(chalk.yellow('\n2. Install other tools:'));
    console.log(chalk.gray('   npm install -g @hanzo/claude-code'));
    console.log(chalk.gray('   pip install aider-chat'));
    
    console.log(chalk.yellow('\n3. Or configure API keys in your .env file:'));
    console.log(chalk.gray('   ANTHROPIC_API_KEY=sk-ant-...'));
    console.log(chalk.gray('   OPENAI_API_KEY=sk-...'));
    process.exit(1);
  }

  // Add built-in editor option
  const choices = [
    {
      name: chalk.bold.yellow('🔧 Built-in Editor - Interactive file editing and MCP tools'),
      value: 'builtin-editor',
      short: 'Built-in Editor'
    }
  ];

  // Add external tools
  for (const tool of availableTools) {
    const toolConfig = TOOLS[tool as keyof typeof TOOLS];
    const isInstalled = await isToolInstalled(tool);
    const hasKey = hasApiKey(tool);
    
    let status = '';
    if (isInstalled && hasKey) {
      status = chalk.green(' [Installed + API Key]');
    } else if (isInstalled) {
      status = chalk.yellow(' [Installed]');
    } else if (hasKey) {
      status = chalk.cyan(' [API Key Only]');
    }
    
    if (toolConfig.isDefault) {
      status += chalk.bold.magenta(' ★ DEFAULT');
    }
    
    choices.push({
      name: `${toolConfig.name} - ${toolConfig.description}${status}`,
      value: tool,
      short: toolConfig.name
    });
  }

  const { selectedTool } = await inquirer.prompt([
    {
      type: 'list',
      name: 'selectedTool',
      message: 'Select a tool to launch:',
      choices: choices,
      default: defaultTool || 'builtin-editor'
    }
  ]);

  if (selectedTool === 'builtin-editor') {
    await interactiveEditMode();
    return;
  }

  const { passDirectory } = await inquirer.prompt([
    {
      type: 'confirm',
      name: 'passDirectory',
      message: `Open in current directory (${process.cwd()})?`,
      default: true
    }
  ]);

  const args = passDirectory ? ['.'] : [];
  runTool(selectedTool, args);
}

// Setup version
const packagePath = path.join(__dirname, '../../package.json');
let version = '2.0.0';
try {
  const packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf-8'));
  version = packageJson.version;
} catch (error) {
  // Use default version
}

program
  .name('dev')
  .description('Hanzo Dev - Meta AI development CLI with built-in editor and tool orchestration')
  .version(version);

// Built-in editor command
program
  .command('edit [path]')
  .description('Launch built-in editor with file editing and MCP tools')
  .action(async (path) => {
    if (path && fs.existsSync(path)) {
      process.chdir(path);
    }
    await interactiveEditMode();
  });

// External tool commands
Object.entries(TOOLS).forEach(([toolKey, toolConfig]) => {
  if (toolKey === 'hanzo-dev') {
    // Special alias for hanzo-dev
    program
      .command('python [args...]')
      .description('Launch Python hanzo-dev (OpenHands)')
      .action(async (args) => {
        const isInstalled = await isToolInstalled('hanzo-dev');
        if (!isInstalled && !hasUvx()) {
          console.error(chalk.red('Hanzo Dev is not installed.'));
          console.log(chalk.yellow('\nTo install:'));
          console.log(chalk.gray('  pip install hanzo-dev'));
          console.log(chalk.gray('  # or'));
          console.log(chalk.gray('  pip install uv && uvx hanzo-dev'));
          process.exit(1);
        }
        runTool('hanzo-dev', args);
      });
  }
  
  program
    .command(`${toolKey} [args...]`)
    .description(`Launch ${toolConfig.name}`)
    .action(async (args) => {
      const isInstalled = await isToolInstalled(toolKey);
      const hasKey = hasApiKey(toolKey);
      
      if (!isInstalled && !hasKey) {
        console.error(chalk.red(`${toolConfig.name} is not available.`));
        process.exit(1);
      }
      
      runTool(toolKey, args);
    });
});

// List command
program
  .command('list')
  .alias('ls')
  .description('List all available AI tools and API keys')
  .action(async () => {
    console.log(chalk.bold.cyan('\n📋 AI Tools Status:\n'));
    
    const envFiles = ['.env', '.env.local', '.env.development', '.env.production'];
    const detectedEnvFiles = envFiles.filter(file => fs.existsSync(path.join(process.cwd(), file)));
    if (detectedEnvFiles.length > 0) {
      console.log(chalk.bold('Environment files:'));
      detectedEnvFiles.forEach(file => {
        console.log(chalk.gray(`  📄 ${file}`));
      });
      console.log();
    }
    
    console.log(chalk.bold('Built-in Features:'));
    console.log(chalk.green('  ✓ Interactive Editor') + chalk.gray(' - File editing with view, create, str_replace'));
    console.log(chalk.green('  ✓ MCP Client') + chalk.gray(' - Model Context Protocol tool integration'));
    console.log(chalk.green('  ✓ Function Calling') + chalk.gray(' - Unified tool interface'));
    console.log();
    
    if (hasUvx()) {
      console.log(chalk.bold('Package Manager:'));
      console.log(chalk.green('  ✓ uvx available (can run Python tools without installation)'));
      console.log();
    }
    
    console.log(chalk.bold('External Tools:'));
    for (const [toolKey, toolConfig] of Object.entries(TOOLS)) {
      const isInstalled = await isToolInstalled(toolKey);
      const hasKey = hasApiKey(toolKey);
      
      let status = chalk.red('✗ Not Available');
      if (toolKey === 'hanzo-dev' && hasUvx()) {
        status = chalk.green('✓ Ready (via uvx)');
      } else if (isInstalled && hasKey) {
        status = chalk.green('✓ Ready (Installed + API Key)');
      } else if (isInstalled) {
        status = chalk.yellow('⚠ Installed (No API Key)');
      } else if (hasKey) {
        status = chalk.cyan('☁ API Mode (Not Installed)');
      }
      
      let displayName = toolConfig.color(toolConfig.name);
      if (toolConfig.isDefault) {
        displayName += chalk.bold.magenta(' ★');
      }
      
      console.log(`  ${status} ${displayName}`);
      console.log(chalk.gray(`     ${toolConfig.description}`));
    }
  });

// Status command
program
  .command('status')
  .description('Show current working directory and environment')
  .action(() => {
    const config = new ConfigManager();
    
    console.log(chalk.bold.cyan('\n📊 Hanzo Dev Status\n'));
    console.log(`Current Directory: ${chalk.green(process.cwd())}`);
    console.log(`User: ${chalk.green(os.userInfo().username)}`);
    console.log(`Node Version: ${chalk.green(process.version)}`);
    console.log(`Platform: ${chalk.green(os.platform())}`);
    console.log(`Dev Version: ${chalk.green(version)}`);
    
    if (hasUvx()) {
      console.log(`UV/UVX: ${chalk.green('✓ Available')}`);
    }
    
    const envFiles = ['.env', '.env.local', '.env.development', '.env.production'];
    const detectedEnvFiles = envFiles.filter(file => fs.existsSync(path.join(process.cwd(), file)));
    if (detectedEnvFiles.length > 0) {
      console.log(`\nEnvironment Files:`);
      detectedEnvFiles.forEach(file => {
        console.log(chalk.green(`  ✓ ${file}`));
      });
    }
    
    console.log(`\nConfiguration:`);
    const cfg = config.getConfig();
    console.log(`  Default Agent: ${chalk.yellow(cfg.defaultAgent)}`);
    console.log(`  Runtime: ${chalk.yellow(cfg.runtime || 'cli')}`);
    console.log(`  Confirmation Mode: ${chalk.yellow(cfg.security.confirmationMode ? 'On' : 'Off')}`);
  });

// Workspace command - unified workspace with shell, editor, browser, planner
program
  .command('workspace')
  .alias('ws')
  .description('Launch unified workspace with shell, editor, browser, and planner')
  .action(async () => {
    const session = new WorkspaceSession();
    await session.start();
  });

// Swarm command - spawn multiple agents for parallel work
program
  .command('swarm [path]')
  .description('Spawn agent swarm for codebase (one agent per file/directory)')
  .option('-t, --type <type>', 'Agent type (claude-code, aider, openhands)', 'claude-code')
  .option('-s, --strategy <strategy>', 'Assignment strategy (one-per-file, one-per-directory, by-complexity)', 'one-per-file')
  .action(async (path, options) => {
    const targetPath = path || process.cwd();
    const network = new PeerAgentNetwork();
    
    try {
      await network.spawnAgentsForCodebase(targetPath, options.type, options.strategy);
      
      // Show network status
      const status = network.getNetworkStatus();
      console.log(chalk.cyan('\n📊 Network Status:'));
      console.log(`  Agents: ${status.totalAgents} (${status.activeAgents} active)`);
      console.log(`  Connections: ${status.totalConnections}`);
      
      // Interactive swarm control
      const { action } = await inquirer.prompt([{
        type: 'list',
        name: 'action',
        message: 'What would you like to do?',
        choices: [
          { name: 'Execute task on swarm', value: 'task' },
          { name: 'Run parallel tasks', value: 'parallel' },
          { name: 'Show network status', value: 'status' },
          { name: 'Exit', value: 'exit' }
        ]
      }]);
      
      if (action === 'task') {
        const { task } = await inquirer.prompt([{
          type: 'input',
          name: 'task',
          message: 'Enter task for swarm:'
        }]);
        
        await network.coordinateSwarm(task);
      } else if (action === 'parallel') {
        console.log(chalk.yellow('Enter tasks (one per line, empty line to finish):'));
        const tasks: Array<{task: string}> = [];
        
        while (true) {
          const { task } = await inquirer.prompt([{
            type: 'input',
            name: 'task',
            message: '>'
          }]);
          
          if (!task) break;
          tasks.push({ task });
        }
        
        if (tasks.length > 0) {
          await network.executeParallelTasks(tasks);
        }
      } else if (action === 'status') {
        const status = network.getNetworkStatus();
        console.log(chalk.cyan('\n📊 Detailed Network Status:'));
        console.log(JSON.stringify(status, null, 2));
      }
      
      await network.cleanup();
    } catch (error) {
      console.error(chalk.red(`Swarm error: ${error}`));
      await network.cleanup();
    }
  });

// Agent command - run a task with CodeAct agent
program
  .command('agent <task>')
  .description('Execute a task using CodeAct agent with automatic planning and error correction')
  .action(async (task) => {
    const agent = new CodeActAgent();
    try {
      await agent.executeTask(task);
    } catch (error) {
      console.error(chalk.red(`Agent error: ${error}`));
    }
  });

// Benchmark command - run SWE-bench evaluation
program
  .command('benchmark')
  .alias('bench')
  .description('Run SWE-bench evaluation to measure performance')
  .option('-d, --dataset <dataset>', 'Dataset to use (swe-bench, swe-bench-lite, custom)', 'swe-bench-lite')
  .option('-a, --agents <number>', 'Number of agents for parallel execution', '5')
  .option('-p, --parallel', 'Run tasks in parallel', true)
  .option('-t, --timeout <ms>', 'Timeout per task in milliseconds', '300000')
  .option('-o, --output <file>', 'Output file for results', 'benchmark-results.json')
  .option('--provider <provider>', 'LLM provider (claude, openai, gemini, local)')
  .option('--max-tasks <number>', 'Maximum number of tasks to run')
  .action(async (options) => {
    console.log(chalk.bold.cyan('\n🏃 Starting Hanzo Dev Benchmark\n'));
    
    // Parse options
    const config: BenchmarkConfig = {
      dataset: options.dataset as any,
      agents: parseInt(options.agents),
      parallel: options.parallel !== 'false',
      timeout: parseInt(options.timeout),
      output: options.output,
      maxTasks: options.maxTasks ? parseInt(options.maxTasks) : undefined
    };

    // Set provider if specified
    if (options.provider) {
      const providers = ConfigurableAgentLoop.getAvailableProviders();
      const provider = providers.find(p => 
        p.type === options.provider || 
        p.name.toLowerCase().includes(options.provider.toLowerCase())
      );
      
      if (provider) {
        config.provider = provider;
      } else {
        console.error(chalk.red(`Provider '${options.provider}' not found or not configured`));
        console.log(chalk.yellow('\nAvailable providers:'));
        providers.forEach(p => {
          console.log(`  - ${p.name} (${p.type})`);
        });
        process.exit(1);
      }
    }

    // Run benchmark
    const runner = new BenchmarkRunner(config);
    
    try {
      await runner.run();
      console.log(chalk.green('\n✅ Benchmark completed successfully'));
    } catch (error) {
      console.error(chalk.red(`\n❌ Benchmark failed: ${error}`));
      process.exit(1);
    }
  });

// Add global options for provider and swarm
program
  .option('--claude', 'Use Claude AI provider')
  .option('--openai', 'Use OpenAI provider') 
  .option('--gemini', 'Use Gemini provider')
  .option('--grok', 'Use Grok provider')
  .option('--local', 'Use local AI provider')
  .option('--swarm <count>', 'Launch swarm of agents (up to 100)')
  .option('-p, --prompt <prompt>', 'Task prompt for agents');

// Swarm mode function
async function runSwarmMode(options: any): Promise<void> {
  // Determine provider
  let provider: SwarmOptions['provider'] = 'claude';
  if (options.claude) provider = 'claude';
  else if (options.openai) provider = 'openai';
  else if (options.gemini) provider = 'gemini';
  else if (options.grok) provider = 'grok';
  else if (options.local) provider = 'local';

  // Parse swarm count
  const count = Math.min(parseInt(options.swarm) || 5, 100);

  if (!options.prompt) {
    console.error(chalk.red('Error: --prompt is required when using --swarm'));
    process.exit(1);
  }

  const swarmOptions: SwarmOptions = {
    provider,
    count,
    prompt: options.prompt,
    cwd: process.cwd(),
    autoLogin: true
  };

  console.log(chalk.bold.cyan(`\n🐝 Hanzo Dev Swarm Mode\n`));
  console.log(chalk.gray(`Provider: ${provider}`));
  console.log(chalk.gray(`Agents: ${count}`));
  console.log(chalk.gray(`Prompt: ${options.prompt}\n`));

  const runner = new SwarmRunner(swarmOptions);

  // Check authentication
  const hasAuth = await runner.ensureProviderAuth();
  if (!hasAuth) {
    console.error(chalk.red(`\nError: ${provider} is not authenticated`));
    console.log(chalk.yellow('\nTo authenticate:'));
    
    switch (provider) {
      case 'claude':
        console.log(chalk.gray('  1. Set ANTHROPIC_API_KEY environment variable'));
        console.log(chalk.gray('  2. Run: claude login'));
        break;
      case 'openai':
        console.log(chalk.gray('  Set OPENAI_API_KEY environment variable'));
        break;
      case 'gemini':
        console.log(chalk.gray('  Set GOOGLE_API_KEY or GEMINI_API_KEY environment variable'));
        break;
      case 'grok':
        console.log(chalk.gray('  Set GROK_API_KEY environment variable'));
        break;
    }
    process.exit(1);
  }

  try {
    await runner.run();
  } catch (error) {
    console.error(chalk.red(`\nSwarm error: ${error}`));
    process.exit(1);
  }
}

// Default action
program
  .action(async (options) => {
    // Determine provider
    let providerType: 'claude' | 'openai' | 'gemini' | 'grok' | 'local' = 'claude';
    if (options.openai) providerType = 'openai';
    else if (options.gemini) providerType = 'gemini';
    else if (options.grok) providerType = 'grok';
    else if (options.local) providerType = 'local';

    // Get provider config
    const providers = ConfigurableAgentLoop.getAvailableProviders();
    const provider = providers.find(p => p.type === providerType);

    if (!provider) {
      console.error(chalk.red(`Provider '${providerType}' not configured`));
      process.exit(1);
    }

    // Create interactive agent with swarm support if requested
    const agent = new InteractiveAgent(provider, options.swarm ? parseInt(options.swarm) : undefined);

    // If prompt provided, run it
    if (options.prompt) {
      await agent.start(options.prompt);
    } else {
      // No prompt - start interactive mode
      await agent.start();
    }
  });

// Parse arguments
program.parse();