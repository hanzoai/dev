# @hanzo/dev

> State-of-the-art AI development platform with swarm intelligence

[![npm version](https://badge.fury.io/js/@hanzo%2Fdev.svg)](https://www.npmjs.com/package/@hanzo/dev)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

@hanzo/dev is an advanced AI development platform that orchestrates multiple AI agents working in parallel. Built with swarm intelligence and Model Context Protocol (MCP) at its core, it achieves industry-leading performance on software engineering benchmarks.

## Features

- 🤖 **Multi-AI Support**: Integrate with Claude, OpenAI, Gemini, and local AI models
- 🔧 **Tool Unification**: Single interface for all AI coding assistants
- 🌐 **MCP Integration**: Full Model Context Protocol support for extensible tools
- 👥 **Peer Agent Networks**: Spawn multiple agents that collaborate via MCP
- 🎯 **CodeAct Agent**: Automatic planning, execution, and self-correction
- 🌍 **Browser Automation**: Control browsers via Hanzo Browser/Extension
- 📝 **Advanced Editing**: File manipulation with undo, chunk localization
- 🚀 **Parallel Execution**: Run multiple tasks concurrently across agents
- 🔍 **SWE-bench Ready**: Optimized for software engineering benchmarks

## Installation

```bash
npm install -g @hanzo/dev
```

Or use directly with npx:

```bash
npx @hanzo/dev
```

## Quick Start

### Interactive Mode

```bash
dev
```

This launches an interactive menu where you can:
- Select your preferred AI tool
- Configure API keys
- Access specialized commands

### Direct Tool Access

```bash
# Launch with specific AI provider
dev --claude
dev --openai
dev --gemini
dev --grok
dev --local

# Advanced modes
dev --workspace    # Unified workspace mode
dev --benchmark    # Run SWE-bench evaluation

# Swarm mode - edit multiple files in parallel
dev --claude --swarm 5 -p "Add copyright header to all files"
dev --openai --swarm 10 -p "Fix all ESLint errors"
dev --gemini --swarm 20 -p "Add JSDoc comments to all functions"
dev --local --swarm 100 -p "Format all files with prettier"
```

### Environment Configuration

Create a `.env` file in your project root:

```env
# API Keys
ANTHROPIC_API_KEY=your_key_here
OPENAI_API_KEY=your_key_here
GEMINI_API_KEY=your_key_here
TOGETHER_API_KEY=your_key_here

# Local AI
HANZO_APP_URL=http://localhost:8080
LOCAL_LLM_URL=http://localhost:11434

# Browser Integration
HANZO_BROWSER_URL=http://localhost:9223
HANZO_EXTENSION_WS=ws://localhost:9222
```

## Advanced Features

### Swarm Mode

Launch multiple agents to edit files in parallel across your codebase:

```bash
# Basic swarm usage
dev --claude --swarm 5 -p "Add copyright header to all files"

# Process specific file types
dev --openai --swarm 20 -p "Add type annotations" --pattern "**/*.ts"

# Maximum parallelism (up to 100 agents)
dev --gemini --swarm 100 -p "Fix linting errors"

# Using local provider for cost efficiency
dev --local --swarm 50 -p "Format with prettier"
```

Features:
- **Lazy agent spawning**: Agents are created as needed, not all at once
- **Automatic authentication**: Handles provider login if API keys are available
- **Parallel execution**: Each agent processes a different file simultaneously
- **Smart file detection**: Automatically finds all editable files in your project
- **Progress tracking**: Real-time status updates as files are processed

Example: Adding copyright headers to 5 files in parallel:

```bash
# Navigate to your test directory
cd test-swarm

# Run swarm with Claude
dev --claude --swarm 5 -p "Add copyright header '// Copyright 2025 Hanzo Industries Inc.' at the top of each file"
```

The swarm will:
1. Find all editable files in the directory
2. Spawn up to 5 Claude agents
3. Assign each agent a file to process
4. Execute edits in parallel
5. Report results when complete

Supported providers:
- `--claude`: Claude AI (requires ANTHROPIC_API_KEY or claude login)
- `--openai`: OpenAI GPT (requires OPENAI_API_KEY)
- `--gemini`: Google Gemini (requires GOOGLE_API_KEY)
- `--grok`: Grok AI (requires GROK_API_KEY)
- `--local`: Local Hanzo agent (no API key required)

### Workspace Mode

Open a unified workspace with all tools available:

```bash
dev workspace
```

Features:
- Integrated shell, editor, browser, and planner
- Persistent session state
- Tool switching without context loss
- Unified command interface

### MCP Server Configuration

Configure MCP servers in `.mcp.json`:

```json
{
  "servers": [
    {
      "name": "filesystem",
      "command": "npx",
      "args": ["@modelcontextprotocol/server-filesystem"],
      "env": { "MCP_ALLOWED_PATHS": "." }
    },
    {
      "name": "git",  
      "command": "npx",
      "args": ["@modelcontextprotocol/server-git"]
    },
    {
      "name": "custom",
      "command": "python",
      "args": ["my-mcp-server.py"],
      "transport": "stdio"
    }
  ]
}
```

## Architecture

### Core Components

1. **Editor Module** (`lib/editor.ts`)
   - View, create, and edit files
   - String replacement with validation
   - Chunk localization for large files
   - Undo/redo functionality

2. **MCP Client** (`lib/mcp-client.ts`)
   - Stdio and WebSocket transports
   - Dynamic tool discovery
   - Session management
   - JSON-RPC protocol

3. **CodeAct Agent** (`lib/code-act-agent.ts`)
   - Automatic task planning
   - Parallel step execution
   - Self-correction with retries
   - State and observation tracking

4. **Peer Agent Network** (`lib/peer-agent-network.ts`)
   - Agent spawning strategies
   - Inter-agent communication
   - MCP tool exposure
   - Swarm optimization

5. **Agent Loop** (`lib/agent-loop.ts`)
   - LLM provider abstraction
   - Browser automation
   - Tool orchestration
   - Execution management

## API Usage

### Programmatic Access

```typescript
import { CodeActAgent, PeerAgentNetwork, ConfigurableAgentLoop } from '@hanzo/dev';

// Create an agent
const agent = new CodeActAgent('my-agent', functionCallingSystem);
await agent.plan('Fix the login bug');
const result = await agent.execute();

// Create a peer network
const network = new PeerAgentNetwork();
await network.spawnAgentsForCodebase('./src', 'claude-code', 'one-per-file');

// Configure agent loop
const loop = new ConfigurableAgentLoop({
  provider: {
    name: 'Claude',
    type: 'anthropic',
    apiKey: process.env.ANTHROPIC_API_KEY,
    model: 'claude-3-opus-20240229',
    supportsTools: true,
    supportsStreaming: true
  },
  maxIterations: 10,
  enableMCP: true,
  enableBrowser: true,
  enableSwarm: true
});

await loop.initialize();
await loop.execute('Refactor the authentication module');
```

### Custom Tool Registration

```typescript
import { FunctionCallingSystem } from '@hanzo/dev';

const functionCalling = new FunctionCallingSystem();

// Register custom tool
functionCalling.registerTool({
  name: 'my_custom_tool',
  description: 'Does something special',
  parameters: {
    type: 'object',
    properties: {
      input: { type: 'string', description: 'Tool input' }
    },
    required: ['input']
  },
  handler: async (args) => {
    // Tool implementation
    return { success: true, result: `Processed: ${args.input}` };
  }
});
```

## Performance & Benchmarks

### SWE-bench Results

Our platform is continuously evaluated on the Software Engineering Benchmark:

| Metric | Score | Details |
|--------|-------|---------|
| Success Rate | 15%+ | Solving real GitHub issues |
| Avg Resolution Time | 90s | Per task completion |
| Cost Efficiency | $0.10/task | Using swarm optimization |
| Parallel Speedup | 4.2x | With 5-agent swarm |

### Running Benchmarks

```bash
# Run full SWE-bench evaluation
dev --benchmark swe-bench

# Run on specific dataset
dev --benchmark swe-bench --dataset lite

# Custom benchmark configuration
dev --benchmark swe-bench \
  --agents 10 \
  --parallel \
  --timeout 300 \
  --output results.json
```

### Performance Optimizations

1. **Swarm Intelligence**: Multiple agents work on different aspects simultaneously
2. **Local Orchestration**: Hanzo Zen manages coordination locally, reducing API calls
3. **Smart Caching**: MCP tools cache results across agents
4. **Parallel Execution**: CodeAct identifies independent steps and runs them concurrently

## Testing

```bash
# Run all tests
npm test

# Run specific test suite
npm run test:swe-bench

# Watch mode
npm run test:watch

# Coverage report
npm run test:coverage
```

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

### Development Setup

```bash
# Clone the repository
git clone https://github.com/hanzoai/dev.git
cd dev/packages/dev

# Install dependencies
npm install

# Run in development mode
npm run dev

# Build
npm run build

# Run tests
npm test
```

## License

MIT © [Hanzo AI](https://hanzo.ai)

## Acknowledgments

Built by [Hanzo AI](https://hanzo.ai) - Advancing AI infrastructure for developers worldwide.