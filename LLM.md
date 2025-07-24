# LLM.md - Hanzo Dev IDE

This file provides context for AI assistants working with the Hanzo Dev IDE codebase.

## Project Overview

Hanzo Dev (formerly IDE) is a unified AI development environment that combines:
- **IDE functionality**: Based on OpenHands/OpenDevin architecture
- **ACI (Agent-Computer Interface)**: Advanced file editing and code analysis capabilities
- **MCP (Model Context Protocol)**: 70+ tools for AI assistants via hanzo-mcp

The project is published as `hanzo-dev` on PyPI and provides the `hanzo-dev` CLI command.

## Architecture

### Core Components

1. **CLI Interface** (`ide/cli/`)
   - Entry point: `main.py` - Main CLI that orchestrates sessions
   - `commands.py` - Command handling and REPL functionality
   - `settings.py` - User settings management
   - `shell_config.py` - Shell alias configuration (ide, oh → hanzo-dev)
   - `tui.py` - Terminal UI components

2. **Agent System** (`ide/agenthub/`)
   - `codeact_agent/` - Primary coding agent with tool calling
   - `browsing_agent/` - Web browsing capabilities
   - `loc_agent/` - Code localization and search
   - Function calling and tool integration

3. **Runtime System** (`ide/runtime/`)
   - Docker, Kubernetes, Local, Remote runtime support
   - File operations, command execution, browser automation
   - MCP proxy integration for tool routing
   - Agent skills and plugins

4. **MCP Integration** (`ide/mcp/`)
   - Client for connecting to MCP servers
   - Tool registration and execution
   - Default integration with hanzo-mcp server

5. **ACI Integration** (via `hanzo-aci` dependency)
   - Advanced file editing with AST understanding
   - Code indexing and search capabilities
   - Multi-format file support (code, docs, media)

### Key Dependencies

```toml
hanzo-aci = "0.3.1"                    # Agent-Computer Interface
hanzo-mcp = { path = "../mcp", develop = true }  # Model Context Protocol
litellm = "^1.60.0"                    # LLM provider abstraction
fastmcp = "^2.5.2"                     # MCP framework
browsergym-core = "0.13.3"             # Browser automation
```

## Development Workflow

### Setup
```bash
# Install dependencies
poetry install

# Run development server
hanzo-dev

# With specific configuration
hanzo-dev --enable-all-tools --allow-path /path/to/project
```

### Key Features

1. **Multi-Agent Support**: Various specialized agents for different tasks
2. **Tool Integration**: 70+ tools via MCP (file ops, search, git, etc.)
3. **Runtime Flexibility**: Docker, local, remote execution environments
4. **Browser Automation**: Full browser control for web tasks
5. **Advanced Editing**: AST-aware code modifications

### Configuration

- Config files: `config.toml`, environment variables
- MCP servers configured in `ide/core/config/mcp_config.py`
- Default hanzo-mcp server automatically started
- Additional MCP servers can be added via configuration

## Code Organization

### Entry Points
- `hanzo-dev` → `ide.cli.main:main`
- Aliases: `ide`, `oh` (configured via shell setup)

### State Management
- Agent state tracked in `controller/state/`
- Session persistence and replay capabilities
- Event-driven architecture with event streams

### Tool System
- Tools exposed via MCP protocol
- Agent skills in `runtime/plugins/agent_skills/`
- File operations, shell commands, browser control

## Integration Points

### MCP Servers
- hanzo-mcp: Primary tool server (70+ tools)
- tavily: Search engine integration (if API key provided)
- Custom MCP servers via stdio/SSE configuration

### External Services
- LLM providers via litellm (OpenAI, Anthropic, etc.)
- GitHub, GitLab, Bitbucket integrations
- Cloud storage (S3, GCS, MinIO)

## Common Tasks

### Adding New Tools
1. Implement in hanzo-mcp if general purpose
2. Or add as agent skill in `runtime/plugins/agent_skills/`
3. Register with appropriate agent

### Modifying Agents
1. Agent implementations in `ide/agenthub/`
2. Update prompts in agent's `prompts/` directory
3. Modify tool registration in agent class

### Runtime Customization
1. Runtime implementations in `ide/runtime/impl/`
2. Docker configurations for sandboxed execution
3. Plugin system for extending capabilities

## Testing

```bash
# Run tests
poetry run pytest

# Specific test categories
poetry run pytest tests/unit/
poetry run pytest tests/runtime/
poetry run pytest tests/integration/
```

## Publishing

```bash
# Build package
poetry build

# Publish to PyPI
poetry publish
```

The package is published as `hanzo-dev` and installs the `hanzo-dev` CLI command.