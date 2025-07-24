# Hanzo Dev

Unified AI Development Environment combining IDE functionality, Agent-Computer Interface (ACI), and Model Context Protocol (MCP) tools.

## Features

- **Multi-Agent System**: Various specialized agents for different tasks
- **70+ Tools**: Via integrated hanzo-mcp server
- **Advanced File Editing**: AST-aware code modifications via ACI
- **Runtime Flexibility**: Docker, Kubernetes, Local, Remote execution
- **Browser Automation**: Full browser control for web tasks
- **MCP Integration**: Connect to any MCP server for additional tools

## Installation

```bash
pip install hanzo-dev
```

## Usage

```bash
# Start the development environment
hanzo-dev

# With specific configuration
hanzo-dev --enable-all-tools --allow-path /path/to/project

# Run with a specific task
hanzo-dev --file task.md
```

## Architecture

Hanzo Dev integrates:
- **IDE Backend**: Agent orchestration and runtime management
- **ACI Library**: Advanced file editing and code analysis
- **MCP Server**: 70+ tools for file operations, search, git, etc.
- **MCP Client**: Connect to external MCP servers

## Development

```bash
# Install in development mode
poetry install

# Run tests
poetry run pytest

# Run the CLI
poetry run hanzo-dev
```