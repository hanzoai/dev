# Hanzo Dev

AI-powered development assistant with GitHub Copilot integration.

## Installation

```bash
cargo install hanzo-dev
```

## Quick Start

```bash
# Setup GitHub Copilot
dev copilot setup --check
dev copilot setup --install

# Interactive chat with Copilot
dev copilot chat

# Get code suggestions
dev copilot suggest "sort an array in Rust"

# Generate shell commands
dev copilot shell "find all Python files and run tests"
```

## Commands

### Copilot Integration

- `dev copilot chat` - Interactive chat with GitHub Copilot
- `dev copilot suggest <input>` - Get code suggestions  
- `dev copilot shell <task>` - Generate shell commands
- `dev copilot setup --check` - Check installation status
- `dev copilot setup --install` - Install Copilot extension

### General

- `dev version` - Show version information
- `dev setup` - Setup and check dependencies

## Requirements

- [GitHub CLI](https://cli.github.com/) (`gh`)
- GitHub Copilot extension: `gh extension install github/copilot`
- GitHub account with Copilot access

## Full Version

This is a minimal Rust implementation. For the complete Hanzo Dev experience with TUI, advanced features, and more integrations, install the full version:

```bash
npm install -g @hanzo/dev
```

## Repository

- **GitHub**: https://github.com/hanzoai/dev
- **Website**: https://hanzo.ai/dev

## License

MIT