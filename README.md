# Coder CLI - Enhanced Terminal Coding Agent

**Coder CLI** is an enhanced fork of OpenAI's Codex CLI that brings powerful AI coding assistance directly to your terminal with additional features for improved developer experience.

## Installation

```bash
npm install -g @just-every/coder
```

Then run:
```bash
coderr
```

## Key Features

### 🎯 Core Capabilities
- **AI-powered coding agent** that runs locally in your terminal
- **File operations**: Read, write, and modify files with AI assistance
- **Command execution**: Run shell commands with intelligent context
- **Project understanding**: Analyzes your codebase to provide relevant suggestions

### ✨ Enhanced Features (Fork Additions)
- **Image Support**: Drag-and-drop, paste, or reference images directly in the terminal
- **Dynamic Reasoning**: Adjust AI reasoning effort mid-conversation with `/reasoning` command
- **Flexible Configuration**: Uses `~/.coder` directory (or `~/.codex` for compatibility)

## Quick Start

### 1. Install and Launch
```bash
npm install -g @just-every/coder
coder
```

### 2. Authenticate
You can use Coder CLI with:
- **ChatGPT Plus/Pro subscription** (recommended)
- **OpenAI API key** (usage-based billing)

### 3. Choose Autonomy Level

**Read/Write Mode** (default - full capabilities):
```bash
coderr
```

**Read-Only Mode** (safer, no file modifications):
```bash
coder --read-only
```

## Example Usage

```bash
# Start an interactive session
code

# Run with a specific prompt
code "refactor this function to use async/await"

# Analyze code without making changes
code --read-only "explain the authentication flow"

# Adjust reasoning effort
/reasoning high  # More thorough analysis
/reasoning low   # Faster responses
```

## Common Tasks

- **Debug code**: "Why is this function returning null?"
- **Add features**: "Add error handling to all API calls"
- **Refactor**: "Convert this class to use TypeScript"
- **Explain**: "How does the authentication system work?"
- **Generate tests**: "Write unit tests for the user service"

## Configuration

Configuration file location: `~/.code/config.toml` (or set `CODE_HOME` environment variable)

### Basic Configuration

```toml
# Model selection
model = "gpt-5"

# Approval settings
approval_policy = "ask"  # ask | automatic | never

# Feature flags
hide_agent_reasoning = false
```

### Environment Variables

- `CODE_HOME` or `CODEX_HOME`: Override default config directory
- `OPENAI_API_KEY`: Use OpenAI API directly
- `OPENAI_BASE_URL`: Use alternative API endpoints

## Enhanced Image Support

The fork adds comprehensive image handling:

1. **Drag and drop** images directly into the terminal
2. **Paste** images from clipboard
3. **Reference** image files by path: "analyze screenshot.png"

## Using the /reasoning Command

Dynamically adjust the AI's reasoning effort:

```
/reasoning high   # Detailed analysis, slower
/reasoning medium # Balanced (default)
/reasoning low    # Quick responses
```

## Terminal Interface Tips

### Text Selection
- **Press Ctrl+M** to toggle between mouse modes:
  - **Scrolling mode** (default): Mouse wheel scrolls chat history
  - **Selection mode**: Allows text selection and copying
- The status bar shows current mouse mode

### Keyboard Shortcuts
- **Enter**: Submit your message
- **Up/Down arrows**: Navigate command history
- **Ctrl+M**: Toggle mouse capture (scrolling vs text selection)
- **Ctrl+C**: Cancel current operation
- **Ctrl+D**: Exit the application

## System Requirements

- **Operating Systems**: macOS, Linux, Windows (WSL recommended)
- **Node.js**: Version 20 or higher
- **Terminal**: Modern terminal with UTF-8 support
- **Memory**: 4GB RAM minimum, 8GB recommended

## Command Line Options

```bash
coder [options] [prompt]

Options:
  --model <name>     Override the model (e.g., gpt-5, claude-opus)
  --read-only        Prevent file modifications
  --no-approval      Skip approval prompts (use with caution)
  --config <key=val> Override config values
  --help            Show help information
  --version         Show version number
```

## Advanced Features

### Model Context Protocol (MCP)

Coder CLI supports MCP for extended capabilities:
- File system operations
- Web browsing
- Database connections
- Custom tool integrations

### Project Documentation

Create a `CODEX.md` file in your project root to provide context:

```markdown
# Project Overview
This is a React application using TypeScript...

## Key Components
- Authentication: src/auth/*
- API Client: src/api/*
```

### Non-Interactive Mode

For CI/CD pipelines:

```bash
coder --non-interactive "update all dependencies"
```

## Troubleshooting

### Common Issues

**Installation fails**: Ensure Node.js 20+ is installed
```bash
node --version  # Should be v20.0.0 or higher
```

**Authentication issues**: Re-run the login flow
```bash
coder --login
```

**Performance problems**: Adjust model or reasoning level
```bash
coder --model gpt-5-turbo
# or use /reasoning low during conversation
```

## Contributing

We welcome contributions! This fork maintains compatibility with upstream Codex while adding new features.

### Development Setup

```bash
# Clone the repository
git clone https://github.com/just-every/code.git
cd code

# Install dependencies
npm install

# Build the project
npm run build

# Run locally
./codex-rs/target/release/coder
```

### Submitting Changes

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## Differences from Original Codex

| Feature | Coder CLI (Fork) | Original Codex |
|---------|----------------|----------------|
| Command | `code` | `codex` |
| Config Directory | `~/.code` | `~/.codex` |
| NPM Package | `@just-every/coder` | `@openai/codex` |
| Image Support | ✅ Enhanced | Basic |
| /reasoning Command | ✅ Available | Not available |
| Environment Variables | CODE_HOME + CODEX_HOME | CODEX_HOME only |

## License

Apache 2.0 - See [LICENSE](LICENSE) file for details.

## Acknowledgments

This project is a fork of [OpenAI Codex CLI](https://github.com/openai/codex). We maintain compatibility while adding community-requested features.

---

**Need help?** Open an issue on [GitHub](https://github.com/just-every/code/issues) or check the [original Codex documentation](https://github.com/openai/codex).