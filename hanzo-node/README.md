# hanzo-node

Hanzo Dev CLI - AI-powered development assistant for Node.js projects.

## Installation

```bash
# Global installation
npm install -g hanzo-node

# Or run directly with npx
npx hanzo-node
```

## Usage

```bash
# Start the interactive CLI
hanzo

# Or use the full command name
hanzo-dev

# Run with a specific prompt
hanzo "explain this codebase"
```

## Features

- AI-powered code generation and explanation
- Context-aware assistance that understands your project
- Multi-provider support (OpenAI, Anthropic, etc.)
- Terminal-based UI with streaming responses
- MCP (Model Context Protocol) integration

## Platform Support

- macOS (Apple Silicon and Intel)
- Linux (x64 and ARM64)
- Windows (x64)

## Configuration

Configuration is stored in `~/.hanzo/config.toml`:

```toml
[default]
model = "claude-sonnet-4-20250514"
provider = "anthropic"
```

## Environment Variables

- `ANTHROPIC_API_KEY` - Anthropic API key
- `OPENAI_API_KEY` - OpenAI API key
- `HANZO_MODEL` - Override default model
- `HANZO_PROVIDER` - Override default provider

## Related Packages

- `@hanzo/dev` - Same CLI with different package name
- `@just-every/code` - Upstream fork compatibility

## License

Apache-2.0

## Links

- [Hanzo AI](https://hanzo.ai)
- [Documentation](https://hanzo.ai/dev)
- [GitHub](https://github.com/hanzoai/dev)
