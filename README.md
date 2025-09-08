# Hanzo Dev

&ensp;

<p align="center">
  <img src="docs/logo.png" alt="Hanzo Dev Logo" width="400">
</p>

&ensp;

**Hanzo Dev** is a fast, local AI-powered development assistant for your terminal. Built by [Hanzo AI](https://hanzo.ai) for developers who demand the best tooling for their AI-accelerated workflows.

&ensp;
## Why Hanzo Dev

  - 🌐 **Browser Integration** - CDP support, headless browsing, screenshots
  - 📝 **Diff Viewer** - Side-by-side diffs with syntax highlighting
  - 🤖 **Multi-Agent Commands** - /plan, /solve, /code with agent panels
  - 🎨 **Theme System** - /themes with live preview and accessibility
  - 🧠 **Reasoning Control** - /reasoning for dynamic effort adjustment
  - 🔌 **MCP support** – Extend with filesystem, DBs, APIs, or your own tools
  - 🔒 **Safety modes** – Read-only, approvals, and workspace sandboxing
  - 🚀 **Hanzo AI Integration** - Works seamlessly with Hanzo's AI infrastructure

&ensp;
| <img src="docs/screenshots/simple.png" alt="Simple interface" width="100%"><br>Simple interface | <img src="docs/screenshots/diff.png" alt="Unified diff viewer" width="100%"><br>Unified diffs |
|:--:|:--:|

| <br><img src="docs/screenshots/browser.png" alt="Browser control" width="100%"><br>Browser control | <br><img src="docs/screenshots/agents.png" alt="Assist with Claude & Gemini" width="100%"><br>Assist with Claude & Gemini |
|:--:|:--:|


&ensp;
## Quickstart

### Run

```bash
npx -y @hanzo/dev
```

### Install & Run

```bash
npm install -g @hanzo/dev
dev
```

**Authenticate** (one of the following):
- **Sign in with ChatGPT** (Plus/Pro/Team; uses models available to your plan)
  - Run `dev` and pick "Sign in with ChatGPT"
  - Stores creds locally at `~/.coder/auth.json` (also reads legacy `~/.codex/auth.json`)
- **API key** (usage-based)
  - Set `export OPENAI_API_KEY=xyz` and run `dev`

### Install Claude & Gemini (optional)

Hanzo Dev supports orchestrating other AI CLI tools. Install these and config to use alongside Hanzo Dev.

```bash
npm install -g @anthropic-ai/claude-code @google/gemini-cli && claude "Just checking you're working! Let me know how I can exit." && gemini -i "Just checking you're working! Let me know how I can exit."
```

&ensp;
## Commands

### Browser
```bash
# Connect to external Chrome browser (running CDP)
/chrome        # Connect with auto-detect port
/chrome 9222   # Connect to specific port

# Switch to internal browser mode
/browser       # Use internal headless browser
/browser https://example.com  # Open URL in internal browser
```

### Agents
```bash
# Plan code changes (Claude, Gemini and GPT-5 consensus)
# All agents review task and create a consolidated plan
/plan "Stop the AI from ordering pizza at 3AM"

# Solve complex problems (Claude, Gemini and GPT-5 race)
# Fastest preferred (see https://arxiv.org/abs/2505.17813)
/solve "Why does deleting one user drop the whole database?"

# Write code! (Claude, Gemini and GPT-5 consensus)
# Creates multiple worktrees then implements the optimal solution
/code "Show dark mode when I feel cranky"
```

### General
```bash
# Try a new theme!
/themes

# Change reasoning level
/reasoning low|medium|high

# Start new conversation
/new
```

## CLI reference

```shell
dev [options] [prompt]

Options:
  --model <name>        Override the model (gpt-5, claude-opus, etc.)
  --read-only          Prevent file modifications
  --no-approval        Skip approval prompts (use with caution)
  --config <key=val>   Override config values
  --oss                Use local open source models
  --sandbox <mode>     Set sandbox level (read-only, workspace-write, etc.)
  --help              Show help information
  --debug             Log API requests and responses to file
  --version           Show version number
```

&ensp;
## Memory & project docs

Hanzo Dev can remember context across sessions:

1. **Create an `AGENTS.md` or `CLAUDE.md` file** in your project root:
```markdown
# Project Context
This is a React TypeScript application with:
- Authentication via JWT
- PostgreSQL database
- Express.js backend

## Key files:
- `/src/auth/` - Authentication logic
- `/src/api/` - API client code  
- `/server/` - Backend services
```

2. **Session memory**: Hanzo Dev maintains conversation history
3. **Codebase analysis**: Automatically understands project structure

&ensp;
## Non-interactive / CI mode

For automation and CI/CD:

```shell
# Run a specific task
dev --no-approval "run tests and fix any failures"

# Generate reports
dev --read-only "analyze code quality and generate report"

# Batch processing
dev --config output_format=json "list all TODO comments"
```

&ensp;
## Model Context Protocol (MCP)

Hanzo Dev supports MCP for extended capabilities:

- **File operations**: Advanced file system access
- **Database connections**: Query and modify databases
- **API integrations**: Connect to external services
- **Custom tools**: Build your own extensions

Configure MCP in `~/.codex/config.toml`:

```toml
[[mcp_servers]]
name = "filesystem"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/project"]
```

&ensp;
## Configuration

Main config file: `~/.codex/config.toml`

```toml
# Model settings
model = "gpt-5"
model_provider = "openai"

# Behavior
approval_policy = "on_request"  # untrusted | on-failure | on-request | never
model_reasoning_effort = "medium" # low | medium | high
sandbox_mode = "workspace_write"

# UI preferences see THEME_CONFIG.md
[tui.theme]
name = "light-photon"

# Add config for specific models
[profiles.gpt-5]
model = "gpt-5"
model_provider = "openai"
approval_policy = "never"
model_reasoning_effort = "high"
model_reasoning_summary = "detailed"
```

### Environment variables

- `CODEX_HOME`: Override config directory location
- `OPENAI_API_KEY`: Use API key instead of ChatGPT auth
- `OPENAI_BASE_URL`: Use alternative API endpoints

&ensp;
## FAQ

**What is Hanzo Dev?**
> Hanzo Dev is Hanzo AI's official developer assistant, a powerful evolution of the popular Codex CLI with enterprise-grade features and integration with Hanzo's AI infrastructure.

**How does this relate to OpenAI Codex?**
> This is a fork of the original OpenAI Codex CLI, enhanced with additional features and maintained by Hanzo AI for the developer community.

**Can I use my existing Codex configuration?**
> Yes! Hanzo Dev is fully backward compatible with existing `~/.codex/` configurations.

**Does this work with ChatGPT Plus?**
> Absolutely. Use the same "Sign in with ChatGPT" flow as the original.

**Is my data secure?**
> Yes. Authentication stays on your machine, and we don't proxy your credentials or conversations.

&ensp;
## Contributing

We welcome contributions! This fork maintains compatibility with upstream while adding community-requested features.

### Development workflow

```bash
# Clone and setup
git clone https://github.com/hanzoai/dev.git
cd dev
npm install

# Build (use fast build for development)
./build-fast.sh

# Run locally
./codex-rs/target/dev-fast/dev
```

### Opening a pull request

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Make your changes
4. Run tests: `cargo test`
5. Build successfully: `./build-fast.sh`
6. Submit a pull request


&ensp;
## About Hanzo AI

[Hanzo AI](https://hanzo.ai) is building the future of AI-powered development tools. Our mission is to accelerate human potential through intelligent automation and augmentation.

- **Website**: [hanzo.ai](https://hanzo.ai)
- **GitHub**: [github.com/hanzoai](https://github.com/hanzoai)
- **Platform**: Enterprise AI infrastructure and tools
- **Focus**: Developer productivity, AI agents, and automation

&ensp;
## Legal & Use

### License & attribution
- This project is a fork of `openai/codex` under **Apache-2.0**. We preserve upstream LICENSE and NOTICE files.
- **Hanzo Dev** is maintained by Hanzo AI and is **not** affiliated with, sponsored by, or endorsed by OpenAI.

### Your responsibilities
Using OpenAI, Anthropic or Google services through Hanzo Dev means you agree to **their Terms and policies**. In particular:
- **Don't** programmatically scrape/extract content outside intended flows.
- **Don't** bypass or interfere with rate limits, quotas, or safety mitigations.
- Use your **own** account; don't share or rotate accounts to evade limits.
- If you configure other model providers, you're responsible for their terms.

### Privacy
- Your auth file lives at `~/.codex/auth.json`.
- Inputs/outputs you send to AI providers are handled under their Terms and Privacy Policy; consult those documents (and any org-level data-sharing settings).

### Subject to change
AI providers can change eligibility, limits, models, or authentication flows. Hanzo Dev supports **both** ChatGPT sign-in and API-key modes so you can pick what fits (local/hobby vs CI/automation).

&ensp;
## License

Apache 2.0 - See [LICENSE](LICENSE) file for details.

This project is a community fork of the original Codex CLI, enhanced and maintained by Hanzo AI. We maintain compatibility while adding enhanced features requested by the developer community.

&ensp;
---
**Need help?** Open an issue on [GitHub](https://github.com/hanzoai/dev/issues) or check our documentation.