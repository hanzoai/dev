<img src="docs/images/every-logo.png" alt="Hanzo Dev Logo" width="400">

&ensp;

**Hanzo Dev** is a fast, local coding agent for your terminal. It's a community-driven fork of `openai/codex` focused on real developer ergonomics: browser integration, multi-agents, theming, and reasoning control — all while staying compatible with upstream.

&ensp;
## What's new in v0.6.0 (December 2025)

- **GitHub Copilot Integration** – Complete CLI integration with GitHub Copilot for code suggestions, review, documentation, and shell commands. Use `dev copilot` for AI-powered assistance directly from the command line.
- **Auto Review** – background ghost-commit watcher runs reviews in a separate worktree whenever a turn changes files; uses `dev-5.1-mini-high` and reports issues plus ready-to-apply fixes without blocking the main thread.
- **Dev Bridge** – Sentry-style local bridge that streams errors, console, screenshots, and control from running apps into Hanzo Dev; ships an MCP server; install by asking Hanzo Dev to pull `https://github.com/hanzoai/dev-bridge` (legacy: `@just-every/code-bridge`).
- **Plays well with Auto Drive** – reviews run in parallel with long Auto Drive tasks so quality checks land while the flow keeps moving.
- **Quality-first focus** – the release shifts emphasis from "can the model write this file" to "did we verify it works".
- _From v0.5.0:_ rename to Hanzo Dev, upgraded `/auto` planning/recovery, unified `/settings`, faster streaming/history with card-based activity, and more reliable `/resume` + `/undo`.

 [Read the full notes in RELEASE_NOTES.md](docs/release-notes/RELEASE_NOTES.md)

&ensp;
## Why Hanzo Dev

- 🚀 **Auto Drive orchestration** – Multi-agent automation that now self-heals and ships complete tasks.
- 🌐 **Browser Integration** – CDP support, headless browsing, screenshots captured inline.
- 🤖 **Multi-agent commands** – `/plan`, `/dev` and `/solve` coordinate multiple CLI agents.
- 🔧 **GitHub Copilot Integration** – Complete CLI integration for code suggestions, review, documentation generation, and shell command assistance.
- 🧭 **Unified settings hub** – `/settings` overlay for limits, theming, approvals, and provider wiring.
- 🎨 **Theme system** – Switch between accessible presets, customize accents, and preview live via `/themes`.
- 🔌 **MCP support** – Extend with filesystem, DBs, APIs, or your own tools.
- 🔒 **Safety modes** – Read-only, approvals, and workspace sandboxing.

&ensp;
## AI Videos

&ensp;
<p align="center">
  <a href="https://www.youtube.com/watch?v=Ra3q8IVpIOc">
    <img src="docs/images/video-auto-review-play.jpg" alt="Play Auto Review video" width="100%">
  </a><br>
  <strong>Auto Review</strong>
</p>

&ensp;
<p align="center">
  <a href="https://youtu.be/UOASHZPruQk">
    <img src="docs/images/video-auto-drive-new-play.jpg" alt="Play Introducing Auto Drive video" width="100%">
  </a><br>
  <strong>Auto Drive Overview</strong>
</p>

&ensp;
<p align="center">
  <a href="https://youtu.be/sV317OhiysQ">
    <img src="docs/images/video-v03-play.jpg" alt="Play Multi-Agent Support video" width="100%">
  </a><br>
  <strong>Multi-Agent Promo</strong>
</p>



&ensp;
## Quickstart

### Run

```bash
npx -y @hanzo/dev
```

### Install & Run

```bash
npm install -g @hanzo/dev
dev // or `coder` if you're using VS Code
```

Note: If another tool already provides a `dev` command (e.g. VS Code), our CLI is also installed as `coder`. Use `coder` to avoid conflicts.

**Authenticate** (one of the following):
- **Sign in with ChatGPT** (Plus/Pro/Team; uses models available to your plan)
  - Run `dev` and pick "Sign in with ChatGPT"
- **API key** (usage-based)
  - Set `export OPENAI_API_KEY=xyz` and run `dev`

### Install Claude & Gemini (optional)

Hanzo Dev supports orchestrating other AI CLI tools. Install these and config to use alongside Hanzo Dev.

```bash
# Ensure Node.js 20+ is available locally (installs into ~/.n)
npm install -g n
export N_PREFIX="$HOME/.n"
export PATH="$N_PREFIX/bin:$PATH"
n 20.18.1

# Install the companion CLIs
export npm_config_prefix="${npm_config_prefix:-$HOME/.npm-global}"
mkdir -p "$npm_config_prefix/bin"
export PATH="$npm_config_prefix/bin:$PATH"
npm install -g @anthropic-ai/claude-dev @google/gemini-cli @qwen-dev/qwen-dev

# Quick smoke tests
claude --version
gemini --version
qwen --version
```

> ℹ️ Add `export N_PREFIX="$HOME/.n"` and `export PATH="$N_PREFIX/bin:$PATH"` (plus the `npm_config_prefix` bin path) to your shell profile so the CLIs stay on `PATH` in future sessions.

&ensp;
## Commands

### Browser
```bash
# Connect dev to external Chrome browser (running CDP)
/chrome        # Connect with auto-detect port
/chrome 9222   # Connect to specific port

# Switch to internal browser mode
/browser       # Use internal headless browser
/browser https://example.com  # Open URL in internal browser
```

### Agents
```bash
# Plan dev changes (Claude, Gemini and GPT-5 consensus)
# All agents review task and create a consolidated plan
/plan "Stop the AI from ordering pizza at 3AM"

# Solve complex problems (Claude, Gemini and GPT-5 race)
# Fastest preferred (see https://arxiv.org/abs/2505.17813)
/solve "Why does deleting one user drop the whole database?"

# Write dev! (Claude, Gemini and GPT-5 consensus)
# Creates multiple worktrees then implements the optimal solution
/dev "Show dark mode when I feel cranky"
```

### Auto Drive
```bash
# Hand off a multi-step task; Auto Drive will coordinate agents and approvals
/auto "Refactor the auth flow and add device login"

# Resume or inspect an active Auto Drive run
/auto status
```

### General
```bash
# Try a new theme!
/themes

# Change reasoning level
/reasoning low|medium|high

# Switch models or effort presets
/model

# Start new conversation
/new
```

## CLI reference

```shell
dev [options] [prompt]

Options:
  --model <name>        Override the model for the active provider (e.g. gpt-5.1)
  --read-only          Prevent file modifications
  --no-approval        Skip approval prompts (use with caution)
  --config <key=val>   Override config values
  --oss                Use local open source models
  --sandbox <mode>     Set sandbox level (read-only, workspace-write, etc.)
  --help              Show help information
  --debug             Log API requests and responses to file
  --version           Show version number
```

Note: `--model` only changes the model name sent to the active provider. To use a different provider, set `model_provider` in `config.toml`. Providers must expose an OpenAI-compatible API (Chat Completions or Responses).

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
- `/src/api/` - API client dev  
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
dev --read-only "analyze dev quality and generate report"

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

Configure MCP in `~/.hanzo/config.toml` Define each server under a named table like `[mcp_servers.<name>]` (this maps to the JSON `mcpServers` object used by other clients):

```toml
[mcp_servers.filesystem]
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/project"]
```

&ensp;
## Configuration

Main config file: `~/.hanzo/config.toml`

> [!NOTE]
> Hanzo Dev reads from `~/.hanzo/` and legacy `~/.code/` or `~/.codex/` for backwards compatibility, but it only writes updates to `~/.hanzo/`. If you switch back to Codex and it fails to start, remove `~/.codex/config.toml`. If Hanzo Dev appears to miss settings after upgrading, copy your legacy config into `~/.hanzo/`.

```toml
# Model settings
model = "gpt-5.1"
model_provider = "openai"

# Behavior
approval_policy = "on-request"  # untrusted | on-failure | on-request | never
model_reasoning_effort = "medium" # low | medium | high
sandbox_mode = "workspace-write"

# UI preferences see THEME_CONFIG.md
[tui.theme]
name = "light-photon"

# Add config for specific models
[profiles.gpt-5]
model = "gpt-5.1"
model_provider = "openai"
approval_policy = "never"
model_reasoning_effort = "high"
model_reasoning_summary = "detailed"
```

### Environment variables

- `HANZO_HOME`: Override config directory location
- `OPENAI_API_KEY`: Use API key instead of ChatGPT auth
- `OPENAI_BASE_URL`: Use OpenAI-compatible API endpoints (chat or responses)
- `OPENAI_WIRE_API`: Force the built-in OpenAI provider to use `chat` or `responses` wiring

&ensp;
## FAQ

**How is this different from the original?**
> This fork adds browser integration, multi-agent commands (`/plan`, `/solve`, `/dev`), theme system, and enhanced reasoning controls while maintaining full compatibility.

**Can I use my existing Hanzo Dev configuration?**
> Yes. Hanzo Dev reads from `~/.hanzo/` (primary) and legacy `~/.code/` or `~/.codex/` directories. We only write to `~/.hanzo/`, so Hanzo Dev will keep running if you switch back; copy or remove legacy files if you notice conflicts.

**Does this work with ChatGPT Plus?**
> Absolutely. Use the same "Sign in with ChatGPT" flow as the original.

**Is my data secure?**
> Yes. Authentication stays on your machine, and we don't proxy your credentials or conversations.

&ensp;
## Contributing

We welcome contributions! Hanzo Dev maintains compatibility with upstream while adding community-requested features.

### Development workflow

```bash
# Clone and setup
git clone https://github.com/hanzoai/dev.git
cd dev
npm install

# Build (use fast build for development)
./build-fast.sh

# Run locally
./code-rs/target/dev-fast/dev
```

#### Git hooks

This repo ships shared hooks under `.githooks/`. To enable them locally:

```bash
git config core.hooksPath .githooks
```

The `pre-push` hook runs `./pre-release.sh` automatically when pushing to `main`.

### Opening a pull request

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Make your changes
4. Run tests: `cargo test`
5. Build successfully: `./build-fast.sh`
6. Submit a pull request


&ensp;
## Legal & Use

### License & attribution
- This project is a community fork of `openai/codex` under **Apache-2.0**. We preserve upstream LICENSE and NOTICE files.
- **Hanzo Dev** is **not** affiliated with, sponsored by, or endorsed by OpenAI.

### Your responsibilities
Using OpenAI, Anthropic or Google services through Hanzo Dev means you agree to **their Terms and policies**. In particular:
- **Don't** programmatically scrape/extract content outside intended flows.
- **Don't** bypass or interfere with rate limits, quotas, or safety mitigations.
- Use your **own** account; don't share or rotate accounts to evade limits.
- If you configure other model providers, you're responsible for their terms.

### Privacy
- Your auth file lives at `~/.hanzo/auth.json`
- Inputs/outputs you send to AI providers are handled under their Terms and Privacy Policy; consult those documents (and any org-level data-sharing settings).

### Subject to change
AI providers can change eligibility, limits, models, or authentication flows. Hanzo Dev supports **both** ChatGPT sign-in and API-key modes so you can pick what fits (local/hobby vs CI/automation).

&ensp;
## License

Apache 2.0 - See [LICENSE](LICENSE) file for details.

Hanzo Dev is a community fork of the original Hanzo Dev CLI. We maintain compatibility while adding enhanced features requested by the developer community.

&ensp;
---
**Need help?** Open an issue on [GitHub](https://github.com/hanzoai/dev/issues) or check our documentation.
