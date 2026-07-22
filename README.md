<p align="center"><img src=".github/hero.svg" alt="Hanzo Dev" width="880"></p>

<img src="docs/images/hanzo-logo.svg" alt="Hanzo Dev" width="400">

&ensp;

**Hanzo Dev** (Dev for short) is a fast, local coding agent for your terminal. It is a community-driven fork of [`just-every/code`](https://github.com/just-every/code) (itself a fork of [`openai/codex`](https://github.com/openai/codex)) focused on real developer ergonomics: Browser integration, multi-agents, theming, and reasoning control — all while staying compatible with upstream and defaulting to the Hanzo LLM gateway.

&ensp;

## What's new

- **Latest long-session stability sweep** (post-0.6): Auto Drive and Auto Review are now decoupled so background reviews no longer block the command flow. `Esc` returns control immediately and typing works while review finalization continues.

- **Operational upgrades in this cycle**
  - Auto Review metadata (branch/worktree context) remains queryable through the active Auto Drive session after completion.
  - Terminal agents are compacted and archived so heavy payloads are reduced while review linkage is preserved.
  - Core `core`, coordinator, and TUI state maps now have hard caps with bounded drop/trim behavior.
  - Auto Drive conversation/update queues are bounded in the coordinator; TUI has bounded prompt/agent/runtime caches.
  - Background review notes are added as non-blocking history-visible notes instead of foreground task-injection.
  - TUI housekeeping lifecycle is bounded with deterministic stop control.
  - Stress tests now cover heavy agent churn plus concurrent Auto Review + Esc/typing responsiveness.

- **New/updated models and agents**
  - Auto Drive CLI model support includes `gpt-5.3-codex` (planning/problem-solving) and `gpt-5.3-codex-spark` (fast coding/fix loops), with `medium | high | xhigh` reasoning controls.
  - Frontline and alias-aware agent model handling now includes `code-gpt-5.3-codex` and `code-gpt-5.3-codex-spark`, with compatibility alias upgrades for `gpt-5.1-codex`, `gpt-5.1-codex-mini`, `gpt-5.2-codex`, etc.
  - Auto Drive decision schema and coordinator payloads now enforce bounded history while preserving goal and recent context.

  See commit `60727b068` and related Auto Drive hardening commits in git history for details.

- **Auto Review** – background ghost-commit watcher runs reviews in a separate worktree whenever a turn changes code; reports issues plus ready-to-apply fixes without blocking the main thread.
- **Plays well with Auto Drive** – reviews run in parallel with long Auto Drive tasks so quality checks land while the flow keeps moving.
- **Quality-first focus** – the release shifts emphasis from "can the model write this file" to "did we verify it works".

&ensp;

## Why Hanzo Dev

- 🚀 **Auto Drive orchestration** – Multi-agent automation that now self-heals and ships complete tasks.
- 🌐 **Browser Integration** – CDP support, headless browsing, screenshots captured inline.
- 🤖 **Multi-agent commands** – `/plan`, `/code` and `/solve` coordinate multiple CLI agents.
- 🧭 **Unified settings hub** – `/settings` overlay for limits, theming, approvals, and provider wiring.
- 🎨 **Theme system** – Switch between accessible presets, customize accents, and preview live via `/themes`.
- 🔌 **MCP support** – Extend with filesystem, DBs, APIs, or your own tools.
- 🔒 **Safety modes** – Read-only, approvals, and workspace sandboxing.

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

Note: If another tool already provides a `code` command (e.g. VS Code), our CLI is also installed as `coder`. Use `coder` to avoid conflicts.

**Authenticate** (one of the following):

- **Hanzo** (default) – Sign in to Hanzo IAM (hanzo.id) or set `export HANZO_API_KEY=xyz`, then run `dev`. Requests route to the Hanzo LLM gateway at `https://api.hanzo.ai/v1`.
- **Sign in with ChatGPT** (Plus/Pro/Team; uses models available to your plan)
  - Run `dev` and pick "Sign in with ChatGPT"
- **OpenAI API key** (usage-based)
  - Set `export OPENAI_API_KEY=xyz` and run `dev`

### Install Claude & Gemini (optional)

Hanzo Dev supports orchestrating other AI CLI tools. Install these to use alongside Dev.

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
npm install -g @anthropic-ai/claude-code @google/gemini-cli @qwen-code/qwen-code

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
# Connect Dev to external Chrome browser (running CDP)
/chrome        # Connect with auto-detect port
/chrome 9222   # Connect to specific port

# Switch to internal browser mode
/browser       # Use internal headless browser
/browser https://example.com  # Open URL in internal browser
```

### Agents

```bash
# Plan code changes (multi-model consensus)
# All agents review task and create a consolidated plan
/plan "Stop the AI from ordering pizza at 3AM"

# Solve complex problems (multi-model race)
# Fastest preferred (see https://arxiv.org/abs/2505.17813)
/solve "Why does deleting one user drop the whole database?"

# Write code! (multi-model consensus)
# Creates multiple worktrees then implements the optimal solution
/code "Show dark mode when I feel cranky"
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

Configure MCP in `config.toml`. Define each server under a named table like `[mcp_servers.<name>]` (this maps to the JSON `mcpServers` object used by other clients):

```toml
[mcp_servers.filesystem]
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/project"]
```

&ensp;

## Configuration

Main config file: `config.toml` in the Dev config home (`$CODEX_HOME`, defaulting to `~/.codex`; legacy `~/.code` is also read).

```toml
# Model settings
model = "gpt-5.1"
model_provider = "hanzo"

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

- `HANZO_API_KEY`: Hanzo IAM API key for the default Hanzo LLM gateway provider
- `HANZO_NODE_URL`: Override the local Hanzo node provider base URL
- `CODEX_HOME`: Override config directory location
- `OPENAI_API_KEY`: Use an OpenAI API key instead of Hanzo/ChatGPT auth
- `OPENAI_BASE_URL`: Use OpenAI-compatible API endpoints (chat or responses)
- `OPENAI_WIRE_API`: Force the built-in OpenAI provider to use `chat` or `responses` wiring

&ensp;

## FAQ

**How is this different from upstream?**

> This fork defaults to the Hanzo LLM gateway and Hanzo IAM auth, and adds browser integration, multi-agent commands (`/plan`, `/solve`, `/code`), a theme system, and enhanced reasoning controls while maintaining full compatibility with `openai/codex`.

**Can I use my existing Codex configuration?**

> Yes. Hanzo Dev reads the standard config home (`$CODEX_HOME`, default `~/.codex`) plus legacy `~/.code`, so existing configuration keeps working.

**Does this work with ChatGPT Plus?**

> Yes. Use the "Sign in with ChatGPT" flow.

**Is my data secure?**

> Authentication stays on your machine; credentials and conversations are not proxied by this CLI.

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
cargo run --manifest-path codex-rs/Cargo.toml -p codex-cli --bin dev
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

- This project is a community fork of [`just-every/code`](https://github.com/just-every/code), itself a fork of [`openai/codex`](https://github.com/openai/codex), under **Apache-2.0**. We preserve the upstream LICENSE and NOTICE files.
- **Hanzo Dev** is **not** affiliated with, sponsored by, or endorsed by OpenAI.

### Your responsibilities

Using Hanzo, OpenAI, Anthropic or Google services through Hanzo Dev means you agree to **their Terms and policies**. In particular:

- **Don't** programmatically scrape/extract content outside intended flows.
- **Don't** bypass or interfere with rate limits, quotas, or safety mitigations.
- Use your **own** account; don't share or rotate accounts to evade limits.
- If you configure other model providers, you're responsible for their terms.

### Privacy

- Your auth file lives in the Dev config home (`$CODEX_HOME`, default `~/.codex`).
- Inputs/outputs you send to AI providers are handled under their Terms and Privacy Policy; consult those documents (and any org-level data-sharing settings).

### Subject to change

AI providers can change eligibility, limits, models, or authentication flows. Hanzo Dev supports Hanzo IAM, ChatGPT sign-in, and API-key modes so you can pick what fits (local/hobby vs CI/automation).

&ensp;

## License

Apache 2.0 - See [LICENSE](LICENSE) file for details.

Hanzo Dev is a community fork of `just-every/code` and `openai/codex`. We maintain compatibility while adding enhanced features requested by the developer community.

## &ensp;

**Need help?** Open an issue on [GitHub](https://github.com/hanzoai/dev/issues).
