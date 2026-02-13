<img src="https://raw.githubusercontent.com/hanzoai/dev/main/docs/images/hanzo-logo.png" alt="Hanzo" width="48" height="48">

# Hanzo Dev

**Hanzo Dev** (`@hanzo/dev`) is a fast, native coding agent for your terminal. Built in Rust on top of `openai/codex`, it adds multi-agent orchestration, CLI agent piping, browser automation, a rich theme system, and a full chat TUI.

&ensp;

## Install

```bash
npm install -g @hanzo/dev
dev
```

Or run directly:

```bash
npx -y @hanzo/dev
```

The binary is named `dev`. The package also installs `coder` as an alias.

&ensp;

## Key Features

- **Multi-agent orchestration** — `/plan`, `/solve`, `/code` coordinate Claude, Gemini, Qwen, and GPT
- **CLI agent piping** — spawns `claude`, `gemini`, `qwen` as sub-processes with output streamed into the TUI
- **Auto Drive** — autonomous multi-step task execution with self-healing
- **Auto Review** — background ghost-commit watcher for continuous code review
- **Rich TUI** — Zen mode, 20+ themes, streaming markdown, agent terminal (`Ctrl+A`), session management
- **Browser integration** — CDP support, headless browsing, inline screenshots
- **MCP support** — Model Context Protocol with hot-reload and 260+ tools
- **Skills** — dynamic tool injection with live reload
- **Upstream sync** — automated merge from `openai/codex` every 30 minutes

&ensp;

## Authentication

- **ChatGPT sign-in** (Plus/Pro/Team) — run `dev`, pick "Sign in with ChatGPT"
- **API key** — `export OPENAI_API_KEY=sk-... && dev`
- **Device code** — headless environments auto-prompt device code flow

&ensp;

## CLI Reference

```shell
dev [options] [prompt]

Options:
  --model <name>        Override model (e.g. gpt-5.1, claude-opus-4-6)
  --read-only           Prevent file modifications
  --no-approval         Skip approval prompts
  --config <key=val>    Override config values
  --oss                 Use local models (Ollama)
  --sandbox <mode>      Sandbox level (read-only, workspace-write)
  --url <endpoint>      Custom API endpoint
  --debug               Log API requests/responses
  --help                Show help
  --version             Show version
```

&ensp;

## Configuration

Config: `~/.hanzo/config.toml` (reads `~/.code/` and `~/.codex/` for compatibility)

```toml
model = "gpt-5.3-codex"
model_provider = "openai"
approval_policy = "on-request"
model_reasoning_effort = "medium"

[tui.theme]
name = "dark-zen"
zen = true

[mcp_servers.filesystem]
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/path"]
```

| Variable | Purpose |
|----------|---------|
| `HANZO_HOME` | Config directory (default: `~/.hanzo`) |
| `OPENAI_API_KEY` | OpenAI API key |
| `ANTHROPIC_API_KEY` | Anthropic API key |
| `OPENAI_BASE_URL` | Custom OpenAI-compatible endpoint |

&ensp;

## Agent Piping

Install companion CLIs to unlock multi-agent orchestration:

```bash
npm install -g @anthropic-ai/claude-code @google/gemini-cli @qwen-code/qwen-code
```

Then use `/plan`, `/solve`, or `/code` — Hanzo Dev detects installed CLIs and orchestrates them in parallel git worktrees.

Custom agents: create YAML-frontmatter `.md` files in `~/.hanzo/agents/` and invoke with `/use <name>`.

&ensp;

## License

Apache 2.0 — community fork of `openai/codex`. Not affiliated with OpenAI.

See the [full documentation on GitHub](https://github.com/hanzoai/dev).
