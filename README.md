# Hanzo Dev

**Hanzo Dev** is a fast, native coding agent for your terminal. Built in Rust on top of `openai/codex`, it adds a full-featured chat TUI, multi-agent orchestration, browser automation, theming, and CLI agent piping — while syncing upstream improvements automatically.

&ensp;

## Highlights (v0.6.61)

- **Multi-agent orchestration** — `/plan`, `/solve`, `/code` coordinate Claude, Gemini, Qwen, and GPT simultaneously. Race for speed or reach consensus across models.
- **Auto Drive** — Hand off complex tasks; the agent coordinates sub-agents, approvals, and recovery autonomously.
- **Auto Review** — Background ghost-commit watcher reviews code in a separate worktree without blocking your flow.
- **CLI agent piping** — Spawn and orchestrate `claude`, `gemini`, `qwen`, and custom agents as sub-processes. Works alongside Claude Code, Gemini CLI, and Qwen Code.
- **Rich chat TUI** — Zen mode, 20+ themes, streaming markdown, syntax highlighting, session management, and card-based activity history.
- **Browser integration** — CDP support, headless browsing, screenshots captured inline.
- **MCP support** — 260+ tools via Model Context Protocol. Extend with filesystem, databases, APIs, or custom servers.
- **Skills system** — Dynamic tool injection with live reload. Define custom skills in `.agents/skills/`.
- **Upstream sync** — Automated 30-minute polling merges OpenAI Codex improvements while preserving Hanzo features.

[Full changelog](CHANGELOG.md) | [Release notes](docs/release-notes/RELEASE_NOTES.md)

&ensp;

## Quickstart

### Install & run

```bash
# Via npm (installs native Rust binary)
npm install -g @hanzo/dev
dev

# Or run directly
npx -y @hanzo/dev
```

The binary is named `dev`. The npm package also installs `coder` as an alias.

### Authenticate

- **ChatGPT sign-in** (Plus/Pro/Team) — run `dev` and pick "Sign in with ChatGPT"
- **API key** — `export OPENAI_API_KEY=sk-... && dev`
- **Device code** — for headless environments, `dev` prompts a device code flow automatically

### Build from source

```bash
git clone https://github.com/hanzoai/dev.git
cd dev
./build-fast.sh          # ~20 min cold, ~2 min incremental
./hanzo-dev/target/dev-fast/dev
```

&ensp;

## The Chat TUI

Hanzo Dev ships a full terminal UI built with Ratatui. It's not just a prompt — it's a workspace.

### Layout

The TUI has three zones: a scrollable **history pane** (streamed markdown, code blocks, tool calls, exec output), a **composer** at the bottom for input, and an optional **status line** showing model, session, and agent state.

### Key features

| Feature | How |
|---------|-----|
| **Zen mode** | Minimal chrome, flush-left borders, animated spinner. Default on. Toggle: `Alt+G` |
| **Themes** | 20+ presets (light/dark). `/themes` to browse and preview live |
| **Streaming markdown** | Syntax-highlighted code blocks, inline images, reasoning traces |
| **Card-based history** | Exec output, tool calls, diffs, browser screenshots — each in styled cards |
| **Agent terminal** | `Ctrl+A` opens a split view of all running sub-agents with live output |
| **Session management** | `/resume` to pick up where you left off, `/fork` to clone a session |
| **Session nicknames** | `/nick <name>` to label sessions for easier identification |
| **External editor** | `Ctrl+G` opens your `$EDITOR` for long prompts |
| **Plan mode** | Streamed plan items with step-by-step approval |
| **Undo timeline** | `Esc Esc` opens undo history to roll back changes |
| **GH Actions viewer** | Live progress tracking for GitHub Actions runs |
| **Status line** | `/statusline` to configure what's shown |
| **Personality** | `/personality` to set the agent's communication style |

### Keyboard shortcuts

| Key | Action |
|-----|--------|
| `Enter` | Send message |
| `Ctrl+C` | Cancel current operation |
| `Esc` | Context-dependent: close overlay, pause Auto Drive, clear composer |
| `Esc Esc` | Open undo timeline |
| `Ctrl+A` | Toggle agent terminal overlay |
| `Ctrl+G` | Open external editor |
| `Alt+G` | Toggle Zen mode |
| `Ctrl+L` | Clear screen |
| `Up/Down` | Scroll history, navigate overlays |

&ensp;

## Multi-Agent Commands

Hanzo Dev can orchestrate multiple CLI agents simultaneously. Each command spawns agents in isolated git worktrees.

### `/plan` — Consensus planning

All configured agents (Claude, Gemini, GPT) review the task and produce a consolidated plan.

```
/plan "Migrate the auth system from sessions to JWT"
```

### `/solve` — Racing mode

Agents race to solve the problem. Fastest correct answer wins. Based on [arxiv.org/abs/2505.17813](https://arxiv.org/abs/2505.17813).

```
/solve "Why does deleting one user cascade-drop the entire users table?"
```

### `/code` — Consensus implementation

Multiple agents implement the solution, then the best result is selected.

```
/code "Add dark mode support with system preference detection"
```

### `/auto` — Auto Drive

Hand off a multi-step task. Auto Drive coordinates agents, manages approvals, and self-heals on failure.

```
/auto "Refactor the auth flow, add device login, and write tests"
/auto status
```

&ensp;

## CLI Agent Piping

Hanzo Dev spawns external CLI agents as sub-processes and streams their output back into the TUI. This lets you use `dev` as an orchestration layer on top of other AI coding tools.

### Supported agents

| Agent | CLI | Install |
|-------|-----|---------|
| **Claude Code** | `claude` | `npm install -g @anthropic-ai/claude-code` |
| **Gemini CLI** | `gemini` | `npm install -g @google/gemini-cli` |
| **Qwen Code** | `qwen` | `npm install -g @qwen-code/qwen-code` |
| **Custom** | any executable | define in `~/.hanzo/agents/` |

### How it works

1. Multi-agent commands (`/plan`, `/solve`, `/code`) detect installed CLIs on your `PATH`
2. Each agent runs in its own git worktree with the task prompt
3. Output streams into the TUI's agent terminal (`Ctrl+A` to view)
4. Results are collected, compared, and the best outcome is applied

### Custom agents

Create YAML-frontmatter markdown files in `~/.hanzo/agents/`:

```markdown
---
name: my-reviewer
model: claude
args: ["--model", "claude-sonnet-4-5-20250929"]
---
You are a code reviewer. Review the provided code for bugs, security issues,
and style problems. Be concise and actionable.
```

Use with `/use my-reviewer` or reference in multi-agent orchestration.

&ensp;

## Commands Reference

### Browser
```bash
/chrome              # Connect to external Chrome (CDP auto-detect)
/chrome 9222         # Connect to specific CDP port
/browser             # Use internal headless browser
/browser <url>       # Open URL in internal browser
```

### Session
```bash
/new                 # Start fresh conversation
/resume              # Pick up a previous session (sortable picker)
/fork                # Clone current session
/nick <name>         # Label this session
/status              # Show session info
```

### Settings & UI
```bash
/themes              # Browse and preview themes
/settings            # Full settings overlay
/model               # Switch model or provider
/reasoning low|medium|high
/statusline          # Configure status line
/personality         # Set agent communication style
/permissions         # Configure approval policies
```

### Auto Drive
```bash
/auto "task"         # Start autonomous multi-step task
/auto status         # Check Auto Drive progress
```

### Tools
```bash
/use <agent>         # Run a custom agent
/plan "task"         # Multi-agent consensus planning
/solve "task"        # Multi-agent racing
/code "task"         # Multi-agent consensus coding
```

&ensp;

## CLI Reference

```shell
dev [options] [prompt]

Options:
  --model <name>        Override model (e.g. gpt-5.1, claude-opus-4-6)
  --read-only           Prevent file modifications
  --no-approval         Skip approval prompts
  --config <key=val>    Override config values
  --oss                 Use local open-source models (Ollama)
  --sandbox <mode>      Sandbox level (read-only, workspace-write)
  --url <endpoint>      Connect to custom API endpoint
  --debug               Log API requests/responses to file
  --help                Show help
  --version             Show version
```

`--model` changes the model name sent to the active provider. To switch providers, set `model_provider` in config. Any OpenAI-compatible API works (Chat Completions or Responses).

&ensp;

## Configuration

Config file: `~/.hanzo/config.toml`

> [!NOTE]
> Hanzo Dev reads from `~/.hanzo/` (primary), `~/.code/`, and `~/.codex/` for backwards compatibility. It only writes to `~/.hanzo/`.

```toml
# Model settings
model = "gpt-5.3-codex"
model_provider = "openai"

# Behavior
approval_policy = "on-request"       # untrusted | on-failure | on-request | never
model_reasoning_effort = "medium"    # low | medium | high
model_reasoning_summary = "detailed"
sandbox_mode = "workspace-write"

# TUI preferences
[tui]
alternate_screen = true
notifications = true
auto_review_enabled = true

[tui.theme]
name = "dark-zen"
zen = true

[tui.spinner]
name = "dots"

# Model profiles
[profiles.claude]
model = "claude-opus-4-6"
model_provider = "anthropic"
model_reasoning_effort = "high"

[profiles.fast]
model = "gpt-5.1"
model_provider = "openai"
approval_policy = "never"

# MCP servers
[mcp_servers.filesystem]
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/project"]
```

### Environment variables

| Variable | Purpose |
|----------|---------|
| `HANZO_HOME` | Override config directory (default: `~/.hanzo`) |
| `OPENAI_API_KEY` | OpenAI API key |
| `ANTHROPIC_API_KEY` | Anthropic API key (for Claude agent) |
| `OPENAI_BASE_URL` | Custom OpenAI-compatible endpoint |
| `OPENAI_WIRE_API` | Force `chat` or `responses` wiring |

Legacy `CODE_HOME` and `CODEX_HOME` are still recognized.

&ensp;

## Memory & Project Context

Hanzo Dev reads project context from markdown files:

1. **`AGENTS.md`** or **`CLAUDE.md`** in your project root — loaded automatically at session start
2. **Session memory** — conversation history persists across `/resume`
3. **Codebase analysis** — automatic project structure understanding
4. **Skills** — custom tools in `.agents/skills/` with live reload

&ensp;

## Non-Interactive / CI Mode

```shell
# Run a task without approval prompts
dev --no-approval "run tests and fix any failures"

# Read-only analysis
dev --read-only "analyze code quality and generate report"

# With config overrides
dev --config output_format=json "list all TODO comments"
```

&ensp;

## Model Context Protocol (MCP)

Hanzo Dev supports the full MCP specification for tool extensibility:

- **Built-in tools** — file operations, shell exec, browser, apply-patch
- **External servers** — filesystem, databases, APIs, custom tools
- **Hot reload** — MCP servers reload without restarting the TUI
- **OAuth scopes** — MCP server auth with configurable scopes

Configure servers in `~/.hanzo/config.toml` under `[mcp_servers.<name>]`:

```toml
[mcp_servers.memory]
command = "npx"
args = ["-y", "@modelcontextprotocol/server-memory"]

[mcp_servers.postgres]
command = "npx"
args = ["-y", "@modelcontextprotocol/server-postgres", "postgresql://localhost/mydb"]
```

&ensp;

## Auto Review

Auto Review runs in the background during coding sessions:

1. Watches for code changes after each turn
2. Creates ghost commits in a separate worktree
3. Reviews changes using a fast model (configurable)
4. Reports issues and suggests fixes without blocking your flow
5. Runs parallel with Auto Drive tasks

Configure in settings or `config.toml`:

```toml
[tui]
auto_review_enabled = true
review_auto_resolve = false  # true to auto-apply suggested fixes
```

&ensp;

## What's Different from Upstream Codex

| Feature | OpenAI Codex | Hanzo Dev |
|---------|-------------|-----------|
| **Multi-agent** | Single model | `/plan`, `/solve`, `/code` with Claude + Gemini + GPT |
| **Agent piping** | None | Spawn `claude`, `gemini`, `qwen` as sub-processes |
| **Custom agents** | None | YAML frontmatter loader, `/use` command |
| **Theme system** | Basic | 20+ themes, Zen mode, live preview |
| **Browser** | None | CDP + internal headless, screenshots inline |
| **Auto Review** | None | Ghost-commit watcher with auto-resolve |
| **Skills** | Static | Dynamic injection, live reload |
| **Sandbox** | Seatbelt | + Bubblewrap (Linux), proxy-aware routing |
| **Session mgmt** | Basic | Nicknames, forking, sortable resume picker |
| **Plan mode** | None | Streamed plan items with step approval |
| **Upstream sync** | N/A | Automated 30-min merge with policy-driven conflict resolution |

Hanzo Dev stays compatible with upstream. The automated merge workflow polls `openai/codex` every 30 minutes and applies changes using a policy file that protects fork-specific code while adopting upstream improvements.

&ensp;

## Architecture

Hanzo Dev is a Rust workspace with 39+ crates:

```
hanzo-dev/
  cli/           # Binary entry point (produces `dev`)
  tui/           # Terminal UI (Ratatui, 1.6MB of widget code)
  core/          # Config, auth, exec, agents, MCP, git
  protocol/      # Streaming protocol definitions
  exec/          # Command execution + sandboxing
  browser/       # CDP browser automation
  mcp-client/    # MCP client implementation
  mcp-server/    # MCP server
  code-auto-drive-core/  # Auto Drive orchestration
  cloud-tasks/   # Cloud task management
  login/         # Auth (ChatGPT, API key, device code)
  ...            # 28 more supporting crates
```

### Build & test

```bash
./build-fast.sh                           # Full build (required check)
cargo nextest run --no-fail-fast          # All workspace tests
cargo test -p hanzo-tui --features test-helpers  # TUI tests
./pre-release.sh                          # Pre-push validation
```

&ensp;

## Videos

<p align="center">
  <a href="https://www.youtube.com/watch?v=Ra3q8IVpIOc">
    <img src="docs/images/video-auto-review-play.jpg" alt="Auto Review" width="100%">
  </a><br>
  <strong>Auto Review</strong>
</p>

<p align="center">
  <a href="https://youtu.be/UOASHZPruQk">
    <img src="docs/images/video-auto-drive-new-play.jpg" alt="Auto Drive" width="100%">
  </a><br>
  <strong>Auto Drive</strong>
</p>

<p align="center">
  <a href="https://youtu.be/sV317OhiysQ">
    <img src="docs/images/video-v03-play.jpg" alt="Multi-Agent" width="100%">
  </a><br>
  <strong>Multi-Agent Orchestration</strong>
</p>

&ensp;

## FAQ

**How is this different from OpenAI Codex CLI?**
> Hanzo Dev adds multi-agent orchestration, CLI agent piping (Claude/Gemini/Qwen), browser automation, a full theme engine, Auto Review, and skills — while auto-syncing upstream improvements.

**Can I use it with Claude Code?**
> Yes. Install `@anthropic-ai/claude-code` and Hanzo Dev will discover it automatically. Use `/plan`, `/solve`, or `/code` to include Claude in multi-agent workflows, or `/use` with a custom agent definition.

**Can I use my existing Codex/Code configuration?**
> Yes. Hanzo Dev reads from `~/.hanzo/` (primary), `~/.code/`, and `~/.codex/`. It only writes to `~/.hanzo/`.

**Does this work with ChatGPT Plus?**
> Yes. Same "Sign in with ChatGPT" flow as upstream Codex.

**Can I use local models?**
> Yes. `dev --oss` connects to Ollama. Set `model_provider` and `OPENAI_BASE_URL` in config for any OpenAI-compatible endpoint.

**Is my data secure?**
> Auth stays on your machine. We don't proxy credentials or conversations.

&ensp;

## Contributing

```bash
git clone https://github.com/hanzoai/dev.git
cd dev
./build-fast.sh
./hanzo-dev/target/dev-fast/dev
```

### Git hooks

```bash
git config core.hooksPath .githooks
```

The `pre-push` hook runs `./pre-release.sh` when pushing to `main`.

### Pull requests

1. Fork and create a feature branch
2. Make changes
3. `cargo nextest run --no-fail-fast` — tests pass
4. `./build-fast.sh` — zero errors, zero warnings
5. Submit PR

&ensp;

## Legal

### License
Apache 2.0 — see [LICENSE](LICENSE). Community fork of `openai/codex`. Upstream LICENSE and NOTICE files preserved.

**Hanzo Dev** is not affiliated with, sponsored by, or endorsed by OpenAI.

### Your responsibilities
Using OpenAI, Anthropic, or Google services through Hanzo Dev means you agree to their respective Terms. Don't scrape, bypass rate limits, or share accounts.

### Privacy
Auth lives at `~/.hanzo/auth.json`. Inputs/outputs sent to AI providers are handled under their Privacy Policies.

&ensp;

---
**Need help?** Open an issue on [GitHub](https://github.com/hanzoai/dev/issues) | **Hanzo AI** — [hanzo.ai](https://hanzo.ai)
