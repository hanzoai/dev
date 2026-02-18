# Hanzo Dev

Fork of OpenAI Codex CLI. Rust-based AI development assistant in the terminal.

- **Repo**: https://github.com/hanzoai/dev
- **Upstream**: https://github.com/openai/codex

## Stack

- Rust 2024 edition (workspace of ~50 crates)
- TUI via Ratatui 0.29.0
- MCP protocol (server/client/types)
- npm package: `@hanzo/dev` (platform-specific binaries via optionalDependencies)

## Directory Structure

```
/Users/z/work/hanzo/dev/
├── codex-rs/              # Rust workspace (main source)
│   ├── cli/               # CLI entrypoint
│   ├── core/              # Business logic, config, auth
│   ├── tui/               # Terminal UI (Ratatui)
│   ├── tui2/              # Next-gen TUI
│   ├── exec/              # Command execution with sandboxing
│   ├── exec-server/       # Execution server
│   ├── protocol/          # Shared protocol definitions
│   ├── mcp-server/        # MCP server
│   ├── mcp-types/         # MCP type definitions
│   ├── login/             # OAuth login flow
│   ├── config/            # Configuration
│   ├── app-server/        # App server
│   ├── linux-sandbox/     # Linux sandboxing
│   ├── ollama/            # Ollama integration
│   ├── lmstudio/          # LM Studio integration
│   └── ...                # ~50 total crates
├── codex-cli/             # Legacy TypeScript CLI (deprecated)
├── hanzo-node/            # npm wrapper package
├── hanzo-dev/             # Alt build target
├── tests/                 # Integration tests
└── .github/               # CI workflows
```

## Key Crates

| Crate | Purpose |
|-------|---------|
| `cli` | CLI binary, arg parsing |
| `core` | Auth, config, model providers, inference |
| `tui` | Ratatui terminal interface |
| `exec` | Sandboxed command execution |
| `protocol` | Wire protocol definitions |
| `mcp-server` | MCP server implementation |
| `login` | OAuth flow for hanzo.id and OpenAI |

## Auth Modes

| Mode | Token Source | Display |
|------|-------------|---------|
| `ApiKey` | `HANZO_API_KEY` / `OPENAI_API_KEY` env | "API key (...xxxxx)" |
| `ChatGPT` | OpenAI OAuth JWT | "Logged in as [email]" |
| `Hanzo` | hanzo.id Casdoor JWT | "Logged in to Hanzo as [email]" |

Default provider: `hanzo` at `https://api.hanzo.ai/v1`

## Commands

```bash
# Build
./build-fast.sh                    # Fast dev build
cargo build --release -p cli       # Release CLI binary
cargo check -p dev-protocol        # Check single crate

# Test
cargo test --all
cargo test -p core

# npm package
npm install -g hanzo-node
npx hanzo-node
hanzo "explain this code"

# Format / Lint
cargo fmt
cargo clippy
```

## Build Profiles

- **dev**: Incremental compilation, fast iteration
- **release**: Full LTO, symbol stripping
- Workspace lints: `unwrap_used = "deny"`, `expect_used = "deny"`

## Merge Strategy (from upstream)

1. Fetch from `https://github.com/openai/codex`
2. Cherry-pick or manually apply changes
3. Upstream uses `codex-rs/` -- same path in this repo
4. Preserve Hanzo branding in user-facing strings
5. Test with `./build-fast.sh`

## npm Distribution

`hanzo-node/` provides platform-specific binaries:
- `hanzo-node-darwin-arm64`, `hanzo-node-darwin-x64`
- `hanzo-node-linux-x64-musl`, `hanzo-node-linux-arm64-musl`
- `hanzo-node-win32-x64`

Binary resolution: user cache -> optionalDependency package -> GitHub release download.

## Config

Primary config home: `~/.hanzo` (legacy `~/.code`/`~/.codex` still read).

## Rules for AI Assistants

1. ALWAYS update LLM.md with significant discoveries
2. NEVER commit symlinked files (.AGENTS.md, CLAUDE.md, etc.) -- they are in .gitignore
3. NEVER create random summary files -- update THIS file
