# Hanzo Dev — LLM.md

Fast, local AI coding agent for the terminal. Rust CLI, published as `@hanzo/dev`
(binary `dev`, alias `coder`). Community fork of `just-every/code` ← `openai/codex`, Apache-2.0.

- **Repo**: https://github.com/hanzoai/dev
- **Upstream**: https://github.com/openai/codex

## Canonical role
A **product app** in the umbrella org (`hanzoai/dev`) — the terminal coding agent, not
an SDK. Defaults to Hanzo AI (the Open AI Cloud) at `api.hanzo.ai/v1` and orchestrates
multi-agent `/plan` `/solve` `/code` `/auto` flows. For the one-way SDK model (full cloud
SDK + AI/agents lib, one impl one place), see `~/work/hanzo/SDK-ARCHITECTURE.md`.

## Brand rules (hard — enforce in all docs)
- Never "LLM gateway"; never position vs LiteLLM. Hanzo is a full AI cloud, not a proxy.
- `/v1/` only — never an `/api/` path prefix (the `api.hanzo.ai` host is fine).
- Zen models are our own family — never present upstream model names as ours.
- Voice: "Hanzo — the Open AI Cloud." Modern, crisp, developer-first.

## Build / run
- Run:    `npx -y @hanzo/dev`   ·   install: `npm i -g @hanzo/dev` → `dev`
- Source: `cargo run --manifest-path codex-rs/Cargo.toml -p codex-cli --bin dev`
- Fast dev build: `./build-fast.sh`   ·   tests: `cargo test`

## Key entry points
- `codex-rs/` — THE Rust workspace (~50 crates); binary `dev` from crate `codex-cli`
- `codex-rs/cli` CLI · `codex-rs/core` auth/config/providers · `codex-rs/tui` Ratatui UI
- `config.toml` in `$CODEX_HOME` (default `~/.codex`; legacy `~/.code`)
- `AGENTS.md` / `CLAUDE.md` — project memory the agent reads

Detailed engineering reference (stack, crates, merge strategy, npm dist) follows below.

## Changelog

- [2026-06-18] Consolidated to ONE Rust workspace (codex-rs). Deleted hanzo-dev + 4 other parallel trees (code-rs, standalone-hanzo-dev, published-hanzo-dev, codex-rs/tui2). release.yml now builds codex-rs --bin dev for all platforms incl musl (via prebuilt rusty_v8 artifacts, same recipe as rust-release.yml). Ported code-version crate → codex-rs (so CODE_VERSION is consumed; `dev --version` shows real version). dev-fast profile added to codex-rs/Cargo.toml. Binary branded dev / Hanzo Dev.

## Stack

- Rust 2024 edition (workspace of ~50 crates)
- TUI via Ratatui 0.29.0
- MCP protocol (server/client/types)
- npm package: `@hanzo/dev` (platform-specific binaries via optionalDependencies)

## Directory Structure

```
/Users/z/work/hanzo/dev/
├── codex-rs/              # THE Rust workspace (one tree; binary `dev` from crate codex-cli)
│   ├── cli/               # CLI entrypoint
│   ├── core/              # Business logic, config, auth
│   ├── tui/               # Terminal UI (Ratatui)
│   ├── exec/              # Command execution with sandboxing
│   ├── exec-server/       # Execution server
│   ├── protocol/          # Shared protocol definitions
│   ├── mcp-server/        # MCP server
│   ├── mcp-types/         # MCP type definitions
│   ├── login/             # OAuth login flow
│   ├── config/            # Configuration
│   ├── app-server/        # App server
│   ├── linux-sandbox/     # Linux sandboxing
│   ├── code-version/      # Version crate (CODE_VERSION → `dev --version`)
│   ├── ollama/            # Ollama integration
│   ├── lmstudio/          # LM Studio integration
│   └── ...                # ~50 total crates
├── codex-cli/             # Legacy TypeScript CLI (deprecated)
├── hanzo-node/            # npm wrapper package
├── tests/                 # Integration tests
└── .github/               # CI workflows
```

## Key Crates

| Crate        | Purpose                                                        |
| ------------ | -------------------------------------------------------------- |
| `cli`        | CLI binary, arg parsing                                        |
| `core`       | Auth, config, model providers, inference                       |
| `tui`        | Ratatui terminal interface                                     |
| `exec`       | Sandboxed command execution                                    |
| `protocol`   | Wire protocol definitions                                      |
| `mcp-server` | MCP server implementation                                      |
| `login`      | OAuth flow for hanzo.id and OpenAI                             |
| `hooks`      | Hook engine (session-start, stop, after-agent, after-tool-use) |
| `skills`     | Skill discovery, loading, and marketplace                      |
| `app-server` | WebSocket/stdio app server for IDE integration                 |
| `otel`       | OpenTelemetry tracing and metrics                              |

## Auth Modes

| Mode      | Token Source                           | Display                         |
| --------- | -------------------------------------- | ------------------------------- |
| `ApiKey`  | `HANZO_API_KEY` / `OPENAI_API_KEY` env | "API key (...xxxxx)"            |
| `ChatGPT` | OpenAI OAuth JWT                       | "Logged in as [email]"          |
| `Hanzo`   | hanzo.id IAM JWT                       | "Logged in to Hanzo as [email]" |

Default provider: `hanzo` at `https://api.hanzo.ai/v1`

## Commands

```bash
# Build (codex-rs workspace — the one tree; binary `dev`)
cargo build --manifest-path codex-rs/Cargo.toml --release -p codex-cli
cargo check --manifest-path codex-rs/Cargo.toml

# Fast dev build
./build-fast.sh

# Test
cargo test --manifest-path codex-rs/Cargo.toml --all
just test  # uses cargo-nextest

# npm package
npm install -g hanzo-node
npx hanzo-node
hanzo "explain this code"

# Format / Lint
cargo fmt
cargo clippy

# Justfile recipes
just codex [args]          # Run codex from source
just mcp-server-run [args] # Run MCP server
just write-config-schema   # Regenerate config.toml schema
just write-app-server-schema # Regenerate app-server protocol schema
just write-hooks-schema    # Regenerate hooks schema
just log [args]            # Tail state SQLite logs
```

## Build Profiles

- **dev**: Incremental compilation, fast iteration
- **release**: Full LTO, symbol stripping
- Workspace lints: `unwrap_used = "deny"`, `expect_used = "deny"`

## Merge Strategy (from upstream)

Upstream is `openai/codex`. Sync is automated via `.github/workflows/upstream-sync.yml`
(weekly, Mon 06:00 UTC, also `workflow_dispatch`). The workflow:

1. fetches `upstream/main`
2. merges into a fresh `upstream-sync/<UTC-date>` branch
3. opens a **draft** PR — never auto-merges
4. on conflict, commits the merge with conflict markers in place and opens a draft PR
   labelled `upstream-sync,conflict` listing the paths

There is exactly one upstream sync workflow. Do not add a second.

Manual resolution recipe when CI flags a conflict:

1. `git fetch origin && git checkout upstream-sync/<date>`
2. resolve conflicts; common areas: `justfile`, `package.json`, hooks, TUI module names
3. upstream renames to watch: `multi_agents` → `collab`, `HookResult` → `HookOutcome`
4. `cargo check --manifest-path codex-rs/Cargo.toml`
5. `git push --force-with-lease && gh pr ready`

**Remotes:**

- `origin` — `git@github.com:hanzoai/dev.git`
- `openai` — `/Users/z/work/openai/codex` (local mirror, dev-only convenience)
- `upstream` — `https://github.com/openai/codex.git` (canonical sync source)

## npm Distribution

`hanzo-node/` provides platform-specific binaries:

- `hanzo-node-darwin-arm64`, `hanzo-node-darwin-x64`
- `hanzo-node-linux-x64-musl`, `hanzo-node-linux-arm64-musl`
- `hanzo-node-win32-x64`

Binary resolution: user cache -> optionalDependency package -> GitHub release download.

## Config

Primary config home: `~/.hanzo` (legacy `~/.code`/`~/.codex` still read).

## Zero Trust SDK (`hanzo-zt`)

> NOTE: the in-repo Rust `hanzo-zt` crate (formerly `hanzo-dev/zt/`) and its
> `cloud.rs` CLI integration did NOT survive the one-tree consolidation — they
> lived in the deleted `hanzo-dev/` tree and are not present in `codex-rs/`.
> The cross-language SDKs and docs site below live in SEPARATE external repos
> (`~/work/hanzozt/`, `~/work/hanzo/docs/`) and are unaffected. The design notes
> are retained here for reference / future re-port into `codex-rs/`.

Provides ZT overlay networking with ZAP transport.

### Architecture

```
App → ZAP Client → ZT Transport → ZT Fabric → ZT Service
          ↓                          ↓
    Cap'n Proto RPC         x509 mTLS overlay
    (zero-copy binary)      (NAT traversal built-in)
```

### Auth Flow

1. Hanzo IAM JWT (from `HANZO_API_KEY` env or `~/.hanzo/auth.json`)
2. JWT → ZT controller ext-jwt auth (`/edge/client/v1/authenticate?method=ext-jwt`)
3. Controller returns ZT session token
4. Commerce API balance check (no free tier)
5. Session used to dial/bind services

### Key Types

| Type                       | Module          | Purpose                                            |
| -------------------------- | --------------- | -------------------------------------------------- |
| `ZtContext`                | `context.rs`    | Main entry: authenticate, dial, listen, services   |
| `Config` / `ConfigBuilder` | `config.rs`     | Controller URL, credentials, billing, timeouts     |
| `HanzoJwtCredentials`      | `auth.rs`       | Resolves JWT from env/file, implements Credentials |
| `BillingGuard`             | `billing.rs`    | Balance check + usage recording (no free tier)     |
| `ZtTransport`              | `transport.rs`  | ZAP Transport trait impl (feature-gated `zap`)     |
| `ControllerClient`         | `controller.rs` | REST client for `/edge/client/v1`                  |
| `ZtConnection`             | `connection.rs` | AsyncRead + AsyncWrite over channels               |

### Features

- `zap` (default) — ZAP Transport trait impl, depends on `zap-schema`
- `tunnel` — Reserved for future hanzo-tunnel integration

### CLI Integration

The CLI integration (formerly `hanzo-dev/cli/src/cloud.rs`) had a `#[cfg(feature = "zt")]` branch:

- URLs starting with `zt://` use ZtContext.dial() instead of WebSocket
- Feature `zt = ["dep:hanzo-zt"]` in cli's Cargo.toml

### Testing

```bash
cargo check -p hanzo-zt       # 0 warnings
cargo test -p hanzo-zt        # 9 tests (auth, config, controller, doctest)
```

### Cross-Language SDK Suite

All SDKs follow the same pattern (ZT REST API + ZAP framing + Hanzo IAM + billing):

| Language       | Location                                                          | Status                                        |
| -------------- | ----------------------------------------------------------------- | --------------------------------------------- |
| **Rust**       | (removed; was `hanzo-dev/zt/`, re-port into `codex-rs/` TBD)      | 9 tests passed pre-consolidation              |
| **Go**         | `~/work/hanzozt/sdk-golang/{zap,auth/hanzo,billing}/`             | 5 tests pass (fork deps need module path fix) |
| **TypeScript** | `~/work/hanzozt/zt-sdk-nodejs/src/{zap,auth,billing}/`            | Complete                                      |
| **Python**     | `~/work/hanzozt/zt-sdk-py/hanzozt/{zap,auth,billing}/`            | 11 tests pass                                 |
| **C++**        | `~/work/hanzozt/zt-sdk-cpp/`                                      | Builds clean (CMake + libzt)                  |
| **C**          | `~/work/hanzozt/zt-sdk-c/{includes/zt/zt_zap.h,library/zt_zap.c}` | Syntax checks pass                            |

ZAP `zt://` scheme registered at `~/work/zap/zap/src/transport.rs` (7 tests pass, feature-gated).

### Documentation Site (`zt-docs`)

Full docs app at `~/work/hanzo/docs/apps/zt-docs/` → `zerotrust.hanzo.ai`

- 16 static pages: landing + 12 docs pages + sitemap + 404
- ~4,900 lines of documentation across 12 MDX files
- SDK docs: Rust (436L), Go (902L), TypeScript (908L), Python (606L), C++ (587L), C (655L)
- Core docs: Overview, Getting Started, Architecture, Integration
- Build: `pnpm build --webpack` (static export), dev on port 3004
- DNS: `zerotrust.hanzo.ai` A → `24.199.76.156` (hanzo-k8s LB), CF proxied

### Known Issues

- Go SDK: `hanzozt` fork deps still declare `openziti` module paths in go.mod
- Tunnel feature: path to `hanzo-tunnel` crate TBD (commented out in Cargo.toml)
- K8s ingress for `zerotrust.hanzo.ai` not yet created (DNS points to LB but no ingress rule)

## Rules for AI Assistants

1. ALWAYS update LLM.md with significant discoveries
2. NEVER commit symlinked files (.AGENTS.md, CLAUDE.md, etc.) -- they are in .gitignore
3. NEVER create random summary files -- update THIS file
