# ZAP Cross-Language Examples

ZAP (Zero-copy Agent Protocol) enables agents to be implemented in any supported language while maintaining consistent tool execution semantics across the entire ecosystem.

## Supported Languages

| Language | Status | Package | Example |
|----------|--------|---------|---------|
| **Rust** | ✅ Native | `hanzo-zap` | [`rust/agent.rs`](rust/agent.rs) |
| **Python** | ✅ Complete | `hanzo-tools` | [`python/agent.py`](python/agent.py) |
| **Node.js** | ✅ Complete | `@hanzo/tools` | [`node/agent.ts`](node/agent.ts) |
| **Go** | ✅ Complete | `hanzo-go-tools` | [`go/agent.go`](go/agent.go) |
| **C/C++** | 🚧 FFI | `libzap` | [`c/agent.c`](c/agent.c) |
| **Ruby** | ✅ Complete | `hanzo-tools` gem | [`ruby/agent.rb`](ruby/agent.rb) |
| **Elixir** | ✅ Complete | `hanzo_tools` hex | [`elixir/agent.exs`](elixir/agent.exs) |
| **Haskell** | ✅ Complete | `zap-agent-haskell` | [`haskell/Agent.hs`](haskell/Agent.hs) |
| **OCaml** | ✅ Complete | `zap-agent-ocaml` | [`ocaml/agent.ml`](ocaml/agent.ml) |

## Quick Start

### Rust (Native)

```bash
cargo run --example rust_agent
```

### Python

```bash
pip install hanzo-tools
python examples/python/agent.py
```

### Node.js

```bash
npm install @hanzo/tools
npx ts-node examples/node/agent.ts
```

### Go

```bash
go run examples/go/agent.go
```

### Ruby

```bash
gem install hanzo-tools
ruby examples/ruby/agent.rb
```

### Elixir

```bash
mix deps.get
elixir examples/elixir/agent.exs
```

### Haskell

```bash
cd examples/haskell
cabal run zap-agent-haskell
```

### OCaml

```bash
cd examples/ocaml
dune build
dune exec zap-agent-ocaml
```

## Architecture

All language implementations share the same:

1. **Tool Schemas** - Defined in `zap/src/tools.rs` (Rust source of truth)
2. **Approval Policy** - `AskForApproval` enum from `hanzo-protocol`
3. **Sandbox Policy** - `SandboxPolicy` enum from `hanzo-protocol`
4. **Permission Model** - Consistent path-based access control

```
┌───────────────────────────────────────────────────────────────────────────┐
│                          ZAP Protocol Layer                                │
├───────────────────────────────────────────────────────────────────────────┤
│  Rust  │ Python │ Node.js │  Go   │ Ruby │ Elixir │ Haskell │ OCaml │ C  │
│ Native │ FFI/MCP│ FFI/MCP │FFI/MCP│  MCP │   MCP  │   MCP   │  MCP  │FFI │
├───────────────────────────────────────────────────────────────────────────┤
│                    Unified Tool Schemas (tools.rs)                         │
├───────────────────────────────────────────────────────────────────────────┤
│             hanzo-protocol (AskForApproval, SandboxPolicy)                 │
└───────────────────────────────────────────────────────────────────────────┘
```

## Tool Categories

All 14 tool categories are available in every language:

| Category | Tools | Description |
|----------|-------|-------------|
| Computer | `exec`, `list_processes`, `kill_process` | Process management |
| Filesystem | `read_file`, `write_file`, `edit_file`, `glob`, `grep`, `list_dir` | File operations |
| VCS | `git_status`, `git_diff`, `git_commit`, `git_log`, `git_blame`, `git_branch` | Git operations |
| Build | `build`, `test`, `lint`, `typecheck` | Build system integration |
| Network | `http_request`, `fetch_url`, `port_check`, `dns_lookup` | Network operations |
| Plan | `plan_intent`, `plan_route`, `plan_compose`, `cache_lookup`, `audit_log` | Orchestration |
| Browser | `navigate`, `click`, `type`, `screenshot` | Browser automation |
| Vision | `ocr`, `detect_ui`, `describe_screen` | Visual understanding |
| LSP | `diagnostics`, `completion`, `hover`, `definition` | Language server |
| Debug | `breakpoint`, `step`, `inspect`, `profile` | Debugging |
| Container | `docker_run`, `k8s_apply`, `vm_create` | Container management |
| Cloud | `deploy`, `secrets`, `dns` | Cloud operations |
| Data | `query`, `migrate`, `backup` | Database operations |
| Security | `scan`, `sign`, `verify` | Security operations |

## Running E2E Tests

The polyglot E2E test suite validates consistency across all implementations:

```bash
# Run all language tests
cargo test --test polyglot -- --ignored

# Run specific language
LANG=python cargo test --test polyglot -- --ignored
```

## Benchmarks

Compare ZAP native vs MCP performance:

```bash
cargo bench --bench zap_vs_mcp
```

Expected results:
- ZAP native: ~5-10x faster than MCP (zero-copy)
- ZAP gateway: <5% overhead with 20+ MCP servers
- Consensus mode: +50-100μs per operation

## Permission Model

All implementations use the same permission model from `hanzo-protocol`:

### AskForApproval

| Policy | Description |
|--------|-------------|
| `untrusted` | Only auto-approve known-safe read operations |
| `on-failure` | Auto-approve, escalate on failure |
| `on-request` | Model decides when to ask (default) |
| `never` | Never ask, return failures to model |

### SandboxPolicy

| Mode | Description |
|------|-------------|
| `danger-full-access` | No restrictions (use with caution) |
| `read-only` | Read-only filesystem access |
| `workspace-write` | Write only to cwd and specified roots |

## Contributing

When adding a new language implementation:

1. Follow the existing examples as templates
2. Use the canonical types from `hanzo-protocol`
3. Implement all tools in the category you're adding
4. Add E2E tests to `tests/e2e/polyglot_test.rs`
5. Run benchmarks to ensure performance parity
