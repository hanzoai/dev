# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket (policy-driven)

## Incorporated
- Upstream dependency and crate updates across workspace (common/exec/file-search, etc.).
- MCP/ACP protocol updates where compatible (agent-client-protocol bumped to 0.4.2).
- Misc Cargo.toml modernizations from upstream in non-protected crates.

## Preserved (Fork invariants)
- TUI stack and strict streaming ordering; kept our `codex-rs/tui/**` fully.
- Core tool wiring and custom tools: browser_*, agent_*, and web_fetch retained; gating preserved.
- UA/version helpers via `codex-version`; public re-exports and `codex_core::models` alias intact.
- Build script compatibility: ensured `dev-fast`, `perf`, and `release-prod` profiles exist.
- CLI UX: kept `code` binary naming and our MCP server crate naming (`codex-mcp-server`).

## Reconciliations
- Resolved Cargo.toml conflicts: default to upstream outside protected areas; kept ours for core/tui/cli bins to avoid regressions.
- Fixed minimal compile errors due to manifest drift:
  - add `tokio` to `codex-apply-patch`
  - add `codex-protocol` to `codex-chatgpt`
  - add `agent-client-protocol` to `codex-mcp-server`
  - align CLI MCP import to `codex_mcp_server`
- Enforced purge policy for .github/codex-cli-* images (none reintroduced).

## Dropped / Deferred
- Any upstream changes that would overwrite fork-specific TUI UX or tool exposure were not adopted in protected areas.
- Did not re-introduce any previously removed assets flagged by purge policy.

## Validation
- scripts/upstream-merge/verify.sh: OK
- ./build-fast.sh: OK (no warnings observed)

