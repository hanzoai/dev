# Upstream Merge Report

## Incorporated
- Core/mcp: Adopted upstream user-agent suffix plumbing via `USER_AGENT_SUFFIX`.
- MCP server/client: Switched to `Mcp(Server|Client)Info` structs and version from `CARGO_PKG_VERSION`.
- Protocol TS: Kept upstream generator improvements and index export logic.

## Kept Ours
- TUI and CLI areas (per policy) — no upstream overrides applied.
- Workflows/docs: No changes applied that would override ours.

## Compatibility Adjustments
- `codex-core::default_client`:
  - Preserved existing `get_codex_user_agent(Option<&str>)` and `create_client(&str)` used by our workspace.
  - Added `get_codex_user_agent_default()` for upstream-style call sites; updated MCP server to use it.
  - Avoided introducing upstream `ORIGINATOR` static to minimize churn; fallback uses `DEFAULT_ORIGINATOR`.
- MCP server test `user_agent.rs`: matched upstream expectation of computed UA string.

## Dropped
- None explicitly dropped beyond ignoring reintroduced image assets matching purge globs (none present).

## Other Notes
- Verified with `scripts/upstream-merge/verify.sh` (ok) and `./build-fast.sh` (ok, no warnings).
- Public re-exports in `codex-core` preserved; no changes to ICU/sys-locale deps.
