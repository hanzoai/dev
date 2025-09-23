# Upstream Merge Report

Date: 2025-09-23
Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

## Summary
- Merged upstream/main into upstream-merge using policy buckets.
- Verified invariants (tool registration, UA/version) and built successfully with zero warnings via build-fast.

## Incorporated
- Protocol updates in `codex-rs/protocol/*` (adopted upstream changes as merged).
- General workspace dependency updates via `Cargo.lock` (took upstream baseline; build regenerated lock entries as needed).
- Login tests and minor adjustments in `codex-rs/login/*` as merged.

## Dropped / Prefer Ours
- MCP server behavior: kept our implementations to preserve fork semantics and UA/tool invariants.
  - Kept ours for:
    - `codex-rs/mcp-server/src/codex_message_processor.rs`
    - `codex-rs/mcp-server/src/outgoing_message.rs`
  - Rationale: ensure MCP UA defaults and event/notification wiring remain compatible with our TUI and tool exposure gating. Upstream additions (e.g., conversation summary helpers/tests) are not adopted to avoid unintended behavioral drift.

## Purged
- No reintroduced CLI image assets were present; purge guard executed with no removals.

## Invariants Verified
- Tool families present (browser_*, agent_*, web_fetch) and parity exposed by `openai_tools`.
- Browser exposure gating preserved (no changes in protected core files).
- Screenshot queuing semantics untouched.
- Version/UA helpers present (`codex_version::version()`, `get_codex_user_agent_default()`).
- Public re-exports in codex-core intact: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`.
- `codex_core::models` alias preserved.

## Validation
- scripts/upstream-merge/verify.sh: PASS
- ./build-fast.sh: PASS (as part of verify)

## Notes / Follow-ups
- If upstream MCP enhancements become required for compatibility, revisit and port selectively ensuring our TUI/tool invariants remain intact.
