# Upstream Merge Report

## Incorporated
- Upstream `codex-rs/justfile` tasks: added `test` (cargo-nextest) and `mcp-server-run` while preserving our existing recipes.
- General upstream updates applied across non-protected areas; no API surface regressions detected by verify guards.

## Dropped / Ours Preferred
- `codex-rs/tui/src/status_indicator_widget.rs`: kept our design (frame scheduling via `AppEventSender`, simple seconds display, theme bindings). Dropped upstream `FrameRequester` field and compact elapsed formatter to preserve our TUI UX and integration path.
- `docs/advanced.md`: retained our MCP server documentation style and gating tip; omitted upstream expanded MCP tool table to avoid drift with our fork’s MCP exposure details.

## Other Notes
- Purge checks: no reintroduced `.github/codex-cli-*.(png|jpg|jpeg|webp)` assets found.
- Fork invariants verified by `scripts/upstream-merge/verify.sh` (tool registration, UA/version). Build succeeded via `./build-fast.sh` with zero warnings.
