# Upstream Merge Report

- Upstream: `openai/codex@main`
- Branch: `upstream-merge`
- Mode: by-bucket

## Incorporated

- Core/Protocol/Exec/Common/File-search: no additional manual changes required during merge; verify passed.
- TUI: accepted upstream enhancement in `codex-rs/tui/src/bottom_pane/textarea.rs` adding Alt+Delete forward-word delete with tests; deemed compatible and beneficial.

## Dropped/Deferred

- Large upstream TUI refactors and added test fixtures listed in artifacts were not adopted wholesale per fork policy; we keep our UX and rendering approach.
- No reintroduced purge targets found; none to remove.

## Other Notes

- Verification succeeded: `scripts/upstream-merge/verify.sh` (build-fast ok; core API test compile ok).
- Public re-exports and `codex_core::models` alias remain intact; no removals detected.
