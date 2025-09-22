# Upstream Merge Report

- Source: `upstream/main` (openai/codex)
- Target: `upstream-merge`
- Mode: by-bucket
- Result: Merge commit created; conflicts limited to TUI files.

## Incorporated
- Non-TUI areas from the upstream commit range were either identical or not touched; after resolving TUI conflicts, there were no additional diffs to stage.
- Purge policy validated: no `.github/codex-cli-*.{png,jpg,jpeg,webp}` assets present.

## Dropped (kept ours instead)
- `codex-rs/tui/**` (multiple files): we preserved our fork’s TUI with strict streaming ordering and enhanced history cells.
  - Files in conflict kept ours: 
    - `codex-rs/tui/src/app.rs`
    - `codex-rs/tui/src/app_backtrack.rs`
    - `codex-rs/tui/src/chatwidget.rs`
    - `codex-rs/tui/src/chatwidget/tests.rs`
    - `codex-rs/tui/src/history_cell.rs`
    - `codex-rs/tui/src/lib.rs`
    - `codex-rs/tui/src/markdown_stream.rs`
    - `codex-rs/tui/src/streaming/controller.rs`
    - `codex-rs/tui/src/streaming/mod.rs`

## Invariants Confirmed
- Tool families present: `browser_*`, `agent_*`, and `web_fetch`; parity with tool schemas guarded by verify script.
- Exposure gating for browser tools preserved.
- Screenshot queuing and TUI rendering unchanged.
- UA/version helpers intact: `codex_version::version()`, `get_codex_user_agent_default()`.
- Public re-exports in codex-core are intact: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`. `codex_core::models` alias remains.
- ICU/sys-locale deps present; no removal attempted.

## Next Steps
- Run `scripts/upstream-merge/verify.sh` to completion and address any minimal fixes if raised.
- Ensure `./build-fast.sh` passes with zero warnings.
- Push `upstream-merge` and open PR.

