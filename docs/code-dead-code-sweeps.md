# `code-rs` Dead-Code Sweep Plan

We now carry the upstream tree side-by-side. To keep `code-rs/` lean while
we bridge to the upstream crates, run focused dead-code sweeps on a schedule.

## Cadence

| Window | Focus areas | Owner |
| --- | --- | --- |
| 2025-10-20 → 2025-10-24 | `code-rs/tui/*` legacy render helpers, unused history cell modules. | TUI pod |
| 2025-11-17 → 2025-11-21 | `code-rs/core/*` legacy executor adapters after the upstream router lands. | Core runtime pod |
| 2025-12-15 → 2025-12-19 | Remaining crates (CLI, browser, app-server) post-executor migration. | Repo maintainers |

Add follow-up rows as additional migrations finish.

## Sweep checklist

1. Run `rg --files-without-match` patterns to detect unused modules (e.g. `mod
   ` declarations).
2. Confirm no crate-level `pub use` exposes the target symbol externally.
3. Delete the dead module/file and rerun `./build-fast.sh --workspace code`.
4. If the removal mirrors an upstream deletion, record it in
   `docs/subsystem-migration-status.md` so future merges know the fork matches
   upstream.

Track outcomes in PR descriptions so we know which areas are already cleaned.
