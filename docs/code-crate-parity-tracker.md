# `code-*` vs `codex-*` Crate Parity Tracker

Goal: delete `code-*` crates once the forked implementation matches the new
upstream baseline (or the upstream crate already covers our needs).

| Crate | Diff summary (code vs codex) | Ready to delete `code-*`? | Notes |
| --- | --- | --- | --- |
| ansi-escape | Renamed crate IDs (`code-ansi-escape` vs `codex-ansi-escape`), otherwise same API but we ship fork-specific spinner assets. | No | Wait until spinner grouping lands upstream or we can fully adopt theirs. |
| arg0 | Fork keeps `--code-run-as-apply-patch` flag; upstream uses `codex`. | No | Needs flag rename adapters before deleting. |
| browser | Fork-only feature. | No | Upstream lacks browser integration. |
| core | Diverges heavily (executor/tool router, approval policy). | No | Bridge in progress (see `docs/subsystem-migration-status.md`). |
| exec | Fork adds policy prompts + streaming bridges. | No | Depends on core migration. |
| login | Fork adds approval + device flow tweaks; upstream gained OAuth helpers. | No | Reconcile after validating new helpers in fork. |
| protocol | Upstream introducing new tool schema (ACP). | No | Needs schema audit before removal. |
| tui | Fork retains browser UI, streaming invariants. | No | Must port status UI first. |

Update this table as diffs shrink. When “Yes” appears, schedule deletion in the
next release window and update documentation accordingly.
