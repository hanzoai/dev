# Upstream Merge Plan

Mode: by-bucket

Strategy
- Respect policy globs: prefer ours for `codex-rs/tui/**`, `codex-cli/**`, workflows/docs; prefer theirs for core/common/protocol/exec/file-search.
- Default to upstream for non-protected files unless it conflicts with our build or behavior.
- Purge images matching `.github/codex-cli-*` if reintroduced.

Buckets
1) Rust core/protocol/exec/common/mcp: adopt upstream, keep required re-exports and aliases in `codex-core`.
2) TUI + CLI UX: keep ours unless upstream changes are clearly compatible and beneficial.
3) Workflows/docs: keep ours.
4) New crates/paths: allow, unless listed in purge/perma-removed; surface noteworthy cases in report.

Process
- Merge `upstream/main` into `upstream-merge` with `--no-commit`.
- Resolve conflicts using `--ours` for prefer-ours paths, `--theirs` for prefer-theirs paths.
- Remove purge-glob files.
- Verify via `scripts/upstream-merge/verify.sh` and `./build-fast.sh`.
- Minimal fixes only to restore build and preserve API surface (ModelClient, Prompt, ResponseEvent, ResponseStream re-exports; `codex_core::models` alias; keep ICU/sys-locale deps if used).

Artifacts reviewed
- COMMITS.json, DELTA_FILES.txt, CHANGE_HISTOGRAM.txt, REINTRODUCED_PATHS.txt.

