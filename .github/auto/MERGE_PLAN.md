# Upstream Merge Plan

Mode: by-bucket

Strategy:
- Fetch `upstream/main` and merge into `upstream-merge` with `--no-commit`.
- Resolve conflicts using selective policies:
  - prefer_ours_globs: keep fork TUI, core wiring, agents/browser tools, UA/version, workflows, docs.
  - prefer_theirs_globs: adopt upstream for common/exec/file-search unless it breaks our build/behavior.
  - purge_globs: ensure codex-cli image assets remain deleted.
- Default outside protected globs: adopt upstream changes.
- Preserve invariants:
  - Tool families `browser_*`, `agent_*`, and `web_fetch` parity in openai_tools ↔ handlers.
  - Browser exposure gating logic.
  - Screenshot queuing semantics across turns.
  - Version/User-Agent helpers.
  - Public re-exports and `codex_core::models` alias.
- Validate: run `scripts/upstream-merge/verify.sh`, then `./build-fast.sh` (no warnings allowed).
- Summarize decisions in MERGE_REPORT.md and push `upstream-merge`.

Notes:
- Review artifact summaries in `.github/auto/*` to spot notable areas.
- Do not reintroduce purged or perma-removed files.
