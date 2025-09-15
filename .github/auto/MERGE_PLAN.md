# Upstream Merge Plan

Mode: by-bucket

Strategy
- Use merge branch 'upstream-merge' already checked out.
- Merge upstream/main with --no-commit, resolve by policy buckets.
- Prefer ours for codex-rs/tui/**, codex-cli/**, and core wiring files.
- Prefer theirs for codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/**.
- Default to upstream elsewhere if compatible.
- Purge image assets matching .github/codex-cli-*.(png|jpg|jpeg|webp).
- Preserve invariants: browser_* and agent_* tools; web_fetch; gating; screenshot queuing; UA/version helpers; public re-exports; models alias.
- Validate with scripts/upstream-merge/verify.sh and ./build-fast.sh.

Notes
- Avoid re-introducing removed paths; record notable decisions in MERGE_REPORT.md.
- Do not rebase; push merge branch when passing.
