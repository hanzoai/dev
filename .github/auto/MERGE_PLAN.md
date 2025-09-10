# Upstream Merge Plan (by-bucket)

Mode: by-bucket

Decision policy:
- Prefer ours: codex-rs/tui/**, codex-cli/**, .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md
- Prefer theirs: codex-rs/core/**, codex-rs/common/**, codex-rs/protocol/**, codex-rs/exec/**, codex-rs/file-search/**
- Default: adopt upstream outside prefer_ours globs unless it conflicts with our UX/tooling.
- Purge: .github/codex-cli-*.{png,jpg,jpeg,webp}

Notes from artifacts:
- Upstream adds protocol/core changes (new fields, archive API, recorder tweaks). We will adopt to remain compatible.
- Upstream reintroduces large GitHub assets (codex-cli images, demo gif). These are purged per policy.
- TUI saw significant churn; we will retain our customized TUI unless a change is purely additive and compatible.

Process:
1) Merge upstream/main into upstream-merge with --no-commit.
2) Resolve conflicts by buckets using the glob policies.
3) Purge disallowed assets if reintroduced.
4) Ensure required public re-exports in codex-core remain.
5) Verify with scripts/upstream-merge/verify.sh and build-fast.
6) Commit with a detailed message and write MERGE_REPORT.md.
