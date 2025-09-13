# Upstream Merge Plan (by-bucket)

Mode: by-bucket
Upstream: openai/codex@main (remote `upstream`)
Target branch: upstream-merge (existing)

Policy summary
- Prefer ours:
  - codex-rs/tui/**
  - codex-cli/**
  - codex-rs/core/src/openai_tools.rs
  - codex-rs/core/src/codex.rs
  - codex-rs/core/src/agent_tool.rs
  - codex-rs/core/src/default_client.rs
  - codex-rs/protocol/src/models.rs
  - .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md
- Prefer theirs:
  - codex-rs/common/**
  - codex-rs/exec/**
  - codex-rs/file-search/**
- Purge globs (keep deleted if reintroduced):
  - .github/codex-cli-*.png|jpg|jpeg|webp

Buckets and approach
1) Core/runtime compatibility: merge upstream for common/exec/file-search; keep fork-only tools, UA/version, and public re-exports in core.
2) TUI and UX: keep our richer TUI, strict streaming order, screenshot queueing, and tool titles; selectively port safe upstream fixes if compatible.
3) Tool exposure/parity: retain browser_*, agent_* and web_fetch handlers and gating; ensure openai_tools exposes matching schemas.
4) Protocol/models: preserve codex_version::version() usage and UA helpers; keep codex_core::models alias.
5) CI and workflows: keep our workflows; adopt benign upstream changes only if compatible.

Process
- Merge upstream/main with --no-commit.
- Resolve conflicts per policy (ours vs theirs buckets).
- Run scripts/upstream-merge/verify.sh and ./build-fast.sh (zero warnings policy).
- Commit with conventional message and short build status.
- Prepare MERGE_REPORT.md with Incorporated / Dropped / Other notes.
