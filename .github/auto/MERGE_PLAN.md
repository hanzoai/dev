# Upstream Merge Plan (by-bucket)

Mode: by-bucket
Upstream: openai/codex@main -> branch `upstream-merge`

Policy application:
- prefer_ours:
  - codex-rs/tui/**
  - codex-cli/**
  - codex-rs/core/src/openai_tools.rs
  - codex-rs/core/src/codex.rs
  - codex-rs/core/src/agent_tool.rs
  - codex-rs/core/src/default_client.rs
  - codex-rs/protocol/src/models.rs
  - .github/workflows/**
  - docs/**
  - AGENTS.md, README.md, CHANGELOG.md
- prefer_theirs:
  - codex-rs/common/**
  - codex-rs/exec/**
  - codex-rs/file-search/**
- purge_globs:
  - .github/codex-cli-*.png|jpg|jpeg|webp

Strategy:
- Attempt a single no-commit merge and resolve conflicts by bucket.
- Keep our TUI, browser/agent/web_fetch tooling and gating logic intact.
- Adopt upstream changes that improve correctness/compat/stability, especially in common/exec/file-search.
- Preserve public re-exports in codex-core and UA/version helpers.
- Do not reintroduce purged image assets.

Validation:
- Run scripts/upstream-merge/verify.sh and ./build-fast.sh; fix minimal issues.
- Ensure zero warnings in Rust build.

Notes:
- If no merge-base, retry with --allow-unrelated-histories.
- Document notable acceptance/rejection decisions in MERGE_REPORT.md.
