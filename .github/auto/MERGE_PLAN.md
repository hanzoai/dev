# Upstream Merge Plan (by-bucket)

Mode: by-bucket

Strategy
- Fetch and merge `upstream/main` into `upstream-merge` with `--no-commit` to allow policy-driven conflict resolution.
- Apply selective reconciliation per policy buckets:
  - prefer_ours: keep our fork-specific TUI/core/tooling unless upstream change is clearly beneficial and compatible.
  - prefer_theirs: adopt upstream for common/exec/file-search unless it breaks build or documented behavior.
  - default: favor upstream while preserving fork invariants (browser/agent/web_fetch tools, exposure gating, screenshot queuing, UA/version helpers, core re-exports).
- Purge assets matching purge globs if reintroduced by upstream.
- Validate with `scripts/upstream-merge/verify.sh` and repo `./build-fast.sh`.

Policy
- prefer_ours_globs:
  - codex-rs/tui/**
  - codex-cli/**
  - codex-rs/core/src/openai_tools.rs
  - codex-rs/core/src/codex.rs
  - codex-rs/core/src/agent_tool.rs
  - codex-rs/core/src/default_client.rs
  - codex-rs/protocol/src/models.rs
  - .github/workflows/**
  - docs/**
  - AGENTS.md
  - README.md
  - CHANGELOG.md
- prefer_theirs_globs:
  - codex-rs/common/**
  - codex-rs/exec/**
  - codex-rs/file-search/**
- purge_globs:
  - .github/codex-cli-*.png/.jpg/.jpeg/.webp

Checks
- Preserve public re-exports in `codex-core` (ModelClient, Prompt, ResponseEvent, ResponseStream) and the `codex_core::models` alias.
- Do not drop ICU/sys-locale deps unless unused across workspace.
- Ensure tool handler↔openai_tools parity and UA/version invariants pass verify script.

