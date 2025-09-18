# Upstream Merge Plan

Mode: by-bucket

Rationale: Upstream has many Rust and TUI changes spread across crates. We will merge in buckets to keep decisions minimal and focused, while preserving our fork-specific UX and tooling.

Policy
- Prefer ours:
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
- Prefer theirs:
  - codex-rs/common/**
  - codex-rs/exec/**
  - codex-rs/file-search/**
- Purge (ensure absent even if reintroduced):
  - .github/codex-cli-*.png|jpg|jpeg|webp

Process
1) Merge upstream/main into upstream-merge with --no-commit.
2) Resolve conflicts using policy: keep ours for protected areas, adopt upstream elsewhere unless it breaks build or fork invariants.
3) Preserve invariants:
   - Tool families: browser_*, agent_*, web_fetch must be present and exposed via openai_tools, with gating intact.
   - Screenshot queuing semantics and TUI rendering order invariants.
   - Version/UA helpers and codex_version::version() use.
   - Public re-exports in codex-core: ModelClient, Prompt, ResponseEvent, ResponseStream. Keep codex_core::models alias.
   - Do not drop ICU/sys-locale unless unused.
4) Verify: scripts/upstream-merge/verify.sh, then ./build-fast.sh with zero warnings.
5) Commit minimal fixes; generate MERGE_REPORT.md with Incorporated/Dropped/Other.
