Upstream Merge Plan (by-bucket)

Mode: by-bucket

Scope and strategy
- Bucket files by policy globs and handle decisions at the bucket level.
- Default to adopting upstream outside protected paths.
- Preserve fork-specific behavior in core, protocol, TUI, and workflows.

Policy buckets
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
- Purge (ensure absent if reintroduced):
  - .github/codex-cli-*.png/.jpg/.jpeg/.webp

Explicit invariants
- Preserve tool families and registration/parity: browser_*, agent_*, web_fetch.
- Keep browser gating logic intact.
- Maintain screenshot queuing semantics across turns.
- Keep version/UA helpers: codex_version::version(), get_codex_user_agent_default().
- Preserve core re-exports: ModelClient, Prompt, ResponseEvent, ResponseStream.
- Keep codex_core::models aliasing protocol models.
- Avoid removing ICU/sys-locale unless verified unused.

Process
1) Merge upstream/main into upstream-merge with --no-commit.
2) Resolve conflicts by buckets per policy; note noteworthy choices.
3) Ensure purge_globs remain deleted.
4) Run scripts/upstream-merge/verify.sh and fix minimally.
5) Run ./build-fast.sh and fix warnings/errors.
6) Commit with a conventional message; push upstream-merge.

Artifacts referenced
- .github/auto/COMMITS.json, DELTA_FILES.txt, DIFFSTAT.txt, CHANGE_HISTOGRAM.txt,
  DELETED_ON_DEFAULT.txt, REINTRODUCED_PATHS.txt (reviewed during conflict resolution).

