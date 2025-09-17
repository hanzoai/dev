Upstream merge plan (by-bucket)

Mode: by-bucket

Strategy
- Adopt upstream broadly outside protected areas; resolve conflicts minimally.
- Prefer ours for TUI and fork-only core glue per policy to preserve browser/agent tools, UA/version semantics, and strict ordering.
- Prefer theirs for shared libraries (common, exec, file-search) unless it breaks our build or documented behavior.
- Purge any reintroduced GitHub image assets matching purge globs.

Buckets
- prefer_ours_globs
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

- prefer_theirs_globs
  - codex-rs/common/**
  - codex-rs/exec/**
  - codex-rs/file-search/**

- purge_globs
  - .github/codex-cli-*.png|jpg|jpeg|webp

Invariants to preserve
- Handlers and schemas for browser_* and agent_* tools; include web_fetch.
- Exposure gating for browser tools remains intact.
- Screenshot queuing semantics across turns preserved.
- Version/UA helpers: codex_version::version() and get_codex_user_agent_default().
- Public re-exports in codex-core: ModelClient, Prompt, ResponseEvent, ResponseStream.
- codex_core::models alias to protocol models remains.
- Do not remove ICU/sys-locale deps if referenced.

Process
1) Merge upstream/main into upstream-merge with --no-commit.
2) Resolve conflicts per buckets (ours/theirs/default adopt upstream).
3) Ensure purge_globs stay deleted if reintroduced.
4) Run scripts/upstream-merge/verify.sh and ./build-fast.sh; fix minimal issues.
5) Commit with a conventional message and summarize results in MERGE_REPORT.md.

