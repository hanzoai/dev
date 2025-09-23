# Upstream Merge Plan (by-bucket)

Mode: by-bucket (selective, policy-driven)

Remotes
- upstream: openai/codex (branch: main)
- working branch: upstream-merge (pre-existing)

Policy Buckets
- Prefer Ours (protect and keep local unless clearly compatible improvement):
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

- Prefer Theirs (adopt upstream unless it breaks build/behavior):
  - codex-rs/common/**
  - codex-rs/exec/**
  - codex-rs/file-search/**

- Purge (ensure absent):
  - .github/codex-cli-*.png/.jpg/.jpeg/.webp

Fork Invariants (must be preserved)
- Tool families and parity: browser_*, agent_*, web_fetch present; exposure gating intact.
- Screenshot queuing semantics unchanged.
- Version/UA: keep codex_version::version() and get_codex_user_agent_default().
- Public re-exports in codex-core: ModelClient, Prompt, ResponseEvent, ResponseStream.
- Keep codex_core::models alias to protocol models.

Procedure
1) Merge upstream/main into upstream-merge with --no-commit.
2) Resolve conflicts by bucket:
   - prefer_ours_globs: choose ours.
   - prefer_theirs_globs: choose theirs.
   - purge_globs: remove if reintroduced.
3) Review notable reintroduced/new crates; do not blanket-delete; document choices in MERGE_REPORT.md.
4) Run scripts/upstream-merge/verify.sh and fix minimally.
5) Validate build with ./build-fast.sh (zero warnings policy).
6) Commit merge with Conventional Commit message and push upstream-merge.
7) Write MERGE_REPORT.md summarizing Incorporated / Dropped / Other notes.

Notes
- Default outside protected areas: adopt upstream.
- No history rewrite; merge only.
- If no merge-base: retry with --allow-unrelated-histories.
