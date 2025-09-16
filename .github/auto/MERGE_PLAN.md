Upstream merge plan

- Mode: by-bucket (per task input MERGE_MODE)
- Upstream: openai/codex@main -> branch: upstream-merge (no fast-forward)

Strategy

- Buckets
  - Prefer ours: codex-rs/tui/**, codex-cli/**, codex-rs/core/src/{openai_tools.rs,codex.rs,agent_tool.rs,default_client.rs}, codex-rs/protocol/src/models.rs, .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md
  - Prefer theirs: codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/**
  - Purge: .github/codex-cli-*.png|jpg|jpeg|webp (keep removed if reintroduced)

- Default rule
  - Outside prefer_ours_globs, adopt upstream unless it breaks our build or documented behavior.
  - In protected areas, keep fork behavior unless an upstream change is clearly compatible and beneficial.

- Invariants to preserve
  - Tool families: keep browser_*, agent_* and web_fetch tool schema registration in openai_tools and parity with handlers.
  - Gating: retain browser tool exposure gating logic.
  - Screenshot UX: keep pending-queue semantics and TUI consumer behavior.
  - Version/UA: continue using codex_version::version() and get_codex_user_agent_default() where applicable.
  - Public API: keep codex-core re-exports (ModelClient, Prompt, ResponseEvent, ResponseStream) and models alias.

Execution steps

1) Ensure upstream remote and fetch both remotes
2) Merge upstream/main into upstream-merge with --no-commit
3) Reconcile diffs using bucket rules and purge list
4) Run scripts/upstream-merge/verify.sh (includes build-fast.sh and guards)
5) Commit with a clear merge message and short status
6) Write MERGE_REPORT.md and push upstream-merge

Notes

- No unrelated refactors. Avoid re-introducing previously removed images and branding assets.
- If histories diverge with no merge-base, re-run merge with --allow-unrelated-histories.
