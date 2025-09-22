# Upstream Merge Plan

Mode: by-bucket
Branch: upstream-merge (pre-created)
Upstream: openai/codex@main

Strategy
- Fetch `origin` and `upstream`, then merge `upstream/main` into `upstream-merge` with `--no-commit`.
- Apply selective reconciliation using policy buckets:
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
    - .github/codex-cli-*.png
    - .github/codex-cli-*.jpg
    - .github/codex-cli-*.jpeg
    - .github/codex-cli-*.webp

Fork invariants to preserve
- Tool families: keep browser_* , agent_* , and web_fetch handler↔tool parity and exposure gating.
- Screenshot queuing semantics across turns (producer/consumer paths unchanged).
- Version/UA helpers: codex_version::version() and get_codex_user_agent_default().
- Public codex-core re-exports: ModelClient, Prompt, ResponseEvent, ResponseStream; keep codex_core::models alias.
- Do not remove ICU/sys-locale unless proven unused.

Process
1. Merge upstream with `--no-commit` (allow unrelated histories if needed).
2. Enforce bucket decisions: checkout ours for prefer_ours_globs; checkout theirs for prefer_theirs_globs.
3. Purge images listed in purge_globs if reintroduced.
4. Run scripts/upstream-merge/verify.sh and ./build-fast.sh; fix minimally to achieve zero warnings.
5. Commit with a conventional message summarizing decisions; push branch and prepare PR.

Notes
- Outside protected areas, favor upstream unless it breaks our build or documented behavior.
- Surface noteworthy dropped or deferred changes in MERGE_REPORT.md.
