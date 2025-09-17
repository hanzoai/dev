Upstream merge plan

Mode
- Strategy: by-bucket (from MERGE_MODE)

Buckets and policy
- Prefer ours: `codex-rs/tui/**`, `codex-cli/**`, `codex-rs/core/src/{openai_tools.rs,codex.rs,agent_tool.rs,default_client.rs}`, `codex-rs/protocol/src/models.rs`, `.github/workflows/**`, `docs/**`, `AGENTS.md`, `README.md`, `CHANGELOG.md`.
  - Rationale: preserve fork UX (TUI ordering, browser/agent tools, UA/version).
  - Action: keep local versions unless a change is clearly compatible and beneficial; otherwise note in report.
- Prefer theirs: `codex-rs/common/**`, `codex-rs/exec/**`, `codex-rs/file-search/**`.
  - Rationale: upstream correctness and compatibility; adopt unless it breaks our build or behavior.
- Default: adopt upstream for other paths when conflict-free and compatible.
- Purge: `.github/codex-cli-*.{png,jpg,jpeg,webp}` remain deleted even if reintroduced.

Explicit invariants to preserve
- Tool families: keep `browser_*`, `agent_*`, and `web_fetch` handlers and schemas; maintain gating for browser tools exposure.
- Screenshot UX: preserve queuing semantics across turns (producer/consumer paths unchanged).
- Version/UA: continue to use `codex_version::version()` and `get_codex_user_agent_default()`.
- Public API: keep `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream` re-exports; keep `codex_core::models` alias.
- Dependencies: do not remove ICU/sys-locale without confirming no usages.

Process
1) Fetch origin/upstream; work on existing `upstream-merge` branch.
2) Merge `upstream/main` with `--no-commit`.
3) Resolve conflicts by bucket rules above; document notable choices in MERGE_REPORT.md.
4) Ensure purge globs are removed.
5) Run `scripts/upstream-merge/verify.sh` and fix minimal issues.
6) Build check via `./build-fast.sh` with zero warnings.
7) Commit with a conventional message and push `upstream-merge`.

Notes from artifacts
- CHANGE_HISTOGRAM indicates heavy TUI churn upstream; we will keep ours unless specific improvements are compatible.
- Upstream touched `codex-rs/core/src/codex.rs` and tests; we will carefully merge non-conflicting improvements while preserving our tool wiring and UA/version semantics.
