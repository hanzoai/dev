# MERGE PLAN

Mode: by-bucket

Strategy:
- Group upstream changes into buckets based on directories and artifact histogram.
- Prefer upstream for: codex-rs/common, codex-rs/exec, codex-rs/file-search.
- Prefer ours for: codex-rs/tui/**, codex-cli/**, codex-rs/core/src/openai_tools.rs, codex-rs/core/src/codex.rs, codex-rs/core/src/agent_tool.rs, codex-rs/core/src/default_client.rs, codex-rs/protocol/src/models.rs, workflows, docs, AGENTS.md, README.md, CHANGELOG.md.
- Default to upstream elsewhere unless it breaks fork invariants or build.
- Purge: .github/codex-cli-*.{png,jpg,jpeg,webp} if reintroduced.

Guardrails:
- Preserve browser_*, agent_*, and web_fetch tool handlers and openai_tools exposure with gating intact.
- Keep screenshot queue semantics; do not regress strict TUI ordering.
- Maintain codex_version::version() and get_codex_user_agent_default() usage.
- Keep codex-core re-exports and models alias.

Process:
1) Merge upstream/main into upstream-merge with --no-commit.
2) Resolve conflicts per policy (ours vs theirs by path).
3) Run scripts/upstream-merge/verify.sh and ./build-fast.sh, fix minimal issues.
4) Commit with conventional message and summary.
5) Push upstream-merge and prepare PR text.
