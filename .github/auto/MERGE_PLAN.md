# Upstream Merge Plan (by-bucket)

Mode: by-bucket
Upstream: openai/codex@main
Branch: upstream-merge (existing)

Artifacts considered:
- COMMITS.json (upstream commits not in default)
- DELTA_FILES.txt, DIFFSTAT.txt (scope + churn)
- CHANGE_HISTOGRAM.txt (area heatmap)
- REINTRODUCED_PATHS.txt (newly reappearing files)

Policy
- Prefer ours: codex-rs/tui/**, codex-cli/**, core openai_tools/codex.rs/agent_tool.rs/default_client.rs, protocol/src/models.rs, repo docs/workflows.
- Prefer theirs: codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/**.
- Purge: .github/codex-cli-*.{png,jpg,jpeg,webp} remain deleted.
- Default: adopt upstream outside protected paths if compatible.

Buckets
1) Core protocol + common libs
   - Adopt upstream in common/exec/file-search.
   - In core/, keep our custom tools, UA/version, browser/agent/web_fetch parity and gating.
2) TUI
   - Keep our strict ordering and streaming deltas. Accept safe upstream improvements only if compatible with our invariants and tests.
3) CLI/docs/workflows
   - Preserve our workflows; adopt upstream docs where compatible.
4) MCP/server/client/tooling
   - Keep UA helpers and function-call serialization semantics.

Process
- Merge upstream/main into upstream-merge with --no-commit.
- Resolve conflicts per policy (ours vs theirs) file-by-file.
- Ensure browser_* and agent_* and web_fetch handlers ↔ tool exposure parity.
- Keep codex_version::version() usages and get_codex_user_agent_default().
- Run scripts/upstream-merge/verify.sh. Fix minimally. Re-run.
- Run ./build-fast.sh to ensure zero warnings.
- Commit with a conventional message and summary. Prepare report.
