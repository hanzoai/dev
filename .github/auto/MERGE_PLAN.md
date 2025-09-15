# Upstream Merge Plan (by-bucket)

Mode: by-bucket
Upstream: openai/codex@main → branch `upstream-merge`

Policy
- Prefer ours: codex-rs/tui/**, codex-cli/**, core wiring files (openai_tools.rs, codex.rs, agent_tool.rs, default_client.rs), protocol models, top-level docs and workflows.
- Prefer theirs: codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/** (unless it breaks our build or fork-specific invariants).
- Purge images matching .github/codex-cli-*.{png,jpg,jpeg,webp}.
- Outside protected paths, adopt upstream to stay current.

Buckets
1) Shared infra/docs/workflows: adopt upstream except for our custom CI/workflow/doco where policy prefers ours.
2) Rust common/exec/file-search: adopt upstream (prefer-theirs), reconcile minimal API drift.
3) Core + Protocol: keep ours; cherry-pick clearly compatible upstream fixes without breaking browser/agent/web_fetch, UA/version, and re-exports.
4) TUI/CLI: keep ours; port obviously-safe bug fixes when non-invasive.

Invariants
- Tool families preserved: browser_*, agent_*, web_fetch. Keep gating/exposure logic intact.
- Screenshot queue semantics unchanged across turns.
- Version/UA helpers: keep codex_version::version() and get_codex_user_agent_default() uses.
- Public re-exports in codex-core stay (ModelClient, Prompt, ResponseEvent, ResponseStream; models alias).

Process
- Merge upstream/main with --no-commit and reconcile per policy.
- Ensure purge_globs remain deleted.
- Run scripts/upstream-merge/verify.sh and fix minimal issues.
- Validate build with ./build-fast.sh with zero warnings.
- Commit with a clear merge message and generate MERGE_REPORT.md.
