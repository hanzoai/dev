Upstream merge report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

Incorporated
- Adopted upstream changes across core, common, exec, and workspace where no fork-only invariants were impacted.
- No newly reintroduced purge_globs found; image purge not required this round.

Dropped/Kept Ours
- codex-rs/tui/src/chatwidget.rs: kept ours to preserve strict streaming ordering and RunningCommand/HistoryCell semantics that our TUI relies on.
- Preserved fork-only core glue and APIs (no upstream changes conflicting this round):
  - codex-rs/core/src/openai_tools.rs (browser_*/agent_* tools and web_fetch exposure)
  - codex-rs/core/src/codex.rs (execution events + UA/version handling)
  - codex-rs/core/src/agent_tool.rs (multi-agent orchestration)
  - codex-rs/core/src/default_client.rs (versioned UA)
  - codex-rs/protocol/src/models.rs (models mapping and aliases)

Other Notes
- Public re-exports (ModelClient, Prompt, ResponseEvent, ResponseStream) were preserved.
- codex_core::models alias remains intact.
- Verify script passed: tools/UA/version guards OK.
- build-fast.sh passed with zero warnings.

