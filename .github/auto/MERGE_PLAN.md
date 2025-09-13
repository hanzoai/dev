# Upstream Merge Plan

Mode: by-bucket
Upstream: openai/codex@main
Branch: upstream-merge

Strategy
- Fetch origin and upstream; merge upstream/main into upstream-merge with --no-commit.
- Apply policy globs:
  - prefer_ours_globs: keep our fork files unless upstream change is clearly compatible and beneficial.
  - prefer_theirs_globs: adopt upstream unless it breaks our build or documented behavior.
  - purge_globs: ensure purged assets remain deleted even if reintroduced upstream.
- Default outside protected areas: adopt upstream to stay current.
- Preserve invariants:
  - Tool families: browser_*, agent_*, web_fetch tool exposure and handler↔tool parity.
  - Browser tools gating logic remains intact post-merge.
  - Screenshot queuing and TUI semantics preserved.
  - Version/UA helpers: codex_version::version(), get_codex_user_agent_default() kept.
- Keep codex-core re-exports and protocol model alias stable.
- Avoid reintroducing paths from .github/auto/DELETED_ON_DEFAULT.txt.

Buckets
- Upstream Rust crates (common, exec, file-search): prefer theirs.
- TUI and CLI: prefer ours, reconcile minimal UI text or safe fixes.
- Core wiring (openai_tools.rs, codex.rs, agent_tool.rs, default_client.rs, protocol models): prefer ours; cherry-pick safe upstream improvements if non-breaking.
- Workflows/docs: prefer ours; adopt upstream critical fixes manually if needed.

Verification
- Run scripts/upstream-merge/verify.sh and fix issues minimally until green.
- Build check with ./build-fast.sh; treat warnings as failures.
