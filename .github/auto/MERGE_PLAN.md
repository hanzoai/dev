# Upstream Merge Plan

Mode: by-bucket (per task input)

Remotes
- origin: our fork
- upstream: openai/codex (main)

Strategy
- Use an incremental, bucketed reconciliation:
  - Prefer Ours: codex-rs/tui/**, codex-cli/**, core plumbing (openai_tools.rs, codex.rs, agent_tool.rs, default_client.rs), protocol models, workflows/docs (see policy).
  - Prefer Theirs: codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/** when compatible.
  - Default: adopt upstream outside protected globs unless it breaks fork invariants or verify.sh.
- Purge any files matching purge_globs if reintroduced.
- Preserve invariants:
  - Tool families: browser_*, agent_*, and web_fetch must remain wired end-to-end with tool schemas exposed when enabled by policy.
  - Browser gating logic must remain intact.
  - Screenshot queuing semantics and TUI rendering stay compatible.
  - Version/UA helpers: codex_version::version(), get_codex_user_agent_default().
  - Public re-exports in codex-core: ModelClient, Prompt, ResponseEvent, ResponseStream. Keep codex_core::models alias.
- Use `git merge --no-ff --no-commit upstream/main`, then resolve conflicts by policy:
  - In prefer_ours_globs, keep ours unless upstream clearly improves and is compatible.
  - In prefer_theirs_globs, lean theirs unless it breaks build/verify.
  - Elsewhere, take upstream by default.

Validation
- Run scripts/upstream-merge/verify.sh repeatedly until clean.
- Final gate: ./build-fast.sh must pass with zero warnings.

Reporting
- Record notable accept/reject decisions and any purged paths in MERGE_REPORT.md.
