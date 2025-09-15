# Upstream Merge Plan

Mode: by-bucket
Upstream: openai/codex@main
Branch: upstream-merge (pre-existing)

Strategy
- Fetch origin and upstream; merge `upstream/main` into `upstream-merge` with `--no-commit`.
- Apply selective resolution guided by policy buckets:
  - prefer_ours_globs:
    - Keep our implementations unless a change is clearly compatible and beneficial.
    - Paths: codex-rs/tui/**, codex-cli/**, codex-rs/core/src/openai_tools.rs, codex-rs/core/src/codex.rs, codex-rs/core/src/agent_tool.rs, codex-rs/core/src/default_client.rs, codex-rs/protocol/src/models.rs, .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md.
  - prefer_theirs_globs:
    - Adopt upstream by default, adjusting only if it breaks our build or documented behavior.
    - Paths: codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/**.
  - purge_globs:
    - Ensure these remain deleted if reintroduced: .github/codex-cli-*.png|jpg|jpeg|webp.

Fork Invariants (must preserve)
- Tool families: all `browser_*`, `agent_*`, and `web_fetch` handlers and schemas stay registered and gated as before.
- Exposure gating for browser tools remains intact after refactors.
- Screenshot queue semantics and TUI rendering remain unchanged unless both producer/consumer are updated together.
- Version/User-Agent: continue using `codex_version::version()` and `get_codex_user_agent_default()`.
- Public re-exports in codex-core: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`; keep `codex_core::models` as alias.
- Do not remove ICU/sys-locale deps unless verified unused.

Buckets and Notes
- Rust core/common/exec/file-search: prefer upstream improvements, reconcile minor API deltas to keep our public API stable.
- TUI and CLI: keep our UX/ordering semantics and extended tooling, selectively cherry-pick upstream fixes if compatible.
- Workflows/docs: keep ours where they diverge to support our policies; adopt upstream security/correctness improvements when non-conflicting.

Verification
- Run `scripts/upstream-merge/verify.sh` and ensure it passes.
- Run `./build-fast.sh` as final smoke to ensure zero warnings.

Output
- Commit with a conventional message summarizing merge and verification status.
- Write `.github/auto/MERGE_REPORT.md` capturing Incorporated / Dropped / Other notes.
