Upstream Merge Plan

Mode: by-bucket

Summary
- Merge `upstream/main` into `upstream-merge` using a bucketed strategy guided by repo policy and auto artifacts.
- Preference buckets:
  - Prefer ours: `codex-rs/tui/**`, `codex-cli/**`, `codex-rs/core/src/openai_tools.rs`, `codex-rs/core/src/codex.rs`, `codex-rs/core/src/agent_tool.rs`, `codex-rs/core/src/default_client.rs`, `codex-rs/protocol/src/models.rs`, `.github/workflows/**`, `docs/**`, `AGENTS.md`, `README.md`, `CHANGELOG.md`.
  - Prefer theirs: `codex-rs/common/**`, `codex-rs/exec/**`, `codex-rs/file-search/**`.
  - Purge (ensure deleted): any `.github/codex-cli-*.{png,jpg,jpeg,webp}`.
- Default outside buckets: adopt upstream when safe, unless it conflicts with fork‑specific invariants documented below.

Key Fork Invariants (must preserve)
- Tool families and parity: all `browser_*`, `agent_*`, and `web_fetch` handlers remain registered and exposed via `openai_tools` with gating intact.
- Browser gating logic retained if upstream refactors tool exposure.
- Screenshot queuing semantics across turns unchanged unless both producer/consumer paths are updated together (we will preserve existing behavior in this pass).
- Version and UA helpers: keep `codex_version::version()` usage and `get_codex_user_agent_default()` where applicable.
- Public exports in `codex-core`: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream` and `codex_core::models` alias.

Procedure
1) Ensure `upstream` remote and fetch both remotes.
2) Create this plan, then perform a no-commit merge: `git merge --no-ff --no-commit upstream/main`.
3) Resolve conflicts by buckets:
   - For prefer‑ours globs: choose ours unless upstream change is clearly beneficial and compatible; otherwise sync minimal shims only.
   - For prefer‑theirs globs: choose upstream unless it breaks our build or documented behavior, then patch forward.
   - Enforce purge globs: re-delete if reintroduced.
4) Run `scripts/upstream-merge/verify.sh` and address failures minimally.
5) Stage, commit with Conventional Commit message, and push `upstream-merge`.
6) Write `.github/auto/MERGE_REPORT.md` summarizing incorporated, dropped, and notable changes.

Notes
- Do not reintroduce previously removed UX/theming assets or images listed in purge globs.
- Avoid refactors; keep changes surgical and scoped to merge and required fixes.
