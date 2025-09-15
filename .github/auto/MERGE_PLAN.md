# Upstream Merge Plan

- Mode: by-bucket (per task instructions)
- Upstream: `openai/codex` @ `main` (`upstream/main`)
- Target branch: `upstream-merge` (existing; do not recreate)

## Strategy

We will merge upstream into `upstream-merge` in one pass, resolving conflicts by buckets with policy-driven preferences. For protected areas we keep our fork’s behavior; for commodity crates we lean upstream. We maintain fork invariants and run guards.

### Preferences

- Prefer ours:
  - `codex-rs/tui/**`
  - `codex-cli/**`
  - `codex-rs/core/src/openai_tools.rs`
  - `codex-rs/core/src/codex.rs`
  - `codex-rs/core/src/agent_tool.rs`
  - `codex-rs/core/src/default_client.rs`
  - `codex-rs/protocol/src/models.rs`
  - `.github/workflows/**`, `docs/**`, `AGENTS.md`, `README.md`, `CHANGELOG.md`

- Prefer theirs:
  - `codex-rs/common/**`
  - `codex-rs/exec/**`
  - `codex-rs/file-search/**`

- Purge (keep deleted):
  - `.github/codex-cli-*.png|jpg|jpeg|webp`

### Fork invariants to preserve

- Tool families and parity: `browser_*`, `agent_*`, `web_fetch` handlers and schemas remain registered and gated appropriately.
- Screenshot queuing semantics and TUI ordering invariants remain unchanged.
- Version/UA helpers: use `codex_version::version()` and `get_codex_user_agent_default()`.
- Public API re-exports in `codex-core`: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`; keep `codex_core::models` alias.
- Do not drop ICU/sys-locale unless verified unused repo-wide.

### Validation

- Run `scripts/upstream-merge/verify.sh` and fix minimally.
- Build: `./build-fast.sh` must pass without errors or warnings.

### Reporting

- Summarize incorporated vs dropped changes and rationale in `.github/auto/MERGE_REPORT.md`.

