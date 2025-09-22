# Upstream Merge Plan (by-bucket)

- Context
  - Upstream: `openai/codex` @ `main`
  - Local merge target: branch `upstream-merge`
  - Mode: by-bucket (use artifacts under `.github/auto`)

- Strategy
  - Start a non-committing merge: `git merge --no-ff --no-commit upstream/main`.
  - Default stance: adopt upstream outside protected areas.
  - Protected “prefer ours” areas (keep our fork unless clearly beneficial and compatible):
    - `codex-rs/tui/**`
    - `codex-cli/**`
    - `codex-rs/core/src/openai_tools.rs`
    - `codex-rs/core/src/codex.rs`
    - `codex-rs/core/src/agent_tool.rs`
    - `codex-rs/core/src/default_client.rs`
    - `codex-rs/protocol/src/models.rs`
    - `.github/workflows/**`
    - `docs/**`, `AGENTS.md`, `README.md`, `CHANGELOG.md`
  - Prefer upstream (“prefer theirs”) where safe:
    - `codex-rs/common/**`
    - `codex-rs/exec/**`
    - `codex-rs/file-search/**`
  - Purge any reintroduced assets matching:
    - `.github/codex-cli-*.png|jpg|jpeg|webp`

- Buckets (based on DELTA_FILES.txt / CHANGE_HISTOGRAM.txt)
  1) Rust core/protocol/tooling – adopt selectively; preserve tool families and UA/version.
  2) TUI & rendering – keep ours unless upstream changes are clearly compatible with strict ordering and our screenshot/agent panels.
  3) Exec/common/file-search crates – generally adopt upstream unless it breaks our build or policies.
  4) Docs/Workflows – keep our branding and workflows; skim upstream for critical fixes.

- Invariants to preserve
  - Tool families and exposure gates: `browser_*`, `agent_*`, and `web_fetch` must keep handler↔tool parity and gating logic.
  - Screenshot queuing semantics across turns and TUI rendering must remain intact.
  - `codex_version::version()` and `get_codex_user_agent_default()` usage must remain.
  - Public re-exports in `codex-core`: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`.
  - `codex_core::models` namespace remains an alias for protocol models.
  - Keep ICU/sys-locale deps unless proven unused.

- Verification & commit
  - Run `scripts/upstream-merge/verify.sh` and address any issues minimally.
  - Validate build with `./build-fast.sh` (zero warnings policy).
  - Commit with a clear conventional message summarizing incorporated vs preserved areas.

