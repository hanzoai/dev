Upstream merge plan (mode: by-bucket)

Overview
- Merge upstream main into our `upstream-merge` branch using a by-bucket strategy.
- Apply fork policy guards to preserve our TUI/UX, custom tools, and UA/version semantics.
- Prefer upstream for shared crates where safe; keep ours for protected core/TUI paths.

Inputs
- Upstream: `openai/codex` @ `main` (remote: upstream)
- Ours: branch `upstream-merge` tracking origin
- Artifacts: `.github/auto/COMMITS.json`, `DELTA_FILES.txt`, `DIFFSTAT.txt`, `REINTRODUCED_PATHS.txt`, `CHANGE_HISTOGRAM.txt`

Policy Buckets
1) prefer-ours
   - codex-rs/tui/**
   - codex-cli/**
   - codex-rs/core/src/openai_tools.rs
   - codex-rs/core/src/codex.rs
   - codex-rs/core/src/agent_tool.rs
   - codex-rs/core/src/default_client.rs
   - codex-rs/protocol/src/models.rs
   - .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md

2) prefer-theirs
   - codex-rs/common/**
   - codex-rs/exec/**
   - codex-rs/file-search/**

3) purge (keep removed)
   - .github/codex-cli-*.png|jpg|jpeg|webp

Method
- Merge with `--no-commit`.
- Resolve conflicts by bucket:
  • For prefer-ours: keep ours unless upstream change is clearly compatible and beneficial; document any exceptions.
  • For prefer-theirs: take upstream unless it breaks our build or documented behavior; adjust minimally if needed.
  • Outside buckets: default to upstream while preserving fork invariants.
- Do not reintroduce paths listed in purge or previously removed branding assets.
- Record noteworthy choices in MERGE_REPORT.md.

Fork invariants
- Keep browser_* and agent_* tools and web_fetch end-to-end (handlers, schemas, TUI titles/UX).
- Preserve exposure gating for browser tools.
- Maintain screenshot queue semantics and TUI updates.
- Keep `codex_version::version()` usage and `get_codex_user_agent_default()` behavior.
- Preserve public re-exports in codex-core: ModelClient, Prompt, ResponseEvent, ResponseStream.
- Keep `codex_core::models` alias; do not drop ICU/sys-locale without confirming unused.

Validation
- Run `scripts/upstream-merge/verify.sh` until clean.
- Final check: `./build-fast.sh` with zero warnings.

Notes
- Do not reset or recreate `upstream-merge`.
- If histories are unrelated, retry merge with `--allow-unrelated-histories`.
