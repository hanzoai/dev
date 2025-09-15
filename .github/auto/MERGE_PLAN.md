Upstream merge plan (by-bucket)

Strategy

- Mode: by-bucket (guided by CHANGE_HISTOGRAM.txt and DELTA_FILES.txt).
- Policy defaults: adopt upstream outside protected paths; keep ours under prefer_ours; lean to upstream for prefer_theirs.
- Preserve fork invariants: browser_*/agent_* tools, web_fetch exposure + gating; screenshot queue semantics; codex_version::version() and get_codex_user_agent_default(); public re-exports in codex-core; codex_core::models alias.

Buckets

- TUI (heavy churn upstream): keep ours by default (codex-rs/tui/**). Manually review simple, UX‑safe wins (labels, minor layout) and accept only if compatible.
- Core execution/auth/session fixes: adopt upstream where not touching our protected files; otherwise reconcile minimally while preserving our tool families and UA/version.
- Common/exec/file-search: prefer upstream unless it conflicts with build or fork‑only semantics.
- Workflows/docs: keep ours; skip upstream branding/assets and keep purge globs enforced.

Artifacts considered

- CHANGE_HISTOGRAM: tui 449, core 104, other 110 – dominated by TUI; treat cautiously.
- COMMITS.json: recent fixes in core exec/session, model family consistency, login/auth, and TUI session header.
- REINTRODUCED_PATHS.txt: reintroduced workflows and images; enforce purge on codex-cli images; keep our workflows.

Conflict resolution rules

- prefer_ours_globs:
  - codex-rs/tui/**, codex-cli/**, codex-rs/core/src/{openai_tools.rs,codex.rs,agent_tool.rs,default_client.rs}, codex-rs/protocol/src/models.rs, .github/workflows/**, docs/**, AGENTS.md, README.md, CHANGELOG.md.
  - Keep ours unless upstream is clearly beneficial and compatible with fork invariants; if accepted, ensure verify.sh passes.
- prefer_theirs_globs:
  - codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/**.
  - Take upstream unless it breaks build or invariants.
- purge_globs: remove any .github/codex-cli-*.{png,jpg,jpeg,webp} that upstream may have reintroduced.

Post-merge checks

- Run scripts/upstream-merge/verify.sh and address any parity or UA/version regressions.
- Build with ./build-fast.sh; fix all warnings as errors policy.

Notes

- Do not reintroduce previously removed crates or paths without explicit review.
- Maintain public re-exports: ModelClient, Prompt, ResponseEvent, ResponseStream.
- Keep codex_core::models as alias to protocol models.
