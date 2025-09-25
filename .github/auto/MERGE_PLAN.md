# Upstream Merge Plan (by-bucket)

## Context
- Branch: `upstream-merge`
- Upstream: `openai/codex` @ `main`
- Mode: **by-bucket** (3 commits in this range)
- Policies: preserve fork TUI/tooling (`prefer_ours_globs`), lean upstream for common/exec/file-search, keep purge list removed.

## Buckets & Intent

### Bucket A – CI / Codespell Bump (d1ed3a4)
- Scope: `.github/workflows/codespell.yml`.
- Our policy keeps `.github/workflows/**` unless clear benefit; inspect for critical security/infra fixes.
- Plan: review diff, cherry-pick improvements if low-risk; otherwise keep ours and note in report.

### Bucket B – Core/TUI State Refactor (250b244)
- Scope: massive changes across `codex-rs/tui`, `codex-rs/core`, config/docs.
- Priority: protect forked TUI history ordering, browser/agent, UA/version, screenshot queue.
- Strategy: merge core/common pieces that improve correctness while guarding prefer_ours areas. For TUI-heavy files, start with ours, selectively adopt upstream pieces that don't break our UX. Flag new upstream assets/tests (e.g., fixtures) for compatibility.

### Bucket C – `/status` Revamp (e363dac)
- Scope: `codex-rs/tui/src/status/**`, snapshots, assets.
- Our fork has custom status logic tied to sandbox policies; will evaluate upstream UX for compatibility. Default to ours, but import discrete improvements if they align with our status panel.

## Cross-Cutting Tasks
- Watch for new assets/images under purge globs and ensure they remain removed.
- After conflict resolution run `scripts/upstream-merge/verify.sh` then `./build-fast.sh` (warnings prohibited).
- Document keep/drop decisions in `.github/auto/MERGE_REPORT.md`.
- Maintain required exports (`ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`) and UA/version helpers.
