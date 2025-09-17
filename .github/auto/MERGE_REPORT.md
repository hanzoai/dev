Upstream Merge Report

Summary
- Merged upstream/main into `upstream-merge` using by-bucket policy.
- Build and guard checks passed.

Incorporated
- Core: upstream additions around review mode; wired by exporting `Review*` types via `codex_core::protocol` for API compatibility.
- Docs: adopted upstream simplification for npm publishing in `docs/release_management.md`.

Dropped/Kept Ours
- Workflows: kept our `.github/workflows/**`; removed upstream `.github/workflows/rust-release.yml` (policy: prefer ours).
- CLI: resolved conflict in `codex-cli/package.json` by keeping ours (policy: prefer ours).
- TUI: upstream touched many files (see histogram) but no direct conflicts; we retained our TUI implementation per policy.

Other Notes
- Purge list: no banned `.github/codex-cli-*.(png|jpg|jpeg|webp)` reintroduced.
- Invariants preserved: browser_*/agent_* tools and gating untouched; screenshot queuing unchanged; UA/version helpers intact; public re-exports unchanged (plus added review re-exports).
- Verification: `scripts/upstream-merge/verify.sh` OK; `./build-fast.sh` succeeded with zero warnings.
