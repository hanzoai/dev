# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

## Incorporated
- Core/protocol/exec/file-search: Adopted upstream changes where compatible (no conflicts detected after resolution).
- Protocol-related tests and minor fixes pulled in transitively.

## Dropped / Kept Ours
- codex-rs/core/src/config.rs: Kept ours. Upstream version referenced protocol types and fields not present in our fork, causing build failures. Retaining our implementation preserves API compatibility and passes verification.
- TUI (`codex-rs/tui/**`): Kept ours per policy; upstream churn not adopted to preserve our UX.
- Workflows (`.github/workflows/**`): Kept ours; upstream CI changes not adopted.
- CLI (`codex-cli/**`): Kept ours branding and behavior.

## Purged
- Disallowed assets remained absent: `.github/codex-cli-*.png|jpg|jpeg|webp` (none reintroduced after merge).

## Other Notes
- Re-exports in `codex-core` confirmed intact: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`, and `codex_core::models` alias.
- ICU/sys-locale dependencies unchanged; no removals attempted.
- Verification succeeded: `scripts/upstream-merge/verify.sh` (build_fast=ok, api_check=ok).

