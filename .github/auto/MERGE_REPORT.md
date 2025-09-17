# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket
Date: $(date -u +%Y-%m-%dT%H:%M:%SZ)

## Incorporated
- Adopted upstream review formatting module `codex-rs/core/src/review_format.rs` (UI-agnostic strings).
- Integrated upstream review tests under `codex-rs/core/tests/suite/review.rs` with minimal path fixes.
- Accepted upstream changes in `codex-rs/core/src/lib.rs` to export `review_format`.

## Dropped / Overridden
- Resolved conflict in `codex-rs/core/src/codex.rs` by keeping our fork version per prefer_ours policy to preserve:
  - browser_* and agent_* tool families and gating
  - screenshot queue semantics
  - UA/version helpers and exports
  - Response export aliases (ModelClient, Prompt, ResponseEvent, ResponseStream)

## Adjustments
- Fixed imports to use protocol crate types for review structures:
  - `codex-rs/core/src/review_format.rs`: `use codex_protocol::protocol::ReviewFinding;`
  - `codex-rs/core/tests/suite/review.rs`: switched Review* imports to `codex_protocol::protocol::*`.
- No purge_globs matches found to remove.

## Verification
- scripts/upstream-merge/verify.sh: PASS (build_fast=ok, api_check=ok)
- ./build-fast.sh: PASS (no warnings observed)

## Notes
- Kept our `codex-core` API re-exports and `models` alias intact.
- No ICU/sys-locale dependency changes needed.
