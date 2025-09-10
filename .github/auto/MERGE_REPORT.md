# Upstream Merge Report

Source: upstream/main (openai/codex)
Target: upstream-merge
Mode: by-bucket

## Incorporated
- Non-conflicting updates across workspace, defaulting to upstream outside protected areas.
- Added `codex-rs/core/src/config_edit.rs` (new helper for config overrides) and wired via existing `lib.rs` module reference.

## Dropped / Deferred
- Large upstream changes in `codex-rs/core/src/{codex.rs,config.rs}` and related type/enum reshapes were NOT adopted in this pass because they broke our current API surface and build. We retained our existing implementations to preserve compatibility.

## Rationale
- Policy prefers upstream for core, but only when it does not break our build or documented behavior. Upstream introduced breaking changes (new `Op` variants, struct field changes, Tool args reshapes) that conflict with our current `codex-core` API and re-exports. Keeping ours is the minimal, surgical choice.
- TUI/CLI areas remain ours by default; no conflicts were raised.
- Purge globs: no `.github/codex-cli-*` images reintroduced in this range.

## Compatibility Checks
- Public re-exports in `codex-core` confirmed present: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`, and `codex_core::models` alias.
- ICU/sys-locale dependencies unchanged.

## Validation
- scripts/upstream-merge/verify.sh: PASS
- ./build-fast.sh: PASS (no warnings)

## Notes
- Future merges can re-attempt adopting upstream core changes incrementally. Suggest staging around: rollout items, revised `Op` enum, and tool config parameter reshapes.

