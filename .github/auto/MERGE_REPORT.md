# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket
Date: $(date -u +%FT%TZ)

## Incorporated
- Core/protocol/mcp updates (rollout items, user-agent suffix, protocol tweaks).
- Auth/config refactor replacing responses_originator override env var.
- Apply-patch message tweak and minor library updates.
- mcp-types generator/checker scripts (staged changes):
  - Added `codex-rs/mcp-types/check_lib_rs.py`.
  - Updated `codex-rs/mcp-types/generate_mcp_types.py`.

## Dropped / Kept Ours
- TUI changes (key handling, resume picker, rendering deltas): kept our TUI to preserve UX and strict stream ordering.
- codex-cli and GitHub workflows: retained our versions/policies.
- Purged reintroduced media under `.github/codex-cli-*.{png,jpg,jpeg,webp}` (none staged after policy pass).

## Compatibility Guarantees
- Verified `codex-core` re-exports remain:
  - `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`.
  - `codex_core::models` alias maintained via tests.
- ICU/sys-locale usage present in `codex-rs/protocol/src/num_format.rs`; dependencies retained.

## Verification
- Ran `scripts/upstream-merge/verify.sh`:
  - build-fast.sh: ok
  - cargo check (core tests compile): ok

## Notes
- Only conflict encountered: `.github/workflows/rust-ci.yml` (deleted on ours, modified upstream). Resolved by keeping deletion per policy.
- Outside protected areas, no additional conflicts arose; upstream changes either already present or merged cleanly.
