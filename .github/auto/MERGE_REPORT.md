# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

## Incorporated
- Adopt upstream updates across `codex-rs/common`, `codex-rs/exec`, and `codex-rs/file-search`.
- Merged core configuration improvements from upstream, including new constant `GPT5_HIGH_MODEL`.
- Synced non-conflicting changes across the Rust workspace and supporting scripts.

## Preserved (Fork-specific)
- TUI UX and streaming ordering invariants under `codex-rs/tui/**` (kept our lib.rs flow; upstream onboarding/model-upgrade block not adopted).
- Core tool families and gating: `browser_*`, `agent_*`, and `web_fetch` handlers + schemas intact.
- Screenshot queuing semantics and TUI history rendering.
- UA/version handling via `codex_version::version()` in default_client.
- Public re-exports and `codex_core::models` alias preserved (no API surface breaks).
- Workflows and docs kept as in fork unless upstream provided compatible improvements.

## Dropped / Deferred
- Upstream onboarding + model-upgrade popup flow in `tui/lib.rs` (would require broader integration; our UX retained). The new `GPT5_HIGH_MODEL` constant is present so we can integrate later if desired.

## Conflicts and Resolutions
- `codex-rs/core/src/config.rs`: Resolved by keeping our base and adding upstream’s `GPT5_HIGH_MODEL` constant. Removed conflict markers.
- `codex-rs/tui/src/lib.rs`: Resolved in favor of our implementation per `prefer_ours_globs` policy.

## Purges
- No reintroduced purge targets (`.github/codex-cli-*.{png,jpg,jpeg,webp}`) found.

## Verification
- scripts/upstream-merge/verify.sh: PASS
  - build-fast.sh: PASS
  - core API surface check: PASS
  - static guards (tools parity + UA/version): PASS

