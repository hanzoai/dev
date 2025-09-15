# Upstream Merge Report

Date: 2025-09-15
Branch: upstream-merge
Source: upstream/main -> ours
Mode: by-bucket

## Incorporated
- Adopted upstream changes in `codex-rs/core/src/config.rs` (minor cleanup) and `codex-rs/core/prompt.md` (docs additions).
- Defaulted to upstream for non-protected areas with no conflicts.

## Dropped/Kept Ours
- Kept our TUI files per policy:
  - `codex-rs/tui/src/new_model_popup.rs`
  - `codex-rs/tui/src/onboarding/onboarding_screen.rs`
  - `codex-rs/tui/src/onboarding/welcome.rs`
  Rationale: preserve strict ordering, browser/agent tool UX, and fork-specific TUI enhancements.

## Purged
- No `.github/codex-cli-*` image assets present; nothing to purge.

## Invariants Verified
- Tool families present and exposed (`browser_*`, `agent_*`, `web_fetch`).
- Exposure gating for browser tools preserved.
- Screenshot queue semantics unchanged.
- Version/UA helpers intact (`codex_version::version()`, `get_codex_user_agent_default()`).
- Public re-exports in `codex-core` and `models` alias intact.

## Validation
- scripts/upstream-merge/verify.sh: PASSED (build-fast ok, API checks ok).

## Notes
- No prefer-theirs conflicts in `codex-rs/common/**`, `codex-rs/exec/**`, or `codex-rs/file-search/**` in this merge window.
- Upstream fetch produced many new branches/tags; merge scope limited to `main`.
