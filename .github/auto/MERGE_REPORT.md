# Upstream Merge Report

Base: upstream/main
Target branch: upstream-merge
Mode: by-bucket

Summary
- Pulled latest from upstream/main and merged into our `upstream-merge` branch with `--no-commit`, resolved conflicts per fork policy, then validated with verify.sh.

Key Decisions
- Prefer Ours:
  - TUI (`codex-rs/tui/**`): kept our ChatWidget/history rendering and strict streaming ordering.
  - Workflows (`.github/workflows/**`): preserved our workflow set; upstream readded `rust-ci.yml` and `rust-release.yml` were removed to keep our policy.
  - Core invariants preserved:
    - Tool families: browser_*, agent_*, and web_fetch remain available and gated as before.
    - Version/UA: continued use of `codex_version::version()` and UA helpers.
    - Screenshot queue semantics unchanged.
    - Public re-exports: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream` remain.
    - `codex_core::models` alias preserved.

- Prefer Theirs / Adopt Upstream:
  - Non-protected core/common updates where compatible: `openai_model_info.rs`, test expectations in `core/tests/suite/client.rs`, and general protocol improvements in `codex-rs/protocol`.

- Compatibility Shims:
  - Kept our rate-limit event type in `core/src/client.rs` (`RateLimitSnapshotEvent`) to avoid breaking TUI display and existing event wiring; mapped from headers via our existing flat snapshot structure.
  - Restored `SessionStateSnapshot` and `SavedSession` in `core/src/rollout/recorder.rs` to preserve resume compatibility with our TUI/core paths.

Conflict Summary
- Deleted upstream workflows reintroduced by upstream: `.github/workflows/rust-ci.yml`, `.github/workflows/rust-release.yml` (policy: prefer ours).
- Resolved conflicts in:
  - `codex-rs/core/src/client.rs`: kept our rate-limit snapshot event; retained upstream SSE parsing improvements otherwise.
  - `codex-rs/core/src/error.rs`: removed upstream test helper to avoid unused imports; tests still compile in API check.
  - `codex-rs/core/src/rollout/recorder.rs`: restored our SavedSession/SessionStateSnapshot structs and added serde imports.
  - `codex-rs/tui/src/chatwidget.rs`, `codex-rs/tui/src/chatwidget/tests.rs`, `codex-rs/tui/src/history_cell.rs`: favored ours.

Purge/Removals
- Purged reintroduced upstream workflow files per policy.

Verification
- Ran `scripts/upstream-merge/verify.sh`:
  - build_fast: ok (./build-fast.sh passed, zero warnings required by policy)
  - api_check: ok (cargo check for `codex-core` test `api_surface`)
  - guards: ok (tool registration + UA/version checks)

Notes
- Upstream adjusted rate-limit JSON shape (nested primary/secondary) in some tests. We retained our flat event wire (`RateLimitSnapshotEvent`) for the TUI and core events while adopting upstream types in `codex_protocol` where safe. No runtime divergence for our consumers.
- No ICU/sys-locale dependency removals; repo-wide search indicates usage remains.

