Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

Incorporated
- Upstream core tests additions:
  - codex-rs/core/tests/common/test_codex.rs
  - codex-rs/core/tests/suite/user_notification.rs
- Upstream user notification module and wiring file (kept, with warning fix):
  - codex-rs/core/src/user_notification.rs (+#[allow(dead_code)] to keep zero warnings)
- Cargo.lock updated to reflect dependency graph post-merge.

Dropped / Prefer-ours
- codex-rs/core/src/codex.rs: resolved conflicts by keeping fork version per policy
  to preserve browser/agent tools, web_fetch, strict streaming ordering, and UA/version semantics.

Reconciled
- Test suite aggregator (codex-rs/core/tests/suite/mod.rs): included both
  `stream_order` (fork) and `user_notification` (upstream) modules.

Purge checks
- No reintroduced purge assets under .github/codex-cli-* detected.

Guards and build
- scripts/upstream-merge/verify.sh: OK (tools parity, UA/version, branding)
- ./build-fast.sh: OK (no warnings after allow(dead_code) on UserNotifier)

Notes
- Preserved public API invariants and core re-exports; no changes detected that
  would break downstream callers.
