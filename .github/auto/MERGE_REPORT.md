# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket
Date: 2025-09-19

## Incorporated
- Upstream test infra additions under `codex-rs/core/tests/common` (new `responses.rs` helpers; `wiremock` dep).
- Minor derive/style tweak in `codex-rs/core/src/exec.rs` (`#[derive(Clone, Debug)]`).
- Test updates in `codex-rs/core/tests/suite` to use shared helpers.

## Kept Ours (prefer_ours)
- `codex-rs/core/src/codex.rs`: preserved fork logic for tool registration, browser gating, UA/version helpers, and response streaming invariants.
- `codex-rs/core/src/openai_tools.rs`: preserved custom tool schemas and handler↔tool parity for `browser_*`, `agent_*`, and `web_fetch`.

Rationale: Maintains fork invariants (tool families, gating, screenshot queue semantics, UA/version), and ensures compatibility with our TUI/tooling.

## Dropped/Rejected
- No upstream files were explicitly dropped beyond automatic policy application; no purge-glob assets present.

## Other Notes
- Purge globs: none present.
- Public API checks: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream` re-exports intact; `codex_core::models` alias intact.
- ICU/sys-locale: no removals performed.

## Validation
- `scripts/upstream-merge/verify.sh`: PASS (build_fast=ok, api_check=ok, branding guard ok).
- `./build-fast.sh`: executed as part of verify; PASS.

