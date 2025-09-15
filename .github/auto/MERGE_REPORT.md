# Upstream Merge Report

Branch: `upstream-merge`
Upstream: `openai/codex@main`
Mode: by-bucket
Date: $(date -u +%Y-%m-%dT%H:%M:%SZ)

## Incorporated
- core/exec_command: adopted upstream `ExecCommandSession` changes (added exit status tracking via `Arc<AtomicBool>`), and updated our `SessionManager` to pass the new argument and set the flag in the wait thread.
- core/unified_exec: upstream module and wiring retained (already present); uses `has_exited()` without changing our external API.
- General upstream updates merged outside protected areas per policy.

## Kept Ours
- TUI: resolved conflicts in `codex-rs/tui/**` in favor of our fork (strict ordering, history cell UX, tool previews, etc.).
- CI/Workflows: kept our `.github/workflows/**` (removed upstream `rust-release.yml` which we previously deleted).
- Core invariants preserved: `openai_tools.rs`, `codex.rs`, `agent_tool.rs`, `default_client.rs`, and protocol models mapping; UA/version helpers and tool gating remain intact (verify guard passed).

## Dropped/Rejected
- Purged any reintroduced `.github/codex-cli-*` images if present (none remained after merge).

## Notes
- Addressed post-merge compile drift by adding exit status handling in `SessionManager` and silencing dead_code warnings on the new `exit_status` field and `has_exited()` to maintain zero-warning policy.
- `scripts/upstream-merge/verify.sh` passed (build_fast=ok, api_check=ok).
- `./build-fast.sh` completes with no warnings.

## Follow-ups (none required)
- No API surface changes; re-exports in `codex-core` preserved.
- No ICU/sys-locale removals detected.
