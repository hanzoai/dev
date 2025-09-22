# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

## Incorporated
- Adopted upstream updates across shared crates (common/exec/file-search) via merge.
- Added upstream login test: cancels_previous_login_server_when_port_is_in_use, integrated with our ServerOptions by setting `originator` and adding required imports.
- Adopted upstream refinements in mcp-server interrupt test structure while keeping TurnAbortReason from protocol crate for workspace consistency.
- Took upstream `codex-rs/Cargo.lock`; build-fast regenerated/normalized as needed.

## Preserved (fork invariants)
- Browser tools, agent tools, and web_fetch handlers with gating logic intact.
- Screenshot queue semantics and strict TUI ordering guarantees unchanged.
- Version/User-Agent helpers (`codex_version::version()`, `get_codex_user_agent_default()`) retained.
- Public re-exports in codex-core (ModelClient, Prompt, ResponseEvent, ResponseStream) kept; `codex_core::models` remains an alias to protocol models.

## Dropped/Adjusted
- Removed unused import in `mcp-server/tests/suite/interrupt.rs` (upstream also removed) to avoid warnings.
- No reintroduced purge assets detected under `.github/`.

## Verification
- scripts/upstream-merge/verify.sh: PASS (build_fast=ok, api_check=ok)
- ./build-fast.sh: PASS (no errors/warnings)

