# Upstream Merge Report

Mode: by-bucket
Upstream: openai/codex@main → branch upstream-merge (no-commit merge)

## Incorporated
- core: Extend `attach_item_ids` to include ids for Message, WebSearchCall, FunctionCall, LocalShellCall, CustomToolCall (merged from upstream in `codex-rs/core/src/client.rs`).
- tests: Updated Azure request id propagation test to cover new item kinds (merged in `codex-rs/core/tests/suite/client.rs`).

## Reconciled
- protocol alignment: Our fork had a slimmer `codex_core::protocol::EventMsg`. Upstream crates (`exec`, `mcp-server`, `tui`) refer to variants now modeled in `codex_protocol::protocol`. To avoid breaking our wiring and keep compatible APIs, we:
  - Added missing variants to `codex_core::protocol::EventMsg` as simple passthroughs to `codex_protocol::protocol` types:
    - `UserMessage`, `TurnAborted`, `ConversationPath`, `EnteredReviewMode`, `ExitedReviewMode`.
  - TUI/exec updated with benign no-op handlers for the new variants to preserve behavior while remaining build‑compatible.
- warnings policy: Annotated `REVIEW_PROMPT` with `#[allow(dead_code)]` to satisfy zero‑warning policy without changing public symbol names.

## Dropped
- None. No upstream files matched purge globs, and no removed paths were reintroduced.

## Invariants Preserved
- Tool schemas: browser_*, agent_* and `web_fetch` tooling remain registered and gated by our existing logic.
- Screenshot UX: no changes to queuing or TUI rendering; only added ignore cases for new protocol events.
- Version/UA: `codex_version::version()` usage and UA helpers unchanged.
- Public re‑exports and namespaces kept stable (ModelClient, Prompt, ResponseEvent, ResponseStream; `codex_core::models` → protocol models).

## Build/Verify
- `scripts/upstream-merge/verify.sh`: PASS
- `./build-fast.sh`: PASS (zero errors and zero warnings after reconcile)

