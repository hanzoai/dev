# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

Summary
- Adopted upstream codex-mcp-server protocol additions by aligning `ConversationId` to new struct API.
- Kept fork UA semantics in MCP server (`get_codex_user_agent_default`) via `message_processor.rs` imports.
- Preserved fork behavior in `codex_message_processor.rs` by not enabling login flows; upstream file was evaluated but we restored our implementation and made minimal API adjustments.
- Integrated upstream conversation pagination/resume protocol types without wiring the new endpoints in our server (policy: prefer ours for server behavior unless clearly beneficial). Tests compile in verify scope; runtime tests not executed per policy.
- Kept ICU/sys-locale usage and dependencies (used by `num_format.rs`).
- Maintained public re-exports and models alias in core (unchanged).

Notable Changes
- `codex-rs/protocol/src/mcp_protocol.rs`:
  - `ConversationId` is now a struct with custom (de)serialization.
  - Added `From<Uuid>` and `From<ConversationId>` impls to preserve core call sites.
  - Switched id generation to `Uuid::new_v4()` to match our uuid features and avoid v7 dependency.
- `codex-rs/core/src/codex.rs` and `codex-rs/mcp-server/src/message_processor.rs`:
  - Updated tuple-style `ConversationId(session_id)` calls to `ConversationId::from(session_id)`.
- `codex-rs/core/src/rollout/recorder.rs`:
  - Replaced direct `.0` access with `Uuid::from(conversation_id)`.
- `codex-rs/protocol/Cargo.toml`:
  - Resolved conflicts; retained ICU/sys-locale, serde_bytes/with; kept `uuid` v4 feature.

Dropped / Deferred
- Upstream login/auth endpoints for MCP server and one-off exec tooling were not enabled in our server to preserve existing fork policy and avoid behavior drift.
- Upstream `ListConversations`/`ResumeConversation` server handlers were evaluated but not wired in to keep the fork’s minimal MCP surface; protocol types remain for compatibility.

Guards
- `scripts/upstream-merge/verify.sh`: PASS
- `./build-fast.sh`: PASS (no warnings)

Follow-ups
- If we want the upstream MCP list/resume/archive endpoints, we can wire our server to expose them in a follow-up PR, ensuring parity with TUI UX and fork gating.
