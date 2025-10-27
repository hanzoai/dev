# Issue Fix Report

## Issue #207 — Remove Codex Requirement (Won’t Fix)

### Problem Summary
Request aimed to drop the Codex requirement entirely so users without Codex access could run the CLI.

### Root Cause
The product still depends on a valid OpenAI (or alternative provider) account, so changing the default model string alone cannot remove that requirement.

### Resolution & Documentation Update
- Commit `e5c111b70` retained clarification-only work: defaults/docs now mention `gpt-5` alongside `gpt-5-codex` and highlight provider setup, but runtime behavior is unchanged.
- Issue is reclassified as “won’t fix” because an active provider account remains mandatory.

### Validation
- No functional change; `./build-fast.sh` already passed in this branch.

### Remaining Risks / Next Steps
None. Provider provisioning guidance is documented; no further engineering planned.

### Quality Rating: n/a
Informational update only; no code fix to evaluate.

---

## Issue #343 — `OPENAI_WIRE_API` Override Ignored

### Problem Summary
Setting `OPENAI_WIRE_API=chat` (documented override) no longer forced the Chat Completions API, leaving users stuck on Responses.

### Root Cause
`built_in_model_providers()` in `code-rs/core/src/model_provider_info.rs` hard-coded `WireApi::Responses`, ignoring the environment override.

### Fix Description
- Added `wire_api_override_from_env` helper to parse `OPENAI_WIRE_API` (`code-rs/core/src/model_provider_info.rs`).
- OpenAI provider construction now respects the helper, falling back to Responses on invalid input.
- Added regression tests (`code-rs/core/tests/openai_wire_api_env.rs`) for defaults, chat/responses selection, invalid input, and case-insensitivity.

### Validation
- New test suite covers five override scenarios.
- `./build-fast.sh` passes.

### Remaining Risks / Next Steps
Minimal. Behavior still requires restart to pick up changed env vars—documented expectation.

### Quality Rating: 5/5
Tight implementation with exhaustive unit coverage and robust fallback handling.

---

## Issue #289 — Legacy Custom Prompts Not Loaded

### Problem Summary
Prompt discovery skipped `~/.codex/prompts`, breaking legacy installations.

### Root Cause
`default_prompts_dir()` in `code-rs/core/src/custom_prompts.rs` joined `prompts` directly on the code-home path instead of using the legacy-aware resolver.

### Fix Description
- Switched to `resolve_code_path_for_read` so legacy directories are discovered.
- Added comprehensive async tests in `code-rs/core/tests/custom_prompts_discovery.rs` covering CODE_HOME, legacy fallback, precedence when both exist, and filtering non-Markdown files.

### Validation
- New tests exercise all discovery branches.
- `./build-fast.sh` passes.

### Remaining Risks / Next Steps
Low. The resolver is shared with other config paths; document precedence if users keep both directories populated.

### Quality Rating: 5/5
One-line fix with strong regression tests and environmental isolation.

---

## Issue #351 — Agents Toggle Didn’t Persist

### Problem Summary
Toggling the “Enabled” checkbox in the Agents overlay didn’t immediately persist; users had to exit the overlay to see updates.

### Root Cause
`AgentEditorView` updated the in-memory flag but only persisted configuration when the Save action fired.

### Fix Description
- Introduced `persist_current_agent()` and invoked it after Left/Right/Space toggle actions and on Enter (`code-rs/tui/src/bottom_pane/agent_editor_view.rs`).
- Expanded smoke helpers to handle `UpdateAgentConfig` and `ShowAgentsOverview` events (`code-rs/tui/src/chatwidget/smoke_helpers.rs`).
- Added VT100 snapshot regression `agents_toggle_claude_opus_persists_via_slash_command` asserting UI and persisted state stay in sync (`code-rs/tui/tests/vt100_chatwidget_snapshot.rs`).

### Validation
- Snapshot test covers toggle → persist → reopen flow.
- `./build-fast.sh` passes.

### Remaining Risks / Next Steps
Low. Persistence still assumes config writes succeed; future work could surface errors in the UI.

### Quality Rating: 5/5
Fix is localized, regression-tested, and improves UX without regressions.

---

## Issue #307 — Gemini CLI Flag Rejected

### Problem Summary
Gemini agents invoked the CLI with `-m`, which newer releases rejected, causing launch failures.

### Root Cause
`agent_defaults.rs` still used the short flag for Gemini specs.

### Fix Description
- Gemini Pro/Flash specs now use `--model` (`code-rs/core/src/agent_defaults.rs`).
- Added unit test ensuring both specs keep the long flag (`code-rs/core/tests/gemini_model_args.rs`).

### Validation
- New unit test passes under `./build-fast.sh`.

### Remaining Risks / Next Steps
Minor. Could consider an integration test that spawns the CLI, but static spec coverage prevents regression in this layer.

### Quality Rating: 4/5
Targeted fix with guardrail; slight deduction for lack of end-to-end validation.

---

## Issue #333 — Duplicate MCP Servers After `/new`

### Problem Summary
Invoking `/new` created a fresh MCP session without shutting down the previous one, leaving duplicate stdio servers running and cluttering the Agents menu.

### Root Cause
The submission loop aborted the old session but didn’t await cleanup of its `McpConnectionManager` clients; legacy stdio processes relied on drop semantics, and RMCP transports lacked an explicit shutdown path.

### Fix Description
- Added `Session::shutdown_mcp_clients` to await `McpConnectionManager::shutdown_all` before constructing the next session (`code-rs/core/src/codex.rs:1123`, `code-rs/core/src/codex.rs:3438`).
- `McpConnectionManager` now stores clients in an `RwLock`, drains them, and calls `McpClientAdapter::into_shutdown` to dispose of each client deterministically (`code-rs/core/src/mcp_connection_manager.rs`). Legacy adapters drop and yield to propagate `kill_on_drop`; RMCP adapters call the new `shutdown` method.
- Added `RmcpClient::shutdown` to cancel the running service (`code-rs/rmcp-client/src/rmcp_client.rs`).
- Introduced async regression test `mcp_session_cleanup` that compiles a stub wrapper and asserts the first MCP process exits before the second manager starts (`code-rs/tui/tests/mcp_session_cleanup.rs`).

### Validation
- Regression test exercises `/new` behavior and fails if the first process never exits.
- `./build-fast.sh` passes.

### Remaining Risks / Next Steps
Moderate. Legacy stdio still relies on `kill_on_drop` plus a `yield_now`; consider adding explicit wait-on-child for extra certainty and covering HTTP transports in tests.

### Quality Rating: 4/5
Substantial improvement with automated coverage; small residual risk remains around stdio teardown timing.
