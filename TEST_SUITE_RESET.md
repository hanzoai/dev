# Test Suite Reset Plan for code-rs

## Executive Summary

This document outlines the plan to reset the code-rs test infrastructure by removing the legacy integration test suite and establishing a minimal, maintainable testing baseline. The primary goal is to reduce CI time and maintenance burden while ensuring `./build-fast.sh --workspace code` remains the sole required gate.

## Current State Analysis

### Test Infrastructure Overview

- **Total test files**: ~99 Rust test files across 13 test directories
- **Major test suites**:
  - `core/tests/`: 32 files (largest suite with ~18k LOC total)
    - Integration tests for CLI streaming, compact/resume/fork, prompt caching, OpenTelemetry, MCP client
    - Largest individual files: `client.rs` (1489 LOC), `otel.rs` (1196 LOC), `compact.rs` (1035 LOC)
  - `app-server/tests/`: 19 files
    - Message processor flows, auth, conversation management, fuzzy search
  - `exec/tests/`: 10 files
    - Event processor, sandbox, resume, output schema
  - `execpolicy/tests/`: 11 files
    - Command parsing tests for various shell commands
  - Smaller suites: `mcp-server` (7 files), `login` (4 files), `apply-patch` (3 files), `chatgpt` (3 files), etc.

### TUI Legacy Tests (Feature-Gated)

The `tui` crate contains **27 inline test modules** gated behind `#[cfg(all(test, feature = "legacy_tests"))]`:
- Text formatting, markdown rendering, live wrapping
- Chat composer, textarea, command popup
- Status indicators, pager overlay, clipboard handling
- Backtrack helpers, user approval widgets
- VT100-based replay tests (`chatwidget_stream_tests`)

These tests are **disabled by default** (feature `legacy_tests = []` in `tui/Cargo.toml` line 20).

### Test Helper Infrastructure

Three shared test support crates exist:
1. **`core_test_support`** (`core/tests/common/`): SSE fixture loading, default config setup, event waiting utilities
2. **`app_test_support`** (`app-server/tests/common/`): MCP process management, mock model server, SSE response helpers
3. **`mcp_test_support`** (`mcp-server/tests/common/`): Similar to app_test_support for MCP server tests

All three are workspace dependencies (defined in root `Cargo.toml`).

### CI Gating Status

- **Current gate**: `./build-fast.sh --workspace code` (confirmed in `.github/workflows/`)
  - Used in `issue-triage.yml` line 188
  - Used in `upstream-merge.yml` lines 351, 676-712
  - Used in `issue-code.yml` lines 170, 185, 251
- **No `cargo test` invocations** found in any workflow file
- Build-only verification is **already enforced** as the sole requirement

---

## Removal Plan

### Phase 1: Delete Legacy Integration Tests

**Target for removal** (complete deletion recommended):

#### Core Tests (`code-rs/core/tests/`)
- **Delete entire directory**: `code-rs/core/tests/`
- Files to remove (32 total):
  - `all.rs`, `api_surface.rs`, `chat_completions_payload.rs`, `chat_completions_sse.rs`, `mcp_manager.rs`
  - `suite/`: All 20 test modules including `client.rs`, `compact.rs`, `compact_resume_fork.rs`, `cli_stream.rs`, `exec.rs`, `exec_stream_events.rs`, `fork_conversation.rs`, `json_result.rs`, `live_cli.rs`, `model_overrides.rs`, `otel.rs`, `prompt_caching.rs`, `review.rs`, `rmcp_client.rs`, `rollout_list_find.rs`, `rollout_resume.rs`, `seatbelt.rs`, `stream_error_allows_next_turn.rs`, `stream_no_completed.rs`, `stream_order.rs`, `user_notification.rs`, `abort_tasks.rs`
  - `common/`: Test fixtures and helpers (`lib.rs`, `responses.rs`, `test_codex.rs`, `test_codex_exec.rs`)

#### App Server Tests (`code-rs/app-server/tests/`)
- **Delete entire directory**: `code-rs/app-server/tests/`
- Files to remove (19 total):
  - `all.rs`
  - `suite/`: `archive_conversation.rs`, `auth.rs`, `code_message_processor_flow.rs`, `config.rs`, `create_conversation.rs`, `fuzzy_file_search.rs`, `interrupt.rs`, `list_resume.rs`, `login.rs`, `send_message.rs`, `set_default_model.rs`, `user_agent.rs`, `user_info.rs`
  - `common/`: `lib.rs`, `mcp_process.rs`, `mock_model_server.rs`, `responses.rs`

#### Exec Tests (`code-rs/exec/tests/`)
- **Delete entire directory**: `code-rs/exec/tests/`
- Files to remove (10 total):
  - `all.rs`, `event_processor_with_json_output.rs`
  - `suite/`: `apply_patch.rs`, `auth_env.rs`, `common.rs`, `mod.rs`, `output_schema.rs`, `resume.rs`, `sandbox.rs`, `server_error_exit.rs`

#### MCP Server Tests (`code-rs/mcp-server/tests/`)
- **Delete entire directory**: `code-rs/mcp-server/tests/`
- Files to remove (7 total):
  - `all.rs`
  - `suite/`: `codex_tool.rs`, `mod.rs`
  - `common/`: `lib.rs`, `mcp_process.rs`, `mock_model_server.rs`, `responses.rs`

#### Login Tests (`code-rs/login/tests/`)
- **Delete entire directory**: `code-rs/login/tests/`
- Files to remove (4 total): `all.rs`, `suite/device_code_login.rs`, `suite/login_server_e2e.rs`, `suite/mod.rs`

#### ChatGPT Tests (`code-rs/chatgpt/tests/`)
- **Delete entire directory**: `code-rs/chatgpt/tests/`
- Files to remove (3 total): `all.rs`, `suite/apply_command_e2e.rs`, `suite/mod.rs`

#### Apply Patch Tests (`code-rs/apply-patch/tests/`)
- **Delete entire directory**: `code-rs/apply-patch/tests/`
- Files to remove (3 total): `all.rs`, `suite/cli.rs`, `suite/mod.rs`

#### ExecPolicy Tests (`code-rs/execpolicy/tests/`)
- **Keep minimal**: These are lightweight unit tests for command parsing logic
- **Action**: Review and potentially keep 2-3 critical tests (e.g., `literal.rs`, `sed.rs`) as examples
- Consider consolidating into inline `#[cfg(test)]` modules instead

#### Other Test Directories (Evaluate Case-by-Case)
- **`cli/tests/`** (2 files: `mcp_add_remove.rs`, `mcp_list.rs`): **DELETE** - MCP config tests
- **`mcp-types/tests/`** (4 files): **KEEP** - Protocol serialization tests are valuable for stability
- **`linux-sandbox/tests/`** (3 files): **KEEP** - Security-critical landlock tests
- **`cloud-tasks/tests/`** (1 file: `env_filter.rs`): **KEEP** - Minimal, focused test

### Phase 2: Remove TUI Legacy Tests

- **Action**: Remove `legacy_tests` feature entirely from `code-rs/tui/Cargo.toml`
- Delete all `#[cfg(all(test, feature = "legacy_tests"))]` test modules from `tui/src/`:
  - 27 test module blocks across files like `text_formatting.rs`, `markdown_renderer.rs`, `live_wrap.rs`, `chatwidget.rs`, etc.
  - Special file: `chatwidget_stream_tests.rs` (VT100 replay tests in `lib.rs` line 98)
  - Test-only helper methods (e.g., `chatwidget.rs` line 911: `pub(crate) fn test_for_request`)
- Also remove `vt100-tests` feature if no longer needed (line 16 in `tui/Cargo.toml`)

### Phase 3: Clean Up Test Dependencies

Remove from `code-rs/Cargo.toml` workspace dependencies:
- `app_test_support = { path = "app-server/tests/common" }` (line 48)
- `core_test_support = { path = "core/tests/common" }` (line 76)
- `mcp_test_support = { path = "mcp-server/tests/common" }` (line 78)

Remove or minimize `[dev-dependencies]` across affected crates:
- `core/Cargo.toml`: Remove `assert_cmd`, `predicates`, `tokio-test`, `wiremock` (keep `tempfile`, `pretty_assertions` if needed for doc tests)
- `app-server/Cargo.toml`: Remove `app_test_support`, `core_test_support`, `assert_cmd`, `base64`, `os_info`, `pretty_assertions`, `tempfile`, `toml`, `wiremock`
- `exec/Cargo.toml`: Review and remove test-specific dependencies
- `tui/Cargo.toml`: Remove `insta`, `strip-ansi-escapes`, `rand`, `pretty_assertions` if only used in legacy tests

---

## Minimal Tests to Keep

### Recommended Smoke/Unit Tests

1. **Protocol/Types Tests** (`mcp-types/tests/`):
   - JSON-RPC serialization/deserialization correctness
   - Progress notification format
   - Initialize handshake format
   - **Rationale**: Catches breaking API changes early

2. **Security Tests** (`linux-sandbox/tests/`):
   - Landlock sandboxing validation
   - **Rationale**: Security-critical, platform-specific

3. **Utility Tests** (`cloud-tasks/tests/env_filter.rs`):
   - Environment variable filtering logic
   - **Rationale**: Low-cost, high-value edge case coverage

4. **ExecPolicy (Minimal)** (`execpolicy/tests/`):
   - Keep 2-3 critical command parsing tests (e.g., `literal.rs`, `sed.rs`)
   - Move to inline `#[cfg(test)]` modules if possible
   - **Rationale**: Critical correctness for shell command allowlisting

5. **Doc Tests** (Existing `#[doc]` examples):
   - Already run by `cargo test` with minimal cost
   - Keep inline examples in library code where helpful
   - **Rationale**: Ensures public API examples compile

### Total Retained Test LOC
- Estimated: **<500 lines** (down from ~18k+ integration tests)

---

## Required Helper Refactors

### No Major Refactors Needed

Since we're **deleting** rather than migrating tests, minimal code changes are required:

1. **Remove workspace test support dependencies** (see Phase 3 above)
2. **Remove test-only pub methods** in production code:
   - Search for `#[cfg(any(test, feature = "legacy_tests"))]` and remove those methods
   - Example: `tui/src/chatwidget.rs` line 911 `pub(crate) fn test_for_request`
3. **Clean up unused imports** flagged by clippy after deletion
4. **Verify no CI breakage** from removed test crates (unlikely since tests aren't run in CI)

### Optional: Consolidate Remaining Tests

For `execpolicy` and other small suites, consider:
- Moving tests from `tests/` directory to inline `#[cfg(test)] mod tests` in `src/`
- Reduces filesystem clutter
- Allows direct access to private functions
- Example pattern:
  ```rust
  // src/lib.rs or src/module.rs
  #[cfg(test)]
  mod tests {
      use super::*;

      #[test]
      fn test_parsing_logic() { /* ... */ }
  }
  ```

---

## CI Gating Strategy

### Current Behavior (No Change Needed)
- **Primary gate**: `./build-fast.sh --workspace code` (build-only)
- **No test execution** in any workflow
- Workflows already enforce:
  - Build success (compile check)
  - Clippy lints pass
  - Formatting via `rustfmt` (if configured)

### Post-Reset Behavior
- **Unchanged**: `./build-fast.sh --workspace code` remains sole gate
- Minimal test suites (mcp-types, linux-sandbox, etc.) can be run **locally** on-demand:
  ```bash
  cargo test -p mcp-types
  cargo test -p code-linux-sandbox
  cargo test -p code-cloud-tasks
  ```
- No automatic test execution in CI prevents:
  - Slow PR feedback
  - Flaky test maintenance burden
  - Need for test infrastructure (mock servers, fixtures, etc.)

### Future Considerations
If integration tests are re-introduced later:
- Gate behind explicit workflow dispatch or nightly schedule
- Use separate workflow file (e.g., `.github/workflows/integration-tests.yml`)
- Mark as non-blocking advisory check

---

## Migration Checklist

### Pre-Deletion Validation
- [ ] Archive current test suite state to a branch (e.g., `archive/legacy-tests-2025-10`)
- [ ] Document any critical test scenarios that should be preserved as manual validation steps
- [ ] Verify no production code has hard dependencies on test crates

### Deletion Steps
1. [ ] Delete test directories (Phase 1):
   - [ ] `code-rs/core/tests/`
   - [ ] `code-rs/app-server/tests/`
   - [ ] `code-rs/exec/tests/`
   - [ ] `code-rs/mcp-server/tests/`
   - [ ] `code-rs/login/tests/`
   - [ ] `code-rs/chatgpt/tests/`
   - [ ] `code-rs/apply-patch/tests/`
   - [ ] `code-rs/cli/tests/`
2. [ ] Remove TUI legacy tests (Phase 2):
   - [ ] Delete `legacy_tests` feature from `tui/Cargo.toml`
   - [ ] Remove all 27 gated test modules in `tui/src/`
   - [ ] Remove `chatwidget_stream_tests.rs`
   - [ ] Remove `vt100-tests` feature if unused
3. [ ] Clean dependencies (Phase 3):
   - [ ] Remove `app_test_support`, `core_test_support`, `mcp_test_support` from workspace `Cargo.toml`
   - [ ] Prune `[dev-dependencies]` in affected crates
4. [ ] Remove test-only code:
   - [ ] Search and remove `#[cfg(any(test, feature = "legacy_tests"))]` methods
   - [ ] Clean up unused imports (run `cargo clippy --fix`)

### Post-Deletion Validation
- [ ] Run `./build-fast.sh --workspace code` successfully
- [ ] Run `cargo clippy --workspace` with no errors
- [ ] Run remaining tests locally: `cargo test -p mcp-types -p code-linux-sandbox -p code-cloud-tasks`
- [ ] Verify no warnings about missing workspace members
- [ ] Confirm CI workflows still pass

---

## Rollback Plan

If issues arise post-deletion:
1. Revert to archived branch: `git checkout archive/legacy-tests-2025-10`
2. Cherry-pick critical fixes onto legacy test branch
3. Evaluate minimal subset of tests to restore

---

## Benefits Summary

1. **Reduced Maintenance**: No need to update/fix tests when refactoring internal APIs
2. **Faster Local Builds**: No test compilation overhead for developers
3. **CI Simplicity**: Single build gate reduces complexity and failure modes
4. **Codebase Clarity**: Less test scaffolding obscuring production code
5. **Lower Barrier to Entry**: New contributors only need to ensure build passes

---

## Risks & Mitigations

| Risk | Likelihood | Mitigation |
|------|------------|------------|
| Loss of regression coverage | Medium | Archive tests; rely on manual validation for critical paths; re-introduce targeted tests if regressions occur |
| Breaking changes undetected | Medium | Build gate + clippy catches most issues; user reports catch integration issues |
| Reduced confidence in refactoring | Low | Strong type system + compiler checks provide safety net |
| Difficulty debugging production issues | Low | Logging/telemetry more valuable than stale tests; can add focused repro tests on-demand |

---

## References

- Current build script: `./build-fast.sh` (lines 465, 633-636 confirm no test execution)
- Workflow files:
  - `.github/workflows/issue-triage.yml` line 188
  - `.github/workflows/upstream-merge.yml` lines 351, 676-712, 733
  - `.github/workflows/issue-code.yml` lines 170, 185, 251
- Feature flags: `code-rs/tui/Cargo.toml` lines 16, 20
- Test helpers: `code-rs/Cargo.toml` lines 48, 76, 78

---

**Document Version**: 1.0
**Last Updated**: 2025-10-05
**Status**: Ready for implementation
