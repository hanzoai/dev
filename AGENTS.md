# Rust/codex-rs

In the codex-rs folder where the rust code lives:

- Crate names are prefixed with `codex-`. For examole, the `core` folder's crate is named `codex-core`
- When using format! and you can inline variables into {}, always do that.
- Never add or modify any code related to `CODEX_SANDBOX_NETWORK_DISABLED_ENV_VAR` or `CODEX_SANDBOX_ENV_VAR`.
  - You operate in a sandbox where `CODEX_SANDBOX_NETWORK_DISABLED=1` will be set whenever you use the `shell` tool. Any existing code that uses `CODEX_SANDBOX_NETWORK_DISABLED_ENV_VAR` was authored with this fact in mind. It is often used to early exit out of tests that the author knew you would not be able to run given your sandbox limitations.
  - Similarly, when you spawn a process using Seatbelt (`/usr/bin/sandbox-exec`), `CODEX_SANDBOX=seatbelt` will be set on the child process. Integration tests that want to run Seatbelt themselves cannot be run under Seatbelt, so checks for `CODEX_SANDBOX=seatbelt` are also often used to early exit out of tests, as appropriate.

Completion/build step

- On completion of work, run only `./build-fast.sh` from the repo root. This script verifies file integrity and is the single required check right now.
- Do not run additional format/lint/test commands on completion (e.g., `just fmt`, `just fix`, `cargo test`) unless explicitly requested for a specific task.

When making individual changes prefer running tests on individual files or projects first if asked, but otherwise rely on `./build-fast.sh` at the end.

## Command Execution Architecture

The command execution flow in Codex follows an event-driven pattern:

1. **Core Layer** (`codex-core/src/codex.rs`):
   - `on_exec_command_begin()` initiates command execution
   - Creates `EventMsg::ExecCommandBegin` events with command details

2. **TUI Layer** (`codex-tui/src/chatwidget.rs`):
   - `handle_codex_event()` processes execution events
   - Manages `RunningCommand` state for active commands
   - Creates `HistoryCell::Exec` for UI rendering

3. **History Cell** (`codex-tui/src/history_cell.rs`):
   - `new_active_exec_command()` - Creates cell for running command
   - `new_completed_exec_command()` - Updates with final output
   - Handles syntax highlighting via `ParsedCommand`

This architecture separates concerns between execution logic (core), UI state management (chatwidget), and rendering (history_cell).

## Ephemeral Context Injection

Codex automatically injects fresh context at the end of each model request:

- **Location**: Ephemeral items are appended as the last items in the request via `get_formatted_input()`
- **Content**: System status (cwd, git branch, reasoning level), browser info (URL, type), screenshots
- **Generation**: `build_turn_ephemeral_items()` creates fresh context for each retry attempt
- **Storage**: Stored in `Prompt.ephemeral_items` field, ensuring they're regenerated per-request

This design ensures the model always receives current environment context without complex refresh logic or state management.
