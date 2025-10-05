# TUI Testing Gap Report

## Current State

The TUI codebase has extensive unit tests for individual components and rendering logic, primarily located in `code-rs/tui/src/chatwidget/tests.rs`. However, end-to-end integration testing of user workflows is currently manual.

## Planned Automation

- **Automated smoke tests**: `code-rs/tui/tests/ui_smoke.rs` provides prototype scaffolding for scripted TUI scenarios
  - Uses `make_chatwidget_manual()` to construct widget instances without terminal I/O dependencies
  - Drives scenarios via `handle_code_event()` with synthetic event streams
  - Asserts rendered output using `buffer_to_string()` on test backend buffers
  - Current coverage: basic markdown streaming, approval flow, render sanity checks
  - Future work: capture/replay real event streams, expand scenario coverage, add specific assertions

## Future Directions

- Event stream capture/replay mechanism for reproducible test scenarios
- Expanded coverage for multi-turn conversations, error handling, tool calls, auto mode
- Integration with CI/CD for regression detection
- Performance benchmarks for rendering hot paths
