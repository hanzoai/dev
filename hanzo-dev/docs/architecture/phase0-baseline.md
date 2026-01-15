# Phase 0 Baseline – Architecture Survey (September 20, 2025)

This note captures the current layout of the `code-rs` workspace before any
phase-one refactors. It should help us reason about the execution pipeline and
identify the modules that will be split up in later phases.

## Workspace Layout

- **Core execution**
  - `hanzo-core`: orchestration crate exposing `Hanzo Dev`, conversation
    management, exec tooling, confirm-guard policy, safety wrappers, and
    project/plan utilities. Most business logic lives here today.
  - `hanzo-exec`: headsless CLI that streams protocol events to stdout/human or
    JSON renderers.
  - `hanzo-cli`: thin binary that wires auth/config to the TUI.
- **UI / presentation**
  - `hanzo-tui`: Ratatui-based interface; owns chat widget, history cells,
    bottom pane, onboarding, streaming controller, and layout logic.
- **Shared utilities**
  - `hanzo-common`: shared config summaries, presets, timers, CLI argument
    parsing helpers.
  - `hanzo-ansi-escape`, `hanzo-browser`, `hanzo-file-search`, `hanzo-login`,
    `hanzo-apply-patch`, etc. provide specialized services re-exported by
    `hanzo-core`.
- **Model/control plane**
  - `hanzo-mcp-*`: client/server/test fixtures for Model Context Protocol.
  - `hanzo-protocol`, `hanzo-protocol-ts`: protocol definitions consumed by
    Rust and TypeScript surfaces.
- **Ancillary crates**
  - Tooling (`hanzo-ollama`, `dev-linux-sandbox`, `hanzo-browser`), smoke
    tests, and TS bindings (`hanzo-browser`, `hanzo-arg0`, etc.).

## Core Command Pipeline (Today)

1. **Conversation spawn** (`hanzo-core::ConversationManager`)
   - Creates `Hanzo Dev` via `HanzoDev::spawn`, yielding an async event stream.
   - Produces `SessionConfiguredEvent` and registers the conversation in a
     shared `RwLock<HashMap<ConversationId, Arc<HanzoDevConversation>>>`.
2. **Event loop** (`hanzo-core::HanzoDev` & `HanzoDevConversation`)
   - `HanzoDev::next_event` polls the MCP transport and channel fan-out to yield
     `EventMsg` values.
   - `hanzo-core/src/hanzo.rs` manages confirm guards, tool dispatch, browser
     snapshots, streaming output buffers, and local command execution.
3. **Frontend consumption**
   - `hanzo-tui::chatwidget` subscribes through `AppEventSender` and mutates a
     `HistoryRenderState` alongside UI layout caches. Rendering merges state
     with Ratatui widgets in `history_cell`, `diff_render`, and friends.
   - `hanzo-exec` consumes the same events for non-interactive sessions.

## Identified Monoliths

- `hanzo-core/src/hanzo.rs` (~3k LOC) interleaves policy checks, tool
  invocation, browser hooks, and response assembly. Later phases will split it
  into smaller services (`ConfirmGuard`, `ToolBroker`, `ResponseAssembler`, etc.).
- `hanzo-tui/src/chatwidget.rs` (~3k LOC) couples event wiring, command state,
  rendering hints, and approval flows. Target is a reducer-style `ChatState`
  plus feature-specific controllers.
- `hanzo-tui/src/history_cell.rs` implements a trait object hierarchy with
  manual downcasts. Moving to a typed enum model will simplify rendering and
  caching.

## Current Testing & Tooling Gaps

- No lightweight integration test ensures event ordering invariants for the
  strict TUI history stream (per the TUI contract).
- `./build-fast.sh` is the single required check; results are not currently
  captured in documentation for regression comparison.

## Baseline Metrics

- `./build-fast.sh` (dev-fast) completed successfully on September 20, 2025 in
  14.87s producing `target/dev-fast/dev` (hash
  `4162f125c8a0afb8f513e6e6a98ba57035aa2fb39959295c2567ec4699f0e965`).

## Next Actions for Phase 0

1. Record baseline `./build-fast.sh` duration and success in this folder.
2. Introduce a smoke test under `hanzo-core/tests/` that drives a short mock
   conversation and asserts strictly ordered `EventMsg` IDs.
3. Evaluate adding a TUI snapshot test harness once the chat state reducer is
   available (tracked for later phases).
