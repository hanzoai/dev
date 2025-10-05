# TUI Gap Report: `codex-rs/tui` vs `code-rs/tui`

## Component Inventory
- **Bottom pane surfaces** – Fork adds agent dashboards, theme/preferences, notifications, cloud tasks, and approval modal views (`code-rs/tui/src/bottom_pane/mod.rs:1-210`, `code-rs/tui/src/bottom_pane/*.rs`); upstream retains the composer plus legacy overlay (`codex-rs/tui/src/bottom_pane/mod.rs:1-190`, `codex-rs/tui/src/bottom_pane/approval_overlay.rs`).
- **History rendering** – Fork decomposes history into typed cells backed by `code_core::history` exports (`code-rs/tui/HISTORY_CELLS_PLAN.md`, `code-rs/tui/src/history_cell/mod.rs:1-210`); upstream relies on a monolithic `HistoryCell` trait (`codex-rs/tui/src/history_cell.rs:1-220`).
- **Chat widget extensions** – Fork hosts auto-coordinator, rate-limit HUD, streaming diff/terminal handlers, retry flows, and browser screenshot support (`code-rs/tui/src/chatwidget/*`); upstream chat widget lacks these modules entirely.
- **Approval UX** – Fork ships `user_approval_widget` with session rules and sandbox escalation (`code-rs/tui/src/user_approval_widget.rs:1-240`); upstream overlay stays list-based (`codex-rs/tui/src/bottom_pane/approval_overlay.rs:1-200`).
- **Assets/build** – Fork bundles spinner/theme assets and build-time normalization (`code-rs/tui/build.rs`, `code-rs/tui/src/assets/`); upstream has no analogous assets or build step.

## API Surface Map
- Fork consumes additional `code_core` APIs: history domain events, rate-limit snapshots, plan tools, slash command formatting, config edit helpers (`code-rs/tui/src/history_cell/mod.rs:10-44`, `code-rs/tui/src/chatwidget.rs:4623-9869`, `code-rs/tui/src/app_event.rs:200-520`).
- Upstream references the older `codex_core` event set (`codex-rs/tui/src/chatwidget.rs:6-200`), missing OrderMeta, browser screenshot events, rate-limit telemetry, and validation toggles.
- CLI binaries align structurally, but the fork enables extra flags for auth mode, sandbox defaults, and theme overrides (`code-rs/tui/src/lib.rs:200-420`) absent upstream (`codex-rs/tui/src/lib.rs:150-340`).

## Integration Points with `code-core`
- **Config bootstrap & persistence** – Fork writes back theme/spinner/trust preferences (`code-rs/tui/src/lib.rs:370-432`, `code-rs/tui/src/app.rs:2760-2820`); upstream never calls those setters.
- **Validation & MCP controls** – Fork wires `Op::UpdateValidationGroup`, MCP server toggles, and agent defaults through chat events (`code-rs/tui/src/app_event.rs:240-360`, `code-rs/tui/src/chatwidget.rs:17100-17650`); upstream lacks these code paths.
- **Rate-limit telemetry** – Fork starts a background refresh worker using `ModelClient` (`code-rs/tui/src/chatwidget/rate_limit_refresh.rs:1-120`); upstream has no equivalent logic.
- **OrderMeta processing** – Fork enforces sequencing for streamed events and handles browser screenshots (`code-rs/tui/src/chatwidget.rs:880-1040`, `9869-9890`); upstream still renders FIFO without identifiers.

## Config / Environment Diffs
- Fork honors extended CLI overrides and `tui.theme.*` keys (`code-rs/tui/src/lib.rs:214-340`); upstream accepts legacy overrides only.
- Fork adjusts sandbox defaults to `AskForApproval::Never` for trusted workspaces (`code-rs/tui/src/lib.rs:600-648`); upstream defaults to `OnRequest` (`codex-rs/tui/src/lib.rs:500-560`).
- Cloud tasks and agent env hooks exist only in the fork (`code-rs/tui/src/cloud_tasks_service.rs`).

## CLI / UX Deltas
- Multi-agent control, resume/update flows, notifications settings, and rate-limit charts appear exclusively in the fork (`code-rs/tui/src/bottom_pane/*`, `code-rs/tui/src/rate_limits_view.rs`).
- Auto-drive, retry, and streaming diff overlays deliver accelerated workflows (`code-rs/tui/src/chatwidget/{auto_coordinator.rs,diff_ui.rs,streaming.rs}`) absent upstream.
- Fork’s approval modal supports session-wide allow rules and sandbox elevation; upstream overlay does not expose those options.

## Breaking Changes & Risks
- Typed history pipeline requires `HistoryState` reliability; pulling fork cells into upstream without the new domain events will break rendering order integration tests.
- Approval widget depends on background order tickets and durable auth decisions; missing those hooks reintroduces approval deadlocks.
- Auto-coordinator spawns async tasks; without matching cancellation plumbing upstream merges risk zombie tasks and UI hangs.
- Config edits for validation/agent settings assume new enums and schema; applying them upstream without schema updates will panic or drop settings silently.

## Proposed Thin Wrappers / Adapters
1. **History Rendering Adapter** – Introduce `tui_history_core` to re-export upstream history traits while providing shim constructors for fork cells (add `code-rs/tui/src/history/compat.rs`; update `history_cell/mod.rs` to import via `crate::history::compat::*`).
2. **Approval Overlay Wrapper** – Expose an `ApprovalUi` trait upstream and implement the modal in the fork behind a feature flag, retaining overlay for upstream (trait in `codex-rs/tui/src/bottom_pane/approval_overlay.rs`; adapter in `code-rs/tui/src/user_approval_widget.rs`).
3. **Event Hook Facade** – Define helper functions for rate-limit and browser screenshot events that no-op upstream but forward in the fork (`tui_event_extensions.rs` shared module; fork implements actual handlers).

**Rollback Note**: Land wrappers behind a dedicated Cargo feature (e.g., `code-fork`) so reverting disables fork-specific behavior without deleting files; document the toggle in `docs/subsystem-migration-status.md`.

## Smoke Tests
1. `./build-fast.sh --workspace both` – confirm dual-workspace builds remain green.
2. Launch fork TUI (`./build-fast.sh --workspace code run`); navigate Agents, Notifications, Theme, Rate Limits panes to verify focus, hotkeys, and exit paths.
3. Markdown/code rendering check – stream a response with fenced markdown and diff blocks (`/plan`, `/apply_patch`) to validate typed history cells.
4. Interactive approval flow – trigger exec and apply-patch approvals to test modal hotkeys, session allow rules, and background completion updates.
5. Auto-drive session – run `/drive` (or equivalent) to exercise auto-coordinator start/cancel, status banners, and history consistency.

## References
- Targeted diffs: `diff -ruN codex-rs/tui code-rs/tui`
- Scoped searches: `rg "code_core" code-rs/tui/src`, `rg "codex_core" codex-rs/tui/src`
- Supporting analysis: `.code/agents/d617a1bb-c914-4995-beec-e56990ebf293/result.txt`, `.code/agents/9f56d115-b1cc-47d0-8cfc-bcd80f8fba5f/result.txt`
