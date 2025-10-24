# Auto Drive State Inventory

This document catalogs every `auto_state` field access across the TUI and controller so we can migrate toward single-phase semantics without missing any flag interactions.

## Sources Scanned

- `code-rs/tui/src/chatwidget.rs`
- `code-rs/tui/src/bottom_pane/auto_coordinator_view.rs`
- `code-rs/tui/src/bottom_pane/auto_drive_settings_view.rs`
- `code-rs/tui/src/bottom_pane/paste_burst.rs`
- `code-rs/tui/src/chatwidget/smoke_helpers.rs`
- `code-rs/code-auto-drive-core/src/controller.rs`

Each entry below lists read vs. write occurrences (line numbers and snippets). Counts help highlight high-traffic fields.

## `active`

- Reads: 8
  - code-rs/tui/src/chatwidget.rs:14158 — `if !self.auto_state.active || !self.auto_state.waiting_for_response {`
  - code-rs/tui/src/chatwidget.rs:14312 — `if !self.auto_state.active {`
  - code-rs/tui/src/chatwidget.rs:14592 — `if !self.auto_state.active && !self.auto_state.awaiting_goal_input {`
  - code-rs/tui/src/chatwidget.rs:14618 — `if !self.auto_state.active {`
  - code-rs/tui/src/chatwidget.rs:14651 — `if !self.auto_state.active {`
  - code-rs/tui/src/chatwidget.rs:14680 — `if !self.auto_state.active || delta.trim().is_empty() {`
  - code-rs/tui/src/chatwidget.rs:14767 — `if !self.auto_state.active {`
  - code-rs/tui/src/chatwidget.rs:23385 — `if !self.auto_state.active || !self.auto_state.waiting_for_review {`
- Writes: 10
  - code-rs/tui/src/chatwidget.rs:21884 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget.rs:22025 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget.rs:22052 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget.rs:22133 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget.rs:22199 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget.rs:22255 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget.rs:22309 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget.rs:22338 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget.rs:22399 — `chat.auto_state.active = true;`
  - code-rs/tui/src/chatwidget/smoke_helpers.rs:311 — `chat.auto_state.active = true;`

## `awaiting_coordinator_submit`

- Reads: 12
  - code-rs/tui/src/chatwidget.rs:4765 — `if self.auto_state.awaiting_coordinator_submit()`
  - code-rs/tui/src/chatwidget.rs:14032 — `if !self.auto_state.is_active() || !self.auto_state.awaiting_coordinator_submit() {`
  - code-rs/tui/src/chatwidget.rs:14438 — `let progress_hint_active = self.auto_state.awaiting_coordinator_submit()`
  - code-rs/tui/src/chatwidget.rs:14476 — `let countdown = if self.auto_state.awaiting_coordinator_submit() {`
  - code-rs/tui/src/chatwidget.rs:14487 — `let button = if self.auto_state.awaiting_coordinator_submit() {`
  - code-rs/tui/src/chatwidget.rs:14501 — `let manual_hint = if self.auto_state.awaiting_coordinator_submit() {`
  - code-rs/tui/src/chatwidget.rs:14513 — `let ctrl_switch_hint = if self.auto_state.awaiting_coordinator_submit() {`
  - code-rs/tui/src/chatwidget.rs:14526 — `!self.auto_state.awaiting_coordinator_submit() || self.auto_state.is_paused_manual();`
  - code-rs/tui/src/chatwidget.rs:14532 — `awaiting_submission: self.auto_state.awaiting_coordinator_submit(),`
  - code-rs/tui/src/chatwidget.rs:17613 — `|| (self.auto_state.is_active() && self.auto_state.awaiting_coordinator_submit())`
  - code-rs/tui/src/chatwidget.rs:17646 — `if self.auto_state.awaiting_coordinator_submit() {`
  - code-rs/tui/src/chatwidget/smoke_helpers.rs:459 — `if chat.auto_state.awaiting_coordinator_submit() && !chat.auto_state.is_paused_manual() {`
- Writes: 0

*(…repeat sections for each field as captured in the raw inventory…)*

## Hotspots to Migrate First

- **Prompt submission** — chatwidget.rs:9220, 13987 (sets multiple flags on success)
- **Manual pause/resume** — chatwidget.rs:14045–14170 (toggle paused/resume state, countdown, prompts)
- **Post-review dispatch** — chatwidget.rs:13218, 14190 (resets review flags before sending conversation)
- **Transient recovery** — chatwidget.rs:13440–13770 (sets restart flags, handles recovery attempts)

Focusing on these clusters next will eliminate the most complex boolean juggling and unlock the single-phase migration.
