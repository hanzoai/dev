# Auto Drive Phase Migration TODO

- Remove legacy fields from `AutoDriveController` that duplicate phase state (`active`, `awaiting_submission`, `waiting_for_response`, `paused_for_manual_edit`, `resume_after_manual_submit`, `waiting_for_review`, `waiting_for_transient_recovery`, `coordinator_waiting`).
- Update remaining TUI call sites (outside ChatWidget hot paths) to use controller helpers (`is_active`, `is_paused_manual`, `resume_after_submit`, `awaiting_coordinator_submit`, `awaiting_review`, `in_transient_recovery`).
- Replace test harness helpers in `tui/tests` that mutate legacy flags with phase-aware helpers or controller transitions.
- Ensure ESC routing (`describe_esc_context`, `execute_esc_intent`) exclusively inspects `AutoRunPhase`/helpers and removes stop-gap flag checks.
- Add unit/VT100 coverage for manual pause/resume and transient recovery sequences under the new phase helpers.
- Acceptance: `./build-fast.sh` green; no direct reads/writes of legacy flags across the repo; ESC flows verified in snapshot tests.
