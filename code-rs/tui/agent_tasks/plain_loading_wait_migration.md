# Plain/Background/Loading/Wait Migration

## Summary
- Replace all plain/background/loading/wait cell insertions and replacements in
  `chatwidget.rs` with `HistoryDomainEvent` calls so `HistoryState` becomes the
  source of truth for these stable cell types.
- Remove any remaining uses of `history_record_from_cell` for these families
  and ensure IDs are assigned via the new domain-event helpers.

## Prerequisites
- `HistoryDomainEvent` infrastructure landed and exported from
  `history/state.rs`.
- Background system notices already routed through
  `history_replace_with_record` (completed 2025-09-27).

## Scope
- `history_insert_plain_cell_with_key`, `history_push_plain_cell`, wait tool
  completions, loading spinners, and generic system helpers (`push_system_cell`).
- Update `assign_history_id` branches that become redundant once the domain
  events populate IDs.
- Extend `events_audit.md` with status notes as paths are migrated.

## Deliverables
- New domain-event constructors for `PlainMessageState`, `WaitStatusState`,
  `LoadingState`, and `BackgroundEventRecord`.
- `chatwidget.rs` updated to call `history_state.apply_domain_event(...)`
  (exact API name per infrastructure work).
- Unit or integration coverage that emits events and confirms `HistoryState`
  stores the expected records without referencing `history_cells` directly.
- Documentation update in `events_audit.md` marking these categories as
  migrated.

## Status (2025-09-27)
- Domain-event constructors landed and `chatwidget.rs` now hydrates inserted
  plain/loading/wait/background cells from `HistoryRecord` responses.
- `events_audit.md` updated to reflect the new flow.
- Outstanding: add automated coverage for the hydration helpers so regressions
  are caught when more cell types migrate.

## Next Steps for Agent
- Extend `chatwidget` tests (or add focused unit coverage) that drive
  `HistoryDomainEvent` inserts for plain/loading/wait/background cells and
  assert the resulting `HistoryState` records.
- Confirm there are no remaining `history_record_from_cell` call sites for
  these variants; remove or document any unavoidable leftovers.
- Update this file with findings and link any follow-up TODOs uncovered during
  testing.

## References
- `code-rs/tui/HISTORY_CELLS_PLAN.md` – Step 3, Event Pipeline Consolidation
  Plan (Phase C – wave 1).
- `code-rs/tui/src/chatwidget/events_audit.md` – mutation inventory.
