# Streaming / Exec / Assistant Migration

## Summary
- Move streaming deltas, finalized assistant messages, exec lifecycle updates,
  and merged exec summaries onto the `HistoryDomainEvent` pipeline.
- Eliminate cell-side state mutations and redundant cached line buffers where
  practical, while preserving the interim cache bridge called out in Step 2.

## Prerequisites
- Infrastructure: `HistoryDomainEvent` enums + helpers merged.
- Wave 1 (plain/background/loading/wait) migrated so supporting utilities are
  battle-tested.

## Scope
- Streaming handlers: `ensure_answer_stream_state`, tail updates, review-flow
  paths, and final message insertions (`chatwidget.rs:11380-11890`).
- Exec lifecycle: active exec state, merged exec summaries, wait notes, and
  tool completion merges (`chatwidget.rs:6515-7330`).
- Ensure `history_cell::ExecCell`, `StreamingContentCell`, and
  `AssistantMarkdownCell` rebuild purely from `HistoryState` data after events.
- Update `assign_history_id` only where still required; prefer storing IDs via
  the domain events.

## Deliverables
- Domain-event variants covering exec begin/end/stream chunks, assistant stream
  deltas, assistant final messages, and merged exec summaries.
- ChatWidget handlers updated to emit domain events instead of mutating
  `history_cells` directly.
- Regression tests (prefer snapshot-based tests around
  `chatwidget::tests::run_script`) validating that streaming/exec flows produce
  equivalent UI output after the migration.
- Notes added to `events_audit.md` marking the executive/assistant entries as
  completed or updated with any residual technical debt.

## Status (2025-09-27)
- Infrastructure: `HistoryDomainEvent` enums + hydration helpers now exist and
  wave 1 (plain/loading/wait/background) paths are migrated.
- Exec/streaming handlers still mutate `history_cells` and downcast cells in
  place; caches must remain until the Step 6 renderer cache lands.
- No automated coverage yet exercises the domain events for these flows.
- ✅ Exec streaming deltas now emit `HistoryDomainEvent::UpdateExecStream`, and
  the resulting `HistoryRecord::Exec` is used to hydrate the running `ExecCell`.

## Next Steps for Agent
- Sketch domain-event variants for exec lifecycle and assistant streaming while
  preserving the interim caches (do not remove them until the shared renderer
  cache is live).
- Convert one representative exec path (e.g., command begin → output chunk →
  completion) and one assistant stream path to use the new domain events.
- Add focused regression tests via `run_script` covering the converted paths.
- Document remaining mutation spots in `events_audit.md` and update this file
  with progress notes or follow-up TODOs.

## References
- `code-rs/tui/HISTORY_CELLS_PLAN.md` – Step 2 bridge & Step 3 Phase C wave 2.
- `code-rs/tui/src/chatwidget/events_audit.md` – exec/stream mutation list.
