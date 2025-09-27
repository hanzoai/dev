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

## References
- `codex-rs/tui/HISTORY_CELLS_PLAN.md` – Step 2 bridge & Step 3 Phase C wave 2.
- `codex-rs/tui/src/chatwidget/events_audit.md` – exec/stream mutation list.
