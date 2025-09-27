# History Renderer Cache Implementation

## Summary
- Implement the shared renderer cache described in Step 6 so per-cell layout
  caches (exec/assistant/diff) can be retired once all migrations are complete.
- Provide APIs for cache lookup, invalidation, and telemetry that integrate with
  the `HistoryDomainEvent` pipeline.

## Prerequisites
- Domain-event migrations for waves 1 & 2 complete (cells reconstruct from
  `HistoryState` without mutating `history_cells`).
- `HistoryRenderState` ready to accept cache invalidation signals.

## Scope
- Introduce `RenderedCell` structs, cache key types, and LRU storage inside
  `history_render.rs` (or a new module) based on the design sketch in
  `HISTORY_CELLS_PLAN.md`.
- Expose invalidation hooks that respond to History mutations (insert/replace/
  remove) and viewport/theme changes.
- Wire ChatWidget rendering paths to consult the shared cache before invoking
  cell-specific rendering logic.
- Define instrumentation (counters or tracing) to measure hit/miss rates.
- Plan removal of legacy per-cell caches once cache hit rates are validated.

## Deliverables
- New caching module with unit tests covering key eviction, width/theme
  invalidation, and reasoning-visibility variants.
- ChatWidget rendering updated to consume cached buffers for all migrated cell
  types.
- Documentation update (HISTORY_CELLS_PLAN.md Step 6) noting completion and
  instructions for removing cell-local caches.

## References
- `codex-rs/tui/HISTORY_CELLS_PLAN.md` – Step 6 renderer cache design sketch.
- `codex-rs/tui/src/chatwidget/history_render.rs` – existing memoization hooks.
