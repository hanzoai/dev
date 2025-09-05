## Strict Ordering In The TUI History

The TUI enforces strict, per‑turn ordering for all streamed content. Every
stream insert (Answer or Reasoning) must be associated with a stable
`(request_ordinal, output_index, sequence_number)` key provided by the model.

- A stream insert MUST carry a non‑empty stream id. The UI seeds an order key
  for `(kind, id)` from the event's `OrderMeta` before any insert.
- The TUI WILL NOT insert streaming content without a stream id. Any attempt to
  insert without an id is dropped with an error log to make the issue visible
  during development.

Developer guidance:

- Always provide a stream id on `AgentReasoning*` and `AgentMessage*` events
  (both deltas and finals) and ensure `OrderMeta` is present.
- If you introduce a new streaming path or a controller flush, propagate the
  current stream id through to the `InsertHistoryWithKind` sink. Do not call the
  no‑id begin path in the controller for Answer/Reasoning inserts.
- If an insert path triggers the log
  "strict ordering: missing stream id …; dropping …", treat it as a bug in the
  emitter and fix the source to supply the id before merging.

