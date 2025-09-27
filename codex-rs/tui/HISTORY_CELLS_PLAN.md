# History Cell Refactor Plan

## Background

We're seeing a lot of issues with the conversation history in the TUI. It's slow to scroll (not easily cached), has inconsistent ordering, does not reconstruct correctly when using resume or undo. We also want to be able to import conversations from sub-agents, so need a portable format. We're also seeing some really large files (such as chatwidget.rs) which we want to reduce to improve maintainability.

To do that we're working on refactoring the history cells so that state is separated from rendering. Each cell should be able to be rendered from a state object along with the system settings. This state should be React-style so determine how the cell works, but the actual rendering logic should be entirely in the cell. The state should be easily serializable in JSON and will be built entirely from events in the new HistoryState. So, for example, the state should not have presentation logic like color etc.. that would be handled by the cell during rendering.


## Primary Goals (Plan & Status)
- [x] **Extract per-cell modules** – core message/tool/plan/upgrade/reasoning/image/loading/animated/assistant/stream files live under `history_cell/`; exec remains inline and pending extraction.
- [ ] **Finish semantic state refactor** – continue replacing ad-hoc `SemanticLine` usage with rich typed structs (`MessageSegment`, `ToolArgument`, etc.) so renderers never infer structure from strings.
- [~] **Introduce single `HistoryState` vector** – foundational types (`HistoryRecord`, `HistoryState`, `HistoryId`) now live in `history/state.rs`; still need to adopt them in `chatwidget.rs` and event handling.
- [ ] **Centralize event → state mapping** – funnel all `handle_*` mutations through `HistoryState::apply_event` and a dedicated render adapter.
- [ ] **Unlock serialization & perf goals** – once the state vector exists, wire serialization, resume snapshots, undo rewind, and cached layout per `(record_id, width)`.
- [ ] **Document interim cache bridge** – keep legacy per-cell layout caches (exec/assistant/diff) in place until the Step 6 renderer cache is implemented, and track their removal behind the new caching system.
- [ ] **Land HistoryDomainEvent layer** – introduce a dedicated domain-event enum + helpers so every TUI mutation flows through `HistoryState::apply_event` without reconstructing records from cells.

## Shared Rendering Inputs
- History viewport width (drives wrapping and layout caches).
- Theme palette (applied when converting stored styles to runtime `Style`).
- Reasoning visibility toggle (`Ctrl+R`, affects collapsed vs expanded reasoning blocks).
- Animation timers for ephemeral UI elements (kept outside the serialized state vector).

## Cell Inventory
Status legend: ✅ complete (semantic deterministic state ready), ⏳ still needs semantic refactor.

### ✅ Plain Messages – `PlainHistoryCell`
- **Desired state:**
  ```rust
  struct PlainMessageState {
      role: HistoryCellRole,
      body: Vec<MessageSegment>, // text segments tagged with semantics (code, bullet, emphasis, etc.)
      metadata: Option<MessageMetadata>,
  }
  ```
- **Current status:** ✅ state now stored as `PlainMessageState` (header + `Vec<MessageLine>`); follow-up: richer `MessageSegment` types for code/bullets plus metadata serialization.

### ✅ Wait Status – `WaitStatusCell`
- **Desired state:** `WaitStatusState { header: WaitHeader, bullet_points: Vec<WaitDetail> }`
- **Status:** ✅ migrated to `WaitStatusState` with explicit header/detail tones; renderer rebuilds lines from structured data.

### ✅ Loading Spinner – `LoadingCell`
- **State:** `LoadingCellState { message: String }`
- **External settings:** theme.

### ✅ Tool Calls – `ToolCallCell`
- **Desired state:**
  ```rust
  struct ToolCallState {
      status: ToolCallStatus,
      title: ToolTitle,
      arguments: Vec<ToolArgument>,
      result_preview: Option<ToolResultPreview>,
  }
  ```
- **Status:** ✅ constructors now populate `ToolArgument`/`ToolResultPreview`; follow-up: polish preview truncation + metadata serialization.
- **External settings:** theme, width.

### ✅ Running Tool Calls – `RunningToolCallCell`
- **Desired state:** same struct but `arguments: Vec<ToolArgument>` instead of rendered lines.
- **Status:** ✅ running tool state stores `Vec<ToolArgument>` with wait caps/call ids tracked separately.
- **External settings:** theme, width, current time (for elapsed label).

### ✅ Plan Updates – `PlanUpdateCell`
- **Desired state:** `PlanUpdateState { icon: PlanIcon, items: Vec<PlanLine>, completion: PlanCompletion }`
- **Status:** ✅ stores `PlanUpdateState` with `PlanProgress`/`PlanStep`; follow-up: richer icons + metadata serialization.
- **External settings:** theme, width.

### ✅ Upgrade Notices – `UpgradeNoticeCell`
- **Desired state:** `UpgradeNoticeState { current_version: String, latest_version: String, message: UpgradeMessage }`
- **Status:** ✅ upgrade notices now keep `{ current_version, latest_version, message }`; follow-up: wire optional CTA metadata.
- **External settings:** theme, width.

### ✅ Background Events – `BackgroundEventCell`
- **Status:** ✅ already semantic (metadata only).
- **External settings:** theme; renderer currently emits metadata text.

### ✅ Animated Welcome – `AnimatedWelcomeCell`
- **Persistence:** not stored in the state vector (UI-only animation).
- **Runtime state:** start time, fade progress, cached height.

### ✅ Reasoning – `CollapsibleReasoningCell`
- **Desired state:** `ReasoningState { id, sections: Vec<ReasoningSection>, in_progress, hide_when_collapsed }` with sections broken into semantic blocks.
- **Status:** ✅ state maintains `ReasoningSection` blocks with persisted summaries and typed bullet markers (2025-09-26 follow-up complete).
- **External settings:** `Ctrl+R` (collapsed vs expanded), theme, width.
- **Widget state:** collapse flag held outside the serialized state (driven by the global toggle).

### ✅ Explore Aggregations – `ExploreAggregationCell`
- **Status:** ✅ already semantic (summary enums + statuses).
- **External settings:** theme, width.

### ⏳ Image Outputs – `ImageOutputCell`
- **Status:** ⏳ verify each uses structured data; replace any remaining rendered-line storage with semantic fields.

### ⏳ Patch Summaries – `PatchSummaryCell`
- **Status:** ⏳ cells build summaries directly from diff hunks and cache per-width render output without touching `HistoryState::Patch`.
- **Current:** `new_patch_event` constructs `PatchSummaryCell` instances ad hoc, so patch events bypass the serialized state vector.
- **Needed:** emit `PatchRecord` entries in `HistoryState`, render from the recorded metadata, and retire the cached per-width line buffer.

### ⏳ Exec Commands – `ExecCell`
- **Status:** [~] `ExecRecord` lives in `HistoryState`, but rendering still depends on the legacy `ExecCell` caches. Streaming stdout/stderr updates now emit
  `HistoryDomainEvent::UpdateExecStream` so `HistoryState` remains the source of
  truth for in-flight commands.
- **Current:** command metadata, layout caches, and wait state remain coupled to the UI struct instead of the serialized record.
- **Decision:** keep those caches energized until the Step 6 renderer cache is available; otherwise exec redraws regressed by >2× during testing.
- **Needed:** read/write `ExecRecord` via a dedicated `history_cell/exec.rs`, and move per-width layout caches into the renderer layer once the shared cache exists.
- **External settings:** theme, width, monotonic time for “running” durations.

### ⏳ Assistant Streaming – `StreamingContentCell`
- **Status:** [~] streaming records capture IDs, markdown deltas, citations, and metadata in `AssistantStreamState`; the cell still owns a redundant `Vec<Line>` cache.
- **Current:** renderer ignores the stored deltas and instead maintains wrapped lines per width.
- **Decision:** hold this cache until the centralized renderer cache exists so streaming updates stay smooth.
- **Needed:** rebuild previews from `AssistantStreamState`, drop the duplicated line cache, and finish piping citations/token usage through serialization.

### ⏳ Assistant Answers – `AssistantMarkdownCell`
- **Status:** [~] finalized messages persist markdown/citations/token usage via `AssistantMessageState`, while the cell caches a wrapped copy for legacy rendering.
- **Current:** raw markdown and per-width layout live in the UI layer, so `HistoryState` snapshots cannot rehydrate the answer alone.
- **Decision:** keep the wrapped-line cache until Step 6 introduces the shared renderer cache to avoid flicker on theme/width changes.
- **Needed:** render directly from `AssistantMessageState`, move layout caching to the renderer, and delete the redundant `lines` buffer.

### ⏳ Merged Exec Summary – `MergedExecCell`
- **Status:** ⏳ still reuses rendered `ExecCell` line pairs instead of the structured `ExecRecord` output chunks.
- **Current:** keeps preformatted preamble/output text, preventing reuse of serialized exec data.
- **Decision:** rely on the existing `ExecCell` cache bridge until the new renderer cache is ready so merged exec performance stays stable.
- **Needed:** rebuild merged summaries from `Vec<ExecRecord>` snapshots once the exec module is refactored.

### ⏳ Diffs – `DiffCell`
- **Status:** [~] `DiffRecord` already stores hunks and `DiffLineKind`; the cell precomputes styled lines for legacy rendering.
- **Current:** cached `Vec<Line<'static>>` duplicates the structured data and bypasses `HistoryState` for updates.
- **Decision:** retain the RefCell layout cache until the shared renderer cache is available; otherwise large diffs stutter on scroll.
- **Needed:** route diff events through `HistoryState`, render directly from `DiffRecord`, then remove the cached line buffer.

### ⏳ Explore Fetch / HTTP – `ExploreRecord`
- **Status:** ⏳ store URL, status, body.
- **Current:** cached display lines.
- **Needed:** serialize fetched URL, status, and body; render into lines at runtime.

### ⏳ Legacy Plain Producers
- **Status:** ⏳ ensure producers emit semantic data before handing to cells.
- **Current:** some paths still inject raw `Vec<Line<'static>>` via `PlainHistoryCell` constructors.
- **Needed:** convert producers to build `RichTextLine` or structured records before handing off.

## Step 1 – Finish Semantic State Refactors *(In Progress)*
1.1 **Plain messages** – ✅ replaced `SemanticLine` caches with `PlainMessageState` (`plain.rs` now stores `MessageHeader + MessageLine` via shared conversion helpers). Follow-up: enrich headers with structured badges and surface metadata once available.
1.2 **Tool calls (running & completed)** – ✅ constructors now emit `ToolArgument`/`ToolResultPreview`; remaining work: tighten JSON summaries + result truncation heuristics.
1.3 **Plan updates** – ✅ `PlanUpdateCell` now renders from `PlanProgress`/`PlanStep` + `PlanIcon`; follow-up: improved summary metadata.
1.4 **Upgrade notice** – ✅ cell consumes `UpgradeNoticeState` (versions + message), custom render derived at draw time.
1.5 **Reasoning** – ✅ sections/blocks stored; block metadata now includes typed bullets and per-section summaries for collapse previews (2025-09-26).
1.6 **Wait status & background notices** – ✅ wait tools use `WaitStatusState`; background notices render via `BackgroundEventRecord`.
1.7 **Documentation** – ✅ inline docs now cover reasoning summaries/bullets; audit of constructors confirmed strongly typed state (2025-09-26).

## Step 2 – Exec / Streaming / Diff Bridge *(In Progress)*
2.1 **Exec state extraction & module split** – [~] `ExecCell` now lives in `history_cell/exec.rs` and consumes `ExecRecord`, but we are deliberately keeping the legacy per-width layout caches and wait-state buffers until the new renderer cache (Step 6) lands to avoid regressions.
2.2 **Streaming assistant module** – [~] `AssistantStreamState` drives the streaming cell and carries token/citation metadata; `StreamingContentCell` still wraps `AssistantMarkdownCell`, so the internal markdown+layout caches remain as an intentional bridge until the shared renderer cache is ready.
2.3 **Finalized assistant markdown module** – [~] `AssistantMessageState` is the source of truth, but `AssistantMarkdownCell` continues to own per-width layout caches for performance; plan to migrate them once Step 6 introduces the centralized cache.
2.4 **Diff module breakout** – [~] diff cells read `DiffRecord` hunks, yet the `DiffCell` keeps a per-width layout cache; removal is blocked on the new caching layer.
2.5 **Merged exec views** – [~] merged exec cells construct segments from `ExecRecord`, but each segment spins up an `ExecCell` to reuse the old caches; this is acceptable until the shared renderer cache replaces both layers.

## Step 3 – HistoryState Manager *(Pending)*
3.1 **HistoryRecord enum** – ✅ complete: `history/state.rs` defines `HistoryRecord`, per-cell state structs, and `HistoryState` scaffolding with ID management helpers.
3.2 **ChatWidget incremental adoption** – Introduce `HistoryState`/`HistoryRenderState` alongside the legacy `history_cells` vector, migrate low-risk cell types (plain, loading, wait status) to the new state, then remove the legacy vector once the path is stable. **Status:** [~] `history_insert_with_key_global_tagged` now mirrors inserts into `HistoryState` to assign `HistoryId`s, but the `history_cells: Vec<Box<dyn HistoryCell>>` is still the primary source of truth and several helpers reconstruct `HistoryRecord` values from existing cells. Next steps: migrate the remaining rate-limit/system notice paths, then flip the ownership so `HistoryState` drives rendering and cells become a derived cache only.
3.3 **Apply-event pipeline** – Implement `HistoryState::apply_event(&mut self, event: &EventMsg)` covering all core/TUI event types (exec lifecycle, tool updates, background notices, resume snapshots, undo) and route migrated cells through it. **Status:** [~] `HistoryState::apply_domain_event` now converts `HistoryDomainRecord` values into `HistoryRecord`s so background/system notices insert via domain events; remaining flows still emit bespoke mutations (e.g., patch success). Follow-up: add typed handlers per event family, eliminate `history_record_from_cell`, and collapse direct cell mutation once Step 3.5 is ready.
3.4 **Undo/resume hooks** – Expose `snapshot`, `restore`, and `truncate_after(id)` to support /undo and resume flows.
3.5 **ChatWidget full integration** – Delete `history_cells: Vec<Box<dyn HistoryCell>>`, wire all helper methods (`history_push`, `history_replace`, etc.) into `HistoryState`, and treat Step 4 as the follow-up for centralized rendering.

### Outstanding Gaps (2025-09-26)
- Patch apply success/failure currently mutates `PatchSummaryCell` in place; emit `HistoryEvent::Replace` once the replace arm is implemented so records stay authoritative.
- Several helpers still rebuild cells inline (e.g., patch success) instead of emitting domain-specific events; consolidate around `HistoryState::apply_event` so the legacy `history_cells` vector can be retired.
- Track exec/assistant/diff layout caches as an intentional bridge; remove them only after the shared renderer cache in Step 6 ships to avoid frame-time regressions.

### Event Pipeline Consolidation Plan
- **Phase A – Inventory:** enumerate every direct mutation (`history_cells[idx] = ...`, `history_cells.remove`, `history_record_from_cell`) and map them to a target `HistoryEvent` variant. Track results in `chatwidget/events_audit.md` (new doc) so progress is visible.
- **Phase B – API surface:** extend `HistoryState::apply_event` to accept domain enums (`HistoryDomainEvent`) which wrap core `EventMsg` payloads; expose helpers like `apply_exec_event` to keep match arms local to each module.
- **Phase C – Migration waves:**
  1. Plain/background/loading/wait/tool/plan/upgrade (already idempotent) – replace inline constructors with `HistoryDomainEvent::InsertPlain` etc.
  2. Exec/stream/assistant – route lifecycle through domain events so streaming merges/final answers run purely on `HistoryState` data.
  3. Diff/patch/image/explore/rate-limit – remove bespoke mutation helpers and rely on event emissions from their source modules.
- **Phase D – Flip ownership:** once all mutations go through domain events, replace `history_cells` with a derived cache built from `HistoryRecord` snapshots + the renderer cache. Provide a debug flag to assert no code path mutates `history_cells` directly.

## Step 4 – Event Mapping & Rendering *(Pending)*
4.1 **Centralize handlers** – Route every mutation in `handle_*` (exec events, tool deltas, diff updates, background notices) through `HistoryState` so ordering/id management lives in one place.
4.2 **Stable IDs for streaming** – Store `HistoryId` + domain IDs (exec id, tool call id, stream id) to ensure in-place updates and dedupe.
4.3 **HistoryRenderState** – Provide adapter that consumes `HistoryRecord` + current settings (theme, width, reasoning collapsed) and produces cached `RenderedCell` structures. Cache keyed by `(HistoryId, width, theme_epoch)`.
4.4 **Renderer migration** – Update TUI drawing code to iterate `HistoryRenderState::visible_cells()`; drop direct use of `HistoryCell` trait.

## Step 5 – Serialization & Persistence *(Blocked on Step 3)*
- Define canonical JSON schema for each record variant (enum tags, tone names instead of colors).
- Add round-trip tests for snapshot/resume.
- Persist history vector in session logs; implement `/undo` by restoring prior snapshot.

## Step 6 – Performance Improvements *(Blocked on Step 4)*
- Ship the shared renderer cache (memoized per-cell layouts keyed by `(HistoryId, width, theme_epoch)`) so the interim exec/assistant/diff caches can be removed.
- Add instrumentation to measure render latency and scrolling cost pre/post refactor.
- Benchmark resume/undo operations using new snapshots.

### Renderer Cache Design Sketch
- **Responsibility:** centralize layout caching in `HistoryRenderState` so individual cells own only semantic state. Cache entries map `(HistoryId, width, theme_epoch, reason_visibility)` → `RenderedCell` (lines + per-row buffers + meta).
- **Structure:**
  - `RenderedCell` wraps immutable `Vec<Line<'static>>` plus precomposed `Vec<Box<[BufferCell]>>` for fast draw.
  - `HistoryRenderState` keeps an `IndexMap<CacheKey, Arc<RenderedCell>>` with LRU eviction (target: 512 entries, tunable via config).
  - `CacheKey` includes `theme_epoch` (incremented on palette change) and any feature flags (e.g., reasoning collapsed) so stale entries drop naturally.
- **Invalidation hooks:**
  - `HistoryState::apply_event` emits `HistoryRenderInvalidate` messages when the underlying record changes.
  - Width changes call `HistoryRenderState::handle_width_change(new_width)` to flush mismatched entries.
  - Theme swaps bump `theme_epoch`; toggling reasoning detail flips a `ReasoningVisibility` enum baked into the key.
- **Warm path:** when rendering, look up `(id, width, theme_epoch, visibility)`. If present, reuse. If missing, invoke cell-specific `render_to_lines(record, settings)` helpers to produce lines, then store.
- **Cold path optimizations:** reuse arena-allocated span buffers to reduce allocations; consider hashing markdown bodies to share across identical answers when history rewinds.
- **Telemetry:** wrap cache hits/misses with counters exported via `/perf` to confirm the shared cache covers ≥90 % of paint operations before removing per-cell caches.
