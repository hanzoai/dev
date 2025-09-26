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
- **Status:** [~] `ExecRecord` lives in `HistoryState`, but rendering still depends on the legacy `ExecCell` caches.
- **Current:** command metadata, layout caches, and wait state remain coupled to the UI struct instead of the serialized record.
- **Needed:** read/write `ExecRecord` via a dedicated `history_cell/exec.rs`, and move per-width layout caches into the renderer layer.
- **External settings:** theme, width, monotonic time for “running” durations.

### ⏳ Assistant Streaming – `StreamingContentCell`
- **Status:** [~] streaming records capture IDs, markdown deltas, citations, and metadata in `AssistantStreamState`; the cell still owns a redundant `Vec<Line>` cache.
- **Current:** renderer ignores the stored deltas and instead maintains wrapped lines per width.
- **Needed:** rebuild previews from `AssistantStreamState`, drop the duplicated line cache, and finish piping citations/token usage through serialization.

### ⏳ Assistant Answers – `AssistantMarkdownCell`
- **Status:** [~] finalized messages persist markdown/citations/token usage via `AssistantMessageState`, while the cell caches a wrapped copy for legacy rendering.
- **Current:** raw markdown and per-width layout live in the UI layer, so `HistoryState` snapshots cannot rehydrate the answer alone.
- **Needed:** render directly from `AssistantMessageState`, move layout caching to the renderer, and delete the redundant `lines` buffer.

### ⏳ Merged Exec Summary – `MergedExecCell`
- **Status:** ⏳ still reuses rendered `ExecCell` line pairs instead of the structured `ExecRecord` output chunks.
- **Current:** keeps preformatted preamble/output text, preventing reuse of serialized exec data.
- **Needed:** rebuild merged summaries from `Vec<ExecRecord>` snapshots once the exec module is refactored.

### ⏳ Diffs – `DiffCell`
- **Status:** [~] `DiffRecord` already stores hunks and `DiffLineKind`; the cell precomputes styled lines for legacy rendering.
- **Current:** cached `Vec<Line<'static>>` duplicates the structured data and bypasses `HistoryState` for updates.
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

## Step 2 – Exec / Streaming / Diff Family *(Pending)*
2.1 **Exec state extraction & module split** – ✅ `ExecCell` now lives in `history_cell/exec.rs`, builds from `ExecRecord` (including wait notes/stream chunks), and keeps layout caches strictly in the renderer layer (2025-09-26).
2.2 **Streaming assistant module** – Capture raw markdown deltas + metadata in `AssistantStreamState`, relocate the streaming renderer to `history_cell/stream.rs`, and support merge/upsert by stream id. **Status:** ✅ streaming renderer now rebuilds cells directly from `AssistantStreamState`, token usage metadata is upserted alongside deltas, and redundant layout caches were removed (2025-09-26).
2.3 **Finalized assistant markdown module** – Use `AssistantMessageState` storing markdown + citations + token metadata. **Status:** ✅ assistant markdown cells now rebuild directly from `AssistantMessageState`, `InsertFinalAnswer` carries stream metadata (citations/token usage) into history state, and redundant raw caching has been removed in favor of state-driven rebuilds (2025-09-26).
2.4 **Diff module breakout** – ✅ diff cells now rebuild from `DiffRecord` state with per-width layouts, ChatWidget persists diff snapshots in `HistoryState`, and the renderer applies line-kind styling with dedicated marker columns (2025-09-26).
2.5 **Merged exec views** – ✅ merged exec cells now clone `ExecRecord` snapshots for each segment, rebuild layouts on demand (theme-aware caches), and ChatWidget merges/retints using the recorded exec state instead of legacy line buffers (2025-09-26).

## Step 3 – HistoryState Manager *(Pending)*
3.1 **HistoryRecord enum** – ✅ complete: `history/state.rs` defines `HistoryRecord`, per-cell state structs, and `HistoryState` scaffolding with ID management helpers.
3.2 **ChatWidget incremental adoption** – Introduce `HistoryState`/`HistoryRenderState` alongside the legacy `history_cells` vector, migrate low-risk cell types (plain, loading, wait status) to the new state, then remove the legacy vector once the path is stable. **Status:** assistant streaming/final answer flows now write into `HistoryState`; plain/background/wait/loading cells, running/completed tool calls, plan updates, upgrade notices, reasoning summaries, exec cells, diff summaries, patch summaries, and image previews now insert exclusively via `history_insert_with_key_global_tagged` + `HistoryState::apply_event`. Next steps: migrate explore aggregations, rate-limit notices, and remaining niche cells before deleting the legacy vector. **Update (2025-09-26):** migration will now happen only via the new `HistoryState::apply_event` entrypoint so every cell’s lifecycle is driven by events rather than bespoke insert/replace helpers.
3.3 **Apply-event pipeline** – Implement `HistoryState::apply_event(&mut self, event: &EventMsg)` covering all core/TUI event types (exec lifecycle, tool updates, background notices, resume snapshots, undo) and route migrated cells through it. **Status:** Insert events now assign `HistoryId`s for plain/background/loading/wait/tool/plan/upgrade/reasoning/exec/diff/assistant/patch/image cells and keep `history_cell_ids` in sync; replacement/removal paths still mirror the legacy vector and need to be ported. Pending: wire explore/rate-limit mutations through dedicated `HistoryEvent` variants, propagate patch success/failure updates via `HistoryEvent::Replace`, and delete the remaining direct `history_cells` manipulations. **Update (2025-09-26):** build `apply_event` first, then iterate cell-by-cell: for each event family (plain messages, loading indicators, wait status, etc.) add a handler in `apply_event`, update `ChatWidget` to publish only events, and adapt rendering to rebuild from the resulting `HistoryRecord`s. The legacy `history_cells` vector will temporarily mirror `HistoryState` via an ID→cell cache until Step 3.5 removes it entirely.
3.4 **Undo/resume hooks** – Expose `snapshot`, `restore`, and `truncate_after(id)` to support /undo and resume flows.
3.5 **ChatWidget full integration** – Delete `history_cells: Vec<Box<dyn HistoryCell>>`, wire all helper methods (`history_push`, `history_replace`, etc.) into `HistoryState`, and treat Step 4 as the follow-up for centralized rendering.

### Outstanding Gaps (2025-09-26)
- Explore aggregation cells still maintain bespoke `ExploreAggregationState`; convert into `ExploreRecord` instances so inserts/replays serialize cleanly.
- Rate-limit warnings and compact panels render outside the history pipeline; decide whether they should emit `HistoryRecord::RateLimits` snapshots or remain overlay-only.
- Patch apply success/failure currently mutates `PatchSummaryCell` in place; emit `HistoryEvent::Replace` once the replace arm is implemented so records stay authoritative.
- `HistoryState::apply_event` only drives inserts today; removal/replace helpers still operate directly on `history_cells` and must migrate before we can drop the legacy vector.

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
- Memoize per-cell layouts keyed by `(HistoryId, width, theme_epoch)`.
- Add instrumentation to measure render latency and scrolling cost pre/post refactor.
- Benchmark resume/undo operations using new snapshots.
