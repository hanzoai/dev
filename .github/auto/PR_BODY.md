Title: merge(upstream): sync fork with openai/codex main (by-bucket)

Summary
- Merge upstream main into our fork’s `upstream-merge` using by-bucket policy.
- Keep our TUI and fork-only core glue; adopt upstream improvements elsewhere.

Key Decisions
- TUI: kept ours for `codex-rs/tui/src/chatwidget.rs` to preserve strict ordering and HistoryCell/RunningCommand semantics.
- Core invariants preserved: browser_*/agent_* tools and web_fetch exposure gating; screenshot queue semantics; UA/version helpers.
- Prefer theirs for shared crates (common/exec/file-search) where applicable.
- No purge_globs assets reintroduced.

Verification
- scripts/upstream-merge/verify.sh: PASS
- ./build-fast.sh: PASS (no warnings)

Notes
- Public re-exports (ModelClient, Prompt, ResponseEvent, ResponseStream) retained.
- codex_core::models alias preserved.

