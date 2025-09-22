Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket (policy-driven)

Summary
- Merged upstream/main into upstream-merge with selective reconciliation.
- Preserved fork-only UX and tool integrations; adopted upstream in shared crates.
- Verified with scripts/upstream-merge/verify.sh (build_fast=ok, api_check=ok, guards=ok).

Incorporated
- codex-rs/common/**, codex-rs/exec/**, codex-rs/file-search/**: took upstream changes per prefer-theirs.
- New TUI snapshot: codex-rs/tui/src/snapshots/codex_tui__pager_overlay__tests__transcript_overlay_apply_patch_scroll_vt100.snap.
- Minor import additions in pager overlay (Text, Clear) from upstream to fix build and match ratatui usage.

Preserved (ours)
- TUI layer and strict streaming ordering invariants under codex-rs/tui/**.
- Core wiring and custom tools: browser_* and agent_* families, plus web_fetch.
- Exposure gating for browser tools (core-side logic retained).
- Screenshot queue semantics and TUI rendering of browser/tool outputs.
- Version/UA semantics: continued use of codex_version::version() and get_codex_user_agent_default().
- Public re-exports in codex-core (ModelClient, Prompt, ResponseEvent, ResponseStream) and models alias.

Dropped / Not Adopted
- No upstream branding or workflow changes that conflict with fork policy were adopted.
- No reintroduction of purged branding assets (*.png/jpg/jpeg/webp under .github/).

Noteworthy Resolutions
- Conflict: codex-rs/tui/src/pager_overlay.rs
  • Strategy: prefer-ours; incorporated upstream’s `use ratatui::text::Text` and `use ratatui::widgets::Clear` which are compatible and required for our current implementation using `Clear`.

Validation
- ./build-fast.sh: success with zero warnings blocking (policy-compliant).
- scripts/upstream-merge/verify.sh: build_fast=ok, api_check=ok, guards=ok; branding=notice/ok (no conflicting strings).

Follow-ups
- None required. CI release monitoring can follow standard workflow if a PR is opened from this branch.
