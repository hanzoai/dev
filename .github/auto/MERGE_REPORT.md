# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

## Incorporated
- Adopt upstream changes broadly outside protected areas.
- Prefer-theirs buckets merged cleanly:
  - codex-rs/common/**
  - codex-rs/exec/**
  - codex-rs/file-search/**
- Adopted upstream `codex-rs/core/src/git_info.rs` updates (no conflicts with UA/version helpers).

## Dropped / Kept Ours
- TUI conflicts in these files resolved to ours (policy protect):
  - codex-rs/tui/src/app_event.rs
  - codex-rs/tui/src/bottom_pane/bottom_pane_view.rs
  - codex-rs/tui/src/bottom_pane/list_selection_view.rs
  - codex-rs/tui/src/bottom_pane/mod.rs
  - codex-rs/tui/src/bottom_pane/selection_popup_common.rs
  - codex-rs/tui/src/chatwidget.rs
  - codex-rs/tui/src/chatwidget/tests.rs
  - codex-rs/tui/src/slash_command.rs
- Rationale: preserve strict streaming ordering, browser/agent tool UX, and bottom-pane behavior.

## Invariants Verified
- Tool handlers present and exposed via openai_tools: browser_* / agent_* / web_fetch.
- Browser gating logic preserved.
- Screenshot queuing semantics unchanged.
- UA/version helpers intact: `codex_version::version()`, `get_codex_user_agent_default()`.
- Public re-exports in codex-core retained: ModelClient, Prompt, ResponseEvent, ResponseStream.
- `codex_core::models` remains alias to protocol models.

## Purged
- No `.github/codex-cli-*` assets present after merge; nothing to purge.

## Notes
- Follow-up: run `scripts/upstream-merge/verify.sh` and address any warnings if surfaced. Build should pass via `./build-fast.sh` (policy: zero warnings).
