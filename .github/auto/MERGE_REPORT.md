# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

## Summary
- Per policy, kept our TUI and core tool wiring while adopting safe upstream improvements.
- Resolved conflicts only in protected TUI files by preferring ours.
- Adopted upstream rate limit schema changes (primary/secondary) across protocol/core/exec/tui.
- Verified fork invariants (tool exposure parity, UA/version helpers) via verify.sh.
- Build validated with ./build-fast.sh (no warnings).

## Incorporated
- Protocol: RateLimitSnapshotEvent field renames
  - primary/secondary used percent and windows
  - ratio renamed to primary_to_secondary_ratio_percent
- Core: map ResponseEvent::RateLimits to updated struct; kept UA/version helpers.
- Exec: human output now shows "hourly • secondary".
- TUI: rate-limit refresh, views, warnings, and reset detection updated to new fields; section renamed to "Secondary Limit".
- Upstream non-conflicting updates in core/client.rs, tests, protocol synced by merge.

## Dropped / Kept Ours
- TUI conflicted files (codex-rs/tui/src/chatwidget.rs, codex-rs/tui/src/history_cell.rs): kept our versions to preserve strict streaming order and UX.
- No reintroduced purged assets (.github/codex-cli-*.png/jpg/jpeg/webp).
- Workflows and docs kept ours per policy.

## Other Notes
- No ICU/sys-locale removals; dependencies remain used.
- Public re-exports in codex-core unchanged (ModelClient, Prompt, ResponseEvent, ResponseStream) and still compile.
- verify.sh summary: build_fast=ok, api_check=ok.

## Suggested PR Title/Body
Title: "Merge upstream/main (by-bucket): protocol rate-limit updates; preserve TUI ordering"

Body:
- Merge upstream/main into upstream-merge following by-bucket policy.
- Adopt upstream RateLimitSnapshotEvent primary/secondary fields across protocol/core/exec/tui.
- Preserve fork-specific TUI behavior (strict ordering, streaming deltas) by keeping our chatwidget/history_cell.
- Invariants kept: browser_/agent_/web_fetch tool exposure parity and UA/version helpers.
- Verified with scripts/upstream-merge/verify.sh and ./build-fast.sh (no warnings).
