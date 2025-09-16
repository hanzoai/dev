# Upstream Merge Report

- Source: openai/codex@main (5e2c4f7e3)
- Target branch: upstream-merge
- Mode: by-bucket

## Incorporated
- docs/config.md: Adopt upstream update to Azure model provider example (clarifies configuration). No conflicts.

## Dropped / Purged
- No new files matched purge globs. Confirmed no reintroduction of `.github/codex-cli-*` images.

## Preserved (fork invariants)
- Tool families and gating: browser_*, agent_*, and web_fetch schemas present and parity with handlers verified by guards.
- Screenshot UX semantics unchanged.
- Version/UA: `codex_version::version()` usage intact in default_client.
- Public API re-exports preserved (ModelClient, Prompt, ResponseEvent, ResponseStream) and models alias.

## Verify Summary
- scripts/upstream-merge/verify.sh: PASSED
  - build-fast.sh: ok
  - codex-core api_surface tests compile: ok
  - static guards (tools + UA/version): ok

## Notes
- No TUI/CLI user-visible branding changes included in this bucket.
- No conflicts encountered; merge committed as a simple upstream sync.
