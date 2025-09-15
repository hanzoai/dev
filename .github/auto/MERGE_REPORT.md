Upstream merge report (openai/codex:main → upstream-merge)

Summary

- Mode: by-bucket. Adopt upstream changes in common/exec/core where safe; keep forked TUI and tool wiring intact. Purge rules checked; none reintroduced.
- Build: ./build-fast.sh OK. scripts/upstream-merge/verify.sh OK (API guards + UA/version parity).

Incorporated

- core: model family and client_common consistency updates.
- core/login: auth flow tweaks and e2e test adjustments.
- common: model presets updates.
- mcp-server tests: auth suite updates from upstream.

Dropped / Kept Ours

- tui (multiple files): kept fork UX and streaming/ordering invariants; upstream session header and minor UX changes not adopted to avoid regressions and preserve fork-only behavior.
- mcp-server/codex_message_processor.rs: kept fork behavior (no login endpoints, GetAuthStatus not exposed). Upstream added auth status support was not adopted to keep the simpler server contract. If desired, can revisit in a follow-up behind a config flag.

Other Notes

- No reintroduced assets matched purge globs (.github/codex-cli-*.{png,jpg,jpeg,webp}).
- Public re-exports and codex_core::models alias remain intact.
- Browser/agent/web_fetch tools and exposure gating preserved through unchanged core files.
