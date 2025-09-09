Upstream merge summary (by-bucket)

Incorporated
- Non‑conflicting changes across the repo outside protected areas.
- Upstream dependency bumps reflected in lockfile resolution during build (no API impact).
- Retained our existing `.github/auto` artifacts and merge policy docs.

Dropped / Deferred
- codex-rs/core, codex-rs/protocol, codex-rs/exec, codex-rs/mcp-server: kept our forked implementations. Rationale: upstream introduced protocol/core API changes (ConversationId, Token events, SandboxPolicy variants, etc.) that are not compatible with our TUI, login server, and exec layers. Adopting them caused widespread breakage. Per policy, we prefer upstream here unless it breaks our build or documented behavior; we therefore retained our implementations for this cycle and will evaluate upstream deltas separately.
- codex-rs/tui: preserved our custom TUI with strict streaming ordering and UX. Upstream TUI changes (composer shortcuts, highlighting tweaks) were not adopted to avoid UX regressions.
- Workflow changes in `.github/workflows`: we kept our versions per protected globs.

Policy applications and resolutions
- Prefer‑ours globs honored for `codex-rs/tui/**`, `codex-cli/**`, `.github/workflows/**`, and `docs/**`.
- Prefer‑theirs globs evaluated for core/protocol/exec/file‑search, but reverted to ours where upstream changes broke our build. All remaining conflicts resolved in favor of our code to restore a clean build.
- Purged disallowed assets: ensured `.github/codex-cli-*.{png,jpg,jpeg,webp}` absent.
- Prevented reintroduction of locally deleted files: dropped upstream’s `codex-rs/tui/src/resume_picker.rs` (we deleted it previously).

Build status
- ./build-fast.sh: SUCCESS (no compiler warnings). Note: lockfile updated during unlocked build; no API changes.

Follow‑ups proposed
- Evaluate upstream protocol/core deltas (ConversationId, token usage structs, serde_with Base64 encoding, etc.) behind a compatibility layer, then migrate incrementally.
- Revisit TUI upstream deltas (number formatting in history, shortcut handling) and cherry‑pick compatible improvements.
