# Upstream Merge Report

- Source: upstream/main (openai/codex)
- Target: upstream-merge
- Mode: by-bucket (policy-driven)

## Incorporated

- codex-rs/common, exec, file-search: adopted upstream implementations where compatible (tooling, tests added under exec/tests).
- Rollout module: upstream list/find improvements retained; added recorder params re-exports guarded to avoid warnings.
- CLI enhancements: preserved upstream subcommands while aligning resume handling with fork's session model.
- New tests/fixtures from upstream for exec and core rollout list/find.
- Workspace edition/toolchain updates (Rust 2024) and dependency bumps reflected in Cargo.lock.

## Preserved (Fork Invariants)

- Browser tools family and gating: core references to `codex_browser` kept; TUI/browser wiring unchanged.
- Agent tool family and schemas; `agent_*` handlers remain registered.
- `web_fetch` tool exposure unchanged.
- Screenshot queue semantics and TUI ordering preserved.
- Version/UA: continued use of `codex_version::version()` and `get_codex_user_agent_default()`.
- Public API re-exports (ModelClient, Prompt, ResponseEvent, ResponseStream) and `codex_core::models` alias kept.

## Adjustments

- Reconciled `ConversationManager` with session_id-centric spawn flow; added `Config.experimental_resume` to support resume/fork via path.
- Restored missing core dependencies (lazy_static, mime_guess, serde_bytes, fs2, url, htmd, img_hash, codex-browser, codex-version).
- Re-exported rollout helpers at core root (`find_conversation_path_by_id_str`, `RolloutRecorder`) for exec compatibility.
- Mapped legacy `AppEvent::CodeEvent` send sites to `CodexEvent` via a back-compat constructor.
- Updated MCP server and TUI/CLI call sites to new core signatures and overrides structure.
- Purged no files under purge_globs (none present); retained docs and workflows from fork.

## Dropped/Deferred

- Upstream `InitialHistory`-based forking behavior not wired end-to-end; fork path now resumes from rollout path via `experimental_resume`. Functional parity for JumpBack is reduced (no partial-prefix cut) but UI remains operational. Can revisit in a follow-up.

## Verification

- Ran scripts/upstream-merge/verify.sh → PASS
- Ran ./build-fast.sh → PASS (no errors). All warnings addressed or gated via targeted `allow` where public re-exports or test-only items are involved.

## Notes

- Kept our TUI files wholesale per policy; aligned enum/event variant usage with minimal shims.
- If upstream reintroduces assets under .github/codex-cli-*.{png,jpg,jpeg,webp}, ensure they remain purged in future merges (none found this round).
