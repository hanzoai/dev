# Upstream Merge Report

Source: openai/codex@main → branch `upstream-merge`
Mode: by-bucket
Date: $(date -u +%Y-%m-%dT%H:%M:%SZ)

## Incorporated
- Exec session lifecycle: adopted upstream addition of `exit_status: Arc<AtomicBool>` in `ExecCommandSession` and constructor. Updated our `session_manager.rs` to track and set exit flag and pass it to the session.
- Common/exec improvements under prefer_theirs_globs (no manual conflicts). Build verified.
- Upstream file rename: `core/swiftfox_prompt.md` → `core/gpt_5_codex_prompt.md` retained as per merge.

## Dropped or Kept Ours
- TUI conflicts (`tui/src/lib.rs`, `tui/src/new_model_popup.rs`): kept our fork’s implementations and branding (strict streaming ordering, composer/title status, onboarding flow differences). Resolved conflicts by taking ours and ensuring the upgrade popup text matches our "Code" branding while retaining the upstream model display constant.
- Workflows/docs in prefer_ours_globs: kept our versions to preserve fork-specific CI and policy.
- Purge guard: no reintroduced `.github/codex-cli-*.{png,jpg,jpeg,webp}` detected in tree after merge.

## Invariants Verified
- Tool families present and registered: `browser_*`, `agent_*`, `web_fetch` remain wired and gated by policy.
- Version/UA usage preserved (`codex_version::version()`, `get_codex_user_agent_default()`).
- Public re-exports in codex-core (`ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`) untouched; `codex_core::models` alias intact.
- ICU/sys-locale: no removals performed; usage unchanged.

## Verification
- scripts/upstream-merge/verify.sh: PASS
  - build-fast.sh: PASS (0 warnings)
  - cargo check codex-core tests: PASS (0 warnings)
  - static guards: PASS
  - branding guard: PASS

## Notes / Follow-ups
- If upstream later relies on `ExecCommandSession::has_exited()`, we already exposed a read in `handle_write_stdin_request` to keep field/method live. Behavior unchanged.
- No reintroduced paths from REINTRODUCED_PATHS.txt required action beyond those above.
