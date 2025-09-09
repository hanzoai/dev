# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket
Date: $(date -u +%F)

## Incorporated
- Protocol and deps: adopted upstream dependency set in `codex-rs/protocol/Cargo.toml` (added `icu_decimal`, `icu_locale_core`, `sys-locale`, `serde_with`, and aligned `ts-rs` features). Also added missing `serde_bytes` used in our fork’s protocol types.
- Core deps: added `mime_guess` and `serde_bytes` to `codex-rs/core/Cargo.toml` to satisfy our protocol usage and MIME detection in core.
- Exec: switched auth enum usage to `codex_protocol::mcp_protocol::AuthMode` to align with upstream type location.
- Workspace lockfile: merged and updated `codex-rs/Cargo.lock` to include new deps.

## Kept Ours (prefer_ours)
- TUI and CLI: no invasive changes were pulled; our `codex-rs/tui/**` and `codex-cli/**` remain as-is.
- Workflows/docs: preserved our workflow choices and deleted upstream ` .github/workflows/rust-ci.yml` (deleted on our default).
- Core API surface behavior: reverted to our `core/src/codex.rs` and `core/src/shell.rs` to avoid broad API/behavioral changes that would break our TUI and tests. Upstream variants introduced incompatible signatures and enums (e.g., ConversationId changes and new Event fields) that conflicted with our fork’s protocol layering.

## Dropped/Deferred
- Multiple upstream TUI UX tweaks and new protocol enums that require coordinated updates across `core/`, `tui/`, and tests. These are deferred to a future, deliberate migration to avoid destabilizing our UI.

## Conflict Resolution Summary
- prefer_theirs: attempted for `codex-rs/core/**` but selectively kept ours for `codex.rs` and `shell.rs` when upstream changes broke build and API expectations.
- prefer_ours: enforced for `codex-rs/tui/**`, `codex-cli/**`, workflows, and top-level docs.
- purge_globs: ensured `.github/codex-cli-*.{png,jpg,jpeg,webp}` remain absent.

## Verification
- Ran `scripts/upstream-merge/verify.sh` (updated to disable sccache wrappers and use repo-local cargo dirs). The script now compiles the `codex-core` API surface test only, per policy.
- Build via `./build-fast.sh`: ok
- Verify script: ok

## Notes
- Public re-exports in `codex-core` remain intact: `ModelClient`, `Prompt`, `ResponseEvent`, `ResponseStream`. `codex_core::models` continues to alias protocol models.
- ICU/sys-locale dependencies are present and used in `protocol/num_format.rs`.
- No reintroduction of previously removed workflow assets.

