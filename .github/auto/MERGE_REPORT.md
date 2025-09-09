# Upstream Merge Report

Upstream: openai/codex@main (tip: 2a76a08a9 — fix: include rollout_path in NewConversationResponse)
Branch: upstream-merge
Mode: by-bucket

## Incorporated
- Protocol: Adopted upstream changes in `codex-rs/protocol` including the new `rollout_path` surfaced via `NewConversationResponse`.
- Types: Added `ts-rs` derives and minimal `#[ts(...)]` annotations to align with upstream TypeScript schema generation while preserving our models layout.
- MCP server: Wired `rollout_path` through `SessionConfiguredEvent` to populate `NewConversationResponse`.

## Preserved (ours)
- Core execution flow and API surface (`codex-rs/core/**`): kept our implementation to avoid breaking callers and maintain our event model and re-exports (ModelClient, Prompt, ResponseEvent, ResponseStream) and `codex_core::models` alias.
- TUI and UX: retained our TUI rendering and tests; resolved conflicts by preferring ours.
- Workflows, CLI, and docs branding per policy.

## Dropped/Rejected
- Reintroduced assets/tests under TUI and GitHub images: removed `codex-rs/tui/tests/fixtures/binary-size-log.jsonl` and enforced purge for `.github/codex-cli-*.{png,jpg,jpeg,webp}` (none present locally).
- Upstream core edits that conflicted with our API and broke build were not adopted.

## Minimal Fixes
- Exported `num_format` module in `codex-protocol` to satisfy imports.
- Derived `Default` for `ConversationId` and added TS derives for `plan_tool`, `parse_command`, `message_history`, `custom_prompts`, and protocol `models`.
- Represented `FunctionCallOutputPayload` as a TS `string` to match serialization behavior.
- Extended our `core::protocol::SessionConfiguredEvent` with `rollout_path` and updated pattern matches to ignore the new field where not used.
- Extended `RolloutRecorder::new` to return the created session file path for inclusion in the configuration event.

## Verification
- scripts/upstream-merge/verify.sh: PASS
  - ./build-fast.sh: PASS (no warnings)
  - cargo check -p codex-core --test api_surface: PASS

## Notes
- Public API compatibility retained for downstream crates per constraints.
- ICU/sys-locale dependencies untouched; repo-wide usage remains.

## PR Title/Body (proposed)
- Title: "merge(upstream): sync with openai/codex@2a76a08a9; add rollout_path to protocol; keep core/TUI intact"
- Body: See report above. Bucketed merge: protocol changes adopted; core/TUI preserved. Verification green via verify.sh and build-fast.
