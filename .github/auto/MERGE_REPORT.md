# Upstream Merge Report

Branch: upstream-merge
Upstream: openai/codex@main
Mode: by-bucket

## Incorporated
- Core config enhancements (model metadata, project overrides, sandbox precedence, review model default logic).
- New TUI model-upgrade popup module retained with adjusted branding and without auto-upgrade wiring.
- Common crate updates and internal storage tweaks.

## Dropped / Kept Ours
- TUI lib wiring: kept fork’s run_main and app lifecycle; dropped upstream onboarding/login/resume rollouts and model auto-upgrade prompting to preserve fork UX and strict streaming order.
- Core config tests for `persist_model_selection`: excluded due to missing upstream helper in our fork; function not used by our TUI.
- Branding: converted user-facing string "Codex" -> "Code" in new model popup.

## Guards & Invariants
- Tool parity: browser_*, agent_*, and web_fetch handlers remain registered via openai_tools; verify.sh guards passed.
- UA/version: `codex_version::version()` referenced in default_client; preserved.
- Public re-exports remain: ModelClient, Prompt, ResponseEvent, ResponseStream.
- `codex_core::models` continues to alias protocol models.

## Purges
- No reintroduced `.github/codex-cli-*.{png,jpg,jpeg,webp}` artifacts detected.

## Build/Verify
- ./build-fast.sh: OK (no warnings).
- scripts/upstream-merge/verify.sh: OK (build_fast=ok, api_check=ok, guards passed).

## Notes
- Upstream introduced model rollout (Swiftfox) and onboarding flows that conflict with our TUI UX; these remain out by design.
- If we later adopt model-persist helpers, we can restore the related tests.
