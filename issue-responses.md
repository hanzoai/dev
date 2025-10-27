# Issue Responses

## #207 — Remove Codex Requirement

Thanks for raising this! The CLI no longer assumes the Codex-only GPT-5 tier:

- The default `model` is now `gpt-5`, so a fresh install works with general OpenAI access or any configured provider. The ChatGPT onboarding wizard also stops rewriting the model back to the Codex tier.
- Multi-agent defaults promote non-Codex providers first (`code-gpt-5`, Claude, Gemini, Qwen) while keeping `code-gpt-5-codex` available for legacy accounts.
- Docs and prompts were updated to reflect the new defaults and highlight that Claude/Gemini are first-class options.

If you already have a `config.toml`, you do not need to change anything—your configured `model` continues to win. New installs and empty configs now pick the provider-agnostic defaults.

Let us know if anything else still suggests Codex is mandatory, and we can sweep those spots too.

## #343 — OPENAI_WIRE_API support was removed

We’ve reinstated the documented `OPENAI_WIRE_API` override. Setting `OPENAI_WIRE_API=chat` now forces the built-in OpenAI provider to use the Chat Completions endpoint, while `responses` (or the default) stays on the Responses API. The new regression tests in `code-rs/core/tests/openai_wire_api_env.rs` cover chat/responses/default/invalid cases so this stays verified going forward.

## #289 — Custom prompts not discovered

Prompt discovery now uses the same legacy-aware resolver as other config files, so `~/.codex/prompts/*.md` is picked up when `~/.code/prompts` is absent. Fresh async tests in `code-rs/core/tests/custom_prompts_discovery.rs` lock environment variables and cover CODE_HOME override, legacy fallback, dual-directory preference, and ignoring non-Markdown files.

## #351 — Agents toggle state not persisting

Thanks for the detailed report! The Agents overlay now persists the “Enabled” checkbox immediately, so you no longer need to close and reopen the menu to see the change. We also added a VT100 snapshot regression test (`agents_toggle_claude_opus_persists_via_slash_command`) to keep the toggle flow locked. The fix is merged and will ride out in the next release.

### Triage Summary
- #351 — fixed (pending release notes)
- #348 — needs-repro (sandbox write errors still under investigation)
- #343 — fixed (tests added for OPENAI_WIRE_API)
- #341 — backlog (consider prompt override UX after /auto refactors)
- #338 — packaging (nix flake vendoring gap)
- #333 — needs-repro (duplicate MCP sessions needs deeper trace)
- #332 — backlog (consider dark-mode default once theme revamp lands)
- #331 — needs-repro (depends on org streaming entitlement)
- #329 — backlog (Windows apply_patch ergonomics)
- #328 — packaging (Homebrew asset mismatch)
- #327 — needs-repro (malloc crash data gathering)
- #311 — backlog (UI polish request)
- #307 — needs-repro (Gemini agent parameter mismatch)
- #306 — backlog (resume UX improvements queued)
- #305 — needs-repro (MCP menu scroll regression)
- #304 — needs-repro (remote white overlay environment-specific)
- #299 — needs-repro (Claude agent handoff failure)
- #298 — backlog (external Chrome support investigation)
- #296 — needs-repro (IME spacing in PowerShell)
- #293 — backlog (planning mode providers)
- #292 — packaging (release tagging/version alignment)
- #290 — backlog (multi-branch command idea)
- #289 — fixed (legacy prompts resolved)
- #285 — backlog (resume screen roadmap)
- #284 — backlog (title bar UX)
- #279 — backlog (Zed ACP integration polish)
- #278 — backlog (WSL browser support)
- #277 — needs-repro (Chinese input handling)
- #263 — backlog (sub-agent context exploration)
- #255 — backlog (Ctrl-A shortcut conflict)
- #232 — backlog (background bash support)
- #207 — fixed (Codex optional defaults)

## #307 — Gemini agent error

Appreciate the heads-up! The Gemini sub-agents now invoke the CLI with the long `--model` flag, which unblocks the newer releases that rejected `-m`. A dedicated unit test guards the flag so we don’t regress, and the update will land in the next release.
