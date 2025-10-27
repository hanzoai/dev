# Issue Responses

## #207 — Remove Codex Requirement

Thanks for the note! This isn’t a functional bug—we still require a valid OpenAI (or other configured) provider account to run. We kept the clarification changes that shipped earlier (docs now highlight provider setup, and defaults mention `gpt-5` alongside the Codex option), but there’s no runtime change here. We’ve reclassified the issue as “won’t fix” because provider access remains mandatory.

## #343 — OPENAI_WIRE_API support was removed

We’ve reinstated the documented `OPENAI_WIRE_API` override. Setting `OPENAI_WIRE_API=chat` now forces the built-in OpenAI provider to use the Chat Completions endpoint, while `responses` (or the default) stays on the Responses API. The new regression tests in `code-rs/core/tests/openai_wire_api_env.rs` cover chat/responses/default/invalid cases so this stays verified going forward.

## #289 — Custom prompts not discovered

Prompt discovery now uses the same legacy-aware resolver as other config files, so `~/.codex/prompts/*.md` is picked up when `~/.code/prompts` is absent. Fresh async tests in `code-rs/core/tests/custom_prompts_discovery.rs` lock environment variables and cover CODE_HOME override, legacy fallback, dual-directory preference, and ignoring non-Markdown files.

## #351 — Agents toggle state not persisting

Thanks for the detailed report! The Agents overlay now persists the “Enabled” checkbox immediately, so you no longer need to close and reopen the menu to see the change. We also added a VT100 snapshot regression test (`agents_toggle_claude_opus_persists_via_slash_command`) to keep the toggle flow locked. The fix is merged and will ride out in the next release.

## #333 — Duplicate MCP servers after `/new`

Great catch—new sessions were launching a fresh MCP stdio server without shutting down the previous one, so duplicate tool providers lingered in the Agents menu. `Session::shutdown_mcp_clients` now drains the connection manager before we rebuild the session, which kills any legacy stdio clients via `kill_on_drop` and awaits RMCP shutdowns. The regression test in `code-rs/tui/tests/mcp_session_cleanup.rs` builds a stub MCP server and fails if the first process is still alive when `/new` spins up the replacement. Look for this fix in the upcoming release.

### Triage Summary
- #351 — fixed (pending release notes)
- #348 — needs-repro (sandbox write errors still under investigation)
- #343 — fixed (tests added for OPENAI_WIRE_API)
- #341 — backlog (consider prompt override UX after /auto refactors)
- #338 — packaging (nix flake vendoring gap)
- #333 — fixed (MCP session cleanup, new shutdown guard)
- #332 — backlog (consider dark-mode default once theme revamp lands)
- #331 — needs-repro (depends on org streaming entitlement)
- #329 — backlog (Windows apply_patch ergonomics)
- #328 — packaging (Homebrew asset mismatch)
- #327 — needs-repro (malloc crash data gathering)
- #311 — backlog (UI polish request)
- #307 — fixed (Gemini CLI flag switched to --model)
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
- #207 — won’t-fix (provider access still required; docs clarified)

## #307 — Gemini agent error

Appreciate the heads-up! The Gemini sub-agents now invoke the CLI with the long `--model` flag, which unblocks the newer releases that rejected `-m`. A dedicated unit test guards the flag so we don’t regress, and the update will land in the next release.
