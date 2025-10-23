# Continue Auto Drive Cleanup and Extraction

```
You’re attached to worktree /home/azureuser/.code/working/code/branches/code-branch-20251023-043432 (branch code-branch-20251023-043432). Context so far: observer/QA plumbing in the TUI was guarded behind #[cfg(FALSE)] and the auto_coordinator loop had its observer wiring stripped. ./build-fast.sh was run successfully, but the build now emits warnings about those #[cfg(FALSE)] blocks plus a bunch of unused structs/fields (observer enums, helper fns, variables like cmd_tx/turn_cli_prompt/read_only_tools).

Goal: finish the cleanup and start the crate extraction.
1. Delete the dormant observer/QA modules instead of #[cfg(FALSE)], including removing ChatWidget fields/methods and the now-unreferenced types in code-rs/tui/src/app_event.rs, auto_coordinator.rs, chatwidget.rs, and any other TUI files that were only serving the observer pipeline. Remove helper functions like compose_developer_intro/push_unique_guidance and dead structs (ObserverHistory entries, AutoReviewOutcome, etc.) that no longer compile-time check out.
2. Resolve every warning from ./build-fast.sh by eliminating unused imports/vars, removing or refactoring dead code. Do NOT rustfmt. Stick to ASCII for edits.
3. Once the TUI builds clean with zero warnings, scaffold a new crate under code-rs (name it code-auto-drive-core) that will house the coordinator. For now: create the crate, move auto_coordinator.rs and minimal dependencies (auto_drive_history.rs, coordinator_router.rs, coordinator_user_schema.rs if required) into it, and expose the coordinator loop via a trait-based interface so the TUI adapter can call into it. Keep the TUI compiling by adding a thin adapter layer in chatwidget.rs that uses the new crate. Update Cargo.toml/workspace members accordingly.
4. After migrating the coordinator, run ./build-fast.sh again (allow 25+ min) and make sure there are zero warnings or errors. Summarize any remaining TODOs for completing the extraction (e.g., migrating history structs or configuration parsing) if you can’t finish within this run.

Constraints:
- Always run commands via ["bash","-lc",…] with workdir set. Never rustfmt.
- Treat codex-rs as read-only; modify files only under code-rs/.
- Finish with a concise summary plus next steps if work remains.
```
