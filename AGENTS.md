# Rust/codex-rs

In the codex-rs folder where the rust code lives:

- Crate names are prefixed with `codex-`. For example, the `core` folder's crate is named `codex-core`
- When using format! and you can inline variables into {}, always do that.

Completion/build step

- Always validate using `./build-fast.sh` from the repo root. This is the single required check and must pass cleanly.
- Policy: All errors AND all warnings must be fixed before you’re done. Treat any compiler warning as a failure and address it (rename unused vars with `_`, remove `mut`, delete dead code, etc.).
- Do not run additional format/lint/test commands on completion (e.g., `just fmt`, `just fix`, `cargo test`) unless explicitly requested for a specific task.
- ***NEVER run rustfmt***

## Strict Ordering In The TUI History

The TUI enforces strict, per‑turn ordering for all streamed content. Every
stream insert (Answer or Reasoning) must be associated with a stable
`(request_ordinal, output_index, sequence_number)` key provided by the model.

- A stream insert MUST carry a non‑empty stream id. The UI seeds an order key
  for `(kind, id)` from the event's `OrderMeta` before any insert.
- The TUI WILL NOT insert streaming content without a stream id. Any attempt to
  insert without an id is dropped with an error log to make the issue visible
  during development.

## Commit Messages

- Review staged changes before every commit: `git --no-pager diff --staged --stat` (and skim `git --no-pager diff --staged` if needed).
- Write a descriptive subject that explains what changed and why. Avoid placeholders like "chore: commit local work".
- Prefer Conventional Commits with an optional scope: `feat(tui/history): …`, `fix(core/exec): …`, `docs(agents): …`.
- Keep the subject ≤ 72 chars; add a short body if rationale or context helps future readers.
- Use imperative, present tense: "add", "fix", "update" (not "added", "fixes").
- For merge commits, skip custom prefixes like `merge(main<-origin/main):`. Use a clear subject such as `Merge origin/main: <what changed and how conflicts were resolved>`.

Examples:

- `feat(tui/history): show exit code and duration for Exec cells`
- `fix(core/codex): handle SIGINT in on_exec_command_begin to avoid orphaned child`
- `docs(agents): clarify commit-message expectations`

## How to Git Push

### Merge-and-Push Policy (Do Not Rebase)

When the user asks you to "push" local work:

- Never rebase in this flow. Do not use `git pull --rebase` or attempt to replay local commits.
- Prefer a simple merge of `origin/main` into the current branch, keeping our local history intact.
- If the remote only has trivial release metadata changes (e.g., `codex-cli/package.json` version bumps), adopt the remote version for those files and keep ours for everything else unless the user specifies otherwise.
- If in doubt or if conflicts touch non-trivial areas, pause and ask before resolving.

Quick procedure (merge-only):

- Commit your local work first:
  - Review: `git --no-pager diff --stat` and `git --no-pager diff`
  - Stage + commit: `git add -A && git commit -m "<descriptive message of local changes>"`
- Fetch remote: `git fetch origin`
- Merge without auto-commit: `git merge --no-ff --no-commit origin/main` (stops before committing so you can choose sides)
- Resolve policy:
  - Default to ours: `git checkout --ours .`
  - Take remote for trivial package/version files as needed, e.g.: `git checkout --theirs codex-cli/package.json`
- Stage and commit the merge with a descriptive message, e.g.:
  - `git add -A && git commit -m "Merge origin/main: adopt remote version bumps; keep ours elsewhere (<areas>)"`
- Run `./build-fast.sh` and then `git push`

## Command Execution Architecture

The command execution flow in Codex follows an event-driven pattern:

1. **Core Layer** (`codex-core/src/codex.rs`):
   - `on_exec_command_begin()` initiates command execution
   - Creates `EventMsg::ExecCommandBegin` events with command details

2. **TUI Layer** (`codex-tui/src/chatwidget.rs`):
   - `handle_codex_event()` processes execution events
   - Manages `RunningCommand` state for active commands
   - Creates `HistoryCell::Exec` for UI rendering

3. **History Cell** (`codex-tui/src/history_cell.rs`):
   - `new_active_exec_command()` - Creates cell for running command
   - `new_completed_exec_command()` - Updates with final output
   - Handles syntax highlighting via `ParsedCommand`

This architecture separates concerns between execution logic (core), UI state management (chatwidget), and rendering (history_cell).

## Writing New UI Regression Tests

- Start with `make_chatwidget_manual()` (or `make_chatwidget_manual_with_sender()`) to build a `ChatWidget` in isolation with in-memory channels.
- Simulate user input by defining a small enum (`ScriptStep`) and feeding key events via `chat.handle_key_event()`; see `run_script()` in `tests.rs` for a ready-to-use helper that also pumps `AppEvent`s.
- After the scripted interaction, render with a `ratatui::Terminal`/`TestBackend`, then use `buffer_to_string()` (wraps `strip_ansi_escapes`) to normalize ANSI output before asserting.
- Prefer snapshot assertions (`assert_snapshot!`) or rich string comparisons so UI regressions are obvious. Keep snapshots deterministic by trimming trailing space and driving commit ticks just like the existing tests do.
- When adding fixtures or updating snapshots, gate rewrites behind an opt-in env var (e.g., `UPDATE_IDEAL=1`) so baseline refreshes remain explicit.

### Monitor Release Workflows After Pushing

- Use `scripts/wait-for-gh-run.sh` to follow GitHub Actions releases without spamming manual `gh` commands.
- Typical release check right after a push: `scripts/wait-for-gh-run.sh --workflow Release --branch main`.
- If you already know the run ID (e.g., from webhook output), run `scripts/wait-for-gh-run.sh --run <run-id>`.
- Adjust the poll cadence via `--interval <seconds>` (defaults to 8). The script exits 0 on success and 1 on failure, so it can gate local automation.
- Pass `--failure-logs` to automatically dump logs for any job that does not finish successfully.
- Dependencies: GitHub CLI (`gh`) and `jq` must be available in `PATH`.
