# Agents & Subagents

Hanzo Dev can launch external CLI “agents” and orchestrate them in multi-agent “subagent” flows such as `/plan`, `/solve`, and `/dev`.

## Agent configuration (`[[agents]]` in `config.toml`)
```toml
[[agents]]
name = "dev-gpt-5.2-dev"       # slug or alias shown in pickers
command = "coder"                # executable; defaults to name
args = ["--foo", "bar"]          # base argv
args_read_only = ["-s", "read-only", "-a", "never", "exec", "--skip-git-repo-check"]
args_write = ["-s", "workspace-write", "--dangerously-bypass-approvals-and-sandbox", "exec", "--skip-git-repo-check"]
env = { CODE_FOO = "1" }
read_only = false                 # force RO even if session allows writes
enabled = true                    # hide from pickers when false
description = "Frontline coding agent"
instructions = "Preamble added to this agent’s prompt"
```
Field recap: `name` (slug/alias), `command` (absolute paths ok), `args*` (RO/RW lists override base), `env`, `read_only`, `enabled`, optional `description` and `instructions`.

### Built-in defaults
If no `[[agents]]` are configured, Hanzo Dev advertises built-ins (gated by env `CODE_ENABLE_CLOUD_AGENT_MODEL` for cloud variants): `dev-gpt-5.2`, `dev-gpt-5.2-dev`, `claude-opus-4.5`, `gemini-3-pro`, `dev-gpt-5.1-dev-mini`, `claude-sonnet-4.5`, `gemini-3-flash`, `claude-haiku-4.5`, `qwen-3-coder`, `cloud-gpt-5.1-dev-max`. Built-ins strip any user `--model/-m` flags to avoid conflicts and inject their own.

Tip: `gemini` resolves to `gemini-3-flash` (fast/cheap). Use `gemini-3-pro` when you want the higher-capacity Gemini option.

## Subagents (`[[subagents.commands]]`)
```toml
[[subagents.commands]]
name = "plan"                     # slash name (/plan, /solve, /dev, or custom)
read_only = true                  # default plan/solve=true, dev=false
agents = ["dev-gpt-5.2-dev", "claude-opus-4.5"]  # falls back to enabled agents or built-ins
orchestrator_instructions = "Guidance for Hanzo Dev before spawning agents"
agent_instructions = "Preamble added to each spawned agent"
```
- `name`: slash command created/overridden.
- `read_only`: forces spawned agents to RO when true.
- `agents`: explicit list; empty → enabled `[[agents]]`; none configured → built-in roster.
- `orchestrator_instructions`: appended to the Hanzo Dev-side prompt before issuing `agent.create`.
- `agent_instructions`: appended to each spawned agent prompt.

The orchestrator fans out agents, waits for results, and merges reasoning according to your `hide_agent_reasoning` / `show_raw_agent_reasoning` settings.

## TUI controls
- `/agents` opens the settings overlay to the Agents section: toggle enabled/read-only, view defaults, and open editors.
- Agent editor: create or edit a single agent (enable/disable, read-only, instructions). Args/env come from `config.toml`.
- Subagent editor: configure per-command agent lists, read-only flag, and instructions. Built-in `/plan` `/solve` `/dev` can be overridden the same way.
- Model pickers are modal and return to the invoking section after selection.

## Auto Drive interaction
- Auto Drive uses the `agents_enabled` toggle in its settings pane; when off, the coordinator skips agent batches.
- If no git repo exists, Auto Drive instructs all agents to run read-only.
- `AUTO_AGENTS.md` is read alongside `AGENTS.md` for Auto Drive–specific guidance.

## AGENTS.md and project memory
- Hanzo Dev loads AGENTS.md files along the path (global, repo root, cwd) up to 32 KiB total; deeper files override higher-level ones.
- Contents become system/developer instructions on the first turn; direct user/developer prompts still take precedence.

## Windows discovery tips
- On Windows, include extensions in `command` (`.exe`, `.cmd`, `.bat`, `.com`).
- NPM globals often live under `C:\\Users\\<you>\\AppData\\Roaming\\npm\\`.
- If PATH is unreliable, use absolute `command` paths in `[[agents]]`.

## Notifications and reasoning visibility
- `hide_agent_reasoning = true` removes agent reasoning streams in both the TUI and `dev exec`.
- `show_raw_agent_reasoning = true` surfaces raw chains-of-thought when provided by the model.
- Notification filtering is controlled via `/notifications` or `config.toml` `notify` / `tui.notifications`.

## Headless `dev exec`
- `dev exec --json` streams JSONL events (agent turns included).
- `--output-schema <schema.json>` enforces structured JSON output; combine with `--output-last-message` to capture only the final payload.
- `dev exec` defaults to read-only; add `--full-auto` plus a writable sandbox to permit edits.

## Quick examples
- Custom agent:
```toml
[[agents]]
name = "my-coder"
command = "/usr/local/bin/coder"
args_write = ["-s", "workspace-write", "--dangerously-bypass-approvals-and-sandbox", "exec", "--skip-git-repo-check"]
enabled = true
```
- Custom context sweep command:
```toml
[[subagents.commands]]
name = "context"
read_only = true
agents = ["dev-gpt-5.2-dev", "claude-opus-4.5"]
orchestrator_instructions = "Have each agent summarize the most relevant files and tests."
agent_instructions = "Return paths plus 1–2 sentence rationale; do not edit files."
```
