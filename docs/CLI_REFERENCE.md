# Hanzo Dev CLI Reference

Complete command-line reference for the `dev` CLI tool.

## Synopsis

```
dev [OPTIONS] [PROMPT]
dev <SUBCOMMAND> [OPTIONS]
```

## Description

Hanzo Dev is an AI-powered coding assistant that runs in your terminal. When invoked without a subcommand, it launches the interactive TUI (Terminal User Interface) mode.

## Global Options

These options can be used with any subcommand or in interactive mode:

| Option | Short | Description |
|--------|-------|-------------|
| `--model <MODEL>` | `-m` | Model the agent should use |
| `--profile <PROFILE>` | `-p` | Configuration profile from config.toml |
| `--sandbox <MODE>` | `-s` | Sandbox policy for shell commands |
| `--ask-for-approval <POLICY>` | `-a` | When to require human approval |
| `--cd <DIR>` | `-C` | Working directory for the agent |
| `--image <FILE>` | `-i` | Attach image(s) to initial prompt (comma-separated) |
| `--oss` | | Use local open source model provider (Ollama) |
| `--full-auto` | | Enable low-friction sandboxed automatic execution |
| `--dangerously-bypass-approvals-and-sandbox` | | Skip all safety checks (DANGEROUS) |
| `--auto` | | Run Auto Drive for non-interactive sessions |
| `--demo <TEXT>` | | Developer message to prepend for demos |
| `--debug` | `-d` | Enable debug logging of LLM requests |
| `-c <KEY=VALUE>` | | Override configuration values |
| `--version` | | Print version information |
| `--help` | `-h` | Print help information |

### Sandbox Modes

- `off` - No sandboxing (commands run directly)
- `workspace-read` - Read-only access to workspace
- `workspace-write` - Read/write access to workspace (default for full-auto)
- `network-off` - Disable network access

### Approval Policies

- `always` - Always ask for approval before executing commands
- `on-failure` - Ask only when a command fails (default for full-auto)
- `on-request` - Ask when the model explicitly requests approval
- `never` - Never ask for approval

---

## Subcommands

### Interactive Mode (default)

```
dev [OPTIONS] [PROMPT]
```

Launch the interactive TUI interface. If PROMPT is provided, it starts the conversation.

**TUI-Specific Options:**

| Option | Description |
|--------|-------------|
| `--search` | Enable web search (default: enabled) |
| `--no-search` | Disable web search |
| `--compact-prompt <TEXT>` | Override compaction prompt text |
| `--compact-prompt-file <FILE>` | Read compaction prompt from file |
| `--order` | Show ordering debug overlays |
| `--timing` | Enable timing diagnostics |

**Examples:**

```bash
# Start interactive session
dev

# Start with a prompt
dev "Help me refactor this function"

# Use a specific model
dev -m gpt-4.1 "Explain this code"

# Attach an image
dev -i screenshot.png "What's in this image?"
```

---

### exec

Run Hanzo Dev non-interactively for a single task.

```
dev exec [OPTIONS] [PROMPT]
```

**Aliases:** `e`

**Options:**

| Option | Description |
|--------|-------------|
| `--auto` | Run Auto Drive instead of single turn |
| `--auto-review` | Use Auto Review models and limits |
| `--json` | Print events to stdout as JSONL |
| `--color <MODE>` | Color output: `always`, `never`, `auto` |
| `--max-seconds <N>` | Maximum wall-clock time budget |
| `--turn-cap <N>` | Maximum Auto Drive coordinator turns |
| `--include-plan-tool` | Include planning tool in conversation |
| `--output-last-message <FILE>` | Write final message to file |
| `--output-schema <FILE>` | JSON Schema for structured output |
| `--review-output-json <FILE>` | Write review output JSON |
| `--skip-git-repo-check` | Allow running outside Git repos |

**Examples:**

```bash
# Single non-interactive task
dev exec "Add error handling to main.py"

# Auto Drive with time limit
dev exec --auto --max-seconds 300 "Implement the TODO items"

# Full auto mode (sandboxed)
dev exec --full-auto "Fix all linting errors"

# JSON output for scripting
dev exec --json "List all functions in src/"
```

---

### auto

Run Auto Drive in headless mode. This is an alias for `exec --auto --full-auto`.

```
dev auto [OPTIONS] [PROMPT]
```

**Examples:**

```bash
# Fully automated task execution
dev auto "Refactor the database module"

# With turn cap
dev auto --turn-cap 10 "Complete the migration"
```

---

### resume

Resume a previous interactive session.

```
dev resume [SESSION_ID] [OPTIONS]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--last` | Continue the most recent session |
| All TUI options | Inherited from interactive mode |

**Examples:**

```bash
# Show session picker
dev resume

# Resume most recent session
dev resume --last

# Resume specific session by ID
dev resume abc123-def456-...

# Resume with different model
dev resume --last -m gpt-4.1
```

---

### login

Manage authentication credentials.

```
dev login [OPTIONS]
dev login status
```

**Options:**

| Option | Description |
|--------|-------------|
| `--with-api-key` | Read API key from stdin |

**Examples:**

```bash
# Interactive login (opens browser)
dev login

# Login with API key from environment
printenv OPENAI_API_KEY | dev login --with-api-key

# Check login status
dev login status
```

---

### logout

Remove stored authentication credentials.

```
dev logout
```

---

### apply

Apply the latest diff produced by the agent to your local working tree.

```
dev apply [OPTIONS]
```

**Aliases:** `a`

**Examples:**

```bash
# Apply the most recent agent diff
dev apply
```

---

### mcp

Manage MCP (Model Context Protocol) servers.

```
dev mcp <SUBCOMMAND>
```

**Aliases:** `acp`

**Subcommands:**

#### mcp list

List configured MCP servers.

```
dev mcp list [--json]
```

#### mcp get

Show details of a specific MCP server.

```
dev mcp get <NAME> [--json]
```

#### mcp add

Add a new MCP server configuration.

```
dev mcp add <NAME> [OPTIONS] [-- COMMAND...]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--url <URL>` | URL of remote MCP server |
| `--bearer-token <TOKEN>` | Bearer token for authentication |
| `--env <KEY=VALUE>` | Environment variables (repeatable) |

**Examples:**

```bash
# Add stdio-based MCP server
dev mcp add my-tool -- /path/to/mcp-server

# Add remote MCP server with OAuth
dev mcp add remote-tool --url https://mcp.example.com/api

# Add remote server with static auth
dev mcp add api-tool --url https://api.example.com --bearer-token $TOKEN

# Add with environment variables
dev mcp add secure-tool --env API_KEY=secret -- my-mcp-server
```

#### mcp remove

Remove an MCP server configuration.

```
dev mcp remove <NAME>
```

---

### bridge

Manage Code Bridge subscription and browser integration.

```
dev bridge <SUBCOMMAND>
```

**Subcommands:**

#### bridge subscription

View or modify bridge subscription settings.

```
dev bridge subscription [OPTIONS]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--show` | Display current subscription |
| `--levels <CSV>` | Set log levels: errors,warn,info,trace |
| `--capabilities <CSV>` | Set capabilities: screenshot,pageview,control,console,error |
| `--filter <MODE>` | LLM filter: off, minimal, aggressive |
| `--clear` | Remove override and revert to defaults |

#### bridge list

Show bridge metadata for the current workspace.

```
dev bridge list
```

**Aliases:** `ls`

#### bridge tail

Stream live bridge events.

```
dev bridge tail [OPTIONS]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--level <LEVEL>` | Minimum level: errors, warn, info, trace |
| `--bridge <PATH\|INDEX>` | Select specific bridge target |
| `--raw` | Print raw JSON frames |

#### bridge screenshot

Request a screenshot from control-capable bridge clients.

```
dev bridge screenshot [OPTIONS]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--timeout <SECONDS>` | Wait timeout (default: 10) |
| `--bridge <PATH\|INDEX>` | Select specific bridge target |

#### bridge javascript

Execute JavaScript via the bridge control channel.

```
dev bridge javascript <CODE> [OPTIONS]
```

**Aliases:** `js`

**Options:**

| Option | Description |
|--------|-------------|
| `--timeout <SECONDS>` | Wait timeout (default: 10) |
| `--bridge <PATH\|INDEX>` | Select specific bridge target |

---

### cloud

Browse and manage Codex Cloud tasks.

```
dev cloud [SUBCOMMAND]
```

**Aliases:** `cloud-tasks`

#### cloud submit

Submit a new task to Codex Cloud.

```
dev cloud submit <PROMPT> [OPTIONS]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--env <ENV_ID>` | Environment ID |
| `--best-of <N>` | Best-of-N attempts (default: 1) |
| `--qa` | Enable QA/review mode |
| `--git-ref <REF>` | Git ref (default: main) |
| `--wait` | Wait for completion |

---

### llm

Side-channel LLM utilities (no TUI events).

```
dev llm <SUBCOMMAND>
```

#### llm request

Send a structured request to the model.

```
dev llm request [OPTIONS]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--developer <TEXT>` | Developer message |
| `--message <TEXT>` | User message |
| `--model <MODEL>` | Model override |
| `--format-type <TYPE>` | Output format type (default: json_schema) |
| `--format-name <NAME>` | Output format name |
| `--format-strict` | Enable strict format (default: true) |
| `--schema-json <JSON>` | Inline JSON schema |
| `--schema-file <FILE>` | Path to JSON schema file |

---

### completion

Generate shell completion scripts.

```
dev completion [SHELL]
```

**Shells:** `bash`, `zsh`, `fish`, `powershell`, `elvish`

**Examples:**

```bash
# Generate bash completions
dev completion bash > ~/.local/share/bash-completion/completions/dev

# Generate zsh completions
dev completion zsh > ~/.zfunc/_dev

# Generate fish completions
dev completion fish > ~/.config/fish/completions/dev.fish
```

---

### doctor

Diagnose PATH, binary collisions, and version conflicts.

```
dev doctor
```

**Output includes:**
- Current executable path and version
- PATH environment variable
- All `dev` and `coder` binaries found
- Version information for each binary
- Detection of package manager conflicts (Bun, npm, Homebrew)

---

### preview

Download and run a preview build by slug.

```
dev preview <SLUG> [OPTIONS] [-- EXTRA_ARGS...]
```

**Options:**

| Option | Description |
|--------|-------------|
| `--repo <OWNER/REPO>` | Override repository |
| `--out <DIR>` | Output directory |

---

### debug

Internal debugging commands.

```
dev debug <SUBCOMMAND>
```

#### debug seatbelt

Run a command under Seatbelt (macOS only).

#### debug landlock

Run a command under Landlock+seccomp (Linux only).

---

## Configuration

Configuration is loaded from `~/.code/config.toml` (or legacy `~/.codex/config.toml`).

Use `-c key=value` to override configuration values:

```bash
# Override model
dev -c model=gpt-4.1

# Override multiple values
dev -c model=gpt-4.1 -c model_provider=openai

# Use with subcommands
dev exec -c model=gpt-4.1 "Your prompt"
```

---

## Environment Variables

| Variable | Description |
|----------|-------------|
| `OPENAI_API_KEY` | OpenAI API key |
| `CODE_HOME` | Override config directory (default: ~/.code) |
| `CODEX_HOME` | Legacy config directory (fallback) |
| `CODEX_SECURE_MODE` | Enable security hardening when set to "1" |

---

## Common Workflows

### Quick Task Execution

```bash
# Fix a bug quickly
dev exec --full-auto "Fix the null pointer exception in user.py"

# Review code changes
dev exec --auto-review "Review the changes in this PR"
```

### Interactive Development

```bash
# Start coding session with context
dev -C /path/to/project "Let's implement the new feature"

# Continue previous work
dev resume --last

# Use specific model for complex tasks
dev -m gpt-4.1 "Architect the new microservice"
```

### CI/CD Integration

```bash
# Automated code review in CI
dev exec --json --max-seconds 120 "Review this diff for issues" | jq .

# Generate documentation
dev exec --output-last-message docs.md "Document the API"
```

### MCP Server Setup

```bash
# Add a custom tool
dev mcp add my-search -- /path/to/search-mcp

# List configured servers
dev mcp list --json

# Remove a server
dev mcp remove my-search
```

---

## Exit Codes

| Code | Description |
|------|-------------|
| 0 | Success |
| 1 | General error |
| 130 | Interrupted (SIGINT) |

---

## See Also

- [Configuration Guide](config.md)
- [Sandbox Policies](sandbox.md)
- [Auto Drive](auto-drive.md)
- [MCP Integration](getting-started.md)
