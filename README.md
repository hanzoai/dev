# JustEvery Coder

**Coder** is a fast, local-first coding agent for your terminal. It's a community-driven fork focused on real developer ergonomics: Chrome DevTools automation, multi-agent flows, live theming, and on-the-fly reasoning control - all while staying compatible with upstream.

## Why Coder

  - 🌐 **Browser Integration** - CDP support, headless browsing, screenshots
  - 🤖 **Multi-Agent Commands** - /plan, /solve, /code with agent panels
  - 🎨 **Theme System** - /themes with live preview and accessibility
  - 🧠 **Reasoning Control** - /reasoning for dynamic effort adjustment
  - 🔌 **MCP support** – Extend with filesystem, DBs, APIs, or your own tools.
  - 🔒 **Safety modes** – Read-only, approvals, and workspace sandboxing.
  - 🔁 **Backwards compatible** – Supports `~/.codex/*` or default `~/.coder/*`

## Quickstart

### Install Coder

```bash
npm install -g @just-every/coder
coder
```

**Authenticate** (one of the following):
- **Sign in with ChatGPT** (Plus/Pro/Team; uses models available to your plan)
  - Run `coder` and pick "Sign in with ChatGPT"
  - Stores creds locally at `~/.coder/auth.json` (also reads legacy `~/.codex/auth.json`)
- **API key** (usage-based)
  - Set `export OPENAI_API_KEY=xyz` and run `coder`

### Install Claude & Gemini (optional)

Coder supports orchestrating other AI CLI tools. Install these and config to use alongside coder.

```bash
npm install -g @anthropic-ai/claude-code
claude

npm install -g @google/gemini-cli
gemini
```



## Commands

### Browser
```bash
# Connect coder to your Chrome browser (running CDP)
/browser local

# Use a headless browser (Agent can also enable)
/browser https://example.com
```

### Agents
```bash
# Plan code changes (Claude, Gemini and GPT-5 consensus)
# All agents review task and create a consolidated plan
/plan "Stop the AI from ordering pizza at 3AM"

# Solve complex problems (Claude, Gemini and GPT-5 race)
# Fastest preferred (see https://arxiv.org/abs/2505.17813)
/solve "Why does deleting one user drop the whole database?"

# Write code! (Claude, Gemini and GPT-5 consensus)
# Creates multiple worktrees then implements the optimal solution
/code "Show dark mode when I feel cranky"
```

### General
```bash
# Try a new theme
/themes

# Change reasoning level
/reasoning low|medium|high

# Change model
/model gpt-5-mini

# Start new conversation
/new
```

## CLI reference

```shell
coder [options] [prompt]

Options:
  --model <name>        Override the model (gpt-5, claude-opus, etc.)
  --read-only          Prevent file modifications
  --no-approval        Skip approval prompts (use with caution)
  --config <key=val>   Override config values
  --oss                Use local open source models
  --sandbox <mode>     Set sandbox level (read-only, workspace-write, etc.)
  --help              Show help information
  --version           Show version number
```

## Memory & project docs

Coder can remember context across sessions:

1. **Create an `AGENTS.md` or `CLAUDE.md` file** in your project root:
```markdown
# Project Context
This is a React TypeScript application with:
- Authentication via JWT
- PostgreSQL database
- Express.js backend

## Key files:
- `/src/auth/` - Authentication logic
- `/src/api/` - API client code  
- `/server/` - Backend services
```

2. **Session memory**: Coder maintains conversation history
3. **Codebase analysis**: Automatically understands project structure

## Non-interactive / CI mode

For automation and CI/CD:

```shell
# Run a specific task
coder --no-approval "run tests and fix any failures"

# Generate reports
coder --read-only "analyze code quality and generate report"

# Batch processing
coder --config output_format=json "list all TODO comments"
```

## Model Context Protocol (MCP)

Coder supports MCP for extended capabilities:

- **File operations**: Advanced file system access
- **Database connections**: Query and modify databases
- **API integrations**: Connect to external services
- **Custom tools**: Build your own extensions

Configure MCP in `~/.codex/config.toml`:

```toml
[[mcp_servers]]
name = "filesystem"
command = "npx"
args = ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/project"]
```

## Configuration

Main config file: `~/.codex/config.toml`

```toml
# Model settings
model = "gpt-5"
model_provider = "openai"

# Behavior
approval_policy = "on_request"  # untrusted | on-failure | on-request | never
model_reasoning_effort = "medium" # low | medium | high
sandbox_mode = "workspace_write"

# UI preferences see THEME_CONFIG.md
[tui.theme]
name = "light-photon"

# Add config for specific models
[profiles.o3]
model = "o3"
model_provider = "openai"
approval_policy = "never"
model_reasoning_effort = "high"
model_reasoning_summary = "detailed"
```

### Environment variables

- `CODEX_HOME`: Override config directory location
- `OPENAI_API_KEY`: Use API key instead of ChatGPT auth
- `OPENAI_BASE_URL`: Use alternative API endpoints

## FAQ

**Q: How is this different from the original?**
A: This fork adds browser integration, multi-agent commands (`/plan`, `/solve`, `/code`), theme system, and enhanced reasoning controls while maintaining full compatibility.

**Q: Can I use my existing Codex configuration?**
A: Yes! This fork is fully backward compatible with existing `~/.codex/` configurations.

**Q: Does this work with ChatGPT Plus?**
A: Absolutely. Use the same "Sign in with ChatGPT" flow as the original.

**Q: Is my data secure?**
A: Yes. Authentication stays on your machine, and we don't proxy your credentials or conversations.

## Contributing

We welcome contributions! This fork maintains compatibility with upstream while adding community-requested features.

### Development workflow

```bash
# Clone and setup
git clone https://github.com/just-every/coder.git
cd coder
npm install

# Build (use fast build for development)
./build-fast.sh

# Run locally
./codex-rs/target/dev-fast/coder
```

### Opening a pull request

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Make your changes
4. Run tests: `cargo test`
5. Build successfully: `./build-fast.sh`
6. Submit a pull request

## Security & responsible AI

- **Sandbox protection**: Commands run in restricted environments
- **Approval prompts**: Review before execution of significant changes
- **No credential access**: Coder can't access your passwords or API keys
- **Local processing**: No data sent to third parties beyond chosen AI provider

## Legal & Use

### License & attribution
- This project is a community fork of [`openai/codex`] under **Apache-2.0**. We preserve upstream LICENSE and NOTICE files.
- **Coder** is **not** affiliated with, sponsored by, or endorsed by OpenAI.

### Your responsibilities
Using AI services through Coder means you agree to **their Terms and policies**. In particular:
- **Don't** programmatically scrape/extract content outside intended flows.
- **Don't** bypass or interfere with rate limits, quotas, or safety mitigations.
- Use your **own** account; don't share or rotate accounts to evade limits.
- If you configure other model providers, you're responsible for their terms.

### Branding
- Third-party model names and trademarks belong to their respective owners. Don't use model names in your app/product name, domain, or logo.
- It's fine to **describe compatibility** truthfully (e.g., "Sign in with ChatGPT" or "uses the API"). Avoid implying partnership or endorsement.

### Privacy
- Your auth file lives at `~/.codex/auth.json`.
- Inputs/outputs you send to AI providers are handled under their Terms and Privacy Policy; consult those documents (and any org-level data-sharing settings).

### Subject to change
AI providers can change eligibility, limits, models, or authentication flows. Coder supports **both** ChatGPT sign-in and API-key modes so you can pick what fits (local/hobby vs CI/automation).

## License

Apache 2.0 - See [LICENSE](LICENSE) file for details.

This project is a community fork of the original Codex CLI. We maintain compatibility while adding enhanced features requested by the developer community.

---

**Need help?** Open an issue on [GitHub](https://github.com/just-every/coder/issues) or check our documentation.