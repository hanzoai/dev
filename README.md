# Code CLI - Enhanced Terminal Coding Agent

**Code CLI** is an enhanced fork of OpenAI's Codex CLI that brings powerful AI coding assistance directly to your terminal with additional features for improved developer experience.

<p align="center"><strong>Codex CLI</strong> is a coding agent from OpenAI that runs locally on your computer.</br>If you are looking for the <em>cloud-based agent</em> from OpenAI, <strong>Codex Web</strong>, see <a href="https://chatgpt.com/codex">chatgpt.com/codex</a>.</p>

<p align="center">
  <img src="./.github/codex-cli-splash.png" alt="Codex CLI splash" width="50%" />
  </p>

---

<details>
<summary><strong>Table of contents</strong></summary>

<!-- Begin ToC -->

- [Quickstart](#quickstart)
  - [Installing and running Codex CLI](#installing-and-running-codex-cli)
  - [Using Codex with your ChatGPT plan](#using-codex-with-your-chatgpt-plan)
  - [Connecting on a "Headless" Machine](#connecting-on-a-headless-machine)
    - [Authenticate locally and copy your credentials to the "headless" machine](#authenticate-locally-and-copy-your-credentials-to-the-headless-machine)
    - [Connecting through VPS or remote](#connecting-through-vps-or-remote)
  - [Usage-based billing alternative: Use an OpenAI API key](#usage-based-billing-alternative-use-an-openai-api-key)
  - [Choosing Codex's level of autonomy](#choosing-codexs-level-of-autonomy)
    - [**1. Read/write**](#1-readwrite)
    - [**2. Read-only**](#2-read-only)
    - [**3. Advanced configuration**](#3-advanced-configuration)
    - [Can I run without ANY approvals?](#can-i-run-without-any-approvals)
    - [Fine-tuning in `config.toml`](#fine-tuning-in-configtoml)
  - [Example prompts](#example-prompts)
- [Running with a prompt as input](#running-with-a-prompt-as-input)
- [Using Open Source Models](#using-open-source-models)
  - [Platform sandboxing details](#platform-sandboxing-details)
- [Experimental technology disclaimer](#experimental-technology-disclaimer)
- [System requirements](#system-requirements)
- [CLI reference](#cli-reference)
- [Memory & project docs](#memory--project-docs)
- [Non-interactive / CI mode](#non-interactive--ci-mode)
- [Model Context Protocol (MCP)](#model-context-protocol-mcp)
- [Tracing / verbose logging](#tracing--verbose-logging)
  - [DotSlash](#dotslash)
- [Configuration](#configuration)
- [FAQ](#faq)
- [Zero data retention (ZDR) usage](#zero-data-retention-zdr-usage)
- [Codex open source fund](#codex-open-source-fund)
- [Contributing](#contributing)
  - [Development workflow](#development-workflow)
  - [Writing high-impact code changes](#writing-high-impact-code-changes)
  - [Opening a pull request](#opening-a-pull-request)
  - [Review process](#review-process)
  - [Community values](#community-values)
  - [Getting help](#getting-help)
  - [Contributor license agreement (CLA)](#contributor-license-agreement-cla)
    - [Quick fixes](#quick-fixes)
  - [Releasing `codex`](#releasing-codex)
- [Security & responsible AI](#security--responsible-ai)
- [License](#license)

<!-- End ToC -->

</details>

---

## Quickstart

### Installing and running Codex CLI

Install globally with your preferred package manager:

```shell
npm install -g @openai/codex  # Alternatively: `brew install codex`
```

Then simply run `codex` to get started:

```shell
codex
```

<details>
<summary>You can also go to the <a href="https://github.com/openai/codex/releases/latest">latest GitHub Release</a> and download the appropriate binary for your platform.</summary>

Each GitHub Release contains many executables, but in practice, you likely want one of these:

- macOS
  - Apple Silicon/arm64: `codex-aarch64-apple-darwin.tar.gz`
  - x86_64 (older Mac hardware): `codex-x86_64-apple-darwin.tar.gz`
- Linux
  - x86_64: `codex-x86_64-unknown-linux-musl.tar.gz`
  - arm64: `codex-aarch64-unknown-linux-musl.tar.gz`

Each archive contains a single entry with the platform baked into the name (e.g., `codex-x86_64-unknown-linux-musl`), so you likely want to rename it to `codex` after extracting it.

</details>

### Using Codex with your ChatGPT plan

<p align="center">
  <img src="./.github/codex-cli-login.png" alt="Codex CLI login" width="50%" />
  </p>

Run `codex` and select **Sign in with ChatGPT**. You'll need a Plus, Pro, or Team ChatGPT account, and will get access to our latest models, including `gpt-5`, at no extra cost to your plan. (Enterprise is coming soon.)

> Important: If you've used the Codex CLI before, follow these steps to migrate from usage-based billing with your API key:
>
> 1. Update the CLI and ensure `codex --version` is `0.20.0` or later
> 2. Delete `~/.codex/auth.json` (this should be `C:\Users\USERNAME\.codex\auth.json` on Windows)
> 3. Run `codex login` again

If you encounter problems with the login flow, please comment on [this issue](https://github.com/openai/codex/issues/1243).

### Connecting on a "Headless" Machine

Today, the login process entails running a server on `localhost:1455`. If you are on a "headless" server, such as a Docker container or are `ssh`'d into a remote machine, loading `localhost:1455` in the browser on your local machine will not automatically connect to the webserver running on the _headless_ machine, so you must use one of the following workarounds:

#### Authenticate locally and copy your credentials to the "headless" machine

The easiest solution is likely to run through the `codex login` process on your local machine such that `localhost:1455` _is_ accessible in your web browser. When you complete the authentication process, an `auth.json` file should be available at `$CODEX_HOME/auth.json` (on Mac/Linux, `$CODEX_HOME` defaults to `~/.codex` whereas on Windows, it defaults to `%USERPROFILE%\.codex`).

Because the `auth.json` file is not tied to a specific host, once you complete the authentication flow locally, you can copy the `$CODEX_HOME/auth.json` file to the headless machine and then `codex` should "just work" on that machine. Note to copy a file to a Docker container, you can do:

```shell
# substitute MY_CONTAINER with the name or id of your Docker container:
CONTAINER_HOME=$(docker exec MY_CONTAINER printenv HOME)
docker exec MY_CONTAINER mkdir -p "$CONTAINER_HOME/.codex"
docker cp auth.json MY_CONTAINER:"$CONTAINER_HOME/.codex/auth.json"
```

whereas if you are `ssh`'d into a remote machine, you likely want to use [`scp`](https://en.wikipedia.org/wiki/Secure_copy_protocol):

```shell
ssh user@remote 'mkdir -p ~/.codex'
scp ~/.codex/auth.json user@remote:~/.codex/auth.json
```

or try this one-liner:

```shell
ssh user@remote 'mkdir -p ~/.codex && cat > ~/.codex/auth.json' < ~/.codex/auth.json
```

#### Connecting through VPS or remote

If you run Codex on a remote machine (VPS/server) without a local browser, the login helper starts a server on `localhost:1455` on the remote host. To complete login in your local browser, forward that port to your machine before starting the login flow:

## Installation

```bash
npm install -g @just-every/code
```

Then run:
```bash
code
```

## Key Features

### 🎯 Core Capabilities
- **AI-powered coding agent** that runs locally in your terminal
- **File operations**: Read, write, and modify files with AI assistance
- **Command execution**: Run shell commands with intelligent context
- **Project understanding**: Analyzes your codebase to provide relevant suggestions

### ✨ Enhanced Features (Fork Additions)
- **Image Support**: Drag-and-drop, paste, or reference images directly in the terminal
- **Dynamic Reasoning**: Adjust AI reasoning effort mid-conversation with `/reasoning` command
- **Flexible Configuration**: Uses `~/.code` directory (or `~/.codex` for compatibility)

## Quick Start

### 1. Install and Launch
```bash
npm install -g @just-every/code
code
```

### 2. Authenticate
You can use Code CLI with:
- **ChatGPT Plus/Pro subscription** (recommended)
- **OpenAI API key** (usage-based billing)

### 3. Choose Autonomy Level

**Read/Write Mode** (default - full capabilities):
```bash
code
```

**Read-Only Mode** (safer, no file modifications):
```bash
code --read-only
```

## Example Usage

```bash
# Start an interactive session
code

# Run with a specific prompt
code "refactor this function to use async/await"

# Analyze code without making changes
code --read-only "explain the authentication flow"

# Adjust reasoning effort
/reasoning high  # More thorough analysis
/reasoning low   # Faster responses
```

## Common Tasks

- **Debug code**: "Why is this function returning null?"
- **Add features**: "Add error handling to all API calls"
- **Refactor**: "Convert this class to use TypeScript"
- **Explain**: "How does the authentication system work?"
- **Generate tests**: "Write unit tests for the user service"

## Configuration

Configuration file location: `~/.code/config.toml` (or set `CODE_HOME` environment variable)

### Basic Configuration

```toml
# Model selection
model = "gpt-5"

# Approval settings
approval_policy = "ask"  # ask | automatic | never

# Feature flags
hide_agent_reasoning = false
```

### Environment Variables

- `CODE_HOME` or `CODEX_HOME`: Override default config directory
- `OPENAI_API_KEY`: Use OpenAI API directly
- `OPENAI_BASE_URL`: Use alternative API endpoints

## Enhanced Image Support

The fork adds comprehensive image handling:

1. **Drag and drop** images directly into the terminal
2. **Paste** images from clipboard
3. **Reference** image files by path: "analyze screenshot.png"

## Using the /reasoning Command

Dynamically adjust the AI's reasoning effort:

```
/reasoning high   # Detailed analysis, slower
/reasoning medium # Balanced (default)
/reasoning low    # Quick responses
```

## System Requirements

- **Operating Systems**: macOS, Linux, Windows (WSL recommended)
- **Node.js**: Version 20 or higher
- **Terminal**: Modern terminal with UTF-8 support
- **Memory**: 4GB RAM minimum, 8GB recommended

## Command Line Options

```bash
code [options] [prompt]

Options:
  --model <name>     Override the model (e.g., gpt-5, claude-opus)
  --read-only        Prevent file modifications
  --no-approval      Skip approval prompts (use with caution)
  --config <key=val> Override config values
  --help            Show help information
  --version         Show version number
```

## Advanced Features

### Model Context Protocol (MCP)

Code CLI supports MCP for extended capabilities:
- File system operations
- Web browsing
- Database connections
- Custom tool integrations

### Project Documentation

Create a `CODEX.md` file in your project root to provide context:

```markdown
# Project Overview
This is a React application using TypeScript...

## Key Components
- Authentication: src/auth/*
- API Client: src/api/*
```

### Non-Interactive Mode

For CI/CD pipelines:

```bash
code --non-interactive "update all dependencies"
```

## Troubleshooting

### Common Issues

**Installation fails**: Ensure Node.js 20+ is installed
```bash
node --version  # Should be v20.0.0 or higher
```

**Authentication issues**: Re-run the login flow
```bash
code --login
```

**Performance problems**: Adjust model or reasoning level
```bash
code --model gpt-5-turbo
# or use /reasoning low during conversation
```

## Contributing

We welcome contributions! This fork maintains compatibility with upstream Codex while adding new features.

### Development Setup

```bash
# Clone the repository
git clone https://github.com/just-every/code.git
cd code

# Install dependencies
npm install

# Build the project
npm run build

# Run locally
./codex-rs/target/release/code
```

### Submitting Changes

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## Differences from Original Codex

| Feature | Code CLI (Fork) | Original Codex |
|---------|----------------|----------------|
| Command | `code` | `codex` |
| Config Directory | `~/.code` | `~/.codex` |
| NPM Package | `@just-every/code` | `@openai/codex` |
| Image Support | ✅ Enhanced | Basic |
| /reasoning Command | ✅ Available | Not available |
| Environment Variables | CODE_HOME + CODEX_HOME | CODEX_HOME only |

## License

Apache 2.0 - See [LICENSE](LICENSE) file for details.

## Acknowledgments

This project is a fork of [OpenAI Codex CLI](https://github.com/openai/codex). We maintain compatibility while adding community-requested features.

---

**Need help?** Open an issue on [GitHub](https://github.com/just-every/code/issues) or check the [original Codex documentation](https://github.com/openai/codex).