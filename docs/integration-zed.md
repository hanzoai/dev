# Using Code with Zed via ACP

The Rust MCP server now exposes the Agent Client Protocol (ACP) tools (`acp/new_session`, `acp/prompt`) that Zed expects. Zed still connects over MCP/JSON-RPC, but every conversation is represented through these ACP calls. This section walks through a minimal setup.

## 1. Configure Code's MCP server

Update your `CODEX_HOME/config.toml` (defaults to `~/.code/config.toml`) so Code knows which "client tools" Zed will handle and how to launch Zed's MCP endpoint:

```toml
[experimental_client_tools]
request_permission = { mcp_server = "zed", tool_name = "permission/request" }
read_text_file     = { mcp_server = "zed", tool_name = "fs/read_text_file" }
write_text_file    = { mcp_server = "zed", tool_name = "fs/write_text_file" }

[mcp_servers.zed]
command = "/Applications/Zed.app/Contents/MacOS/zed"  # adjust for your OS
args    = ["mcp", "--stdio"]
env     = {}
```

Any existing MCP servers can remain in this table; the exiting overrides simply add Zed as another entry.

## 2. Launch the MCP server under the new name

If you globally installed Code from npm (`npm install -g @just-every/code`), launch the MCP server with the built-in subcommand (`code mcp`; `code acp` is an equivalent alias and works well when `code` already points to VS Code):

```bash
code mcp
```

Prefer building from source? The previous workflow still works:

```bash
cargo run -p code-mcp-server -- --stdio
```

The server will advertise four tools during the handshake: `codex`, `codex-reply`, `acp/new_session`, and `acp/prompt`.

## 3. Point Zed at Code's MCP endpoint

Add an entry to Zed's `settings.json` under `agent_servers` (see [Zed’s external agents guide](https://zed.dev/docs/ai/external-agents#add-custom-agents)). The minimal configuration looks like:

```jsonc
{
  "agent_servers": {
    "Code": {
      "command": "coder",
      "args": ["acp"]
    }
  }
}
```

You can swap `coder` for an absolute path or `code` depending on your PATH preferences. Environment overrides such as `CODEX_HOME` or `RUST_LOG` are optional—set them only if you need a custom config directory or debug logging.

When Zed launches this server it connects over MCP, then issues ACP tool calls (`acp/new_session`, `acp/prompt`) that we expose. Those tool invocations are bridged into full Codex sessions, and we stream ACP `session_update` notifications back so Zed can render reasoning, tool executions, and approvals.

## 4. Permission flow summary

- File reads/writes go through the MCP tools you listed under `experimental_client_tools`.
- Explicit approvals (for shell commands or apply_patch) are raised via `permission/request`.
- Code retains confirm guards, sandbox policies, and validation harnesses; the server simply forwards the resulting events over ACP.

Once configured, Zed users can work with Code without any terminal interaction beyond starting `coder acp` (or `code mcp`, whichever alias you prefer).
