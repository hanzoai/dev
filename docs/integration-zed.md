# Using Code with Zed via ACP

The Rust MCP server now exposes the Agent Client Protocol (ACP) tools that Zed expects. This section walks through a minimal setup.

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

If you globally installed Code from npm (`npm install -g @just-every/code`), launch the MCP server with the built-in subcommand:

```bash
code mcp
```

Prefer building from source? The previous workflow still works:

```bash
cargo run -p code-mcp-server -- --stdio
```

The server will advertise four tools during the handshake: `codex`, `codex-reply`, `acp/new_session`, and `acp/prompt`.

## 3. Point Zed at Code's MCP endpoint

Add an entry to Zed's MCP configuration (for example, `~/.config/zed/mcp.json`):

```json
{
  "servers": {
    "code": {
      "command": "/path/to/code",
      "args": ["mcp"],
      "env": {
        "CODEX_HOME": "/Users/you/.code" ,
        "RUST_LOG": "info"
      }
    }
  }
}
```

When Zed starts the server it will discover the ACP tools, call `acp/new_session` to create a Codex conversation, and use `acp/prompt` for additional turns. Code streams `acp/session_update` notifications back to Zed, including exec/patch events and approval requests.

## 4. Permission flow summary

- File reads/writes go through the MCP tools you listed under `experimental_client_tools`.
- Explicit approvals (for shell commands or apply_patch) are raised via `permission/request`.
- Code retains confirm guards, sandbox policies, and validation harnesses; the server simply forwards the resulting events over ACP.

Once configured, Zed users can work with Code without any terminal interaction beyond starting `code mcp` (or `coder mcp` if the `code` alias is unavailable).
