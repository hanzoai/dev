# Using Code with Zed via ACP

The Rust MCP server now exposes the Agent Client Protocol (ACP) primitives (`session/new`, `session/prompt`, plus streaming `session/update` notifications) that Zed expects. Zed still connects over MCP/JSON-RPC, but every conversation is represented through these ACP calls. This section walks through a minimal setup.

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

If you prefer a one-off launch without installing anything globally, run the MCP server via `npx`:

```bash
npx -y @just-every/code acp
# or pin to the latest dist-tag explicitly
npx -y @just-every/code@latest acp
```

Want a globally available binary instead? Install once (`npm install -g @just-every/code`) and then use the subcommand aliases (`code mcp`, `code acp`, or `coder acp`).

Prefer building from source? The previous workflow still works:

```bash
cargo run -p code-mcp-server -- --stdio
```

The server will advertise four tools during the handshake: `codex`, `codex-reply`, `session/new`, and `session/prompt`.

## 3. Point Zed at Code's MCP endpoint

Add an entry to Zed's `settings.json` under `agent_servers` (see [Zed’s external agents guide](https://zed.dev/docs/ai/external-agents#add-custom-agents)). The minimal configuration looks like:

```jsonc
{
  "agent_servers": {
    "Code": {
      "command": "npx",
      "args": ["-y", "@just-every/code", "acp"]
    }
  }
}
```

Pinning explicitly to the latest dist-tag works as well: replace "@just-every/code" with "@just-every/code@latest" in the `args` array. If you already have the CLI installed globally, swap in "coder" (or any absolute path) for the command and pass ["acp"] as the arguments. Environment overrides such as `CODEX_HOME` or `RUST_LOG` are optional—set them only if you need a custom config directory or debug logging.

When Zed launches this server it connects over MCP, then issues ACP tool calls (`session/new`, `session/prompt`) that we expose. Those tool invocations are bridged into full Codex sessions, and we stream ACP `session/update` notifications back so Zed can render reasoning, tool executions, and approvals. Zed can also send `session/cancel` to interrupt a running turn, which the server now honors by propagating an interrupt to Codex and replying with `stopReason: "cancelled"`.

## 4. Permission flow summary

- File reads/writes go through the MCP tools you listed under `experimental_client_tools`.
- Explicit approvals (for shell commands or apply_patch) are raised via `permission/request`.
- Code retains confirm guards, sandbox policies, and validation harnesses; the server simply forwards the resulting events over ACP.

Once configured, Zed users can work with Code without any terminal interaction beyond starting `coder acp` (or `code mcp`, whichever alias you prefer).
