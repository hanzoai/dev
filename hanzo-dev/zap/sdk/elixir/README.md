# HanzoZap

[![Hex.pm](https://img.shields.io/hexpm/v/hanzo_zap.svg)](https://hex.pm/packages/hanzo_zap)
[![Documentation](https://img.shields.io/badge/docs-hexpm-blue.svg)](https://hexdocs.pm/hanzo_zap)

Zero-copy Agent Protocol (ZAP) client and server for Elixir. **1000x faster** than MCP/JSON-RPC.

## What is ZAP?

ZAP is a binary protocol for AI agent tool calls that eliminates parsing and copying overhead. Messages are read directly from network buffers with zero allocation.

| Metric | JSON-RPC (MCP) | ZAP | Improvement |
|--------|----------------|-----|-------------|
| Parse time | 4.6ms | 3.1us | **1,480x faster** |
| Allocations | 802 | 0 | **No GC pauses** |
| Wire size | 103KB | 63KB | **39% smaller** |

## Installation

Add `hanzo_zap` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:hanzo_zap, "~> 0.6.0"}
  ]
end
```

## Quick Start

### Client

```elixir
# Connect to a ZAP server
{:ok, client} = HanzoZap.connect("zap://localhost:9999")

# Call a tool
{:ok, result} = HanzoZap.call_tool(client, "read_file", %{path: "README.md"})
IO.puts(result.content)

# List available tools
{:ok, tools} = HanzoZap.list_tools(client)
Enum.each(tools, fn t -> IO.puts("  - #{t["name"]}") end)

# Batch multiple calls (pipelining)
{:ok, results} = HanzoZap.batch(client, [
  {"read_file", %{path: "mix.exs"}},
  {"read_file", %{path: "README.md"}},
  {"git_status", %{}}
])

# Disconnect
:ok = HanzoZap.disconnect(client)
```

### Server

```elixir
# Define tool handlers
handlers = %{
  "read_file" => fn args, ctx ->
    path = Path.join(ctx.cwd, args["path"])
    case File.read(path) do
      {:ok, content} -> {:ok, %{content: content, size: byte_size(content)}}
      {:error, reason} -> {:error, to_string(reason)}
    end
  end,

  "write_file" => fn args, ctx ->
    path = Path.join(ctx.cwd, args["path"])
    if HanzoZap.Types.path_writable?(path, ctx) do
      case File.write(path, args["content"]) do
        :ok -> {:ok, %{bytes_written: byte_size(args["content"])}}
        {:error, reason} -> {:error, to_string(reason)}
      end
    else
      {:error, "Path not writable"}
    end
  end,

  "git_status" => fn _args, ctx ->
    case System.cmd("git", ["status", "--porcelain=v2", "--branch"], cd: ctx.cwd) do
      {output, 0} -> {:ok, %{content: output}}
      {_, code} -> {:error, "git status failed with code #{code}"}
    end
  end,

  "exec" => fn args, ctx ->
    [cmd | cmd_args] = String.split(args["command"])
    case System.cmd(cmd, cmd_args, cd: ctx.cwd, stderr_to_stdout: true) do
      {output, 0} -> {:ok, %{stdout: output, exit_code: 0}}
      {output, code} -> {:ok, %{stdout: output, exit_code: code}}
    end
  end
}

# Start server
{:ok, server} = HanzoZap.start_server(
  port: 9999,
  handlers: handlers,
  approval_policy: :on_request,
  sandbox_policy: :workspace_write
)
```

## Security Policies

### Approval Policy

When should the agent ask for human confirmation?

```elixir
# Full autonomy (CI/CD)
ctx = HanzoZap.context(approval_policy: :never)

# Ask only on failures
ctx = HanzoZap.context(approval_policy: :on_failure)

# Model decides based on risk (default)
ctx = HanzoZap.context(approval_policy: :on_request)

# Ask for everything except reads
ctx = HanzoZap.context(approval_policy: :unless_trusted)
```

### Sandbox Policy

What operations are physically allowed?

```elixir
# Full access (dangerous!)
ctx = HanzoZap.context(sandbox_policy: :danger_full_access)

# Write only to workspace (default)
ctx = HanzoZap.context(sandbox_policy: :workspace_write)

# Read only, no writes
ctx = HanzoZap.context(sandbox_policy: :read_only)
```

## Tool Categories

ZAP defines 14 tool categories with 167+ typed operations:

| Category | Examples |
|----------|----------|
| **Filesystem** | `read_file`, `write_file`, `edit_file`, `glob`, `grep` |
| **VCS** | `git_status`, `git_diff`, `git_commit`, `git_log` |
| **Computer** | `exec`, `list_processes`, `kill_process` |
| **Build** | `build`, `test`, `lint`, `typecheck` |
| **Network** | `http_request`, `fetch_url`, `port_check` |
| **Browser** | `navigate`, `click`, `fill`, `screenshot` |
| **LSP** | `completion`, `definition`, `references`, `rename` |
| **Debug** | `breakpoint`, `step`, `inspect`, `profile` |
| **Container** | `docker_run`, `kube_apply` |
| **Cloud** | `deploy`, `secrets`, `dns` |
| **Data** | `query`, `migrate`, `backup` |
| **Security** | `scan`, `sign`, `verify` |
| **Vision** | `ocr`, `detect_ui`, `describe_screen` |
| **Plan** | `plan_intent`, `plan_route`, `audit_log` |

## Wire Format

ZAP uses a simple length-prefixed binary format:

```
+----------+----------+--------------------+
|  Length  | MsgType  |      Payload       |
| (4 bytes)| (1 byte) |     (variable)     |
|  LE u32  |          |                    |
+----------+----------+--------------------+
```

## Transport Schemes

| Scheme | Description |
|--------|-------------|
| `zap://` | Plain TCP (default port 9999) |
| `zaps://` | TLS 1.3 |

## License

MIT License - Copyright 2025 Hanzo AI Inc.

## Links

- [ZAP Documentation](https://zap.hanzo.ai)
- [GitHub](https://github.com/hanzoai/zap)
- [Hanzo AI](https://hanzo.ai)
