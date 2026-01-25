#!/usr/bin/env elixir
# ZAP Agent Example - Elixir
#
# Demonstrates a complete ZAP agent with tool execution in Elixir.
#
# Usage:
#   elixir agent.exs

defmodule ApprovalPolicy do
  @moduledoc "Approval policy enum (matches hanzo-protocol)"

  def untrusted, do: :untrusted
  def on_failure, do: :on_failure
  def on_request, do: :on_request
  def never, do: :never
end

defmodule SandboxMode do
  @moduledoc "Sandbox mode enum (matches hanzo-protocol)"

  def danger_full_access, do: :danger_full_access
  def read_only, do: :read_only
  def workspace_write, do: :workspace_write
end

defmodule ExecutorContext do
  @moduledoc "Execution context for ZAP agent"

  defstruct [
    :cwd,
    :env,
    :session_id,
    :approval_policy,
    :sandbox_mode,
    :writable_roots,
    :network_access,
    :timeout_ms
  ]

  def new(cwd \\ ".") do
    %__MODULE__{
      cwd: Path.expand(cwd),
      env: System.get_env(),
      session_id: "elixir-agent-#{:os.getpid()}",
      approval_policy: ApprovalPolicy.on_request(),
      sandbox_mode: SandboxMode.workspace_write(),
      writable_roots: [],
      network_access: true,
      timeout_ms: 30_000
    }
  end
end

defmodule ToolResult do
  @moduledoc "Tool execution result"

  defstruct [:content, :error]

  def ok(content), do: %__MODULE__{content: content, error: nil}
  def error(message), do: %__MODULE__{content: nil, error: message}
end

defmodule ZapAgent do
  @moduledoc "ZAP-compatible agent for Elixir"

  defstruct [:ctx]

  def new(cwd \\ ".") do
    %__MODULE__{ctx: ExecutorContext.new(cwd)}
  end

  def execute(%__MODULE__{ctx: ctx} = _agent, name, args \\ %{}) do
    case name do
      "read_file" -> read_file(ctx, args[:path])
      "write_file" -> write_file(ctx, args[:path], args[:content])
      "list_dir" -> list_dir(ctx, args[:path] || ".")
      "git_status" -> git_status(ctx)
      "git_log" -> git_log(ctx, args[:limit] || 10)
      "exec" -> exec_command(ctx, args[:command])
      _ -> ToolResult.error("Unknown tool: #{name}")
    end
  end

  def list_tools do
    ~w[read_file write_file list_dir git_status git_log exec]
  end

  # Private implementations

  defp read_file(ctx, path) do
    full_path = Path.join(ctx.cwd, path)

    case File.read(full_path) do
      {:ok, content} -> ToolResult.ok(content)
      {:error, reason} -> ToolResult.error("#{reason}")
    end
  end

  defp write_file(ctx, path, content) do
    if ctx.sandbox_mode == SandboxMode.read_only() do
      ToolResult.error("Write not allowed in read-only mode")
    else
      full_path = Path.join(ctx.cwd, path)

      case File.write(full_path, content) do
        :ok ->
          ToolResult.ok(%{bytes_written: byte_size(content), path: full_path})

        {:error, reason} ->
          ToolResult.error("#{reason}")
      end
    end
  end

  defp list_dir(ctx, path) do
    full_path = Path.join(ctx.cwd, path)

    case File.ls(full_path) do
      {:ok, entries} ->
        result =
          entries
          |> Enum.reject(&String.starts_with?(&1, "."))
          |> Enum.map(fn name ->
            entry_path = Path.join(full_path, name)

            %{
              name: name,
              is_dir: File.dir?(entry_path),
              size: File.stat!(entry_path).size
            }
          end)

        ToolResult.ok(result)

      {:error, reason} ->
        ToolResult.error("#{reason}")
    end
  end

  defp git_status(ctx) do
    case System.cmd("git", ["status", "--porcelain=v2", "--branch"], cd: ctx.cwd) do
      {output, 0} -> ToolResult.ok(output)
      {_, code} -> ToolResult.error("Git status failed with code #{code}")
    end
  rescue
    e -> ToolResult.error(Exception.message(e))
  end

  defp git_log(ctx, limit) do
    case System.cmd("git", ["log", "--format=%H|%an|%s", "-#{limit}"], cd: ctx.cwd) do
      {output, 0} -> ToolResult.ok(output)
      {_, code} -> ToolResult.error("Git log failed with code #{code}")
    end
  rescue
    e -> ToolResult.error(Exception.message(e))
  end

  defp exec_command(ctx, command) do
    if ctx.approval_policy == ApprovalPolicy.untrusted() do
      ToolResult.error("Exec requires approval in untrusted mode")
    else
      case System.cmd("sh", ["-c", command], cd: ctx.cwd) do
        {output, 0} -> ToolResult.ok(output)
        {output, code} -> %ToolResult{content: output, error: "Exit code: #{code}"}
      end
    end
  rescue
    e -> ToolResult.error(Exception.message(e))
  end
end

# Main
IO.puts("ZAP Elixir Agent Example")
IO.puts("========================\n")

agent = ZapAgent.new(".")

# List tools
tools = ZapAgent.list_tools()
IO.puts("Available tools (#{length(tools)}):")
Enum.each(tools, fn t -> IO.puts("  - #{t}") end)
IO.puts("")

# Example 1: Read file
IO.puts("Example 1: Read file")
result = ZapAgent.execute(agent, "read_file", path: "Cargo.toml")

if result.error do
  IO.puts("  Error: #{result.error}")
else
  IO.puts("  Result: #{String.length(result.content)} bytes read")
end

IO.puts("")

# Example 2: List directory
IO.puts("Example 2: List directory")
result = ZapAgent.execute(agent, "list_dir", path: ".")

if result.error do
  IO.puts("  Error: #{result.error}")
else
  preview = result.content |> inspect() |> String.slice(0..100)
  IO.puts("  Result: #{preview}...")
end

IO.puts("")

# Example 3: Git status
IO.puts("Example 3: Git status")
result = ZapAgent.execute(agent, "git_status")

if result.error do
  IO.puts("  Error: #{result.error}")
else
  preview = String.slice(result.content, 0..100)
  IO.puts("  Result: #{preview}...")
end
