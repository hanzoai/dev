defmodule HanzoZap do
  @moduledoc """
  Zero-copy Agent Protocol (ZAP) - 1000x faster than MCP/JSON-RPC.

  ZAP is a binary protocol for AI agent tool calls that eliminates parsing
  and copying overhead. Messages are read directly from network buffers with
  zero allocation.

  ## Quick Start

      # Connect to a ZAP server
      {:ok, client} = HanzoZap.connect("zap://localhost:9999")

      # Call a tool
      {:ok, result} = HanzoZap.call_tool(client, "read_file", %{path: "src/main.rs"})
      IO.puts(result.content)

      # Disconnect
      :ok = HanzoZap.disconnect(client)

  ## Starting a Server

      # Define tool handlers
      handlers = %{
        "read_file" => fn args, _ctx -> File.read(args["path"]) end,
        "git_status" => fn _args, ctx -> System.cmd("git", ["status"], cd: ctx.cwd) end
      }

      # Start server
      {:ok, server} = HanzoZap.start_server(port: 9999, handlers: handlers)

  ## Features

  - **Zero-copy reads**: Access data directly from network buffers
  - **1000x faster**: Eliminates JSON parsing and base64 encoding
  - **MCP compatible**: Gateway bridges to existing MCP servers
  - **Security policies**: Built-in approval and sandbox policies
  - **14 tool categories**: 167+ typed operations
  """

  alias HanzoZap.{Client, Server, Types}

  @type url :: String.t()
  @type tool_name :: String.t()
  @type tool_args :: map()
  @type client :: pid()
  @type server :: pid()

  # Client API

  @doc """
  Connect to a ZAP server.

  ## Options

  - `:name` - Agent name for handshake (default: "elixir-agent")
  - `:version` - Agent version (default: "1.0.0")
  - `:timeout` - Connection timeout in ms (default: 5000)

  ## Examples

      {:ok, client} = HanzoZap.connect("zap://localhost:9999")
      {:ok, client} = HanzoZap.connect("zaps://secure.example.com:9999", name: "my-agent")
  """
  @spec connect(url(), keyword()) :: {:ok, client()} | {:error, term()}
  def connect(url, opts \\ []) do
    Client.start_link(url, opts)
  end

  @doc """
  Disconnect from a ZAP server.
  """
  @spec disconnect(client()) :: :ok
  def disconnect(client) do
    Client.stop(client)
  end

  @doc """
  Call a tool on the ZAP server.

  ## Examples

      {:ok, result} = HanzoZap.call_tool(client, "read_file", %{path: "README.md"})
      {:ok, result} = HanzoZap.call_tool(client, "exec", %{command: "ls -la"})
  """
  @spec call_tool(client(), tool_name(), tool_args(), keyword()) ::
          {:ok, Types.tool_result()} | {:error, term()}
  def call_tool(client, name, args \\ %{}, opts \\ []) do
    Client.call_tool(client, name, args, opts)
  end

  @doc """
  List available tools on the server.
  """
  @spec list_tools(client()) :: {:ok, [Types.tool_definition()]} | {:error, term()}
  def list_tools(client) do
    Client.list_tools(client)
  end

  @doc """
  Send a batch of tool calls.

  Returns results in the same order as requests.
  """
  @spec batch(client(), [{tool_name(), tool_args()}], keyword()) ::
          {:ok, [Types.tool_result()]} | {:error, term()}
  def batch(client, calls, opts \\ []) do
    Client.batch(client, calls, opts)
  end

  @doc """
  Ping the server to check connectivity.
  """
  @spec ping(client()) :: :pong | {:error, term()}
  def ping(client) do
    Client.ping(client)
  end

  # Server API

  @doc """
  Start a ZAP server.

  ## Options

  - `:port` - TCP port to listen on (default: 9999)
  - `:handlers` - Map of tool name to handler function
  - `:approval_policy` - Approval policy (default: :on_request)
  - `:sandbox_policy` - Sandbox policy (default: :workspace_write)
  - `:name` - Server name (default: "hanzo-zap-elixir")

  ## Examples

      handlers = %{
        "read_file" => fn args, ctx ->
          path = Path.join(ctx.cwd, args["path"])
          case File.read(path) do
            {:ok, content} -> {:ok, %{content: content}}
            {:error, reason} -> {:error, to_string(reason)}
          end
        end
      }

      {:ok, server} = HanzoZap.start_server(port: 9999, handlers: handlers)
  """
  @spec start_server(keyword()) :: {:ok, server()} | {:error, term()}
  def start_server(opts \\ []) do
    Server.start_link(opts)
  end

  @doc """
  Stop a ZAP server.
  """
  @spec stop_server(server()) :: :ok
  def stop_server(server) do
    Server.stop(server)
  end

  # Convenience functions

  @doc """
  Create an executor context with defaults.

  ## Examples

      ctx = HanzoZap.context(cwd: "/home/user/project")
      ctx = HanzoZap.context(approval_policy: :never, sandbox_policy: :read_only)
  """
  @spec context(keyword()) :: Types.executor_context()
  def context(opts \\ []) do
    Types.executor_context(opts)
  end
end
