defmodule HanzoZap.Server do
  @moduledoc """
  ZAP server GenServer for accepting client connections.

  Handles incoming connections, tool dispatch, permission checking,
  and response encoding.

  ## Usage

      handlers = %{
        "read_file" => fn args, ctx ->
          path = Path.join(ctx.cwd, args["path"])
          case File.read(path) do
            {:ok, content} -> {:ok, %{content: content, size: byte_size(content)}}
            {:error, reason} -> {:error, to_string(reason)}
          end
        end,
        "git_status" => fn _args, ctx ->
          case System.cmd("git", ["status", "--porcelain"], cd: ctx.cwd) do
            {output, 0} -> {:ok, %{content: output}}
            {_, code} -> {:error, "git status failed with code \#{code}"}
          end
        end
      }

      {:ok, server} = HanzoZap.Server.start_link(port: 9999, handlers: handlers)
  """

  use GenServer
  require Logger

  alias HanzoZap.Types

  @default_port 9999

  defmodule State do
    @moduledoc false
    defstruct [
      :listen_socket,
      :port,
      :handlers,
      :name,
      :version,
      :approval_policy,
      :sandbox_policy,
      :clients,
      :cwd
    ]
  end

  defmodule ClientState do
    @moduledoc false
    defstruct [
      :socket,
      :name,
      :version,
      :buffer,
      :ctx
    ]
  end

  # Public API

  @doc """
  Start a ZAP server.

  ## Options

  - `:port` - TCP port (default: 9999)
  - `:handlers` - Map of tool name to handler function
  - `:name` - Server name (default: "hanzo-zap-elixir")
  - `:version` - Server version (default: "0.6.0")
  - `:approval_policy` - Default approval policy (default: :on_request)
  - `:sandbox_policy` - Default sandbox policy (default: :workspace_write)
  - `:cwd` - Working directory (default: File.cwd!())
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Stop the server.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(server) do
    GenServer.stop(server, :normal)
  end

  @doc """
  Register a tool handler.
  """
  @spec register_handler(GenServer.server(), String.t(), function()) :: :ok
  def register_handler(server, name, handler) do
    GenServer.call(server, {:register_handler, name, handler})
  end

  @doc """
  Get list of registered tools.
  """
  @spec list_handlers(GenServer.server()) :: [String.t()]
  def list_handlers(server) do
    GenServer.call(server, :list_handlers)
  end

  # GenServer Callbacks

  @impl true
  def init(opts) do
    port = Keyword.get(opts, :port, @default_port)
    handlers = Keyword.get(opts, :handlers, %{})

    state = %State{
      port: port,
      handlers: handlers,
      name: Keyword.get(opts, :name, "hanzo-zap-elixir"),
      version: Keyword.get(opts, :version, "0.6.0"),
      approval_policy: Keyword.get(opts, :approval_policy, :on_request),
      sandbox_policy: Keyword.get(opts, :sandbox_policy, :workspace_write),
      cwd: Keyword.get(opts, :cwd, File.cwd!()),
      clients: %{}
    }

    case :gen_tcp.listen(port, [:binary, active: false, reuseaddr: true]) do
      {:ok, listen_socket} ->
        state = %{state | listen_socket: listen_socket}
        Logger.info("ZAP server listening on port #{port}")
        send(self(), :accept)
        {:ok, state}

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_call({:register_handler, name, handler}, _from, state) do
    handlers = Map.put(state.handlers, name, handler)
    {:reply, :ok, %{state | handlers: handlers}}
  end

  @impl true
  def handle_call(:list_handlers, _from, state) do
    {:reply, Map.keys(state.handlers), state}
  end

  @impl true
  def handle_info(:accept, state) do
    case :gen_tcp.accept(state.listen_socket, 100) do
      {:ok, client_socket} ->
        :inet.setopts(client_socket, active: true)

        client_state = %ClientState{
          socket: client_socket,
          buffer: <<>>,
          ctx:
            Types.executor_context(
              cwd: state.cwd,
              approval_policy: state.approval_policy,
              sandbox_policy: state.sandbox_policy
            )
        }

        clients = Map.put(state.clients, client_socket, client_state)
        send(self(), :accept)
        {:noreply, %{state | clients: clients}}

      {:error, :timeout} ->
        send(self(), :accept)
        {:noreply, state}

      {:error, :closed} ->
        {:stop, :normal, state}

      {:error, reason} ->
        Logger.error("Accept error: #{inspect(reason)}")
        send(self(), :accept)
        {:noreply, state}
    end
  end

  @impl true
  def handle_info({:tcp, socket, data}, state) do
    case Map.get(state.clients, socket) do
      nil ->
        {:noreply, state}

      client ->
        client = %{client | buffer: client.buffer <> data}
        {client, state} = process_client_buffer(client, state)
        clients = Map.put(state.clients, socket, client)
        {:noreply, %{state | clients: clients}}
    end
  end

  @impl true
  def handle_info({:tcp_closed, socket}, state) do
    clients = Map.delete(state.clients, socket)
    Logger.debug("Client disconnected")
    {:noreply, %{state | clients: clients}}
  end

  @impl true
  def handle_info({:tcp_error, socket, reason}, state) do
    Logger.warning("Client error: #{inspect(reason)}")
    clients = Map.delete(state.clients, socket)
    {:noreply, %{state | clients: clients}}
  end

  @impl true
  def terminate(_reason, state) do
    if state.listen_socket do
      :gen_tcp.close(state.listen_socket)
    end

    Enum.each(state.clients, fn {socket, _} ->
      :gen_tcp.close(socket)
    end)

    :ok
  end

  # Private Functions

  defp process_client_buffer(client, state) do
    case client.buffer do
      <<length::little-32, rest::binary>> when byte_size(rest) >= length ->
        <<msg_type::8, payload::binary-size(length - 1), remaining::binary>> = rest
        client = %{client | buffer: remaining}
        {client, state} = handle_client_message(client, msg_type, payload, state)
        process_client_buffer(client, state)

      _ ->
        {client, state}
    end
  end

  defp handle_client_message(client, 0x00, payload, state) do
    # Init message
    case Jason.decode(payload) do
      {:ok, %{"name" => name, "version" => version}} ->
        client = %{client | name: name, version: version}
        Logger.debug("Client connected: #{name} v#{version}")

        init_ack = %{
          name: state.name,
          version: state.version,
          capabilities: %{
            tools: true,
            resources: false,
            prompts: false,
            logging: true
          }
        }

        send_message(client.socket, :init_ack, init_ack)
        {client, state}

      {:error, _} ->
        send_error(client.socket, Types.error_parse(), "Invalid init message")
        {client, state}
    end
  end

  defp handle_client_message(client, 0x10, _payload, state) do
    # List tools
    tools =
      Enum.map(state.handlers, fn {name, _handler} ->
        %{
          name: name,
          description: "Tool: #{name}",
          inputSchema: %{type: "object"}
        }
      end)

    send_message(client.socket, :list_tools_response, tools)
    {client, state}
  end

  defp handle_client_message(client, 0x12, payload, state) do
    # Call tool
    case Jason.decode(payload) do
      {:ok, %{"id" => id, "name" => name} = request} ->
        args = Map.get(request, "arguments", %{})
        handle_tool_call(client, id, name, args, state)

      {:error, _} ->
        send_error(client.socket, Types.error_parse(), "Invalid request")
        {client, state}
    end
  end

  defp handle_client_message(client, 0x14, payload, state) do
    # Batch call
    case Jason.decode(payload) do
      {:ok, %{"id" => batch_id, "requests" => requests}} ->
        responses =
          Enum.map(requests, fn %{"id" => id, "name" => name} = req ->
            args = Map.get(req, "arguments", %{})

            case execute_tool(name, args, client.ctx, state) do
              {:ok, result} ->
                %{id: id, content: result}

              {:error, reason} ->
                %{id: id, error: reason}
            end
          end)

        send_message(client.socket, :batch_response, %{id: batch_id, responses: responses})
        {client, state}

      {:error, _} ->
        send_error(client.socket, Types.error_parse(), "Invalid batch request")
        {client, state}
    end
  end

  defp handle_client_message(client, 0xE0, _payload, state) do
    # Ping
    send_message(client.socket, :pong, <<>>)
    {client, state}
  end

  defp handle_client_message(client, _msg_type, _payload, state) do
    {client, state}
  end

  defp handle_tool_call(client, id, name, args, state) do
    # Check permission
    case Types.check_permission(name, client.ctx) do
      :allowed ->
        case execute_tool(name, args, client.ctx, state) do
          {:ok, result} ->
            response = %{id: id, content: result}
            send_message(client.socket, :call_tool_response, response)

          {:error, reason} ->
            response = %{id: id, error: reason}
            send_message(client.socket, :call_tool_response, response)
        end

      {:requires_approval, reason} ->
        # In a real implementation, this would trigger approval flow
        Logger.warning("Operation requires approval: #{reason}")
        response = %{id: id, error: "Approval required: #{reason}"}
        send_message(client.socket, :call_tool_response, response)

      {:denied, reason} ->
        response = %{id: id, error: "Permission denied: #{reason}"}
        send_message(client.socket, :call_tool_response, response)
    end

    {client, state}
  end

  defp execute_tool(name, args, ctx, state) do
    case Map.get(state.handlers, name) do
      nil ->
        {:error, "Tool not found: #{name}"}

      handler when is_function(handler, 2) ->
        try do
          case handler.(args, ctx) do
            {:ok, result} -> {:ok, result}
            {:error, reason} -> {:error, to_string(reason)}
            result -> {:ok, result}
          end
        rescue
          e -> {:error, Exception.message(e)}
        catch
          :exit, reason -> {:error, "Handler exited: #{inspect(reason)}"}
        end

      handler when is_function(handler, 1) ->
        try do
          case handler.(args) do
            {:ok, result} -> {:ok, result}
            {:error, reason} -> {:error, to_string(reason)}
            result -> {:ok, result}
          end
        rescue
          e -> {:error, Exception.message(e)}
        catch
          :exit, reason -> {:error, "Handler exited: #{inspect(reason)}"}
        end
    end
  end

  defp send_message(socket, msg_type, payload) do
    msg_code = Types.msg_type_code(msg_type)
    json_payload = if is_binary(payload), do: payload, else: Jason.encode!(payload)
    length = byte_size(json_payload) + 1

    frame = <<length::little-32, msg_code::8, json_payload::binary>>
    :gen_tcp.send(socket, frame)
  end

  defp send_error(socket, code, message) do
    error = %{code: code, message: message}
    send_message(socket, :error, error)
  end
end
