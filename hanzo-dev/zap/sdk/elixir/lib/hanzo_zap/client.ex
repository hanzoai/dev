defmodule HanzoZap.Client do
  @moduledoc """
  ZAP client GenServer for connecting to ZAP servers.

  Handles connection lifecycle, request/response matching, pipelining,
  and automatic reconnection.

  ## Usage

      {:ok, client} = HanzoZap.Client.start_link("zap://localhost:9999")
      {:ok, result} = HanzoZap.Client.call_tool(client, "read_file", %{path: "README.md"})
      :ok = HanzoZap.Client.stop(client)
  """

  use GenServer
  require Logger

  alias HanzoZap.Types

  @default_port 9999
  @connect_timeout 5_000
  @request_timeout 30_000

  # Client state
  defmodule State do
    @moduledoc false
    defstruct [
      :socket,
      :host,
      :port,
      :tls,
      :name,
      :version,
      :server_capabilities,
      :buffer,
      :pending_requests,
      :request_counter
    ]
  end

  # Public API

  @doc """
  Start a ZAP client and connect to the server.

  ## Options

  - `:name` - Agent name (default: "elixir-agent")
  - `:version` - Agent version (default: "1.0.0")
  - `:timeout` - Connection timeout in ms (default: 5000)
  """
  @spec start_link(String.t(), keyword()) :: GenServer.on_start()
  def start_link(url, opts \\ []) do
    GenServer.start_link(__MODULE__, {url, opts})
  end

  @doc """
  Stop the client and close the connection.
  """
  @spec stop(GenServer.server()) :: :ok
  def stop(client) do
    GenServer.stop(client, :normal)
  end

  @doc """
  Call a tool on the server.

  ## Options

  - `:timeout` - Request timeout in ms (default: 30000)
  - `:metadata` - Additional metadata to include
  """
  @spec call_tool(GenServer.server(), String.t(), map(), keyword()) ::
          {:ok, Types.tool_result()} | {:error, term()}
  def call_tool(client, name, args \\ %{}, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @request_timeout)
    metadata = Keyword.get(opts, :metadata, %{})

    GenServer.call(client, {:call_tool, name, args, metadata}, timeout)
  end

  @doc """
  List available tools on the server.
  """
  @spec list_tools(GenServer.server()) :: {:ok, [Types.tool_definition()]} | {:error, term()}
  def list_tools(client) do
    GenServer.call(client, :list_tools, @request_timeout)
  end

  @doc """
  Send a batch of tool calls.
  """
  @spec batch(GenServer.server(), [{String.t(), map()}], keyword()) ::
          {:ok, [Types.tool_result()]} | {:error, term()}
  def batch(client, calls, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, @request_timeout)
    GenServer.call(client, {:batch, calls}, timeout)
  end

  @doc """
  Ping the server.
  """
  @spec ping(GenServer.server()) :: :pong | {:error, term()}
  def ping(client) do
    GenServer.call(client, :ping, 5_000)
  end

  # GenServer Callbacks

  @impl true
  def init({url, opts}) do
    case parse_url(url) do
      {:ok, host, port, tls} ->
        state = %State{
          host: host,
          port: port,
          tls: tls,
          name: Keyword.get(opts, :name, "elixir-agent"),
          version: Keyword.get(opts, :version, "1.0.0"),
          buffer: <<>>,
          pending_requests: %{},
          request_counter: 0
        }

        timeout = Keyword.get(opts, :timeout, @connect_timeout)

        case connect(state, timeout) do
          {:ok, state} -> {:ok, state}
          {:error, reason} -> {:stop, reason}
        end

      {:error, reason} ->
        {:stop, reason}
    end
  end

  @impl true
  def handle_call({:call_tool, name, args, metadata}, from, state) do
    {request_id, state} = next_request_id(state)

    request = %{
      id: request_id,
      name: name,
      arguments: args,
      metadata: metadata
    }

    case send_message(state.socket, :call_tool, request, state.tls) do
      :ok ->
        state = %{state | pending_requests: Map.put(state.pending_requests, request_id, from)}
        {:noreply, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call(:list_tools, from, state) do
    {request_id, state} = next_request_id(state)

    case send_message(state.socket, :list_tools, %{id: request_id}, state.tls) do
      :ok ->
        state = %{
          state
          | pending_requests: Map.put(state.pending_requests, "list_tools", from)
        }

        {:noreply, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:batch, calls}, from, state) do
    {request_id, state} = next_request_id(state)

    requests =
      Enum.with_index(calls, fn {name, args}, idx ->
        %{id: "#{request_id}-#{idx}", name: name, arguments: args}
      end)

    batch_request = %{id: request_id, requests: requests}

    case send_message(state.socket, :batch_call, batch_request, state.tls) do
      :ok ->
        state = %{
          state
          | pending_requests: Map.put(state.pending_requests, request_id, {:batch, from, length(calls)})
        }

        {:noreply, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call(:ping, from, state) do
    case send_message(state.socket, :ping, <<>>, state.tls) do
      :ok ->
        state = %{state | pending_requests: Map.put(state.pending_requests, "ping", from)}
        {:noreply, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_info({:tcp, socket, data}, %{socket: socket} = state) do
    state = %{state | buffer: state.buffer <> data}
    state = process_buffer(state)
    {:noreply, state}
  end

  @impl true
  def handle_info({:ssl, socket, data}, %{socket: socket} = state) do
    state = %{state | buffer: state.buffer <> data}
    state = process_buffer(state)
    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp_closed, _socket}, state) do
    Logger.warning("ZAP connection closed")
    {:stop, :connection_closed, state}
  end

  @impl true
  def handle_info({:ssl_closed, _socket}, state) do
    Logger.warning("ZAP TLS connection closed")
    {:stop, :connection_closed, state}
  end

  @impl true
  def handle_info({:tcp_error, _socket, reason}, state) do
    Logger.error("ZAP connection error: #{inspect(reason)}")
    {:stop, {:connection_error, reason}, state}
  end

  @impl true
  def handle_info({:ssl_error, _socket, reason}, state) do
    Logger.error("ZAP TLS connection error: #{inspect(reason)}")
    {:stop, {:connection_error, reason}, state}
  end

  @impl true
  def terminate(_reason, state) do
    if state.socket do
      if state.tls do
        :ssl.close(state.socket)
      else
        :gen_tcp.close(state.socket)
      end
    end

    :ok
  end

  # Private Functions

  defp parse_url(url) do
    uri = URI.parse(url)

    case uri.scheme do
      "zap" ->
        {:ok, uri.host || "localhost", uri.port || @default_port, false}

      "zaps" ->
        {:ok, uri.host || "localhost", uri.port || @default_port, true}

      "zap+unix" ->
        {:error, :unix_socket_not_supported}

      _ ->
        {:error, {:invalid_scheme, uri.scheme}}
    end
  end

  defp connect(state, timeout) do
    opts = [:binary, active: true, packet: :raw]

    result =
      if state.tls do
        ssl_opts = opts ++ [verify: :verify_peer, cacerts: :public_key.cacerts_get()]
        :ssl.connect(to_charlist(state.host), state.port, ssl_opts, timeout)
      else
        :gen_tcp.connect(to_charlist(state.host), state.port, opts, timeout)
      end

    case result do
      {:ok, socket} ->
        state = %{state | socket: socket}
        do_handshake(state)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_handshake(state) do
    init_msg = %{name: state.name, version: state.version}

    case send_message(state.socket, :init, init_msg, state.tls) do
      :ok ->
        case receive_init_ack(state) do
          {:ok, capabilities} ->
            {:ok, %{state | server_capabilities: capabilities}}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp receive_init_ack(state) do
    receive do
      {:tcp, _socket, data} ->
        parse_init_ack(data)

      {:ssl, _socket, data} ->
        parse_init_ack(data)

      {:tcp_error, _socket, reason} ->
        {:error, reason}

      {:ssl_error, _socket, reason} ->
        {:error, reason}
    after
      @connect_timeout ->
        {:error, :handshake_timeout}
    end
  end

  defp parse_init_ack(data) do
    case data do
      <<_length::little-32, 0x01, payload::binary>> ->
        case Jason.decode(payload) do
          {:ok, %{"capabilities" => caps}} -> {:ok, caps}
          {:ok, _} -> {:ok, %{}}
          {:error, reason} -> {:error, {:json_error, reason}}
        end

      <<_length::little-32, 0xFE, payload::binary>> ->
        case Jason.decode(payload) do
          {:ok, error} -> {:error, {:server_error, error}}
          {:error, _} -> {:error, :handshake_failed}
        end

      _ ->
        {:error, :invalid_init_ack}
    end
  end

  defp send_message(socket, msg_type, payload, tls) do
    msg_code = Types.msg_type_code(msg_type)
    json_payload = if is_binary(payload), do: payload, else: Jason.encode!(payload)
    length = byte_size(json_payload) + 1

    frame = <<length::little-32, msg_code::8, json_payload::binary>>

    if tls do
      :ssl.send(socket, frame)
    else
      :gen_tcp.send(socket, frame)
    end
  end

  defp process_buffer(state) do
    case state.buffer do
      <<length::little-32, rest::binary>> when byte_size(rest) >= length ->
        <<msg_type::8, payload::binary-size(length - 1), remaining::binary>> = rest
        state = %{state | buffer: remaining}
        state = handle_message(Types.msg_type_from_code(msg_type), payload, state)
        process_buffer(state)

      _ ->
        state
    end
  end

  defp handle_message(:call_tool_response, payload, state) do
    case Jason.decode(payload) do
      {:ok, response} ->
        request_id = response["id"]

        case Map.pop(state.pending_requests, request_id) do
          {nil, pending} ->
            Logger.warning("Received response for unknown request: #{request_id}")
            %{state | pending_requests: pending}

          {from, pending} ->
            result = %{
              id: request_id,
              content: response["content"],
              error: response["error"],
              metadata: response["metadata"]
            }

            GenServer.reply(from, {:ok, result})
            %{state | pending_requests: pending}
        end

      {:error, reason} ->
        Logger.error("Failed to parse response: #{inspect(reason)}")
        state
    end
  end

  defp handle_message(:list_tools_response, payload, state) do
    case Map.pop(state.pending_requests, "list_tools") do
      {nil, pending} ->
        %{state | pending_requests: pending}

      {from, pending} ->
        case Jason.decode(payload) do
          {:ok, tools} when is_list(tools) ->
            GenServer.reply(from, {:ok, tools})

          {:ok, %{"tools" => tools}} ->
            GenServer.reply(from, {:ok, tools})

          {:error, reason} ->
            GenServer.reply(from, {:error, {:json_error, reason}})
        end

        %{state | pending_requests: pending}
    end
  end

  defp handle_message(:batch_response, payload, state) do
    case Jason.decode(payload) do
      {:ok, %{"id" => request_id, "responses" => responses}} ->
        case Map.pop(state.pending_requests, request_id) do
          {{:batch, from, _count}, pending} ->
            results =
              Enum.map(responses, fn resp ->
                %{
                  id: resp["id"],
                  content: resp["content"],
                  error: resp["error"],
                  metadata: resp["metadata"]
                }
              end)

            GenServer.reply(from, {:ok, results})
            %{state | pending_requests: pending}

          {nil, pending} ->
            Logger.warning("Received batch response for unknown request: #{request_id}")
            %{state | pending_requests: pending}
        end

      {:error, reason} ->
        Logger.error("Failed to parse batch response: #{inspect(reason)}")
        state
    end
  end

  defp handle_message(:pong, _payload, state) do
    case Map.pop(state.pending_requests, "ping") do
      {nil, pending} ->
        %{state | pending_requests: pending}

      {from, pending} ->
        GenServer.reply(from, :pong)
        %{state | pending_requests: pending}
    end
  end

  defp handle_message(:error, payload, state) do
    case Jason.decode(payload) do
      {:ok, error} ->
        Logger.error("Server error: #{inspect(error)}")

        # Reply to all pending requests with error
        Enum.each(state.pending_requests, fn
          {_id, {:batch, from, _}} ->
            GenServer.reply(from, {:error, error})

          {_id, from} when is_tuple(from) ->
            GenServer.reply(from, {:error, error})

          _ ->
            :ok
        end)

        %{state | pending_requests: %{}}

      {:error, reason} ->
        Logger.error("Failed to parse error: #{inspect(reason)}")
        state
    end
  end

  defp handle_message(msg_type, _payload, state) do
    Logger.debug("Received unhandled message type: #{inspect(msg_type)}")
    state
  end

  defp next_request_id(state) do
    id = state.request_counter + 1
    {"req-#{id}", %{state | request_counter: id}}
  end
end
