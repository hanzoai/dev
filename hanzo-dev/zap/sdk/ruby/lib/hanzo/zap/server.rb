# frozen_string_literal: true

require "socket"
require "json"

module Hanzo
  module Zap
    # ZAP Server for hosting tools.
    #
    # @example
    #   server = Hanzo::Zap::Server.new(name: "my-tools", version: "1.0.0")
    #   server.register_tool("greet", "Greet someone") do |args|
    #     "Hello, #{args[:name]}!"
    #   end
    #   server.listen(9999)
    #
    class Server
      attr_reader :info

      def initialize(name:, version:)
        @info = ServerInfo.new(
          name: name,
          version: version,
          capabilities: { tools: true, resources: false, prompts: false }
        )
        @tools = {}
        @server = nil
      end

      # Register a tool.
      #
      # @param name [String] Tool name
      # @param description [String] Tool description
      # @param input_schema [Hash] JSON schema for input
      # @yield [args] Block to handle tool calls
      # @yieldparam args [Hash] Tool arguments
      # @yieldreturn [Object] Tool result
      def register_tool(name, description, input_schema = {}, &block)
        tool = Tool.new(name: name, description: description, input_schema: input_schema)
        @tools[name] = { tool: tool, handler: block }
      end

      # Start listening for connections.
      #
      # @param port [Integer] Port to listen on
      # @param host [String] Host to bind to
      def listen(port, host: "0.0.0.0")
        @server = TCPServer.new(host, port)

        loop do
          client = @server.accept
          Thread.new(client) { |c| handle_connection(c) }
        end
      end

      # Stop the server.
      def stop
        @server&.close
      end

      private

      def handle_connection(socket)
        loop do
          header = socket.read(5)
          break if header.nil? || header.bytesize < 5

          total_len, msg_type = header.unpack("VC")
          payload_len = total_len - 1
          payload = payload_len.positive? ? JSON.parse(socket.read(payload_len), symbolize_names: true) : {}

          handle_message(socket, msg_type, payload)
        end
      rescue StandardError
        # Connection error, close silently
      ensure
        socket.close
      end

      def handle_message(socket, msg_type, payload)
        case msg_type
        when MessageType::INIT
          send_message(socket, MessageType::INIT_ACK, info_to_hash)

        when MessageType::LIST_TOOLS
          tools = @tools.values.map { |t| tool_to_hash(t[:tool]) }
          send_message(socket, MessageType::LIST_TOOLS_RESPONSE, tools)

        when MessageType::CALL_TOOL
          result = execute_tool(payload)
          send_message(socket, MessageType::CALL_TOOL_RESPONSE, result_to_hash(result))

        when MessageType::PING
          send_message(socket, MessageType::PONG, {})

        else
          send_message(socket, MessageType::ERROR, { message: "Unknown message type: #{msg_type}" })
        end
      rescue StandardError => e
        send_message(socket, MessageType::ERROR, { message: e.message })
      end

      def execute_tool(call)
        name = call[:name]
        args = call[:args] || {}
        call_id = call[:id] || ""

        entry = @tools[name]
        return ToolResult.new(id: call_id, content: nil, error: "Unknown tool: #{name}") unless entry

        begin
          content = entry[:handler].call(args)
          ToolResult.new(id: call_id, content: content)
        rescue StandardError => e
          ToolResult.new(id: call_id, content: nil, error: e.message)
        end
      end

      def send_message(socket, type, payload)
        payload_bytes = payload.is_a?(Hash) || payload.is_a?(Array) ? JSON.generate(payload) : ""
        total_len = 1 + payload_bytes.bytesize

        header = [total_len, type].pack("VC")
        socket.write(header + payload_bytes)
      end

      def info_to_hash
        {
          name: @info.name,
          version: @info.version,
          capabilities: @info.capabilities
        }
      end

      def tool_to_hash(tool)
        {
          name: tool.name,
          description: tool.description,
          inputSchema: tool.input_schema
        }
      end

      def result_to_hash(result)
        hash = { id: result.id, content: result.content }
        hash[:error] = result.error if result.error
        hash
      end
    end
  end
end
