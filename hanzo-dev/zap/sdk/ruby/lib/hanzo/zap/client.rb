# frozen_string_literal: true

require "socket"
require "json"
require "uri"

module Hanzo
  module Zap
    # ZAP Client for connecting to ZAP servers.
    #
    # @example
    #   client = Hanzo::Zap::Client.connect("zap://localhost:9999")
    #   tools = client.list_tools
    #   result = client.call_tool("read_file", path: "README.md")
    #   client.close
    #
    class Client
      MAX_MESSAGE_SIZE = 16 * 1024 * 1024 # 16MB

      attr_reader :server_info

      def initialize(socket)
        @socket = socket
        @server_info = nil
        @request_id = 0
      end

      # Connect to a ZAP server.
      #
      # @param url [String] Server URL (zap:// or zaps:// for TLS)
      # @return [Client] Connected client instance
      def self.connect(url)
        uri = URI.parse(url)
        use_tls = uri.scheme == "zaps"
        host = uri.host || "localhost"
        port = uri.port || 9999

        socket = TCPSocket.new(host, port)
        if use_tls
          require "openssl"
          ctx = OpenSSL::SSL::SSLContext.new
          ctx.verify_mode = OpenSSL::SSL::VERIFY_NONE
          socket = OpenSSL::SSL::SSLSocket.new(socket, ctx)
          socket.connect
        end

        client = new(socket)
        client.send(:handshake)
        client
      end

      # List available tools.
      #
      # @return [Array<Tool>] List of tools
      def list_tools
        send_message(MessageType::LIST_TOOLS, {})
        _, payload = recv_message
        payload.map { |t| Tool.new(**transform_keys(t)) }
      end

      # Call a tool by name.
      #
      # @param name [String] Tool name
      # @param args [Hash] Tool arguments
      # @return [ToolResult] Tool result
      def call_tool(name, **args)
        @request_id += 1
        call = { id: "req-#{@request_id}", name: name, args: args }
        send_message(MessageType::CALL_TOOL, call)
        _, payload = recv_message
        ToolResult.new(**transform_keys(payload))
      end

      # Call multiple tools in a batch.
      #
      # @param calls [Array<Hash>] Array of {name:, args:} hashes
      # @return [Array<ToolResult>] Results
      def batch(calls)
        calls.map { |c| call_tool(c[:name], **c[:args]) }
      end

      # Send ping to check connection.
      def ping
        send_message(MessageType::PING, {})
        msg_type, = recv_message
        raise ProtocolError, "Expected PONG, got #{msg_type}" unless msg_type == MessageType::PONG
      end

      # Close the connection.
      def close
        @socket.close
      end

      private

      def handshake
        client_info = { name: "hanzo-zap", version: VERSION }
        send_message(MessageType::INIT, client_info)
        msg_type, payload = recv_message

        raise ProtocolError, "Expected INIT_ACK, got #{msg_type}" unless msg_type == MessageType::INIT_ACK

        @server_info = ServerInfo.new(**transform_keys(payload))
      end

      def send_message(type, payload)
        payload_bytes = payload.empty? ? "" : JSON.generate(payload)
        total_len = 1 + payload_bytes.bytesize

        header = [total_len, type].pack("VC")
        @socket.write(header + payload_bytes)
      end

      def recv_message
        header = @socket.read(5)
        raise ConnectionError, "Connection closed" if header.nil? || header.bytesize < 5

        total_len, msg_type = header.unpack("VC")
        raise ProtocolError, "Message too large: #{total_len}" if total_len > MAX_MESSAGE_SIZE

        payload_len = total_len - 1
        payload = payload_len.positive? ? JSON.parse(@socket.read(payload_len), symbolize_names: true) : {}

        raise ToolError, payload[:message] || "Server error" if msg_type == MessageType::ERROR

        [msg_type, payload]
      end

      def transform_keys(hash)
        hash.transform_keys { |k| k.to_s.gsub(/([a-z])([A-Z])/, '\1_\2').downcase.to_sym }
      end
    end
  end
end
