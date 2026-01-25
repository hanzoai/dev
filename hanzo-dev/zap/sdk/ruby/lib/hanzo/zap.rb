# frozen_string_literal: true

require_relative "zap/version"
require_relative "zap/types"
require_relative "zap/client"
require_relative "zap/server"

module Hanzo
  # Zero-copy Agent Protocol (ZAP) SDK for Ruby
  #
  # 1000x faster than MCP/JSON-RPC through binary wire protocol.
  #
  # @example Client usage
  #   require "hanzo/zap"
  #
  #   client = Hanzo::Zap::Client.connect("zap://localhost:9999")
  #   tools = client.list_tools
  #   result = client.call_tool("read_file", path: "README.md")
  #   client.close
  #
  # @example Server usage
  #   server = Hanzo::Zap::Server.new(name: "my-tools", version: "1.0.0")
  #   server.register_tool("greet", "Greet someone") do |args|
  #     "Hello, #{args[:name]}!"
  #   end
  #   server.listen(9999)
  #
  module Zap
    class Error < StandardError; end
    class ConnectionError < Error; end
    class ProtocolError < Error; end
    class ToolError < Error; end
  end
end
