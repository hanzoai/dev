# frozen_string_literal: true

module Hanzo
  module Zap
    # Approval policy for tool execution (from hanzo-protocol)
    module ApprovalPolicy
      UNLESS_TRUSTED = "unless-trusted" # Only auto-approve known-safe reads
      ON_FAILURE = "on-failure"         # Auto-approve, escalate on failure
      ON_REQUEST = "on-request"         # Model decides (default)
      NEVER = "never"                   # Never ask
    end

    # Sandbox mode for tool execution
    module SandboxMode
      DANGER_FULL_ACCESS = "danger-full-access"
      READ_ONLY = "read-only"
      WORKSPACE_WRITE = "workspace-write"
    end

    # Wire protocol message types
    module MessageType
      INIT = 0x01
      INIT_ACK = 0x02

      LIST_TOOLS = 0x10
      LIST_TOOLS_RESPONSE = 0x11
      CALL_TOOL = 0x12
      CALL_TOOL_RESPONSE = 0x13

      LIST_RESOURCES = 0x20
      LIST_RESOURCES_RESPONSE = 0x21
      READ_RESOURCE = 0x22
      READ_RESOURCE_RESPONSE = 0x23

      LIST_PROMPTS = 0x30
      LIST_PROMPTS_RESPONSE = 0x31
      GET_PROMPT = 0x32
      GET_PROMPT_RESPONSE = 0x33

      PING = 0xF0
      PONG = 0xF1
      ERROR = 0xFF
    end

    # Tool definition
    Tool = Struct.new(:name, :description, :input_schema, keyword_init: true)

    # Tool call request
    ToolCall = Struct.new(:id, :name, :args, :metadata, keyword_init: true)

    # Tool execution result
    ToolResult = Struct.new(:id, :content, :error, :metadata, keyword_init: true)

    # Server info from handshake
    ServerInfo = Struct.new(:name, :version, :capabilities, keyword_init: true)

    # Client info for handshake
    ClientInfo = Struct.new(:name, :version, keyword_init: true)

    # Sandbox policy configuration
    SandboxPolicy = Struct.new(:mode, :writable_roots, :network_access, :allow_git_writes, keyword_init: true) do
      def self.danger_full_access
        new(mode: SandboxMode::DANGER_FULL_ACCESS)
      end

      def self.read_only
        new(mode: SandboxMode::READ_ONLY)
      end

      def self.workspace_write(writable_roots: [], network_access: false, allow_git_writes: false)
        new(
          mode: SandboxMode::WORKSPACE_WRITE,
          writable_roots: writable_roots,
          network_access: network_access,
          allow_git_writes: allow_git_writes
        )
      end
    end
  end
end
