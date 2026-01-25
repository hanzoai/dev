defmodule HanzoZap.Types do
  @moduledoc """
  Type definitions for the ZAP protocol.

  This module defines all types used in ZAP communication including:
  - Tool categories and operations
  - Message types (request, response, notification)
  - Security policies (approval, sandbox)
  - Executor context
  """

  # Tool Categories
  # ZAP defines 14 tool categories with 167+ typed operations

  @typedoc "Filesystem operations: read_file, write_file, edit_file, glob, grep, list_dir"
  @type file_ops ::
          :read_file
          | :write_file
          | :edit_file
          | :glob
          | :grep
          | :list_dir
          | :mkdir
          | :rm
          | :mv
          | :cp

  @typedoc "Git/VCS operations: git_status, git_diff, git_commit, git_log, etc."
  @type git_ops ::
          :git_status
          | :git_diff
          | :git_commit
          | :git_log
          | :git_blame
          | :git_branch
          | :git_checkout
          | :git_merge
          | :git_rebase
          | :git_stash
          | :git_tag
          | :git_remote

  @typedoc "Process/command execution: exec, list_processes, kill_process"
  @type exec_ops ::
          :exec
          | :list_processes
          | :kill_process
          | :get_env
          | :set_env

  @typedoc "Build/test operations: build, test, lint, typecheck"
  @type build_ops ::
          :build
          | :test
          | :lint
          | :typecheck
          | :format
          | :clean

  @typedoc "Network operations: http_request, port_check, dns_lookup"
  @type network_ops ::
          :http_request
          | :fetch_url
          | :port_check
          | :dns_lookup
          | :ssh_exec

  @typedoc "Browser automation: navigate, click, fill, screenshot"
  @type browser_ops ::
          :navigate
          | :click
          | :fill
          | :query_selector
          | :browser_screenshot
          | :evaluate
          | :wait_for

  @typedoc "LSP/IDE operations: completion, definition, references, rename"
  @type lsp_ops ::
          :lsp_completion
          | :lsp_definition
          | :lsp_references
          | :lsp_rename
          | :lsp_hover
          | :lsp_signature
          | :lsp_format
          | :lsp_diagnostics

  @typedoc "Debugger operations: attach, breakpoint, step, inspect"
  @type debug_ops ::
          :debug_attach
          | :debug_breakpoint
          | :debug_step
          | :debug_continue
          | :debug_inspect
          | :debug_evaluate

  @typedoc "Container operations: docker, kubernetes"
  @type container_ops ::
          :docker_build
          | :docker_run
          | :docker_ps
          | :docker_logs
          | :kube_apply
          | :kube_get
          | :kube_describe
          | :kube_logs

  @typedoc "Cloud/IaC operations: terraform, pulumi, deploy"
  @type cloud_ops ::
          :iac_plan
          | :iac_apply
          | :secrets_get
          | :secrets_set
          | :deploy
          | :dns_record

  @typedoc "Database operations: query, migrate, backup"
  @type data_ops ::
          :db_query
          | :db_migrate
          | :db_backup
          | :cache_get
          | :cache_set
          | :cache_delete

  @typedoc "Security operations: scan, sign, verify"
  @type security_ops ::
          :secret_scan
          | :vuln_scan
          | :sign
          | :verify
          | :encrypt
          | :decrypt

  @typedoc "Vision/UI operations: ocr, detect_elements"
  @type vision_ops ::
          :screenshot
          | :detect_elements
          | :ocr
          | :describe_screen

  @typedoc "Planning operations: intent, routing, audit"
  @type plan_ops ::
          :plan_intent
          | :plan_route
          | :plan_compose
          | :audit_log

  @typedoc "All tool operation types"
  @type tool_op ::
          file_ops()
          | git_ops()
          | exec_ops()
          | build_ops()
          | network_ops()
          | browser_ops()
          | lsp_ops()
          | debug_ops()
          | container_ops()
          | cloud_ops()
          | data_ops()
          | security_ops()
          | vision_ops()
          | plan_ops()

  # Message Types

  @typedoc """
  ZAP message types with their byte codes.

  Protocol messages:
  - 0x00: Init
  - 0x01: InitAck

  Tool messages:
  - 0x10: ListTools
  - 0x11: ListToolsResponse
  - 0x12: CallTool
  - 0x13: CallToolResponse
  - 0x14: BatchCall
  - 0x15: BatchResponse

  Control messages:
  - 0xE0: Ping
  - 0xE1: Pong
  - 0xF0: Cancel
  - 0xF1: CancelAck
  - 0xFE: Error
  """
  @type msg_type ::
          :init
          | :init_ack
          | :list_tools
          | :list_tools_response
          | :call_tool
          | :call_tool_response
          | :batch_call
          | :batch_response
          | :ping
          | :pong
          | :cancel
          | :cancel_ack
          | :error

  @doc "Get byte code for message type"
  @spec msg_type_code(msg_type()) :: byte()
  def msg_type_code(:init), do: 0x00
  def msg_type_code(:init_ack), do: 0x01
  def msg_type_code(:list_tools), do: 0x10
  def msg_type_code(:list_tools_response), do: 0x11
  def msg_type_code(:call_tool), do: 0x12
  def msg_type_code(:call_tool_response), do: 0x13
  def msg_type_code(:batch_call), do: 0x14
  def msg_type_code(:batch_response), do: 0x15
  def msg_type_code(:ping), do: 0xE0
  def msg_type_code(:pong), do: 0xE1
  def msg_type_code(:cancel), do: 0xF0
  def msg_type_code(:cancel_ack), do: 0xF1
  def msg_type_code(:error), do: 0xFE

  @doc "Get message type from byte code"
  @spec msg_type_from_code(byte()) :: msg_type() | :unknown
  def msg_type_from_code(0x00), do: :init
  def msg_type_from_code(0x01), do: :init_ack
  def msg_type_from_code(0x10), do: :list_tools
  def msg_type_from_code(0x11), do: :list_tools_response
  def msg_type_from_code(0x12), do: :call_tool
  def msg_type_from_code(0x13), do: :call_tool_response
  def msg_type_from_code(0x14), do: :batch_call
  def msg_type_from_code(0x15), do: :batch_response
  def msg_type_from_code(0xE0), do: :ping
  def msg_type_from_code(0xE1), do: :pong
  def msg_type_from_code(0xF0), do: :cancel
  def msg_type_from_code(0xF1), do: :cancel_ack
  def msg_type_from_code(0xFE), do: :error
  def msg_type_from_code(_), do: :unknown

  # Request/Response Types

  @typedoc "Tool call request"
  @type tool_request :: %{
          required(:id) => String.t(),
          required(:name) => String.t(),
          optional(:arguments) => map(),
          optional(:metadata) => map()
        }

  @typedoc "Tool call result"
  @type tool_result :: %{
          required(:id) => String.t(),
          optional(:content) => term(),
          optional(:error) => String.t(),
          optional(:metadata) => map()
        }

  @typedoc "Tool definition from server"
  @type tool_definition :: %{
          required(:name) => String.t(),
          required(:description) => String.t(),
          optional(:input_schema) => map()
        }

  @typedoc "Server capabilities"
  @type capabilities :: %{
          optional(:tools) => boolean(),
          optional(:resources) => boolean(),
          optional(:prompts) => boolean(),
          optional(:logging) => boolean()
        }

  # Security Policy Types

  @typedoc """
  Approval policy - when to ask human for confirmation.

  - `:never` - Full autonomy (CI/CD pipelines)
  - `:on_failure` - Ask only when operations fail
  - `:on_request` - Model decides based on operation risk
  - `:unless_trusted` - Ask for everything except known-safe reads (default)
  """
  @type approval_policy :: :never | :on_failure | :on_request | :unless_trusted

  @typedoc """
  Sandbox policy - what operations are physically allowed.

  - `:danger_full_access` - Full filesystem, network, and process access
  - `:workspace_write` - Read anywhere, write only to workspace (default)
  - `:read_only` - Read only, no writes or network
  """
  @type sandbox_policy :: :danger_full_access | :workspace_write | :read_only

  @typedoc """
  Permission levels for tool operations.

  - `:read` - Information retrieval (read_file, git_status)
  - `:write` - Data modification (write_file, git_commit)
  - `:execute` - Process execution (exec, build)
  - `:admin` - System changes (kill_process)
  """
  @type permission_level :: :read | :write | :execute | :admin

  @typedoc "Permission check result"
  @type permission_result ::
          :allowed
          | {:requires_approval, String.t()}
          | {:denied, String.t()}

  # Executor Context

  @typedoc "Execution context for tool operations"
  @type executor_context :: %{
          cwd: String.t(),
          env: map(),
          session_id: String.t(),
          approval_policy: approval_policy(),
          sandbox_policy: sandbox_policy(),
          writable_roots: [String.t()],
          network_access: boolean(),
          timeout_ms: non_neg_integer()
        }

  @doc """
  Create an executor context with defaults.

  ## Options

  - `:cwd` - Working directory (default: ".")
  - `:env` - Environment variables (default: System.get_env())
  - `:session_id` - Session identifier (default: auto-generated)
  - `:approval_policy` - Approval policy (default: :on_request)
  - `:sandbox_policy` - Sandbox policy (default: :workspace_write)
  - `:writable_roots` - Additional writable directories (default: [])
  - `:network_access` - Allow network access (default: true)
  - `:timeout_ms` - Default timeout in ms (default: 30000)
  """
  @spec executor_context(keyword()) :: executor_context()
  def executor_context(opts \\ []) do
    %{
      cwd: Keyword.get(opts, :cwd, ".") |> Path.expand(),
      env: Keyword.get(opts, :env, System.get_env()),
      session_id: Keyword.get(opts, :session_id, generate_session_id()),
      approval_policy: Keyword.get(opts, :approval_policy, :on_request),
      sandbox_policy: Keyword.get(opts, :sandbox_policy, :workspace_write),
      writable_roots: Keyword.get(opts, :writable_roots, []),
      network_access: Keyword.get(opts, :network_access, true),
      timeout_ms: Keyword.get(opts, :timeout_ms, 30_000)
    }
  end

  defp generate_session_id do
    "elixir-#{:os.getpid()}-#{:erlang.unique_integer([:positive])}"
  end

  # Error Codes (JSON-RPC compatible)

  @typedoc "Standard ZAP error codes"
  @type error_code :: integer()

  @doc "Parse error - invalid JSON"
  @spec error_parse() :: error_code()
  def error_parse, do: -32700

  @doc "Invalid request structure"
  @spec error_invalid_request() :: error_code()
  def error_invalid_request, do: -32600

  @doc "Tool not found"
  @spec error_method_not_found() :: error_code()
  def error_method_not_found, do: -32601

  @doc "Invalid parameters"
  @spec error_invalid_params() :: error_code()
  def error_invalid_params, do: -32602

  @doc "Internal server error"
  @spec error_internal() :: error_code()
  def error_internal, do: -32603

  @doc "Request timeout"
  @spec error_timeout() :: error_code()
  def error_timeout, do: -32001

  @doc "Permission denied"
  @spec error_permission_denied() :: error_code()
  def error_permission_denied, do: -32002

  @doc "Request cancelled"
  @spec error_cancelled() :: error_code()
  def error_cancelled, do: -32003

  # Permission Classification

  @doc """
  Get permission level for a tool operation.
  """
  @spec operation_level(String.t()) :: permission_level()
  def operation_level(tool_name) do
    case tool_name do
      # Read operations
      name when name in ~w[read_file glob grep list_dir git_status git_diff git_log
                           git_blame list_processes get_env fetch_url port_check
                           dns_lookup cache_lookup plan_intent plan_route
                           plan_compose audit_log] ->
        :read

      # Write operations
      name when name in ~w[write_file edit_file git_commit git_branch mkdir rm mv cp] ->
        :write

      # Execute operations
      name when name in ~w[exec build test lint typecheck http_request docker_build
                           docker_run kube_apply db_query] ->
        :execute

      # Admin operations
      name when name in ~w[kill_process iac_apply] ->
        :admin

      # Default to execute (requires approval)
      _ ->
        :execute
    end
  end

  @doc """
  Check if an operation is allowed given the current context.
  """
  @spec check_permission(String.t(), executor_context()) :: permission_result()
  def check_permission(tool_name, ctx) do
    level = operation_level(tool_name)

    case {ctx.approval_policy, level} do
      {:never, _} ->
        :allowed

      {:on_failure, _} ->
        :allowed

      {:on_request, :read} ->
        :allowed

      {:on_request, _} ->
        {:requires_approval, "Operation '#{tool_name}' may require approval"}

      {:unless_trusted, :read} ->
        :allowed

      {:unless_trusted, _} ->
        {:requires_approval, "Operation '#{tool_name}' requires approval in untrusted mode"}
    end
  end

  @doc """
  Check if a path is writable given the sandbox policy.
  """
  @spec path_writable?(String.t(), executor_context()) :: boolean()
  def path_writable?(path, ctx) do
    canonical = Path.expand(path)

    case ctx.sandbox_policy do
      :danger_full_access ->
        true

      :read_only ->
        false

      :workspace_write ->
        cwd = Path.expand(ctx.cwd)

        String.starts_with?(canonical, cwd) or
          Enum.any?(ctx.writable_roots, fn root ->
            String.starts_with?(canonical, Path.expand(root))
          end)
    end
  end
end
