#!/usr/bin/env ruby
# frozen_string_literal: true

# ZAP Agent Example - Ruby
#
# Demonstrates a complete ZAP agent with tool execution in Ruby.
#
# Usage:
#   gem install hanzo-tools
#   ruby agent.rb

require 'json'
require 'open3'
require 'pathname'

# Approval policy (matches hanzo-protocol)
module ApprovalPolicy
  UNTRUSTED = 'untrusted'
  ON_FAILURE = 'on-failure'
  ON_REQUEST = 'on-request'
  NEVER = 'never'
end

# Sandbox mode (matches hanzo-protocol)
module SandboxMode
  DANGER_FULL_ACCESS = 'danger-full-access'
  READ_ONLY = 'read-only'
  WORKSPACE_WRITE = 'workspace-write'
end

# Execution context
class ExecutorContext
  attr_accessor :cwd, :env, :session_id, :approval_policy, :sandbox_mode,
                :writable_roots, :network_access, :timeout_ms

  def initialize(cwd: '.')
    @cwd = Pathname.new(cwd).expand_path.to_s
    @env = ENV.to_h
    @session_id = "ruby-agent-#{Process.pid}"
    @approval_policy = ApprovalPolicy::ON_REQUEST
    @sandbox_mode = SandboxMode::WORKSPACE_WRITE
    @writable_roots = []
    @network_access = true
    @timeout_ms = 30_000
  end
end

# Tool result
class ToolResult
  attr_accessor :content, :error

  def initialize(content: nil, error: nil)
    @content = content
    @error = error
  end

  def to_h
    { content: @content, error: @error }
  end
end

# ZAP-compatible agent
class ZapAgent
  def initialize(cwd: '.')
    @ctx = ExecutorContext.new(cwd: cwd)
  end

  def execute(name, **args)
    case name
    when 'read_file'
      read_file(args[:path])
    when 'write_file'
      write_file(args[:path], args[:content])
    when 'list_dir'
      list_dir(args[:path] || '.')
    when 'git_status'
      git_status
    when 'git_log'
      git_log(args[:limit] || 10)
    when 'exec'
      exec_command(args[:command])
    else
      ToolResult.new(error: "Unknown tool: #{name}")
    end
  end

  def list_tools
    %w[
      read_file
      write_file
      list_dir
      git_status
      git_log
      exec
    ]
  end

  private

  def read_file(path)
    full_path = File.join(@ctx.cwd, path)
    content = File.read(full_path)
    ToolResult.new(content: content)
  rescue StandardError => e
    ToolResult.new(error: e.message)
  end

  def write_file(path, content)
    return ToolResult.new(error: 'Write not allowed in read-only mode') if @ctx.sandbox_mode == SandboxMode::READ_ONLY

    full_path = File.join(@ctx.cwd, path)
    File.write(full_path, content)
    ToolResult.new(content: { bytes_written: content.bytesize, path: full_path })
  rescue StandardError => e
    ToolResult.new(error: e.message)
  end

  def list_dir(path)
    full_path = File.join(@ctx.cwd, path)
    entries = Dir.entries(full_path).reject { |e| e.start_with?('.') }.map do |name|
      entry_path = File.join(full_path, name)
      {
        name: name,
        is_dir: File.directory?(entry_path),
        size: File.size(entry_path)
      }
    end
    ToolResult.new(content: entries)
  rescue StandardError => e
    ToolResult.new(error: e.message)
  end

  def git_status
    stdout, stderr, status = Open3.capture3('git', 'status', '--porcelain=v2', '--branch', chdir: @ctx.cwd)
    if status.success?
      ToolResult.new(content: stdout)
    else
      ToolResult.new(error: stderr)
    end
  rescue StandardError => e
    ToolResult.new(error: e.message)
  end

  def git_log(limit)
    stdout, stderr, status = Open3.capture3('git', 'log', "--format=%H|%an|%s", "-#{limit}", chdir: @ctx.cwd)
    if status.success?
      ToolResult.new(content: stdout)
    else
      ToolResult.new(error: stderr)
    end
  rescue StandardError => e
    ToolResult.new(error: e.message)
  end

  def exec_command(command)
    return ToolResult.new(error: 'Exec requires approval in untrusted mode') if @ctx.approval_policy == ApprovalPolicy::UNTRUSTED

    stdout, stderr, status = Open3.capture3(command, chdir: @ctx.cwd)
    if status.success?
      ToolResult.new(content: stdout)
    else
      ToolResult.new(content: stdout, error: "Exit code: #{status.exitstatus}\n#{stderr}")
    end
  rescue StandardError => e
    ToolResult.new(error: e.message)
  end
end

# Main
if __FILE__ == $PROGRAM_NAME
  puts 'ZAP Ruby Agent Example'
  puts '======================'
  puts

  agent = ZapAgent.new(cwd: '.')

  # List tools
  tools = agent.list_tools
  puts "Available tools (#{tools.length}):"
  tools.each { |t| puts "  - #{t}" }
  puts

  # Example 1: Read file
  puts 'Example 1: Read file'
  result = agent.execute('read_file', path: 'Cargo.toml')
  if result.error
    puts "  Error: #{result.error}"
  else
    puts "  Result: #{result.content.length} bytes read"
  end
  puts

  # Example 2: List directory
  puts 'Example 2: List directory'
  result = agent.execute('list_dir', path: '.')
  if result.error
    puts "  Error: #{result.error}"
  else
    preview = result.content.to_json[0..100]
    puts "  Result: #{preview}..."
  end
  puts

  # Example 3: Git status
  puts 'Example 3: Git status'
  result = agent.execute('git_status')
  if result.error
    puts "  Error: #{result.error}"
  else
    preview = result.content[0..100]
    puts "  Result: #{preview}..."
  end
end
