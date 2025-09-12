# MCP Integration Architecture for Hanzo Dev

## Vision
Create a unified Rust-based MCP implementation that consolidates all tooling from `hanzo/mcp` and `hanzo/ide` into the main `dev` project, following the principle of "one and exactly ONE way to do everything."

## Current State Analysis

### Hanzo MCP (TypeScript)
- **20+ MCP tools** in TypeScript
- File operations, search, shell, editing tools
- Background process management
- Claude Desktop integration
- Dynamic system prompt generation

### Hanzo IDE (Python/TypeScript)
- **70+ MCP tools** with advanced capabilities
- Web-based IDE interface
- Multi-agent orchestration (CodeAct, RepoExplorer, Browsing)
- Docker/remote runtime environments
- ACI (Agent Computer Interface) with AST manipulation

### Hanzo Dev (Rust)
- CLI-based development assistant
- Basic MCP client/server implementation
- TUI with Ratatui
- Limited tool set compared to other projects

## Proposed Architecture

### Core Principles
1. **Single Implementation**: Each tool exists in exactly ONE place
2. **MCP-First**: All functionality exposed as MCP tools
3. **Composable**: Tools can be orchestrated by multiple agents
4. **Sandboxing Optional**: Security features can be completely disabled
5. **Rust-Native**: Performance and safety through Rust implementation

### Architectural Design

```
hanzo/dev/
├── src/rs/
│   ├── mcp-tools/           # New: Unified MCP tool implementations
│   │   ├── file/            # File operations (20+ tools)
│   │   ├── search/          # Search and pattern matching
│   │   ├── shell/           # Command execution
│   │   ├── edit/            # File editing
│   │   ├── git/             # Version control
│   │   ├── ast/             # Code analysis (from ACI)
│   │   ├── browser/         # Web interaction
│   │   ├── ai/              # Agent orchestration
│   │   ├── project/         # Project intelligence
│   │   └── registry.rs      # Tool registration system
│   ├── mcp-runtime/         # New: MCP execution environment
│   │   ├── sandbox.rs       # Optional sandboxing (can be disabled)
│   │   ├── executor.rs      # Tool execution engine
│   │   ├── session.rs       # Session management
│   │   └── transport.rs     # Multiple transport support
│   └── agents/              # New: Agent orchestration
│       ├── codeact.rs       # Code action agent
│       ├── explorer.rs      # Repository explorer
│       ├── browser.rs       # Web browsing agent
│       └── orchestrator.rs  # Multi-agent coordination
```

## Feature Migration Plan

### Phase 1: Core MCP Tools (from hanzo/mcp)
Implement in Rust the 20+ essential tools:

#### File Operations
- [x] read_file → Already exists as Read tool
- [ ] write_file → Enhance existing Write tool
- [ ] list_files → Implement with glob support
- [ ] create_file → Add overwrite protection
- [ ] delete_file → Add recursive option
- [ ] move_file → Implement with safety checks
- [ ] get_file_info → Add metadata retrieval
- [ ] directory_tree → Visual tree generation

#### Search Tools
- [x] grep → Exists, needs ripgrep integration
- [ ] find_files → Pattern-based file search
- [ ] search → Unified search combining strategies

#### Shell Tools
- [x] bash → Exists as Bash tool
- [ ] run_background → Background process management
- [ ] list_processes → Process tracking
- [ ] get_process_output → Output retrieval
- [ ] kill_process → Process termination

#### Edit Tools
- [x] edit_file → Exists as Edit tool
- [ ] multi_edit → Atomic multiple edits

### Phase 2: Advanced Tools (from hanzo/ide)
Port the additional 50+ tools from IDE:

#### Code Intelligence (from ACI)
- [ ] ast_search → Tree-sitter based search
- [ ] ast_edit → Structural code editing
- [ ] get_symbols → Symbol extraction
- [ ] get_dependencies → Dependency analysis
- [ ] get_imports → Import analysis

#### Git Tools
- [ ] git_status → Repository status
- [ ] git_diff → Show changes
- [ ] git_commit → Create commits
- [ ] git_push → Push changes
- [ ] git_history → Log analysis

#### AI Orchestration
- [ ] delegate_task → Task delegation to agents
- [ ] consensus → Multi-agent consensus
- [ ] critic → Code review and critique
- [ ] plan → Task planning
- [ ] execute_plan → Plan execution

#### Project Intelligence
- [ ] discover_rules → Project convention discovery
- [ ] manage_todos → TODO tracking
- [ ] analyze_structure → Architecture analysis
- [ ] suggest_improvements → Code suggestions

### Phase 3: Agent System
Implement agent orchestration in Rust:

```rust
// src/rs/agents/orchestrator.rs
pub struct AgentOrchestrator {
    agents: HashMap<String, Box<dyn Agent>>,
    mcp_runtime: McpRuntime,
}

pub trait Agent {
    fn name(&self) -> &str;
    fn capabilities(&self) -> Vec<String>;
    fn execute(&self, task: Task) -> Result<Response>;
}

// Agents communicate only through MCP tools
impl CodeActAgent {
    fn execute(&self, task: Task) -> Result<Response> {
        // Use MCP tools for all operations
        let files = self.mcp.call_tool("list_files", params)?;
        let content = self.mcp.call_tool("read_file", params)?;
        let result = self.mcp.call_tool("edit_file", params)?;
        Ok(result)
    }
}
```

## Sandboxing Configuration

Make sandboxing completely optional through configuration:

```toml
# ~/.codex/config.toml

[mcp]
# Sandboxing can be completely disabled
sandbox_enabled = false  # Default: true

# When disabled, all tools run with full permissions
sandbox_mode = "disabled"  # Options: "disabled", "read_only", "workspace", "strict"

# Fine-grained control
[mcp.sandbox]
file_access = "unrestricted"  # Or "workspace_only", "read_only"
network_access = true
process_spawn = true
system_calls = true
```

```rust
// src/rs/mcp-runtime/sandbox.rs
pub struct Sandbox {
    enabled: bool,
    mode: SandboxMode,
}

impl Sandbox {
    pub fn execute<T>(&self, f: impl FnOnce() -> T) -> T {
        if !self.enabled {
            // Direct execution without any sandboxing
            return f();
        }
        // Apply sandbox restrictions based on mode
        self.apply_restrictions(f)
    }
}
```

## Implementation Strategy

### 1. Tool Registry System
Create a unified registry for all MCP tools:

```rust
// src/rs/mcp-tools/registry.rs
pub struct ToolRegistry {
    tools: HashMap<String, Box<dyn Tool>>,
}

pub trait Tool {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn schema(&self) -> JsonSchema;
    fn execute(&self, params: Value) -> Result<Value>;
}

impl ToolRegistry {
    pub fn register_all() -> Self {
        let mut registry = Self::new();
        
        // Register all tools from different categories
        file::register_tools(&mut registry);
        search::register_tools(&mut registry);
        shell::register_tools(&mut registry);
        // ... etc
        
        registry
    }
}
```

### 2. MCP Server Implementation
Enhance the existing MCP server with full tool support:

```rust
// src/rs/mcp-server/src/server.rs
pub struct McpServer {
    registry: ToolRegistry,
    runtime: McpRuntime,
}

impl McpServer {
    pub async fn handle_tool_call(&self, name: &str, params: Value) -> Result<Value> {
        let tool = self.registry.get(name)?;
        
        // Execute through runtime (with optional sandboxing)
        self.runtime.execute_tool(tool, params).await
    }
}
```

### 3. CLI Integration
Expose all tools through the CLI:

```bash
# List all available tools
dev mcp list-tools

# Execute a specific tool
dev mcp call read_file --path="/path/to/file"

# Start MCP server
dev mcp serve --transport stdio

# Install for Claude Desktop
dev mcp install-desktop
```

## Benefits of This Architecture

1. **Unified Tooling**: All tools in one place, no duplication
2. **Language Consistency**: Everything in Rust for performance and safety
3. **Flexible Security**: Sandboxing can be completely disabled when needed
4. **Agent Composability**: Any agent can use any tool through MCP
5. **Clear Separation**: Tools are independent of agents
6. **Easy Extension**: New tools can be added to the registry
7. **Multiple Transports**: Support stdio, HTTP, WebSocket

## Migration Timeline

### Week 1-2: Core Infrastructure
- [ ] Implement tool registry system
- [ ] Create MCP runtime with optional sandboxing
- [ ] Port basic file and search tools

### Week 3-4: Tool Migration
- [ ] Port remaining tools from hanzo/mcp
- [ ] Implement background process management
- [ ] Add advanced search capabilities

### Week 5-6: Agent System
- [ ] Implement agent trait and orchestrator
- [ ] Port CodeAct agent from IDE
- [ ] Create agent composition system

### Week 7-8: Integration & Testing
- [ ] CLI integration for all tools
- [ ] Claude Desktop integration
- [ ] Comprehensive testing
- [ ] Documentation

## Configuration Examples

### Minimal Config (No Sandboxing)
```toml
[mcp]
sandbox_enabled = false
```

### Development Config
```toml
[mcp]
sandbox_mode = "workspace"
file_access = "workspace_only"
network_access = true
process_spawn = true
```

### Production Config
```toml
[mcp]
sandbox_mode = "strict"
file_access = "read_only"
network_access = false
process_spawn = false
allowed_paths = ["/workspace"]
```

## Conclusion

This architecture provides a clean, unified approach to MCP tooling in Hanzo Dev, consolidating the best features from all three projects while maintaining the principle of "one way to do everything." The optional sandboxing ensures flexibility for different use cases, and the Rust implementation guarantees performance and safety.