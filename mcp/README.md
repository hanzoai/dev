# @hanzo/mcp

Model Context Protocol tools for Hanzo Dev - TypeScript/JavaScript bindings for the MCP tool ecosystem.

## Installation

```bash
npm install @hanzo/mcp
```

## Usage

```typescript
import { ToolRegistry } from '@hanzo/mcp';

// Initialize the registry
const registry = new ToolRegistry();

// List all available tools
const tools = registry.listTools();
console.log(`Available tools: ${tools.length}`);

// Execute a tool
const result = await registry.execute('read_file', {
  path: '/path/to/file.txt'
});

if (result.success) {
  console.log('File content:', result.data.content);
} else {
  console.error('Error:', result.error);
}
```

## Available Tools

### File Operations
- `read_file` - Read file contents
- `write_file` - Write content to file
- `list_files` - List directory contents
- `file_info` - Get file metadata
- `copy_file` - Copy files
- `move_file` - Move/rename files
- `delete_file` - Delete files
- `create_directory` - Create directories
- `find_files` - Find files by pattern

### Search Tools
- `grep` - Search for patterns in files
- `ripgrep` - Fast pattern search
- `find_text` - Find text occurrences
- `search_ast` - Search Abstract Syntax Tree
- `search_symbols` - Search code symbols

### Shell Tools
- `bash` - Execute bash commands
- `shell` - Execute shell commands
- `exec` - Execute programs

### Edit Tools
- `edit_file` - Edit file contents
- `multi_edit` - Multiple edits in one operation
- `patch_file` - Apply patches to files

### Git Tools
- `git_status` - Get repository status
- `git_diff` - Show changes
- `git_commit` - Create commits
- `git_log` - View commit history

### AST Tools
- `parse_ast` - Parse code into AST
- `search_ast` - Search AST structures
- `modify_ast` - Modify AST nodes

### Browser Tools
- `screenshot` - Capture screenshots
- `navigate` - Navigate to URLs
- `click` - Click elements
- `type` - Type text

### AI Tools
- `think` - AI reasoning
- `agent` - Launch AI agents
- `orchestrate` - Orchestrate multiple agents

### Project Tools
- `analyze_project` - Analyze project structure
- `generate_docs` - Generate documentation
- `test_suite` - Run test suites

## Architecture

The package provides both native Rust bindings (when available) and JavaScript fallback implementations. This ensures compatibility across different environments while maximizing performance when native bindings are available.

## License

MIT