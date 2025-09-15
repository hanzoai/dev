# MCP Tool Parity Audit: Python vs Rust

## Current Status

### Rust Implementation (30 tools)
```
agent_execute, apply_patch, bash, build_project, copy_file, delete_file,
dependency_tree, edit_file, embeddings_create, file_info, find_files,
git_branch, git_commit, git_diff, git_log, git_pull, git_push, git_status,
grep, list_files, llm_complete, move_file, multi_edit, project_analyze,
read_file, refactor_code, search_ast, search_symbols, test_run, write_file
```

### Python Implementation Categories (from registration)

#### Filesystem Tools
- ✅ read (read_file)
- ✅ write (write_file)
- ✅ edit (edit_file)
- ✅ multi_edit
- ✅ directory_tree (list_files)
- ✅ ast (search_ast)
- ❌ rules
- ✅ search (grep)
- ✅ find (find_files)
- ❌ batch_search
- ❌ unified_search
- ❌ content_replace
- ❌ diff
- ❌ watch
- ❌ tree (enhanced directory tree)
- ❌ symbols (search_symbols needs enhancement)
- ❌ git_search

#### Shell Tools
- ✅ bash (run_command)
- ❌ run_background
- ❌ npx
- ❌ npx_background
- ❌ uvx
- ❌ uvx_background
- ❌ open
- ❌ pkill
- ❌ processes
- ❌ logs
- ❌ streaming_command
- ❌ zsh

#### Agent Tools
- ✅ agent (agent_execute)
- ❌ swarm
- ❌ claude
- ❌ codex
- ❌ gemini
- ❌ grok
- ❌ code_auth
- ❌ clarification
- ❌ critic
- ❌ review
- ❌ iching
- ❌ network

#### Todo Tools
- ❌ todo (unified todo management)
- ❌ todo_read
- ❌ todo_write

#### Thinking Tools
- ❌ think (reasoning tool)

#### Vector/Index Tools
- ❌ vector_index
- ❌ vector_search

#### Database Tools
- ❌ sql_query
- ❌ sql_search
- ❌ sql_stats
- ❌ graph_add
- ❌ graph_remove
- ❌ graph_query
- ❌ graph_search
- ❌ graph_stats

#### MCP Tools
- ❌ mcp (unified MCP management)
- ❌ mcp_add
- ❌ mcp_remove
- ❌ mcp_stats

#### System Tools
- ❌ tool_enable
- ❌ tool_disable
- ❌ tool_list
- ❌ stats
- ❌ mode
- ❌ config

#### Editor Tools
- ❌ neovim_edit
- ❌ neovim_command
- ❌ neovim_session

#### LLM Tools
- ✅ llm (llm_complete)
- ❌ consensus
- ❌ llm_manage
- ❌ provider-specific tools (openai, anthropic, etc.)

#### Memory Tools
- ❌ recall_memories
- ❌ create_memories
- ❌ update_memories
- ❌ delete_memories
- ❌ manage_memories
- ❌ recall_facts
- ❌ store_facts
- ❌ summarize_to_memory
- ❌ manage_knowledge_bases

#### Jupyter Tools
- ❌ jupyter (unified)
- ❌ notebook_read
- ❌ notebook_edit

#### LSP Tools
- ❌ lsp (Language Server Protocol)

#### Batch Tool
- ❌ batch (execute multiple tools)

## Missing Tools Summary

**Total Python Tools**: ~100+
**Implemented in Rust**: 30
**Missing**: 70+

## Priority Implementation List

### Critical Tools (Must Have)
1. **Todo Management**: todo, todo_read, todo_write
2. **Shell Enhancements**: run_background, processes, pkill
3. **Search Enhancements**: unified_search, batch_search, git_search
4. **System Tools**: tool_enable, tool_disable, tool_list, stats, mode

### Important Tools (Should Have)
5. **Database**: sql_query, graph_query, graph_add
6. **Vector/Index**: vector_index, vector_search
7. **Memory**: recall_memories, store_facts
8. **Agent Enhancements**: swarm, critic, review
9. **Editor**: neovim_edit
10. **Jupyter**: notebook_read, notebook_edit

### Nice to Have
11. **CLI Agents**: claude, codex, gemini, grok
12. **Advanced Shell**: npx, uvx, zsh, streaming_command
13. **Specialized**: iching, network, consensus
14. **MCP Management**: mcp, mcp_add, mcp_remove