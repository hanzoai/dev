# Rust ↔ Python Tool Parity Analysis

## Overview

This document tracks parity between:

- **Rust**: `@hanzo/dev` (hanzo-dev crate) - npm package
- **Python**: `hanzo-mcp` (via hanzo-tools-\* packages) - PyPI packages

## Tool Comparison Matrix

### Core Execution Tools

| Tool        | Rust (dev)             | Python (hanzo-mcp)   | Parity Status        |
| ----------- | ---------------------- | -------------------- | -------------------- |
| Shell       | `local_shell`, `shell` | `zsh`, `bash`, `ps`  | ⚠️ Different schemas |
| File Read   | via apply_patch        | `read`               | ✅ Equivalent        |
| File Write  | via apply_patch        | `write`              | ✅ Equivalent        |
| File Edit   | `apply_patch` (Lark)   | `edit`, `multi_edit` | ⚠️ Different formats |
| File Tree   | -                      | `tree`               | ❌ Missing in Rust   |
| File Find   | -                      | `find`               | ❌ Missing in Rust   |
| File Search | -                      | `search`, `ast`      | ❌ Missing in Rust   |

### Agent & Planning Tools

| Tool  | Rust (dev)                                      | Python (hanzo-mcp)                           | Parity Status          |
| ----- | ----------------------------------------------- | -------------------------------------------- | ---------------------- |
| Plan  | `update_plan`                                   | -                                            | ❌ Missing in Python   |
| Agent | `agent` (create/status/wait/result/cancel/list) | `agent`, `claude`, `codex`, `gemini`, `grok` | ⚠️ Different actions   |
| Wait  | `wait` (background polling)                     | -                                            | ❌ Missing in Python   |
| Kill  | `kill`                                          | `ps --kill`                                  | ⚠️ Different interface |

### Memory & State Tools

| Tool            | Rust (dev) | Python (hanzo-mcp)            | Parity Status      |
| --------------- | ---------- | ----------------------------- | ------------------ |
| Memory Recall   | -          | `recall_memories`             | ❌ Missing in Rust |
| Memory Create   | -          | `create_memories`             | ❌ Missing in Rust |
| Memory Update   | -          | `update_memories`             | ❌ Missing in Rust |
| Memory Delete   | -          | `delete_memories`             | ❌ Missing in Rust |
| Memory Manage   | -          | `manage_memories`             | ❌ Missing in Rust |
| Facts           | -          | `recall_facts`, `store_facts` | ❌ Missing in Rust |
| Knowledge Bases | -          | `manage_knowledge_bases`      | ❌ Missing in Rust |
| Summarize       | -          | `summarize_to_memory`         | ❌ Missing in Rust |
| Todo            | -          | `todo`                        | ❌ Missing in Rust |

### Reasoning Tools

| Tool    | Rust (dev) | Python (hanzo-mcp) | Parity Status      |
| ------- | ---------- | ------------------ | ------------------ |
| Think   | -          | `think`            | ❌ Missing in Rust |
| Critic  | -          | `critic`           | ❌ Missing in Rust |
| Review  | -          | `review`           | ❌ Missing in Rust |
| I Ching | -          | `iching`           | ❌ Missing in Rust |

### Code Intelligence Tools

| Tool     | Rust (dev) | Python (hanzo-mcp) | Parity Status      |
| -------- | ---------- | ------------------ | ------------------ |
| LSP      | -          | `lsp`              | ❌ Missing in Rust |
| Refactor | -          | `refactor`         | ❌ Missing in Rust |

### Browser & UI Tools

| Tool       | Rust (dev)             | Python (hanzo-mcp)     | Parity Status        |
| ---------- | ---------------------- | ---------------------- | -------------------- |
| Browser    | `browser` (Playwright) | `browser` (Playwright) | ⚠️ Different actions |
| Image View | `view_image`           | -                      | ❌ Missing in Python |
| Screen     | -                      | `screen`               | ❌ Missing in Rust   |
| Computer   | -                      | `computer`             | ❌ Missing in Rust   |

### Network & API Tools

| Tool       | Rust (dev)   | Python (hanzo-mcp) | Parity Status        |
| ---------- | ------------ | ------------------ | -------------------- |
| Web Search | `web_search` | -                  | ❌ Missing in Python |
| API        | -            | `api`              | ❌ Missing in Rust   |
| Curl       | -            | `curl`             | ❌ Missing in Rust   |
| Wget       | -            | `wget`             | ❌ Missing in Rust   |

### Integration Tools

| Tool            | Rust (dev)            | Python (hanzo-mcp) | Parity Status        |
| --------------- | --------------------- | ------------------ | -------------------- |
| GH Actions Wait | `gh_actions_run_wait` | -                  | ❌ Missing in Python |
| Bridge          | `bridge`              | -                  | ❌ Missing in Python |
| NPX             | -                     | `npx`              | ❌ Missing in Rust   |
| UVX             | -                     | `uvx`              | ❌ Missing in Rust   |
| Open            | -                     | `open`             | ❌ Missing in Rust   |
| JQ              | -                     | `jq`               | ❌ Missing in Rust   |

### Configuration Tools

| Tool      | Rust (dev) | Python (hanzo-mcp) | Parity Status      |
| --------- | ---------- | ------------------ | ------------------ |
| Config    | -          | `config`           | ❌ Missing in Rust |
| Mode      | -          | `mode`             | ❌ Missing in Rust |
| Version   | -          | `version`          | ❌ Missing in Rust |
| Tool Mgmt | -          | `tool`             | ❌ Missing in Rust |

### Database Tools

| Tool  | Rust (dev) | Python (hanzo-mcp)       | Parity Status      |
| ----- | ---------- | ------------------------ | ------------------ |
| SQL   | -          | `sql_query`, `sql_stats` | ❌ Missing in Rust |
| Graph | -          | `graph_*`                | ❌ Missing in Rust |

---

## Schema Alignment Needed

### 1. Shell Tool

**Rust Schema:**

```json
{
  "command": ["array", "of", "strings"],
  "workdir": "string",
  "timeout": "number"
}
```

**Python Schema:**

```json
{
  "command": "string", // Single string, shell parses
  "cwd": "string",
  "timeout": "number",
  "env": { "key": "value" }
}
```

**Action:** Align to support both array and string command formats.

### 2. Apply Patch vs Edit

**Rust (apply_patch):** Lark grammar format

```
*** Begin Patch
*** Update File: path
@@ context
-old
+new
*** End Patch
```

**Python (edit):**

```json
{
  "file_path": "string",
  "old_string": "string",
  "new_string": "string"
}
```

**Action:** Add `apply_patch` format support to Python, expose `edit` in Rust.

### 3. Agent Tool

**Rust Actions:** create, status, wait, result, cancel, list

**Python Actions:** Separate tools (agent, claude, codex, gemini, grok)

**Action:** Unify into single agent tool with action parameter.

### 4. Browser Tool

**Rust Actions:** open, status, fetch, close, click, move, type, key, javascript, scroll, history, inspect, console, cleanup, cdp

**Python Actions:** Full Playwright API (50+ actions)

**Action:** Align core actions, keep Python extended actions.

---

## Implementation Plan

### Phase 1: Add Missing Python Tools (Priority) - ✅ COMPLETED

1. ✅ **plan.update** - Plan tracking with step status (added to hanzo-tools-plan)
2. ✅ **proc.wait** - Background command polling (added to hanzo-tools-shell)
3. ✅ **fs.patch** - Rust-style patch format (added to hanzo-tools-fs)
4. ✅ **proc.exec** - Array command format support (Rust parity)
5. 🔲 **gh_actions_run_wait** tool - GitHub Actions monitoring
6. 🔲 **bridge** tool - Client-side integration
7. 🔲 **view_image** tool - Image viewing with alt text
8. 🔲 **web_search** tool - Web search integration

### Phase 2: Full Browser & Computer Control (Via MCP) - ✅ AVAILABLE

Rust dev connects to hanzo-mcp Python server, providing access to:

**Browser Tool (90+ actions):**

- Navigation: navigate, reload, go_back, go_forward, close
- Input: click, dblclick, type, fill, clear, press, select_option
- Forms: check, uncheck, upload
- Mouse: hover, drag, mouse_move, mouse_down, mouse_up, scroll
- Touch: tap, swipe, pinch
- Locators: get_by_role, get_by_text, get_by_label, get_by_test_id, etc.
- State: is*visible, is_enabled, is_checked, expect*\* assertions
- Network: route, unroute (mock/block requests)
- Storage: cookies, clear_cookies, storage, storage_state
- Context: new_page, new_context, tabs, connect

**Extension Integration (ports 9223/9224):**

- CDP bridge for browser extensions
- Multi-client routing with namespaced targets
- Real-time WebSocket communication
- Falls back to Playwright if extension unavailable

**Computer Tool (40+ actions):**

- Mouse: click, double_click, right_click, move, drag, scroll
- Keyboard: type, write, press, key_down, key_up, hotkey
- Screen: screenshot, screenshot_region, get_screens
- Windows: get_active_window, list_windows, focus_window
- Batch: Execute multiple actions atomically

**Screen Tool:**

- session: Record screen → analyze → extract keyframes
- record/stop: Manual recording control
- Native backends: Quartz (macOS), xdotool (Linux), win32 (Windows)

**Other MCP Tools:**

- Memory: recall/create/update/delete memories, facts, knowledge bases
- Reasoning: think, critic, review, iching
- LSP/Refactor: semantic code analysis
- Database: SQL and graph operations
- Config/Mode: tool configuration

### Phase 3: Schema Alignment

1. Unify shell command format (support both array and string)
2. Add apply_patch format to Python edit tool
3. Consolidate agent tool actions
4. Align browser tool action names

---

## MCP Server Integration

Rust dev connects to hanzo-mcp via `mcp_servers.hanzo` config:

```toml
[mcp_servers.hanzo]
command = "hanzo-mcp"
args = []
startup_timeout_sec = 15
tool_timeout_sec = 60
```

This provides Rust access to all Python tools via MCP protocol.

---

## Version Tracking

| Component          | Current Version | Target Version |
| ------------------ | --------------- | -------------- |
| @hanzo/dev (npm)   | 3.0.9           | 3.1.0          |
| hanzo-mcp (PyPI)   | 0.11.7          | 0.12.0         |
| hanzo-tools (PyPI) | 0.3.0           | 0.4.0          |

---

## Testing Strategy

1. **Unit Tests:** Each tool has matching test coverage
2. **Integration Tests:** Cross-language tool invocation
3. **Snapshot Tests:** Output format consistency
4. **E2E Tests:** Full workflow validation

---

## Notes

- Rust provides core low-latency tools (shell, patch, agent)
- Python provides extended tool ecosystem (memory, reasoning, LSP)
- MCP protocol bridges both ecosystems
- Focus on schema alignment for interoperability
