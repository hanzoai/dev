# Built-in Tools Reference

ZAP provides 14 categories of tools covering the complete agentic tool surface. This document provides comprehensive API documentation for all built-in tools.

## Tool Categories Overview

| Category | Tools | Description |
|----------|-------|-------------|
| [Computer/OS](#1-computeros-automation) | 25+ | Desktop automation, input, filesystem |
| [Browser](#2-browser-automation) | 20+ | Web automation, DOM, network |
| [Vision](#3-computer-vision) | 5+ | UI understanding, OCR |
| [LSP/IDE](#4-lspide-toolchain) | 15+ | Language services, refactoring |
| [VCS](#5-version-control) | 12+ | Git operations, code review |
| [Build/Test](#6-buildtest) | 8+ | Compilation, testing, validation |
| [Debug](#7-debugging) | 10+ | Debugger, profiling |
| [Containers](#8-containers) | 12+ | Docker, Kubernetes |
| [Cloud](#9-cloud) | 8+ | IaC, secrets, deploy |
| [Network](#10-network) | 8+ | HTTP, SSH, port scanning |
| [Data](#11-data) | 6+ | Database clients, queries |
| [Security](#12-security) | 6+ | Scanning, signing |
| [Knowledge](#13-knowledge) | 6+ | Search, embeddings |
| [Plan](#14-planning) | 4+ | Intent, routing, scheduling |

---

## 1. Computer/OS Automation

### Window Management

#### `list_windows`

List all open windows.

**Arguments:** None

**Result:**
```json
{
  "windows": [
    {
      "id": "0x1234",
      "title": "Code - main.rs",
      "app": "Visual Studio Code",
      "x": 0,
      "y": 0,
      "width": 1920,
      "height": 1080,
      "focused": true
    }
  ]
}
```

#### `focus_window`

Focus a specific window.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `window_id` | string | Yes | Window identifier |

#### `screenshot`

Capture screen or window.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `window_id` | string | No | Specific window (null = full screen) |
| `region` | object | No | Capture region {x, y, width, height} |
| `format` | string | No | "png", "jpeg", "webp" (default: "png") |

**Result:**
```json
{
  "data": "base64-encoded-image-data",
  "width": 1920,
  "height": 1080,
  "format": "png"
}
```

### Input Simulation

#### `key_press`

Simulate keyboard input.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `key` | string | Yes | Key name ("a", "Enter", "Escape") |
| `modifiers` | string[] | No | ["ctrl", "shift", "alt", "meta"] |

**Example:**
```json
{"key": "s", "modifiers": ["ctrl"]}  // Ctrl+S
```

#### `text_insert`

Type text with optional delay.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `text` | string | Yes | Text to type |
| `delay_ms` | number | No | Delay between characters |

#### `mouse_click`

Simulate mouse click.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `x` | number | Yes | X coordinate |
| `y` | number | Yes | Y coordinate |
| `button` | string | No | "left", "right", "middle" |
| `clicks` | number | No | 1=single, 2=double |

### Process Management

#### `exec`

Execute a command.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `command` | string | Yes | Command to execute |
| `args` | string[] | No | Command arguments |
| `cwd` | string | No | Working directory |
| `env` | object | No | Environment variables |
| `timeout_ms` | number | No | Timeout in milliseconds |
| `stdin` | string | No | Standard input |

**Result:**
```json
{
  "exit_code": 0,
  "stdout": "output text",
  "stderr": "",
  "duration_ms": 1234
}
```

**Permission:** `Execute`

#### `list_processes`

List running processes.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `filter` | string | No | Process name filter |

**Result:**
```json
{
  "processes": [
    {
      "pid": 1234,
      "ppid": 1,
      "name": "code",
      "cpu_percent": 5.2,
      "memory_percent": 3.1
    }
  ]
}
```

#### `kill_process`

Terminate a process.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `pid` | number | Yes | Process ID |
| `signal` | number | No | Signal (15=TERM, 9=KILL) |

**Permission:** `Admin`

### Filesystem Operations

#### `read_file`

Read file contents.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | Yes | File path |
| `offset` | number | No | Byte offset |
| `limit` | number | No | Maximum bytes |
| `encoding` | string | No | "utf-8", "base64", "binary" |

**Result:**
```json
{
  "content": "file contents here",
  "size": 1234,
  "mime_type": "text/plain"
}
```

**Permission:** `Read`

#### `write_file`

Write file contents.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | Yes | File path |
| `content` | string | Yes | File contents |
| `encoding` | string | No | "utf-8", "base64" |
| `create_dirs` | boolean | No | Create parent directories |
| `overwrite` | boolean | No | Overwrite existing file |

**Permission:** `Write`

#### `edit_file`

Apply edits to a file.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | Yes | File path |
| `edits` | array | Yes | List of {old_text, new_text} |

**Example:**
```json
{
  "path": "src/main.rs",
  "edits": [
    {"old_text": "fn old()", "new_text": "fn new()"}
  ]
}
```

**Permission:** `Write`

#### `glob`

Find files matching pattern.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `pattern` | string | Yes | Glob pattern ("**/*.rs") |
| `base_path` | string | No | Base directory |
| `max_results` | number | No | Maximum results |

**Result:**
```json
{
  "files": [
    "src/main.rs",
    "src/lib.rs",
    "tests/test.rs"
  ]
}
```

#### `grep`

Search file contents.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `pattern` | string | Yes | Regex pattern |
| `path` | string | Yes | File or directory |
| `recursive` | boolean | No | Search subdirectories |
| `ignore_case` | boolean | No | Case-insensitive |
| `max_results` | number | No | Maximum results |
| `context_lines` | number | No | Context lines around match |

**Result:**
```json
{
  "matches": [
    {
      "path": "src/main.rs",
      "line_number": 42,
      "line": "    let result = authenticate(user);",
      "context_before": ["fn login(user: &str) {"],
      "context_after": ["    Ok(result)"]
    }
  ]
}
```

#### `list_dir`

List directory contents.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | Yes | Directory path |
| `recursive` | boolean | No | Include subdirectories |
| `show_hidden` | boolean | No | Show hidden files |
| `max_depth` | number | No | Maximum recursion depth |

---

## 2. Browser Automation

#### `navigate`

Navigate to URL.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `url` | string | Yes | Target URL |
| `wait_until` | string | No | "load", "domcontentloaded", "networkidle" |
| `timeout_ms` | number | No | Navigation timeout |

#### `click`

Click an element.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `selector` | string | Yes | CSS selector |
| `button` | string | No | "left", "right", "middle" |
| `click_count` | number | No | Number of clicks |

#### `fill`

Fill a form field.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `selector` | string | Yes | CSS selector |
| `value` | string | Yes | Value to enter |
| `clear_first` | boolean | No | Clear field first |

#### `query_selector`

Query DOM elements.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `selector` | string | Yes | CSS selector |
| `all` | boolean | No | Return all matches |

**Result:**
```json
{
  "elements": [
    {
      "selector": "button.submit",
      "tag": "button",
      "text": "Submit",
      "attributes": {"class": "submit", "type": "submit"},
      "visible": true
    }
  ]
}
```

#### `browser_screenshot`

Screenshot browser page.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `selector` | string | No | Element to capture |
| `full_page` | boolean | No | Full page screenshot |
| `format` | string | No | "png", "jpeg" |

---

## 3. Computer Vision

#### `detect_elements`

Detect UI elements in screenshot.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `screenshot` | string | No | Base64 image or path |
| `element_types` | string[] | No | ["button", "input", "link"] |

**Result:**
```json
{
  "elements": [
    {
      "element_type": "button",
      "bounding_box": {"x": 100, "y": 200, "width": 80, "height": 30},
      "confidence": 0.95,
      "text": "Submit"
    }
  ]
}
```

#### `ocr`

Extract text from image.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `screenshot` | string | No | Base64 image or path |
| `region` | object | No | Region to scan |
| `language` | string | No | Language hint |

**Result:**
```json
{
  "text": "Extracted text content",
  "blocks": [
    {
      "text": "Extracted",
      "bounding_box": {"x": 10, "y": 10, "width": 80, "height": 20},
      "confidence": 0.98
    }
  ]
}
```

---

## 4. LSP/IDE Toolchain

#### `lsp_completion`

Get code completions.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `uri` | string | Yes | File URI |
| `position` | object | Yes | {line, character} |
| `trigger_character` | string | No | Trigger character |

**Result:**
```json
{
  "items": [
    {
      "label": "println!",
      "kind": "macro",
      "detail": "Print to stdout",
      "insert_text": "println!(\"$1\");$0"
    }
  ]
}
```

#### `lsp_definition`

Go to definition.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `uri` | string | Yes | File URI |
| `position` | object | Yes | {line, character} |

**Result:**
```json
{
  "locations": [
    {
      "uri": "file:///src/lib.rs",
      "range": {
        "start": {"line": 10, "character": 0},
        "end": {"line": 10, "character": 20}
      }
    }
  ]
}
```

#### `lsp_references`

Find all references.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `uri` | string | Yes | File URI |
| `position` | object | Yes | {line, character} |
| `include_declaration` | boolean | No | Include declaration |

#### `lsp_rename`

Rename symbol.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `uri` | string | Yes | File URI |
| `position` | object | Yes | {line, character} |
| `new_name` | string | Yes | New symbol name |

**Result:**
```json
{
  "changes": {
    "file:///src/main.rs": [
      {"range": {...}, "new_text": "new_name"}
    ],
    "file:///src/lib.rs": [
      {"range": {...}, "new_text": "new_name"}
    ]
  }
}
```

---

## 5. Version Control

#### `git_status`

Get repository status.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | No | Repository path |

**Result:**
```json
{
  "branch": "main",
  "ahead": 2,
  "behind": 0,
  "staged": [{"path": "src/main.rs", "status": "modified"}],
  "unstaged": [],
  "untracked": ["new_file.txt"]
}
```

#### `git_diff`

Get diff of changes.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | No | File or directory |
| `staged` | boolean | No | Show staged changes |
| `commit` | string | No | Compare with commit |

#### `git_commit`

Create a commit.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `message` | string | Yes | Commit message |
| `files` | string[] | No | Files to commit |
| `amend` | boolean | No | Amend last commit |

**Permission:** `Write`

#### `git_log`

Get commit history.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | No | File or directory |
| `limit` | number | No | Maximum entries |
| `since` | string | No | Start date |
| `until` | string | No | End date |

**Result:**
```json
{
  "commits": [
    {
      "commit": "abc123",
      "author": "Alice",
      "email": "alice@example.com",
      "timestamp": "2025-01-15T10:30:00Z",
      "message": "Fix authentication bug"
    }
  ]
}
```

#### `git_blame`

Get line-by-line blame.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | Yes | File path |
| `start_line` | number | No | Start line |
| `end_line` | number | No | End line |

#### `git_branch`

Branch operations.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `action` | string | Yes | "list", "create", "delete", "switch" |
| `name` | string | No | Branch name |
| `from` | string | No | Source branch/commit |

---

## 6. Build/Test

#### `build`

Build the project.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `target` | string | No | Build target |
| `release` | boolean | No | Release build |
| `features` | string[] | No | Features to enable |

**Result:**
```json
{
  "success": true,
  "duration_ms": 12345,
  "stdout": "...",
  "stderr": "...",
  "artifacts": ["target/release/myapp"]
}
```

**Permission:** `Execute`

#### `test`

Run tests.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `filter` | string | No | Test name filter |
| `verbose` | boolean | No | Verbose output |
| `coverage` | boolean | No | Generate coverage |
| `timeout_ms` | number | No | Test timeout |

**Result:**
```json
{
  "success": true,
  "duration_ms": 5678,
  "passed": 42,
  "failed": 0,
  "skipped": 2
}
```

**Permission:** `Execute`

#### `lint`

Run linter.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | No | Path to lint |
| `fix` | boolean | No | Auto-fix issues |

#### `typecheck`

Run type checker.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | No | Path to check |

---

## 7. Debugging

#### `debug_attach`

Attach debugger to process.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `pid` | number | No | Process ID |
| `command` | string[] | No | Command to debug |
| `debugger` | string | No | "gdb", "lldb", "node" |

#### `debug_breakpoint`

Manage breakpoints.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `action` | string | Yes | "set", "remove", "list" |
| `file` | string | No | File path |
| `line` | number | No | Line number |
| `condition` | string | No | Breakpoint condition |

#### `debug_step`

Step execution.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `action` | string | Yes | "into", "over", "out", "continue" |

---

## 8. Containers

#### `docker_build`

Build Docker image.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `context` | string | Yes | Build context |
| `dockerfile` | string | No | Dockerfile path |
| `tag` | string | Yes | Image tag |
| `build_args` | object | No | Build arguments |

**Permission:** `Execute`

#### `docker_run`

Run Docker container.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `image` | string | Yes | Image name |
| `command` | string[] | No | Command |
| `env` | object | No | Environment |
| `volumes` | string[] | No | Volume mounts |
| `ports` | string[] | No | Port mappings |

**Permission:** `Execute`

#### `kube_apply`

Apply Kubernetes manifest.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `manifest` | string | Yes | Manifest content or path |
| `namespace` | string | No | Namespace |
| `dry_run` | boolean | No | Dry run mode |

**Permission:** `Execute`

---

## 9. Cloud

#### `iac_plan`

Plan infrastructure changes.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tool` | string | Yes | "terraform", "pulumi", "helm" |
| `path` | string | Yes | Configuration path |
| `vars` | object | No | Variables |

#### `iac_apply`

Apply infrastructure changes.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `tool` | string | Yes | "terraform", "pulumi", "helm" |
| `path` | string | Yes | Configuration path |
| `auto_approve` | boolean | No | Skip approval |

**Permission:** `Admin`

---

## 10. Network

#### `http_request`

Make HTTP request.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `method` | string | Yes | HTTP method |
| `url` | string | Yes | Request URL |
| `headers` | object | No | Request headers |
| `body` | string | No | Request body |
| `timeout_ms` | number | No | Request timeout |

**Result:**
```json
{
  "status": 200,
  "headers": {"content-type": "application/json"},
  "body": "{...}"
}
```

**Permission:** `Execute`

#### `port_check`

Check if port is open.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `host` | string | Yes | Host address |
| `port` | number | Yes | Port number |
| `timeout_ms` | number | No | Connection timeout |

#### `dns_lookup`

DNS lookup.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `hostname` | string | Yes | Hostname to resolve |
| `record_type` | string | No | "A", "AAAA", "MX", etc. |

---

## 11. Data

#### `db_query`

Execute database query.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `connection` | string | Yes | Connection string |
| `query` | string | Yes | SQL query |
| `params` | array | No | Query parameters |

**Result:**
```json
{
  "rows": [
    {"id": 1, "name": "Alice"},
    {"id": 2, "name": "Bob"}
  ],
  "affected_rows": 0
}
```

**Permission:** `Execute`

---

## 12. Security

#### `secret_scan`

Scan for secrets in code.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | Yes | Path to scan |
| `patterns` | string[] | No | Custom patterns |

**Result:**
```json
{
  "findings": [
    {
      "file": "config.py",
      "line": 42,
      "type": "api_key",
      "severity": "high"
    }
  ]
}
```

#### `vuln_scan`

Scan for vulnerabilities.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `path` | string | Yes | Project path |
| `type` | string | No | "dependencies", "code", "all" |

---

## 13. Knowledge

#### `search`

Search documents.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `query` | string | Yes | Search query |
| `sources` | string[] | No | Sources to search |
| `max_results` | number | No | Maximum results |

**Result:**
```json
{
  "results": [
    {
      "title": "Authentication Guide",
      "url": "https://docs.example.com/auth",
      "snippet": "To authenticate users...",
      "score": 0.95
    }
  ]
}
```

#### `embed`

Generate embeddings.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `text` | string | Yes | Text to embed |
| `model` | string | No | Embedding model |

**Result:**
```json
{
  "embedding": [0.1, -0.2, 0.3, ...],
  "dimensions": 1536
}
```

---

## 14. Planning

#### `plan_intent`

Parse user intent.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `input` | string | Yes | User input |
| `context` | object | No | Additional context |

**Result:**
```json
{
  "intent": "fix_bug",
  "entities": {
    "file": "src/auth.rs",
    "issue": "authentication failure"
  },
  "confidence": 0.92
}
```

#### `plan_route`

Route to appropriate tools.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `intent` | object | Yes | Parsed intent |
| `available_tools` | string[] | No | Available tools |

**Result:**
```json
{
  "steps": [
    {"tool": "read_file", "args": {"path": "src/auth.rs"}},
    {"tool": "grep", "args": {"pattern": "authenticate"}},
    {"tool": "edit_file", "args": {...}}
  ]
}
```

#### `audit_log`

Log to audit trail.

**Arguments:**
| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `event` | string | Yes | Event type |
| `details` | object | No | Event details |

---

## Permission Summary

| Permission | Tools |
|------------|-------|
| `Read` | read_file, glob, grep, list_dir, git_status, git_diff, git_log, git_blame, list_processes, port_check, dns_lookup, search |
| `Write` | write_file, edit_file, git_commit, git_branch |
| `Execute` | exec, build, test, lint, http_request, docker_build, docker_run, db_query |
| `Admin` | kill_process, iac_apply |

---

## Error Codes

| Code | Name | Description |
|------|------|-------------|
| -32601 | ToolNotFound | Tool does not exist |
| -32602 | InvalidParams | Invalid arguments |
| -32002 | PermissionDenied | Not authorized |
| -32001 | Timeout | Operation timed out |

## Next Steps

- [Getting Started](getting-started.md): Quick start guide
- [Security](security.md): Permission configuration
- [Language Bindings](languages.md): Use tools in your language
