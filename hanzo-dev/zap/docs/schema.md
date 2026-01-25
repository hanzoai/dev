# Schema Definition Language

ZAP uses typed schemas to define tool interfaces. Unlike JSON Schema (used by MCP), ZAP schemas are native to each language, providing compile-time type safety and zero-cost abstraction.

## Overview

Every ZAP tool has two associated types:
- **Args**: Input parameters for the tool
- **Result**: Output returned by the tool

These types are defined in the implementation language (Rust, Python, Go, etc.) and automatically generate compatible JSON Schema for MCP interoperability.

## Type System

### Primitive Types

| ZAP Type | Rust | Python | Go | JSON Schema |
|----------|------|--------|-----|-------------|
| `bool` | `bool` | `bool` | `bool` | `boolean` |
| `u8` | `u8` | `int` | `uint8` | `integer` |
| `u32` | `u32` | `int` | `uint32` | `integer` |
| `u64` | `u64` | `int` | `uint64` | `integer` |
| `i32` | `i32` | `int` | `int32` | `integer` |
| `i64` | `i64` | `int` | `int64` | `integer` |
| `f32` | `f32` | `float` | `float32` | `number` |
| `f64` | `f64` | `float` | `float64` | `number` |
| `string` | `String` | `str` | `string` | `string` |
| `bytes` | `Vec<u8>` | `bytes` | `[]byte` | `string` (base64) |

### Compound Types

#### Optional Fields

Optional fields may be omitted from requests:

```rust
// Rust
#[derive(Serialize, Deserialize)]
pub struct ReadFileArgs {
    pub path: String,              // Required
    pub offset: Option<u64>,       // Optional
    pub limit: Option<u64>,        // Optional
    pub encoding: Option<String>,  // Optional
}
```

```python
# Python
@dataclass
class ReadFileArgs:
    path: str                      # Required
    offset: int | None = None      # Optional
    limit: int | None = None       # Optional
    encoding: str | None = None    # Optional
```

```go
// Go
type ReadFileArgs struct {
    Path     string  `json:"path"`              // Required
    Offset   *uint64 `json:"offset,omitempty"`  // Optional
    Limit    *uint64 `json:"limit,omitempty"`   // Optional
    Encoding *string `json:"encoding,omitempty"` // Optional
}
```

#### Lists

```rust
// Rust
pub struct GlobArgs {
    pub patterns: Vec<String>,     // List of strings
}
```

```python
# Python
@dataclass
class GlobArgs:
    patterns: list[str]            # List of strings
```

#### Maps / Dictionaries

```rust
// Rust
pub struct ExecArgs {
    pub command: String,
    pub env: Option<HashMap<String, String>>,  // String -> String map
}
```

```python
# Python
@dataclass
class ExecArgs:
    command: str
    env: dict[str, str] | None = None  # String -> String map
```

#### Enumerations

```rust
// Rust
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MouseButton {
    Left,
    Right,
    Middle,
}

#[derive(Serialize, Deserialize)]
pub struct MouseClickArgs {
    pub x: i32,
    pub y: i32,
    pub button: MouseButton,
}
```

```python
# Python
from enum import Enum

class MouseButton(str, Enum):
    LEFT = "left"
    RIGHT = "right"
    MIDDLE = "middle"

@dataclass
class MouseClickArgs:
    x: int
    y: int
    button: MouseButton
```

#### Nested Structures

```rust
// Rust
#[derive(Serialize, Deserialize)]
pub struct Region {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32,
}

#[derive(Serialize, Deserialize)]
pub struct ScreenshotArgs {
    pub region: Option<Region>,
    pub format: Option<String>,
}
```

#### Union Types (Tagged)

```rust
// Rust - Tagged union with serde
#[derive(Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Transport {
    Stdio {
        command: String,
        args: Vec<String>,
    },
    Http {
        url: String,
    },
    Zap {
        url: String,
    },
}
```

JSON representation:
```json
// Stdio variant
{ "type": "stdio", "command": "mcp-server-fs", "args": ["/home"] }

// Http variant
{ "type": "http", "url": "https://api.example.com" }

// Zap variant
{ "type": "zap", "url": "zap://localhost:9999" }
```

## Defining Tool Schemas

### Complete Tool Definition (Rust)

```rust
use serde::{Deserialize, Serialize};

/// Arguments for the read_file tool
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadFileArgs {
    /// Path to the file to read
    pub path: String,

    /// Byte offset to start reading from (default: 0)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub offset: Option<u64>,

    /// Maximum number of bytes to read (default: entire file)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub limit: Option<u64>,

    /// Character encoding (default: "utf-8")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub encoding: Option<String>,
}

/// Result from the read_file tool
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReadFileResult {
    /// File content as string (if text encoding)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<String>,

    /// File content as base64 (if binary)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<String>,

    /// MIME type of the content
    pub mime_type: String,

    /// Total file size in bytes
    pub size: u64,
}
```

### Complete Tool Definition (Python)

```python
from dataclasses import dataclass
from typing import Optional

@dataclass
class ReadFileArgs:
    """Arguments for the read_file tool."""

    path: str
    """Path to the file to read."""

    offset: Optional[int] = None
    """Byte offset to start reading from (default: 0)."""

    limit: Optional[int] = None
    """Maximum number of bytes to read (default: entire file)."""

    encoding: Optional[str] = None
    """Character encoding (default: 'utf-8')."""


@dataclass
class ReadFileResult:
    """Result from the read_file tool."""

    mime_type: str
    """MIME type of the content."""

    size: int
    """Total file size in bytes."""

    content: Optional[str] = None
    """File content as string (if text encoding)."""

    data: Optional[str] = None
    """File content as base64 (if binary)."""
```

## Schema Evolution

ZAP schemas are designed for forward and backward compatibility.

### Adding Optional Fields (Safe)

```rust
// Version 1
pub struct ReadFileArgs {
    pub path: String,
}

// Version 2 - Added optional field (backward compatible)
pub struct ReadFileArgs {
    pub path: String,
    pub encoding: Option<String>,  // NEW: Old clients will omit this
}
```

Old clients continue to work because the new field is optional.

### Adding Required Fields (Breaking)

```rust
// Version 1
pub struct ReadFileArgs {
    pub path: String,
}

// Version 2 - Added required field (BREAKING CHANGE)
pub struct ReadFileArgs {
    pub path: String,
    pub workspace: String,  // BREAKING: Old clients don't send this
}
```

**Mitigation**: Use `#[serde(default)]` with a sensible default:

```rust
pub struct ReadFileArgs {
    pub path: String,
    #[serde(default = "default_workspace")]
    pub workspace: String,
}

fn default_workspace() -> String {
    ".".to_string()
}
```

### Removing Fields (Safe for Optional)

Removing optional fields is safe - old values are simply ignored.

### Renaming Fields (Breaking)

Use `#[serde(alias)]` for backward compatibility:

```rust
pub struct ReadFileArgs {
    #[serde(alias = "file_path")]  // Accept old name too
    pub path: String,
}
```

### Changing Types (Breaking)

Type changes are always breaking. Use a new field instead:

```rust
// Version 1
pub struct LimitArgs {
    pub limit: u32,
}

// Version 2 - Support both for compatibility
pub struct LimitArgs {
    #[serde(alias = "limit")]
    pub limit_bytes: u64,  // Renamed with new type
}
```

## JSON Schema Generation

ZAP can generate MCP-compatible JSON Schema for interoperability:

```rust
use schemars::JsonSchema;

#[derive(JsonSchema, Serialize, Deserialize)]
pub struct ReadFileArgs {
    /// Path to the file to read
    pub path: String,
    /// Maximum bytes to read
    pub limit: Option<u64>,
}

// Generate JSON Schema
let schema = schemars::schema_for!(ReadFileArgs);
println!("{}", serde_json::to_string_pretty(&schema)?);
```

Output:
```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "ReadFileArgs",
  "type": "object",
  "required": ["path"],
  "properties": {
    "path": {
      "type": "string",
      "description": "Path to the file to read"
    },
    "limit": {
      "type": "integer",
      "format": "uint64",
      "minimum": 0,
      "description": "Maximum bytes to read"
    }
  }
}
```

## Tool Categories

ZAP organizes tools into 14 categories:

| Category | Module | Description |
|----------|--------|-------------|
| Computer/OS | `window`, `input`, `screen`, `process`, `filesystem`, `packaging` | Desktop automation |
| Browser | `browser` | Web automation |
| Vision | `vision` | UI understanding, OCR |
| LSP/IDE | `lsp`, `project`, `codemod` | Language services |
| VCS | `vcs` | Git operations |
| Build/Test | `build` | Compilation, testing |
| Debug | `debug` | Debugging, profiling |
| Containers | `container` | Docker, Kubernetes |
| Cloud | `cloud` | Infrastructure |
| Network | `network` | HTTP, SSH |
| Data | `data` | Databases |
| Security | `security` | Scanning, signing |
| Knowledge | `knowledge` | Search, embeddings |
| Plan | `plan` | Agent orchestration |

See [Tool Reference](tools.md) for complete API documentation.

## Best Practices

### 1. Use Descriptive Field Names

```rust
// Good
pub struct GrepArgs {
    pub pattern: String,
    pub path: String,
    pub ignore_case: Option<bool>,
}

// Bad
pub struct GrepArgs {
    pub p: String,
    pub dir: String,
    pub i: Option<bool>,
}
```

### 2. Provide Sensible Defaults

```rust
#[derive(Default)]
pub struct ExecArgs {
    pub command: String,
    #[serde(default = "default_timeout")]
    pub timeout_ms: u64,
}

fn default_timeout() -> u64 {
    30_000  // 30 seconds
}
```

### 3. Use Enums for Fixed Choices

```rust
// Good
pub enum OutputFormat {
    Json,
    Yaml,
    Toml,
}

// Bad
pub format: String,  // What values are valid?
```

### 4. Document Everything

```rust
/// Arguments for file search operations.
///
/// Searches for files matching a glob pattern within a directory tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GlobArgs {
    /// Glob pattern to match (e.g., "**/*.rs", "src/*.py")
    pub pattern: String,

    /// Base directory for search (default: current working directory)
    pub base_path: Option<String>,

    /// Maximum number of results to return (default: unlimited)
    pub max_results: Option<u32>,
}
```

## Next Steps

- [Wire Format](encoding.md): How schemas are encoded on the wire
- [RPC Protocol](rpc.md): Request/response patterns
- [Tool Reference](tools.md): All built-in tool schemas
