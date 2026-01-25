# ZAP Agent Example - Haskell

A complete ZAP (Zero-copy Agent Protocol) agent implementation in Haskell.

## Quick Start

```bash
# With cabal
cabal run zap-agent-haskell

# With stack
stack run

# Or compile and run directly
ghc -O2 Agent.hs -o zap-agent && ./zap-agent
```

## Features

- **ApprovalPolicy** - ADT matching hanzo-protocol (Untrusted, OnFailure, OnRequest, Never)
- **SandboxPolicy** - ADT with workspace-write configuration
- **ExecutorContext** - Full execution state management
- **ToolResult** - Functor/Applicative/Monad for idiomatic error handling
- **5 Tools** - read_file, list_dir, git_status, git_log, exec

## Tools

| Tool | Arguments | Description |
|------|-----------|-------------|
| `read_file` | `path :: FilePath` | Read file contents |
| `list_dir` | `path :: FilePath`, `showHidden :: Bool` | List directory entries |
| `git_status` | none | Get git repository status |
| `git_log` | `limit :: Int` | Get git commit history |
| `exec` | `command :: Text` | Execute shell command |

## Type Design

The implementation uses idiomatic Haskell patterns:

```haskell
-- Sum type for approval policy
data ApprovalPolicy = Untrusted | OnFailure | OnRequest | Never

-- Product type with record syntax for sandbox policy
data SandboxPolicy
    = DangerFullAccess
    | ReadOnly
    | WorkspaceWrite { writableRoots :: [FilePath], ... }

-- Custom result type with Functor/Applicative/Monad instances
data ToolResult a = ToolSuccess a | ToolError Text

-- Execute with pattern matching on tool name
execute :: ZapAgent -> Text -> ToolArgs -> IO (ToolResult Text)
```

## Dependencies

- `base` - Standard library
- `aeson` - JSON serialization
- `bytestring` - Binary data
- `containers` - Map/Set
- `directory` - Filesystem operations
- `filepath` - Path manipulation
- `process` - Command execution
- `text` - Unicode text
- `unix` - POSIX operations
