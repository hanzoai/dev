# @hanzo/dev - Unified AI Developer Tool

## Vision

`@hanzo/dev` is a unified CLI that orchestrates multiple AI coding assistants through a single interface with built-in safety (guard), data collection (extract), and MCP tool integration.

```bash
npm install -g @hanzo/dev
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                         @hanzo/dev CLI                               │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  User Input                                                          │
│      │                                                               │
│      ▼                                                               │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                    hanzo-guard (Input)                        │   │
│  │  • PII redaction (SSN, credit cards, emails, API keys)       │   │
│  │  • Injection detection                                        │   │
│  │  • Content filtering                                          │   │
│  └──────────────────────────────────────────────────────────────┘   │
│      │                                                               │
│      ▼                                                               │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                    Orchestration Layer                         │   │
│  │  ┌────────┐ ┌────────┐ ┌────────┐ ┌────────┐ ┌────────┐      │   │
│  │  │ Claude │ │ Hanzo Dev  │ │ Qwen   │ │ Gemini │ │ Ollama │      │   │
│  │  │  CLI   │ │  CLI   │ │  CLI   │ │  CLI   │ │ (local)│      │   │
│  │  └────────┘ └────────┘ └────────┘ └────────┘ └────────┘      │   │
│  │                                                                │   │
│  │  Provider selection via:                                       │   │
│  │  • --provider flag                                             │   │
│  │  • HANZO_PROVIDER env var                                     │   │
│  │  • Auto-selection based on task type                          │   │
│  └──────────────────────────────────────────────────────────────┘   │
│      │                                                               │
│      ▼                                                               │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                         MCP Layer                              │   │
│  │  • 260+ tools from @hanzo/mcp                                 │   │
│  │  • Tool routing and execution                                  │   │
│  │  • Guard integration for tool I/O                             │   │
│  └──────────────────────────────────────────────────────────────┘   │
│      │                                                               │
│      ▼                                                               │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                    hanzo-guard (Output)                        │   │
│  │  • Output sanitization                                         │   │
│  │  • Response filtering                                          │   │
│  └──────────────────────────────────────────────────────────────┘   │
│      │                                                               │
│      ▼                                                               │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                    hanzo-extract                               │   │
│  │  • Conversation logging (opt-in)                              │   │
│  │  • Training data extraction                                    │   │
│  │  • Session metadata                                            │   │
│  └──────────────────────────────────────────────────────────────┘   │
│      │                                                               │
│      ▼                                                               │
│  Output to User                                                      │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

## CLI Commands

### Primary Usage

```bash
# Default: uses configured provider (or auto-selects)
dev "fix the bug in auth.ts"

# Explicit provider selection
dev --provider claude "refactor this function"
dev --provider codex "write tests for utils.ts"
dev --provider qwen "explain this code"
dev --provider gemini "optimize performance"
dev --provider vibe "mistral-powered assistance"
dev --provider copilot "github copilot integration"
dev --provider ollama "local inference"

# Interactive mode
dev -i
dev --interactive

# With MCP tools
dev --tools "filesystem,git,docker" "create a new feature branch"
```

### Configuration

```bash
# Set default provider
dev config set provider claude

# Enable/disable guard
dev config set guard.enabled true
dev config set guard.pii true
dev config set guard.injection true

# Enable extract for training data
dev config set extract.enabled true
dev config set extract.anonymize true
dev config set extract.output ~/.hanzo/conversations/

# Configure MCP
dev config set mcp.servers "hanzo-mcp,filesystem,git"
```

### Provider Management

```bash
# List available providers
dev providers list

# Check provider status
dev providers status

# Install provider CLI
dev providers install claude
dev providers install dev
dev providers install qwen
dev providers install gemini
```

## Integration Components

### 1. hanzo-guard (Rust)

Located at: `github.com/hanzoai/guard`

```rust
// Integration in orchestrator
use hanzo_guard::{Guard, GuardConfig};

let guard = Guard::new(GuardConfig::default());
let safe_input = guard.sanitize_input(user_input).await?;
// ... send to provider ...
let safe_output = guard.sanitize_output(provider_response).await?;
```

### 2. hanzo-extract (Rust)

Located at: `github.com/hanzoai/extract`

```rust
// Conversation extraction
use hanzo_extract::{ConversationExtractor, ConversationFormat};

let extractor = ConversationExtractor::new(config);
extractor.log_exchange(input, output, provider, metadata).await?;
extractor.export(ConversationFormat::Jsonl).await?;
```

### 3. @hanzo/mcp (TypeScript)

Located at: `github.com/hanzoai/mcp`

```typescript
import { createMCPServer, Guard } from "@hanzo/mcp";

const server = await createMCPServer({
  tools: ["filesystem", "git", "docker"],
  guard: { enabled: true, pii: true },
});
```

## Provider Adapters

Each provider has a standardized adapter interface:

```typescript
interface ProviderAdapter {
  name: string;
  isInstalled(): Promise<boolean>;
  install(): Promise<void>;
  execute(input: string, options: ExecuteOptions): AsyncIterable<string>;
  getCapabilities(): ProviderCapabilities;
}

interface ProviderCapabilities {
  streaming: boolean;
  tools: boolean;
  vision: boolean;
  maxContext: number;
}
```

### Implemented Adapters

| Provider | Package                     | CLI Tool     | Status |
| -------- | --------------------------- | ------------ | ------ |
| Claude   | `@anthropic-ai/claude-code` | `claude`     | ✅     |
| Codex    | `@hanzo/dev`                | `dev/codex`  | ✅     |
| Qwen     | `qwen-cli`                  | `qwen`       | ✅     |
| Gemini   | `@google/gemini-cli`        | `gemini`     | ✅     |
| Vibe     | `vibe`                      | `vibe`       | ✅     |
| Copilot  | `gh (extension)`            | `gh copilot` | ✅     |
| Ollama   | `ollama`                    | `ollama`     | ✅     |

## Data Flow

### 1. Input Flow

```
User Input
    │
    ├─► Guard.sanitizeInput()
    │       ├─► PII Detection & Redaction
    │       ├─► Injection Detection
    │       └─► Content Filtering
    │
    ├─► Provider Selection
    │       ├─► --provider flag
    │       ├─► HANZO_PROVIDER env
    │       └─► Auto-select by task
    │
    └─► Provider Execution
            └─► Stream to output
```

### 2. Output Flow

```
Provider Response
    │
    ├─► Guard.sanitizeOutput()
    │       ├─► PII Detection & Redaction
    │       └─► Content Filtering
    │
    ├─► Extract.logExchange()
    │       ├─► Anonymization (if enabled)
    │       └─► JSONL storage
    │
    └─► Display to User
```

### 3. Tool Execution Flow

```
Tool Call
    │
    ├─► Guard.sanitizeToolInput()
    │
    ├─► MCP Tool Execution
    │       └─► @hanzo/mcp server
    │
    ├─► Guard.sanitizeToolOutput()
    │
    └─► Return to Provider
```

## Configuration File

`~/.hanzo/dev.yaml`:

```yaml
# Default provider
provider: claude

# Guard settings
guard:
  enabled: true
  pii:
    enabled: true
    redact:
      - ssn
      - credit_card
      - email
      - phone
      - api_key
  injection:
    enabled: true
    block: true

# Extract settings
extract:
  enabled: false # opt-in
  anonymize: true
  output: ~/.hanzo/conversations/
  format: jsonl

# MCP settings
mcp:
  servers:
    - hanzo-mcp
    - filesystem
    - git
    - docker
  guard_integration: true

# Provider settings
providers:
  claude:
    model: claude-sonnet-4-20250514
    api_key: ${ANTHROPIC_API_KEY}
  dev:
    model: gpt-4
    api_key: ${OPENAI_API_KEY}
  qwen:
    model: qwen-max
    api_key: ${DASHSCOPE_API_KEY}
  gemini:
    model: gemini-pro
    api_key: ${GOOGLE_API_KEY}
  ollama:
    model: qwen3:0.6b
    endpoint: http://localhost:11434
```

## Implementation Plan

### Phase 1: Core Orchestration (Week 1-2)

- [ ] Create orchestration layer in `dev-cli/src/orchestrator/`
- [ ] Implement provider adapter interface
- [ ] Add Claude adapter (primary)
- [ ] Add Hanzo Dev adapter
- [ ] Integrate hanzo-guard for I/O sanitization
- [ ] Add configuration management

### Phase 2: MCP Integration (Week 3)

- [ ] Integrate @hanzo/mcp tools
- [ ] Add guard middleware for tool calls
- [ ] Implement tool routing

### Phase 3: Extract Integration (Week 4)

- [ ] Integrate hanzo-extract
- [ ] Add conversation logging
- [ ] Implement anonymization
- [ ] Add export functionality

### Phase 4: Additional Providers (Week 5-6)

- [ ] Add Qwen adapter
- [ ] Add Gemini adapter
- [ ] Add Ollama adapter
- [ ] Implement auto-selection logic

### Phase 5: Polish & Release (Week 7-8)

- [ ] Documentation
- [ ] Testing
- [ ] NPM publishing
- [ ] GitHub Actions CI/CD

## Related Repositories

- **hanzo-guard**: https://github.com/hanzoai/guard
- **hanzo-extract**: https://github.com/hanzoai/extract
- **hanzo-mcp**: https://github.com/hanzoai/mcp
- **@hanzo/dev**: https://github.com/hanzoai/dev (this repo)

## Security Considerations

1. **API Keys**: Never log or expose API keys
2. **PII**: Always redact before logging/extract
3. **Injection**: Block detected injection attempts
4. **Local-first**: Support local models via Ollama for sensitive data
5. **Opt-in Extract**: Training data collection must be explicitly enabled
