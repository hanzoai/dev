# Rust-Python SDK Parity Plan

## Python SDK Architecture (Reference)

```
hanzoai/                    # Core client library (generated from OpenAPI)
├── _client.py              # Main client class
├── llm_client.py           # Simplified LLM interface
├── mcp/                    # Model Context Protocol
├── agents/                 # Agent orchestration
├── auth/                   # Authentication
├── types/                  # Type definitions
└── cluster/                # Distributed compute

hanzo/                      # Meta-package with CLI and tools
├── dev.py                  # Dev client implementation
├── batch_orchestrator.py   # Batch processing
├── memory_manager.py       # Conversation memory
├── model_registry.py       # Model management
├── streaming.py            # SSE streaming
└── tools/                  # Tool implementations

hanzo-mcp/                  # MCP protocol implementation
hanzo-agents/               # Agent framework
hanzo-memory/               # Memory persistence
hanzo-network/              # P2P networking
```

## Rust SDK Architecture (Target)

```
hanzoai/                    # Core client library (matches Python)
├── src/
│   ├── client.rs           # Main client struct
│   ├── llm_client.rs       # Simple LLM interface
│   ├── mcp/                # Model Context Protocol
│   ├── agents/             # Agent orchestration
│   ├── auth/               # Authentication
│   ├── types/              # Type definitions
│   └── cluster/            # Distributed compute

hanzo/                      # Meta-crate with CLI and runtime
├── src/
│   ├── dev.rs              # Dev client implementation
│   ├── batch_orchestrator.rs
│   ├── memory_manager.rs
│   ├── model_registry.rs
│   ├── streaming.rs
│   └── tools/

hanzo-mcp/                  # MCP protocol crate
hanzo-agents/               # Agent framework crate
hanzo-memory/               # Memory persistence crate
hanzo-network/              # P2P networking crate
```

## Key Design Decisions

### 1. Client Structure Parity
Both Python and Rust use the same client initialization pattern:

```python
# Python
client = Hanzo(api_key="sk-...")
response = client.chat.create(messages=[...])
```

```rust
// Rust
let client = Hanzo::new("sk-...");
let response = client.chat().create(messages).await?;
```

### 2. Module Organization
- **hanzoai**: Core API client (OpenAPI-generated in Python, hand-written in Rust)
- **hanzo**: High-level convenience wrapper with CLI
- **hanzo-***: Specialized functionality in separate crates

### 3. Async/Sync Parity
Python provides both sync and async clients:
```python
client = Hanzo()        # Sync
client = AsyncHanzo()   # Async
```

Rust is async-first but provides blocking wrappers:
```rust
let client = Hanzo::new();           // Async
let client = BlockingHanzo::new();   // Blocking wrapper
```

### 4. Error Handling
Both use typed errors with similar hierarchy:
- HanzoError (base)
- APIError
- AuthenticationError
- RateLimitError
- etc.

### 5. Tool System
Same trait/protocol approach:
```python
class Tool(Protocol):
    def execute(self, params: Dict) -> Any
```

```rust
trait Tool {
    async fn execute(&self, params: Value) -> Result<Value>
}
```

## Implementation Plan

### Phase 1: Core Client (`hanzoai` crate)
```rust
// hanzoai/src/lib.rs
pub mod client;
pub mod auth;
pub mod types;
pub mod mcp;
pub mod agents;

pub use client::{Hanzo, AsyncHanzo};
pub use auth::Auth;

// Simple entry point matching Python
pub fn completion(prompt: &str) -> Result<String> {
    // Quick completion API
}

pub fn set_api_key(key: &str) {
    // Global API key setting
}
```

### Phase 2: Dev Package (`hanzo` crate)
```rust
// hanzo/src/lib.rs
pub use hanzoai::*;  // Re-export core

pub mod dev;
pub mod batch_orchestrator;
pub mod memory_manager;
pub mod model_registry;
pub mod streaming;
pub mod tools;

// CLI entry point
pub mod cli;
```

### Phase 3: MCP Implementation (`hanzo-mcp` crate)
```rust
// hanzo-mcp/src/lib.rs
pub mod protocol;
pub mod server;
pub mod client;
pub mod tools;

// Match Python's bridge pattern
pub struct Bridge {
    tools: HashMap<String, Box<dyn Tool>>,
}
```

### Phase 4: Agents (`hanzo-agents` crate)
```rust
// hanzo-agents/src/lib.rs
pub trait Agent {
    async fn think(&self, context: Context) -> Thought;
    async fn act(&self, thought: Thought) -> Action;
}
```

## Benefits of Parity

1. **Consistent API**: Developers can switch between Python and Rust with minimal friction
2. **Shared Documentation**: Same concepts, same structure
3. **Tool Compatibility**: Tools written in either language work the same way
4. **Testing Parity**: Test suites can be ported directly
5. **Migration Path**: Easy to move from Python prototype to Rust production

## Example: Identical Usage

```python
# Python
from hanzoai import Hanzo

client = Hanzo(api_key="sk-...")
response = client.chat.create(
    model="gpt-5",
    messages=[{"role": "user", "content": "Hello"}]
)
print(response.choices[0].message.content)
```

```rust
// Rust
use hanzoai::Hanzo;

let client = Hanzo::new("sk-...");
let response = client.chat().create(
    "gpt-5",
    vec![Message::user("Hello")]
).await?;
println!("{}", response.choices[0].message.content);
```

## Testing Strategy

1. **Port Python Tests**: Every Python test gets a Rust equivalent
2. **Cross-Language Testing**: Ensure both SDKs produce identical outputs
3. **Protocol Testing**: Verify wire format compatibility
4. **Performance Benchmarks**: Rust should be faster but functionally identical

## Migration Guide

For users moving from Python to Rust:

| Python | Rust |
|--------|------|
| `client = Hanzo()` | `let client = Hanzo::new()` |
| `client.chat.create()` | `client.chat().create()` |
| `@tool` decorator | `#[tool]` attribute macro |
| `async def` | `async fn` |
| `Dict[str, Any]` | `HashMap<String, Value>` |
| `Optional[T]` | `Option<T>` |
| `Union[A, B]` | `enum { A(A), B(B) }` |

## Next Steps

1. Generate Rust types from OpenAPI spec (like Python)
2. Implement core client with same method signatures
3. Port Python tests to Rust
4. Create cross-language integration tests
5. Document migration patterns