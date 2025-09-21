# Rust Code Simplification Plan

## Key Issues Identified

### 1. Type System Issues
- **Problem**: Mixed use of String and &str, unnecessary cloning
- **Solution**: Use `Cow<'_, str>` for flexible string ownership, implement `AsRef<str>` traits

### 2. Configuration Complexity
- **Problem**: Giant Config struct with 50+ fields, no clear separation of concerns
- **Solution**: Break into smaller, focused config structs with builder pattern

### 3. Error Handling
- **Problem**: Mix of Result types, unclear error propagation
- **Solution**: Unified error type with thiserror, proper error contexts

### 4. Async/Sync Mixing
- **Problem**: Inconsistent async usage, blocking operations in async contexts
- **Solution**: Clear async boundaries, use tokio::spawn_blocking for CPU-intensive work

### 5. Model Family Design
- **Problem**: String-based model identification, runtime parsing
- **Solution**: Strong enum types, compile-time guarantees

## Proposed Improvements

### 1. Simplified Config Structure
```rust
// Instead of one giant Config struct, use composition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub core: CoreConfig,
    pub model: ModelConfig,
    pub sandbox: SandboxConfig,
    pub tools: ToolsConfig,
    pub ui: UiConfig,
}

// Each sub-config is focused and manageable
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModelConfig {
    pub provider: Provider,
    pub settings: ModelSettings,
    pub auth: AuthConfig,
}

// Use builder pattern for construction
impl Config {
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::default()
    }
}
```

### 2. Better Error Types
```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum HanzoError {
    #[error("Configuration error: {0}")]
    Config(#[from] ConfigError),

    #[error("Model error: {0}")]
    Model(#[from] ModelError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Network error: {0}")]
    Network(#[from] reqwest::Error),
}

pub type Result<T> = std::result::Result<T, HanzoError>;
```

### 3. Improved Model Family
```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModelFamily {
    Gpt35,
    Gpt4 { version: Gpt4Version },
    Gpt5 { variant: Gpt5Variant },
    Qwen { version: QwenVersion },
    Custom(Box<CustomModel>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Gpt5Variant {
    Standard,
    Codex,
}

impl ModelFamily {
    pub const fn supports_reasoning(&self) -> bool {
        matches!(self, Self::Gpt5 { .. })
    }

    pub const fn default_temperature(&self) -> f32 {
        match self {
            Self::Gpt5 { variant: Gpt5Variant::Codex } => 0.2,
            _ => 0.7,
        }
    }
}
```

### 4. Simplified Conversation Management
```rust
// Use Arc<RwLock> more idiomatically
pub struct ConversationManager {
    conversations: Arc<RwLock<HashMap<Uuid, Conversation>>>,
    auth: Arc<AuthManager>,
}

pub struct Conversation {
    id: Uuid,
    codex: Arc<Codex>,
    metadata: ConversationMetadata,
}

impl ConversationManager {
    pub async fn create(&self, config: Config) -> Result<ConversationHandle> {
        let codex = Codex::spawn(config.clone()).await?;
        let conversation = Conversation::new(codex);
        let id = conversation.id;

        self.conversations.write().await.insert(id, conversation);

        Ok(ConversationHandle {
            id,
            manager: Arc::downgrade(&Arc::new(self.clone())),
        })
    }
}

// Handle provides safe access without exposing internals
pub struct ConversationHandle {
    id: Uuid,
    manager: Weak<ConversationManager>,
}
```

### 5. Stream Processing
```rust
// Use proper stream traits
use futures::stream::{Stream, StreamExt};
use tokio::sync::mpsc;

pub struct EventStream {
    receiver: mpsc::UnboundedReceiver<Event>,
}

impl Stream for EventStream {
    type Item = Event;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.receiver.poll_recv(cx)
    }
}
```

### 6. Tool Registry Pattern
```rust
// Instead of hardcoded tool lists, use a registry
pub struct ToolRegistry {
    tools: HashMap<String, Box<dyn Tool>>,
}

#[async_trait]
pub trait Tool: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    async fn execute(&self, params: Value) -> Result<Value>;
}

impl ToolRegistry {
    pub fn register<T: Tool + 'static>(&mut self, tool: T) {
        self.tools.insert(tool.name().to_string(), Box::new(tool));
    }

    pub async fn execute(&self, name: &str, params: Value) -> Result<Value> {
        self.tools
            .get(name)
            .ok_or_else(|| HanzoError::ToolNotFound(name.to_string()))?
            .execute(params)
            .await
    }
}
```

## Immediate Actions

1. **Create new error module** with thiserror-based types
2. **Refactor Config** into smaller, composed structs
3. **Implement proper builder patterns** for complex types
4. **Use type state pattern** for Codex initialization
5. **Replace string-based model detection** with enums
6. **Implement proper Stream traits** for event handling
7. **Add #[must_use] attributes** where appropriate
8. **Use Cow<str> instead of String clones**
9. **Implement From/TryFrom traits** for conversions
10. **Add proper lifetime annotations** where beneficial

## Testing Strategy

1. **Property-based testing** with proptest for parsers
2. **Snapshot testing** for serialization
3. **Integration tests** with test containers
4. **Benchmark critical paths** with criterion

## Migration Path

1. Create new modules alongside old ones
2. Implement compatibility layers
3. Gradually migrate consumers
4. Remove old implementations
5. Update documentation

## Performance Improvements

1. **Reduce allocations**: Use &str, Cow, and ArrayVec
2. **Lazy initialization**: Use OnceCell for expensive computations
3. **String interning**: For repeated model names
4. **Zero-copy parsing**: Where possible with nom or winnow
5. **Async optimizations**: Proper use of tokio::select! and join!

## Code Quality

1. **Clippy lints**: Enable pedantic lints gradually
2. **Format**: Consistent rustfmt configuration
3. **Documentation**: Doc comments with examples
4. **Examples**: Working examples for each module
5. **Benchmarks**: Track performance regressions