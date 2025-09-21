# Hanzo Dev - Composable Library Refactoring

## Core Design Principles

### 1. DRY (Don't Repeat Yourself)
- **Single Source of Truth**: Each piece of knowledge has exactly one representation
- **One Way to Do Things**: Clear, canonical paths for all operations
- **No Alternative Implementations**: Remove duplicate functionality

### 2. Orthogonality
- **Independent Dimensions**: Components operate on separate concerns
- **No Feature Overlap**: Each module has a distinct responsibility
- **Clean Composition**: Modules combine without interference

### 3. First Principles Architecture
```
hanzo-core/
├── protocol/       # Wire protocols (pure data types)
├── transport/      # Network layer (HTTP, WebSocket, SSE)
├── auth/          # Authentication (API keys, tokens)
├── models/        # Model definitions and capabilities
├── tools/         # Tool abstractions and registry
├── sandbox/       # Execution sandboxing
├── conversation/  # Conversation management
└── client/        # Unified client interface
```

## Refactored Module Structure

### `hanzo-protocol` - Pure Data Types
```rust
// No business logic, just types
pub mod wire {
    pub enum WireProtocol { Chat, Responses, Completion }
    pub trait Message: Serialize + DeserializeOwned {}
}

pub mod events {
    pub enum Event { Text(String), Tool(ToolCall), Complete }
    pub trait EventStream: Stream<Item = Event> {}
}
```

### `hanzo-transport` - Network Abstraction
```rust
// Single responsibility: network communication
pub trait Transport: Send + Sync {
    type Error;
    async fn send(&self, msg: impl Message) -> Result<Response, Self::Error>;
    fn stream(&self, msg: impl Message) -> impl EventStream;
}

// Implementations
pub struct HttpTransport { client: reqwest::Client }
pub struct WebSocketTransport { ws: tokio_tungstenite::WebSocket }
```

### `hanzo-auth` - Authentication Layer
```rust
// Orthogonal to transport and models
pub trait AuthProvider: Send + Sync {
    async fn authenticate(&self, req: &mut Request) -> Result<()>;
}

pub enum Auth {
    ApiKey(String),
    Bearer(String),
    ChatGPT { access: String, account: String },
}

impl AuthProvider for Auth {
    // Single implementation, no alternatives
}
```

### `hanzo-models` - Model Registry
```rust
// Model capabilities as data, not code
pub struct Model {
    pub id: ModelId,
    pub family: ModelFamily,
    pub capabilities: Capabilities,
}

pub struct Capabilities {
    pub context_window: usize,
    pub max_output: usize,
    pub supports_tools: bool,
    pub supports_reasoning: bool,
}

// Registry pattern instead of hardcoded lists
pub struct ModelRegistry {
    models: HashMap<ModelId, Model>,
}

impl ModelRegistry {
    pub fn register(&mut self, model: Model);
    pub fn get(&self, id: &ModelId) -> Option<&Model>;
}
```

### `hanzo-tools` - Tool System
```rust
// Tools as first-class citizens
#[async_trait]
pub trait Tool: Send + Sync {
    fn schema(&self) -> Schema;
    async fn execute(&self, params: Value) -> Result<Value>;
}

pub struct ToolRegistry {
    tools: HashMap<String, Box<dyn Tool>>,
}

// Composable tool builders
pub struct ToolBuilder<T> {
    phantom: PhantomData<T>,
}

impl<T: Tool> ToolBuilder<T> {
    pub fn with_retry(self, policy: RetryPolicy) -> RetryTool<T>;
    pub fn with_cache(self, ttl: Duration) -> CachedTool<T>;
    pub fn with_sandbox(self, policy: SandboxPolicy) -> SandboxedTool<T>;
}
```

### `hanzo-sandbox` - Execution Isolation
```rust
// Single sandbox abstraction
pub trait Sandbox: Send + Sync {
    async fn execute(&self, cmd: Command) -> Result<Output>;
}

// Policy-based configuration
pub struct SandboxPolicy {
    pub filesystem: FsPolicy,
    pub network: NetPolicy,
    pub resources: ResourceLimits,
}

// Platform-specific implementations
#[cfg(target_os = "linux")]
pub struct SeccompSandbox;

#[cfg(target_os = "macos")]
pub struct SeatbeltSandbox;
```

### `hanzo-conversation` - State Management
```rust
// Immutable conversation history
pub struct Conversation {
    id: ConversationId,
    messages: Arc<RwLock<Vec<Message>>>,
}

// Event-sourced updates
pub enum ConversationEvent {
    MessageAdded(Message),
    ToolExecuted(ToolCall, ToolResult),
}

impl Conversation {
    pub async fn apply(&self, event: ConversationEvent) -> Result<()>;
    pub async fn snapshot(&self) -> ConversationState;
}
```

### `hanzo-client` - Unified Interface
```rust
// Single client, composable configuration
pub struct Client {
    transport: Arc<dyn Transport>,
    auth: Arc<dyn AuthProvider>,
    models: Arc<ModelRegistry>,
    tools: Arc<ToolRegistry>,
}

impl Client {
    pub fn builder() -> ClientBuilder {
        ClientBuilder::default()
    }

    pub async fn chat(&self, req: ChatRequest) -> Result<ChatResponse>;
    pub fn stream(&self, req: ChatRequest) -> impl Stream<Item = Event>;
}

// Fluent builder API
impl ClientBuilder {
    pub fn transport(self, transport: impl Transport) -> Self;
    pub fn auth(self, auth: impl AuthProvider) -> Self;
    pub fn model(self, model: Model) -> Self;
    pub fn tool(self, name: &str, tool: impl Tool) -> Self;
    pub fn build(self) -> Client;
}
```

## Migration Strategy

### Phase 1: Extract Core Types
1. Create `hanzo-protocol` crate with pure types
2. Move wire protocols from scattered locations
3. Unify event definitions

### Phase 2: Isolate Transport
1. Extract HTTP/SSE logic to `hanzo-transport`
2. Remove transport code from model-specific logic
3. Implement transport trait for all backends

### Phase 3: Centralize Auth
1. Create `hanzo-auth` with all auth strategies
2. Remove auth logic from client code
3. Implement auth provider trait

### Phase 4: Model Registry
1. Replace hardcoded model lists with registry
2. Move model detection to data-driven approach
3. Load models from configuration

### Phase 5: Tool System
1. Extract tools to `hanzo-tools`
2. Implement tool trait for all tools
3. Create composable tool decorators

### Phase 6: Client Unification
1. Create single `hanzo-client` entry point
2. Remove alternative client implementations
3. Provide migration guide

## Benefits

### For Hanzo Ecosystem
- **Clean Integration**: Import only needed components
- **No Forking Required**: Extend via traits, not modifications
- **Stable API**: Versioned interfaces with compatibility

### For Developers
- **Single Import**: `use hanzo_client::Client`
- **Discoverable API**: Builder pattern guides usage
- **Type Safety**: Compile-time guarantees

### For Maintenance
- **Clear Boundaries**: Each module has one job
- **Easy Testing**: Mock at trait boundaries
- **Independent Evolution**: Modules version separately

## Example Usage

```rust
use hanzo_client::{Client, Auth};
use hanzo_tools::shell::ShellTool;

#[tokio::main]
async fn main() -> Result<()> {
    let client = Client::builder()
        .auth(Auth::from_env()?)
        .model("gpt-5")
        .tool("shell", ShellTool::sandboxed())
        .build();

    let response = client.chat("Hello, world!").await?;
    println!("{}", response);

    Ok(())
}
```

## Next Steps

1. Create new crate structure
2. Move code incrementally
3. Maintain compatibility layer
4. Deprecate old interfaces
5. Release as Hanzo SDK v1.0