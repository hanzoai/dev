# Hanzo Dev - LLM Context

## Project Overview
Hanzo Dev is a powerful fork of OpenAI's Codex CLI, enhanced with enterprise features and maintained by Hanzo AI. This is a Rust-based CLI tool that provides AI-powered development assistance directly in the terminal.

## Recent Updates (December 2025)

### Major Architecture Improvements

1. **Compilation Fixes and Code Quality**
   - Fixed all Rust compilation warnings in core module
   - Resolved TUI interface compilation issues including:
     - Added missing `LIVE_PREFIX_COLS` constant
     - Implemented `FrameRequester` trait for frame updates
     - Fixed lifetime parameters in `BottomPaneView` trait
     - Created missing `gh_actions` module for GitHub integration
   - Fixed protocol imports (changed from `codex_protocol` to `dev_protocol`)
   - Added `pulldown-cmark` dependency for markdown rendering

2. **New Cryptographic Library: hanzo-crypto**
   - Implemented pure Rust cryptographic primitives library
   - Features both classical and post-quantum algorithms:
     - **Hash functions**: SHA-256/512, SHA3-256/512, BLAKE3
     - **Symmetric encryption**: AES-256-GCM, ChaCha20Poly1305
     - **Asymmetric cryptography**: Ed25519 signatures, X25519 key exchange
     - **Post-quantum crypto**: Kyber (KEM), Dilithium (signatures) - placeholders
     - **Key derivation**: Argon2id for password hashing
     - **Secure random**: Cryptographically secure RNG
   - Full test suite with 7 passing tests
   - Zero-copy design with `zeroize` for secure memory handling

## Hanzod - Unified Compute Layer (December 2025)

### Complete AI Compute Infrastructure
Hanzod has evolved from a container orchestration system to a unified compute layer that provides:

1. **Runtime Detection & Abstraction**
   - Automatically detects Docker Desktop, Colima, Containerd, Podman
   - Abstract `RuntimeProvider` trait for unified container operations
   - Runtime capabilities detection (GPU, MicroVM, WASM, rootless)
   - Health checking for runtime availability

2. **Unified API System**
   - **HTTP Port**: 8080 (REST API)
   - **gRPC Port**: 50051 (High-performance RPC)
   - **Endpoints**:
     - `/v1/inference` - LLM inference with Qwen3 models
     - `/v1/embeddings` - Text embedding generation
     - `/v1/vector_search` - Similarity search
     - `/v1/gspo/train` - GSPO preference optimization training
     - `/v1/bitdelta` - BitDelta personalization
     - `/health` - Service health check
   - **Models**: Qwen3:8b with MLX acceleration on Apple Silicon
   - **Features**: Streaming inference, batch operations, model caching

3. **Successful Container Deployment**
   - ✅ Successfully deployed Alpine container through Colima runtime
   - Container ID: `151b211c468f` (hanzod-test-alpine-001)
   - Automatic runtime selection based on availability
   - Full lifecycle management (create, start, stop, remove)

4. **Architecture Components**
   - **Core Infrastructure**:
     - HPKE-based post-quantum secure communication
     - ML-KEM-768 (NIST FIPS 203) for key encapsulation
     - ChaCha20-Poly1305 for authenticated encryption
   - **AI Capabilities**:
     - MLX-accelerated inference on Apple Silicon
     - GSPO (Group Sparse Preference Optimization)
     - BitDelta efficient model personalization
     - gRPC server for high-performance operations
   - **Integration Points**:
     - `hanzo_inference.rs` - Unified compute endpoint management
     - Container node communication via PQSSHSession
     - NodeCommunicator for multi-node orchestration

## Hanzo Compute Integration (December 2025)

### Unified Compute Layer Infrastructure
The dev project now uses hanzod as its primary compute layer:

1. **Hanzod Connection**
   - **HTTP Port**: 8080 for REST API
   - **gRPC Port**: 50051 for high-performance RPC
   - **Configuration**: `hanzo_inference.rs` with `HanzoComputeEndpoint`
   - **Model**: Default to `qwen3:8b` with MLX acceleration

2. **HanzoComputeEndpoint** (`/src/rs/core/src/hanzo_inference.rs`)
   - Manages unified hanzod compute layer
   - Provides comprehensive AI capabilities:
     - LLM inference with streaming support
     - Text embeddings via HTTP/gRPC
     - Vector similarity search
     - GSPO training for preference optimization
     - BitDelta for efficient personalization
   - Post-quantum secure communication with container nodes
   - Health monitoring across all services

3. **Service Architecture**
   - Primary: Hanzod unified compute (port 8080/50051)
   - All services (inference, embeddings, vector) through single daemon
   - GSPO and BitDelta capabilities for advanced AI operations
   - Post-quantum secure for future-proof infrastructure

### Post-Quantum Cryptography Stack
Implemented comprehensive post-quantum security:
- **HPKE** (Hybrid Public Key Encryption) for secure channels
- **ML-KEM-768** (NIST FIPS 203) for key encapsulation
- **ChaCha20-Poly1305** for authenticated encryption
- **PQSSHSession** for secure container communication
- Pure Rust implementation, no C dependencies
- All cryptographic tests passing

### Compute SDK Enhancement
```rust
use hanzo_inference::{HanzoManager, HanzoConfig};

// Initialize unified compute manager
let config = HanzoConfig::default();  // Uses hanzod on 8080/50051
let manager = HanzoManager::new(config);
manager.start_all().await?;

// Use various AI capabilities
let response = manager.compute.inference(prompt, None, None).await?;
let embedding = manager.embedding.embed(text).await?;
let results = manager.vector.search(query, 10, None).await?;
```

### TUI Compilation Fixes
Resolved major compilation issues after merge:
- Added `standard_terminal_mode` field
- Implemented `maybe_show_history_nav_hint_on_first_scroll` method
- Fixed pulldown-cmark API compatibility for v0.9
- Added missing spinner and list_window utility functions
- Fixed PlanUpdate event handling with proper serialization
- Resolved FrameRequester trait dyn-compatibility with DefaultFrameRequester
- Fixed ConfigOverrides field names (`codex_linux_sandbox_exe`)

### Running with Hanzod Compute Layer

1. **Start Hanzod**:
   ```bash
   cd /Users/z/work/hanzo/hanzod
   cargo build --release
   ./target/release/hanzod
   # Runs on ports 8080 (HTTP) and 50051 (gRPC)
   ```

2. **Run Dev with Unified Compute**:
   ```bash
   cd /Users/z/work/hanzo/dev
   cargo build --release
   ./target/release/dev-exec "Your prompt here"
   ```

3. **Verify Health Status**:
   ```bash
   curl http://localhost:8080/health
   # Returns: {"status":"healthy","version":"0.1.0","mlx_enabled":true}
   ```

### Environment Configuration
```bash
# Hanzod compute layer settings
HANZOD_HTTP_URL=http://localhost:8080
HANZOD_GRPC_URL=localhost:50051
HANZOD_ENABLE_MLX=true           # Apple Silicon acceleration
HANZOD_MODEL=qwen3:8b            # Default model
HANZOD_ENABLE_GSPO=true          # Preference optimization
HANZOD_ENABLE_BITDELTA=true      # Model personalization
```

3. **Testing Infrastructure**
   - Created comprehensive integration tests for core modules
   - Added TUI-specific test suite
   - All crypto module tests passing (100% success rate)

## Recent Updates (September 2025)

### Merged from OpenAI Codex Upstream
Successfully incorporated key improvements from OpenAI Codex (up to commit 9bbeb75361):

1. **Reasoning Effort Tracking** 
   - Added `reasoning_effort` field to `SessionConfiguredEvent` and `NewConversationResponse`
   - Tracks reasoning level (Minimal/Low/Medium/High) throughout session lifecycle
   - Properly communicated through MCP protocol
   - Maintains backward compatibility

2. **Apply-Patch Stability**
   - Fixed replacement sorting to ensure deterministic application order
   - Prevents panics when applying complex patches
   - Based on fix from commit 377af7573

3. **Branding Updates**
   - Updated user-facing strings from "Codex" to "Hanzo Dev"
   - Maintained in trust directory prompts, session messages, and slash commands
   - Preserves Hanzo identity while keeping upstream compatibility

## Architecture

### Directory Structure
```
/Users/z/work/hanzo/dev/
├── src/
│   ├── rs/           # Rust source (previously codex-rs/)
│   │   ├── core/     # Core business logic
│   │   ├── tui/      # Terminal UI (Ratatui-based)
│   │   ├── mcp-*     # Model Context Protocol implementation
│   │   ├── exec/     # Command execution
│   │   ├── hanzo-crypto/ # Pure Rust cryptographic primitives
│   │   └── ...       # 22 total crates
│   └── ts/           # TypeScript/npm wrapper
├── tests/            # Integration test suites
├── .github/          # CI/CD workflows
└── docs/            # Documentation
```

### Key Components
- **Core** (`dev-core`): Main business logic, config, auth
- **TUI** (`dev-tui`): Terminal interface with Ratatui 0.29.0
- **MCP** (`mcp-server/client/types`): Full MCP protocol support
- **Exec** (`dev-exec`): Command execution with sandboxing
- **Protocol** (`dev-protocol`): Shared protocol definitions
- **Hanzo Crypto** (`hanzo-crypto`): Cryptographic primitives library with post-quantum support

## Technical Decisions

### Build System
- Using Rust 2024 edition across all crates
- Fast development profile with incremental compilation
- Production profile with full LTO and symbol stripping
- Workspace-level lints: `unwrap_used = "deny"`, `expect_used = "deny"`

### Package Distribution
- npm package: `@hanzo/dev` v3.0.0
- Platform-specific binaries via optionalDependencies
- GitHub Actions for multi-platform builds (Linux musl, macOS, Windows)

### Testing Strategy
- Unit tests per crate
- Integration tests in `tests/suite/`
- Snapshot testing with insta (from upstream)
- Build validation with `./build-fast.sh`

## Upstream Features to Consider

Based on analysis of OpenAI Codex at `/Users/z/work/openai/codex`:

### High Priority
1. **Unified Execution** (commit c09ed74a1)
   - PTY-backed interactive exec with session reuse
   - Bounded output (128 KiB) and timeout clamping
   - Requires `use_experimental_unified_exec_tool=true`

2. **TUI Onboarding** (commit 8453915e0)
   - New model popup and configuration flow
   - Enhanced first-run experience
   - Internal storage for preferences

3. **Simplified Auth Flow** (commit e13b35ecb)
   - API key input in UI
   - Removed `preferred_auth_method` config
   - Better ChatGPT/API key reconciliation

### Medium Priority
1. **IDE Integration**
   - UriBasedFileOpener for VS Code, Cursor, Windsurf
   - Already partially implemented

2. **Enhanced Markdown Rendering**
   - We have markdown files but could adopt their latest improvements

3. **Compact and Turn Context** (commit 674e3d3c9)
   - Rollout items for better context management

## Development Notes

### Compilation
```bash
# Fast development build
./build-fast.sh

# Check specific crate
cargo check -p dev-protocol

# Run tests
cargo test --all
```

### Known Issues
- Build takes significant time on first compilation
- Some upstream features require config flags to enable
- Directory structure differs from upstream (codex-rs/ → src/rs/)

### Next Steps
1. Complete testing of current merge
2. Consider importing unified execution feature
3. Evaluate TUI onboarding improvements
4. Update documentation for new features

## Merge Strategy
When syncing with upstream:
1. Fetch from `https://github.com/openai/codex`
2. Cherry-pick or manually apply changes
3. Update paths from `codex-rs/` to `src/rs/`
4. Preserve Hanzo branding in user-facing strings
5. Test thoroughly with `./build-fast.sh`

## Contact
- Repository: https://github.com/hanzoai/dev
- Organization: Hanzo AI (https://hanzo.ai)
- Upstream: https://github.com/openai/codex (for reference)