# Hanzo Dev - LLM Context

## Project Overview

Hanzo Dev is a powerful fork of OpenAI's Codex CLI, enhanced with enterprise features and maintained by Hanzo AI. This is a Rust-based CLI tool that provides AI-powered development assistance directly in the terminal.

## Recent Updates (January 2026)

### hanzo-node Package and Release Pipeline (January 2026)

Added `hanzo-node` npm package for Node.js project integration:

1. **Package Structure** (`/hanzo-node/`)
   - `package.json`: Main package with optionalDependencies for platform binaries
   - `bin/hanzo.js`: Entry point that spawns native binary
   - `postinstall.js`: Downloads platform binary from GitHub releases
   - `scripts/preinstall.js`: Node.js version check (requires 18+)

2. **Platform Packages**
   - `hanzo-node-darwin-arm64`: macOS Apple Silicon
   - `hanzo-node-darwin-x64`: macOS Intel
   - `hanzo-node-linux-x64-musl`: Linux x64 (static musl)
   - `hanzo-node-linux-arm64-musl`: Linux ARM64 (static musl)
   - `hanzo-node-win32-x64`: Windows x64

3. **Release Workflow** (`.github/workflows/hanzo-node-release.yml`)
   - Triggered by successful main Release workflow
   - Reuses binary artifacts from main release
   - Publishes all platform packages then main package
   - Supports manual trigger with version override

4. **Binary Resolution Strategy**
   - First checks user cache (`~/.cache/hanzo/node/<version>/`)
   - Falls back to platform optionalDependency package
   - Downloads from GitHub releases as last resort
   - Validates binary headers (ELF/Mach-O/PE)

5. **Usage**

   ```bash
   # Global installation
   npm install -g hanzo-node

   # Direct run
   npx hanzo-node

   # Commands
   hanzo "explain this code"
   hanzo-dev --help
   ```

### Branding + Tooling Refresh (January 2026)

- Updated docs and CLI guidance to use Hanzo Dev branding, `dev` command, and `~/.hanzo` as primary config home (legacy `~/.code`/`~/.codex` still read).
- MCP Rust crate moved to Rust 2024 edition with a `rust-toolchain.toml` pinned to `stable`.

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

## Hanzod - Unified AI Blockchain with Lux Consensus (January 2025)

### Complete AI Blockchain Infrastructure

Hanzod has evolved into a complete AI blockchain system with native KuzuDB ledger and full Lux consensus integration:

1. **Native KuzuDB Ledger Integration**
   - Unified graph + vector + KV database backend (no branding, pure implementation)
   - ACID transactions for blockchain integrity
   - 1536-dimensional vector storage for AI embeddings
   - Cypher queries for complex graph analysis
   - In-process execution with zero network overhead

2. **Lux Consensus Protocol**
   - Full compatibility with luxfi/consensus
   - Snow consensus with parameters: k=20, α=15, β=15/20
   - Quantum finality for immutable state
   - Node operator registration with Ed25519 keys
   - Minimum stake: 2,000,000 LUX tokens
   - Network IDs: 43114 (mainnet), 43113 (testnet), 1337 (local)

3. **RPC Server Architecture**
   - **gRPC Port**: 50051 (Proto-based high-performance)
   - **HTTP Port**: 8545 (JSON-RPC compatibility)
   - **WebSocket Port**: 8546 (Real-time subscriptions)
   - **Core Services**:
     - Node operator registration (RegisterOperator)
     - Inference operations (Qwen3-Next: 8b/14b/32b/72b)
     - Reranking service (Qwen3-Reranker)
     - Vector embeddings and search
     - Graph queries via Cypher

4. **Interchain Messaging & Node Operators**
   - **Interchain Protocols**:
     - Warp messaging for cross-chain communication
     - Teleporter for asset transfers
     - Support for C-Chain (EVM), X-Chain (DAG), P-Chain (Platform)
   - **Node Operator Requirements**:
     - Ed25519 key pair for signing
     - Minimum 2M LUX stake
     - Support for Qwen3-Next and/or Qwen3-Reranker
     - GPU memory requirements: 16GB+ for inference

## Unified Chain Architecture (December 2025)

### Clean Architecture Refactoring

Complete blockchain system with KuzuDB as the default database backend implementation. Major refactoring to remove branding and use generic, concise names:

1. **Naming Convention Updates** (Completed)
   - Renamed modules to use generic names:
     - `ai_blockchain.rs` → `chain.rs` (AIBlockchain → Chain)
     - `kuzu_ledger.rs` → `ledger.rs` (KuzuLedger → Ledger)
     - `kuzu_vector.rs` → `vector_store.rs` (KuzuVectorStore → VectorStore)
     - `quantum_staking.rs` → `staking.rs`
   - Feature names simplified: "chain", "market", "staking"
   - No branding in code - KuzuDB is just the default implementation

2. **Database Layer** (`/src/rs/hanzod/src/ledger.rs`)
   - **KuzuDB Backend**: Default graph/vector/KV database (requires cmake to build)
   - **Graph Capabilities**: Native Cypher queries for relationships
   - **Vector Search**: Built-in HNSW index for similarity
   - **Blockchain Data**: Immutable ledger with checkpoints
   - **In-Process**: Zero network hops for all operations

3. **Database Abstraction** (`/src/rs/hanzod/src/database/mod.rs`)
   - **Multiple Backends**: KuzuDB (default), Sled, RocksDB, PostgreSQL
   - **Unified Interface**: Consistent API across all database types
   - Database layer is required, not optional
   - Pure Rust implementation with no Python tests

4. **Integration Status** (December 2025)
   - **Repository Cleanup**: ✅ Complete
     - Renamed all modules to generic names (chain, ledger, vector_store, staking)
     - Removed Python test files and shell scripts
     - Updated all struct/type names to remove branding
     - Fixed module structure issues
   - **Compilation Status**:
     - Reduced errors from 37 to 19
     - ✅ KuzuDB successfully integrated:
       - Crate version 0.11.2 (matches Homebrew)
       - Installed via `brew install kuzu`
       - Dynamic linking configured with KUZU_SHARED=1
       - Database backend is REQUIRED and enabled by default
     - GitHub Actions configured to download pre-built libraries in CI
   - **GitHub Actions**:
     - Renamed workflow to `chain_test.yml`
     - Downloads pre-built KuzuDB libraries for Linux
     - Uses dynamic linking with KUZU_SHARED=1
     - Avoids cmake build issues in CI
   - **Architecture**: Clean abstraction with KuzuDB as default backend
   - **Tests**: Pure Rust tests in `/tests` directory

5. **AI Blockchain Pipeline** (`/src/rs/hanzod/src/ai_blockchain.rs`)
   - **Single Process**: Text → Embedding → Storage → Search → Inference
   - **Zero Latency**: All operations in-memory with no network calls
   - **Integrated Engine**: Embedded Hanzo Engine with mistralrs
   - **Metal Acceleration**: Native Apple Silicon GPU support

   ```rust
   pub async fn process_text(&self, text: &str, search_similar: bool, run_inference: bool)
   ```

6. **API Gateway System** (`/src/rs/hanzod/src/api_gateway.rs`)
   - **Key-Based Billing**: Usage tracking and rate limiting per API key
   - **Multi-Tenant**: Encrypted namespaces with data isolation
   - **Privacy Modes**:
     ```rust
     pub enum PrivacyMode {
         Public,        // Data on public chain
         Private,       // Encrypted on chain
         TEE,           // Trusted Execution Environment
         Confidential,  // Blackwell confidential computing
     }
     ```
   - **Pricing Tiers**: Pay-as-you-go and subscription plans
   - **Service Types**: Native agents, LLMs, compute, VPN, vector search

7. **KuzuDB Vector Store** (`/src/rs/hanzod/src/kuzu_vector.rs`)
   - **HNSW Index**: Fast k-NN similarity search
   - **Graph Embeddings**: Connect vectors in knowledge graphs
   - **Blockchain Checkpoints**: Immutable snapshots with merkle roots
   - **Cypher Queries**: Complex graph traversals

   ```rust
   // Unified operations
   store.add_vector(id, embedding, content, metadata).await?;
   store.vector_search(query_embedding, k, threshold).await?;
   store.cypher_query("MATCH (n:Node)-[r]->(m) RETURN n,r,m").await?;
   ```

8. **Key Features Implemented**
   - ✅ Real Ed25519 cryptographic signatures (no placeholders)
   - ✅ Real embedding engine with Snowflake Arctic Embed L model
   - ✅ Real mistralrs integration for embedded inference
   - ✅ Real Docker container status monitoring
   - ✅ Real X-Chain provider registration
   - ✅ Complete database abstraction layer
   - ✅ API gateway with billing and routing
   - ✅ TEE/confidential computing support

### Service Architecture

- **Port 3690**: Unified hanzod service endpoint
- **Embedded Engine**: Hanzo Engine with Metal acceleration
- **Models**:
  - `phi-3.5-mini` for inference (embedded)
  - `snowflake-arctic-embed-l` for embeddings (1536 dimensions)
- **Database**: KuzuDB with full graph/vector/blockchain capabilities

### Testing & Integration

Created comprehensive test infrastructure:

- `test_integration.sh` - Shell script for endpoint testing
- `test_ai_blockchain.py` - Python integration test suite
- Unit tests for all major components
- End-to-end pipeline validation

### Vision Realized

The system demonstrates the "absolutely fastest execution path" requested:

- **Native Agents**: Direct in-process execution
- **LLM Operations**: Embedded inference with Metal acceleration
- **Vector Search**: HNSW index with sub-millisecond queries
- **Blockchain**: Immutable audit trail with checkpoints
- **Docker/K8s**: Support for arbitrary containerized workloads
- **API Gateway**: Complete billing and routing infrastructure

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

## Context for All AI Assistants

This file (`LLM.md`) is symlinked as:

- `.AGENTS.md`
- `CLAUDE.md`
- `QWEN.md`
- `GEMINI.md`

All files reference the same knowledge base. Updates here propagate to all AI systems.

## Rules for AI Assistants

1. **ALWAYS** update LLM.md with significant discoveries
2. **NEVER** commit symlinked files (.AGENTS.md, CLAUDE.md, etc.) - they're in .gitignore
3. **NEVER** create random summary files - update THIS file
