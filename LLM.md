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

## Hanzo Node Integration (September 2025)

### Local Inference Infrastructure
The dev project now integrates with Hanzo Node for local AI capabilities:

1. **Hanzo Node Connection**
   - **Port**: 3690 (corrected from 11434 which is Ollama's port)
   - **Engine Port**: 36900 for Hanzo Engine
   - **Configuration**: `hanzo_integration.rs` and `hanzo_inference.rs`
   - **Model**: Default to `gpt-oss:20b` for local inference

2. **HanzoInferenceManager** (`/src/rs/core/src/hanzo_inference.rs`)
   - Manages local hanzod process lifecycle
   - Provides OpenAI-compatible API interface
   - Supports:
     - LLM inference without external APIs
     - Local embeddings generation
     - Vector search capabilities
   - Auto-backgrounding for long-running processes

3. **Provider Fallback Chain**
   - Primary: Hanzo Local (port 3690)
   - Fallback 1: OpenAI (if API key present)
   - Fallback 2: Claude/Anthropic (if API key present)
   - Fallback 3: Custom API endpoints

### Pure Rust Cryptography Migration
Replaced C-based OQS with pure-Rust saorsa-pqc v0.3.13:
- **ML-KEM-768** (FIPS 203) for key encapsulation
- **ML-DSA-65** (FIPS 204) for digital signatures
- Improved compilation (no C dependencies)
- All 7 cryptographic tests passing

### Agent SDK Enhancement
Created `HanzoNodeProvider` in the agent library:
```python
from agents import HanzoNodeProvider, create_hanzo_node_provider

# Direct connection to Hanzo node
provider = create_hanzo_node_provider(port=3690)
model = provider.get_model("gpt-oss:20b")
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

### Running with Hanzo Node

1. **Start the Hanzo Node**:
   ```bash
   cd /Users/z/work/hanzo/node
   cargo run --release --bin hanzod
   ```

2. **Run Dev with Local Inference**:
   ```bash
   cd /Users/z/work/hanzo/dev
   ./target/debug/dev-exec --oss "Your prompt here"
   ```

3. **Test Agent SDK Integration**:
   ```bash
   cd /Users/z/work/hanzo/agent
   python test_hanzo_node.py
   ```

### Environment Configuration
```bash
# Hanzo Node settings
HANZO_NODE_URL=http://localhost:3690/v1
HANZO_ENGINE_URL=http://localhost:36900
HANZO_ENABLE_QWEN3=true
HANZO_ENABLE_RERANKER=true
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