# Hanzo Dev - LLM Context

## Project Overview
Hanzo Dev is a powerful fork of OpenAI's Codex CLI, enhanced with enterprise features and maintained by Hanzo AI. This is a Rust-based CLI tool that provides AI-powered development assistance directly in the terminal.

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
│   │   └── ...       # 21 total crates
│   └── ts/           # TypeScript/npm wrapper
├── .github/          # CI/CD workflows
└── docs/            # Documentation
```

### Key Components
- **Core** (`dev-core`): Main business logic, config, auth
- **TUI** (`dev-tui`): Terminal interface with Ratatui 0.29.0
- **MCP** (`mcp-server/client/types`): Full MCP protocol support
- **Exec** (`dev-exec`): Command execution with sandboxing
- **Protocol** (`dev-protocol`): Shared protocol definitions

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