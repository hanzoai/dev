## @hanzo/dev v3.1.0

Major upstream sync with OpenAI Codex - 3363 commits merged.

### Highlights
- **Upstream Sync**: Merged latest openai/codex codebase (3363 commits)
- **TUI Improvements**: Updated status indicator with shimmer animations and cleaner elapsed time formatting
- **API Updates**: Latest codex-rs improvements including app-server protocol updates
- **SDK Enhancements**: TypeScript SDK with improved streaming support

### Breaking Changes
- Some auto-review tests temporarily disabled pending upstream fixes
- Config schema updates may require re-running `just write-config-schema`

### Install
```
npm install -g @hanzo/dev@latest
dev
```

Compare: https://github.com/hanzoai/dev/compare/v3.0.6...v3.1.0

---

## @hanzo/dev v3.0.6

This release adds polyglot SDKs for the ZAP protocol across multiple languages.

### Changes
- ZAP SDKs: Added comprehensive SDKs for OCaml, Ada, Fortran, Haskell, Swift, and Kotlin
- Protocol: Zero-latency Async Protocol bindings with Cap'n Proto serialization
- Cross-platform: Support for all major platforms via native bindings

### Install
```
npm install -g @hanzo/dev@latest
dev
```

Compare: https://github.com/hanzoai/dev/compare/v3.0.5...v3.0.6
