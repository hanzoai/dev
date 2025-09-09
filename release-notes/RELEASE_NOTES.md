## @just-every/code v0.2.101

This release improves build reliability, restores core exports, and brings a few TUI and MCP usability fixes.

### Changes
- Build: remove OpenSSL by using rustls in codex-ollama; fix macOS whoami scope.
- Core: restore API re-exports and resolve visibility warning.
- TUI: Ctrl+C clears non-empty prompts.
- TUI: paste with Ctrl+V checks file_list.
- MCP: add per-server startup timeout.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.100...v0.2.101

