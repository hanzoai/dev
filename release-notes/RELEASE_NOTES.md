## @just-every/code v0.2.165

This release polishes terminal theming, stabilizes agents, and tightens automation defaults.

### Changes

- TUI/Theme: cache terminal background detection and skip OSC probe when theme is explicit.
- Agents: clear idle spinner and avoid empty task preview text in chat.
- Workflows: escape issue titles in PR fallback for issue-code automation.
- MCP Server: use codex_mcp_server imports for bundled tooling compatibility.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.164...v0.2.165
