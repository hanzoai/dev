## @just-every/code v0.2.172

This release expands auto-drive guidance, rounds out MCP connectivity, and adds a proxy command for Responses API workflows.

### Changes

- CLI: introduce a responses API proxy command so shared hosts can forward Responses calls securely.
- MCP: add streamable HTTP client support and tighten per-call timeout handling.
- Auto-drive: stream coordinator reasoning, keep plan context, and smooth heading presentation.
- TUI/History: route diff and explore cells through domain events for consistent playback.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.171...v0.2.172
