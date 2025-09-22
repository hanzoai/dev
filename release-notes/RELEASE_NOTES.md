## @just-every/code v0.2.158

This release integrates ACP, improves CLI MCP access, refines TUI limits, and includes stability fixes.

### Changes
- Core/ACP: integrate ACP support and sync protocol updates.
- CLI: expose MCP via code subcommand and add acp alias; ship code-mcp-server on install.
- TUI/Limits: refresh layout, show compact usage, align hourly/weekly windows.
- TUI/Limits: fix hourly window display and reset timing.
- Stability: respect web search flag; clear spinner after final answer; accept numeric MCP protocolVersion.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.157...v0.2.158
