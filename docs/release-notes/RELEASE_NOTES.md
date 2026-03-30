## @just-every/code v0.6.86

This release improves authentication reliability, CLI piping workflows, and stability across TUI, MCP, and sandbox paths.

### Changes
- Auth: suppress stale tokens after refresh failures and avoid duplicate refresh attempts for more reliable sign-in.
- CLI: add stdin piping support for `codex exec` to improve shell composition workflows.
- TUI: polish app-server UX with plugin menu cleanup, skills picker scrolling fixes, and ghost subagent entry fixes.
- MCP: improve startup reliability with increased startup timeout and fixes for startup warning regressions.
- Sandbox: harden Windows and Linux sandbox behavior with network proxy support and safer `bwrap` resolution.

### Install
```bash
npm install -g @just-every/code@latest
code
```

### Thanks
Thanks to @felipecoury, @siggisim, and @sluongng for contributions!

Compare: https://github.com/just-every/code/compare/v0.6.85...v0.6.86
