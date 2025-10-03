## @just-every/code v0.2.184

This update sharpens model selection and exec visibility while adding clearer server diagnostics.

### Changes

- Core: Record pid alongside port in server info to simplify local process debugging.
- CLI: Support CODEX_API_KEY in `codex exec` so credentials can be set via environment.
- TUI: Make the model switcher a two-stage flow to prevent accidental model swaps.
- TUI: Surface live context window usage while tasks run to clarify token budgets.
- TUI: Show a placeholder when commands produce no output to keep history legible.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.183...v0.2.184
