## @just-every/code v0.2.87

This release improves history navigation and clarifies the /branch workflow with consistent background updates.

### Changes

- TUI/History: Make Shift+Up/Down navigate history in all popups; persist UI-only slash commands to history.
- TUI/Branch: Preserve visibility by emitting 'Switched to worktree: <path>' after session swap; avoid losing the confirmation message on reset.
- TUI/Branch: Use BackgroundEvent for all /branch status and errors; retry with a unique name if the branch exists; propagate effective branch to callers.
- TUI/Branch: Split multi-line worktree message into proper lines for clarity.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.86...v0.2.87
