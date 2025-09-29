## @just-every/code v0.2.176

This release improves auto-drive observability and smooths several TUI upgrade and theming workflows.

### Changes

- Auto-drive: add an observer thread and telemetry stream to watch automation health in real time.
- TUI/Update: harden guided upgrade flows to recover cleanly from partial runs.
- TUI/Theme: introduce dedicated 16-color palettes so limited terminals render accurately.
- TUI: trim fallback prompt copy and reset upgrade flags after completion.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.175...v0.2.176
