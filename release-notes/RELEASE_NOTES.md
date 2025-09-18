## @just-every/code v0.2.153

This release improves TUI reliability, restores resume history, and refines configuration defaults.

### Changes
- Core/Config: prioritize ~/.code for legacy config reads and writes.
- TUI/History: strip sed/head/tail pipes when showing line ranges.
- TUI: skip alternate scroll on Apple Terminal for smoother scrolling.
- Resume: restore full history replay.
- Core: persist GPT-5 overrides across sessions.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.152...v0.2.153
