## @just-every/code v0.2.156

This release introduces a dedicated rate limits view, faster TUI rendering, and several UX and stability improvements.

### Changes
- TUI/Limits: add /limits view with live snapshots and persisted reset times.
- Performance: speed up exec/history rendering via layout and metadata caching.
- Approval: require confirmation for manual terminal commands; add semantic prefix matching.
- Core: report OS and tool info for better diagnostics.
- TUI/History: show run duration, collapse wait tool output, and finalize cells cleanly.

### Install
```
npm install -g @just-every/code@latest
code
```

### Thanks
Thanks to Ahmed Ibrahim, Jeremy Rose, and Michael Bolin for contributions!

Compare: https://github.com/just-every/code/compare/v0.2.155...v0.2.156
