## @just-every/code v0.2.167

This release smooths shell workflows and makes the status view richer and easier to audit.

### Changes

- TUI/Terminal: allow a blank dollar prompt to open a shell instantly.
- TUI/Status: rebuild /status with card layout and richer reasoning context.
- TUI/Limits: persist rate-limit warning logs across sessions so spikes stay visible.
- Core/Compact: store inline auto-compaction history to stabilize collapsed output.
- TUI/Input: restore Warp.dev command+option editing for smoother text adjustments.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.166...v0.2.167
