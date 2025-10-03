## @just-every/code v0.2.183

This release refines explore sessions so TUI context stays accurate.

### Changes

- TUI/Explore: keep the Exploring header until the next non-reasoning entry to maintain exploration context.
- TUI/Explore: sync reasoning visibility changes with explore cells to avoid stale header state.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.182...v0.2.183
