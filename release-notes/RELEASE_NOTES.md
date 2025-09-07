## @just-every/code v0.2.85

This release improves TUI stream ordering and approvals UX, and fixes web search event ordering.

### Changes

- TUI: insert plan/background events near-time and keep reasoning ellipsis during streaming.
- TUI: approvals cancel immediately on deny and use a FIFO queue.
- Core: fix web search event ordering by stamping OrderMeta for in-turn placement.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.84...v0.2.85
