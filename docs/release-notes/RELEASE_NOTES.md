## @hanzo/dev v0.6.77

This release improves automatic context selection defaults and strengthens context-mode reliability.

### Changes

- Core/Context: default session context mode to `auto` for better out-of-the-box context selection.
- Auto Context: enrich 1M-judge risk signals to improve context quality and decision reliability.
- TUI/Context: persist explicit disabled state for 1M mode so settings stay consistent across sessions.

### Install

```bash
npm install -g @hanzo/dev@latest
dev
```

Compare: https://github.com/hanzoai/dev/compare/v0.6.76...v0.6.77
