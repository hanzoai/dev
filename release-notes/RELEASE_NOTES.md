## @just-every/code v0.2.86

This release adds a new branching workflow in the TUI and improves execution reliability, with updated slash‑command docs.

### Changes

- TUI: add `/branch` to create worktrees, switch sessions, and finalize merges.
- Core: treat only exit 126 as sandbox denial to avoid false escalations.
- Docs: add comprehensive slash command reference and link from README.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.85...v0.2.86
