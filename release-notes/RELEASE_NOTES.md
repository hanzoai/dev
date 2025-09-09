## @just-every/code v0.2.99

This release improves branch finalize reliability and speeds up history rendering.

### Changes

- TUI/Branch: finalize merges default into worktree first; prefer fast-forward; start agent on conflicts.
- TUI/History: cache Exec wrap counts and precompute PatchSummary layout per width to reduce measurement.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.98...v0.2.99
