## @just-every/code v0.2.92

This release improves agent isolation with dedicated worktrees and strengthens sandboxing to protect your repositories.

### Changes

- Core/Git Worktree: create agent worktrees under ~/.code/working/<repo>/branches for isolation.
- Core/Agent: sandbox non-read-only agent runs to worktree to prevent writes outside branch.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.91...v0.2.92

