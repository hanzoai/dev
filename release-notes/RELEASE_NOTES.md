## @just-every/code v0.2.147

This release adds an opt-in for mirroring modified Git submodule pointers to support advanced workflows while keeping defaults stable.

### Changes

- Core/Git Worktree: add opt-in mirroring of modified submodule pointers via CODEX_BRANCH_INCLUDE_SUBMODULES.
- Core/Git: keep default behavior unchanged to avoid unexpected submodule pointer updates.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.146...v0.2.147
