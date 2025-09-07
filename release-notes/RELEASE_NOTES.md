## @just-every/code v0.2.81

Maintenance release with upstream‑merge and CI stability improvements.

### Changes

- CI: run TUI invariants guard only on TUI changes and downgrade to warnings to reduce false failures.
- CI: upstream-merge workflow hardens context prep; handle no merge-base and forbid unrelated histories.
- CI: faster, safer fetch and tools — commit-graph/blobless fetch, cached ripgrep/jq, skip tag fetch to avoid clobbers.
- CI: improve reliability — cache Cargo registry, guard apt installs, upload .github/auto artifacts and ignore in git; fix DEFAULT_BRANCH.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.80...v0.2.81
