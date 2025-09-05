## @just-every/code v0.2.61

Maintenance-only release improving release automation; no functional changes.

### Changes

- No functional changes; maintenance-only release focused on CI.
- CI: trigger releases only from tags; parse version from tag to prevent unintended runs.
- CI: reduce noise by enforcing [skip ci] on notes-only commits and ignoring notes-only paths.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.60...v0.2.61
