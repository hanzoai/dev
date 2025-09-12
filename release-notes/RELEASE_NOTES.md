## @just-every/code v0.2.141

This release refines exec output controls and hardens CI for stability.

### Changes

- Exec: allow suppressing per‑turn diff output via CODE_SUPPRESS_TURN_DIFF to reduce noise.
- CI: speed up issue‑code jobs with cached ripgrep/jq and add guards for protected paths and PR runtime.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.140...v0.2.141

