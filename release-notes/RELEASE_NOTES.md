## @just-every/code v0.2.121

Bugfix release improving CLI ESM compatibility.

### Changes

- CLI: make coder.js pure ESM; replace internal require() with fs ESM APIs.
- CLI: avoid require in isWSL() to prevent CJS issues under ESM.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.120...v0.2.121

