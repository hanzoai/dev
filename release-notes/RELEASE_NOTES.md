## @just-every/code v0.2.107

This release fixes a planning crash and improves runtime stability.

### Changes
- Core: Fix planning crash on UTF-8 boundary when previewing streamed text.
- Stability: Use char-safe slicing for last 800 chars to prevent panics.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.106...v0.2.107
