## @just-every/code v0.2.144

This release improves the reliability and signal of our CI-driven issue comment automation.

### Changes

- CI/Issue comments: make agent assertion non-fatal; fail only on proxy 5xx; keep fallback path working.
- CI: gate agent runs on OPENAI key; fix secrets condition syntax; reduce noisy stream errors; add proxy log tail for debug.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.143...v0.2.144
