## @just-every/code v0.2.142

This release improves CI automation for cleaner, more reliable runs.

### Changes

- CI: avoid placeholder-only issue comments to reduce noise.
- CI: gate Code generation on OPENAI_API_KEY; skip gracefully when missing.
- CI: ensure proxy step runs reliably in workflows.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.141...v0.2.142
