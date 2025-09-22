## @just-every/code v0.2.162

This release improves CLI resume reliability and hardens runtime handling.

### Changes

- CLI/Resume: fix --last to reliably select the most recent session under active runtimes.
- Stability: avoid nested Tokio runtime creation during resume lookup to prevent sporadic failures.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.161...v0.2.162

