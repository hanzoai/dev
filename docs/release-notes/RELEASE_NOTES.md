## @just-every/code v0.6.89

This release improves CI reliability so release validation is less likely to fail on slow hosted runners.

### Changes

- CI: increase hosted-runner time budget for argument lint jobs to reduce timeout-related release failures.
- CI: improve release pipeline stability by giving slower lint runs more time before cancellation.

### Install

```bash
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.6.88...v0.6.89
