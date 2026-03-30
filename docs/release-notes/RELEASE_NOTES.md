## @just-every/code v0.6.87

This release improves CI reliability for cross-platform validation and release readiness.

### Changes

- CI: switch `rust-ci-full` Windows jobs to hosted GitHub runners to reduce runner pool dependency in release validation.
- CI: move Linux and Windows target matrices in `rust-ci-full` to hosted runners, improving cross-platform build reliability in restricted environments.

### Install

```bash
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.6.86...v0.6.87
