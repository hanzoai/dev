## @just-every/code v0.2.102

This release tightens triage push behavior and improves agent setup reliability.

### Changes
- CI/Triage: fetch remote before push and fall back to force-with-lease on non-fast-forward for bot-owned branches.
- Agents: pre-create writable CARGO_HOME and target dirs for agent runs to avoid permission errors.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.101...v0.2.102
