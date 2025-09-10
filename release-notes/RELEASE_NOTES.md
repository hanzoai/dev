## @just-every/code v0.2.115

Improves TUI status resilience during transient connection issues.

### Changes

- TUI/Status: keep spinner visible; show 'Reconnecting' on transient errors.
- TUI/Status: treat retry/disconnect errors as background notices, not fatal.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.114...v0.2.115
