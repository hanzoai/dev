## @just-every/code v0.2.91

Stability update focused on terminal recovery during unexpected background panics.

### Changes

- TUI/Panic: restore terminal state and exit cleanly on any thread panic.
- TUI/Windows: prevent broken raw mode/alt-screen after background panics under heavy load.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.90...v0.2.91
