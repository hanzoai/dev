## @just-every/code v0.2.120

This release improves Windows/WSL installation reliability by hardening paths and locks.

### Changes

- CLI/Install: harden Windows and WSL install paths to avoid misplacement.
- CLI/Install: improve file locking to reduce conflicts during upgrade.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.119...v0.2.120
