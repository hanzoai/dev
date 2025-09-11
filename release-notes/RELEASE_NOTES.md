## @just-every/code v0.2.119

This patch fixes Windows global upgrade failures and improves installer/launcher behavior for reliable upgrades.

### Changes

- CLI/Windows: fix global upgrade failures (EBUSY/EPERM) by caching the native binary per-user and preferring the cached launcher.
- Installer: on Windows, install binary to %LocalAppData%\just-every\code\<version>; avoid leaving a copy in node_modules.
- Launcher: prefer running from cache; mirror into node_modules only on Unix for smoother upgrades.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.118...v0.2.119
