## @just-every/code v0.2.177

This release hardens secure-mode startup and keeps auto-drive guidance in sync with the latest plan.

### Changes

- Core/CLI: centralize pre-main process hardening into codex-process-hardening and invoke it automatically when secure mode is enabled.
- CLI/Proxy: rename the responses proxy binary to codex-responses-api-proxy, harden startup, and remove request timeouts so streaming stays reliable.
- Auto-drive: relay plan updates to the coordinator so guidance stays aligned with the latest steps.
- TUI/Auto-drive: show the waiting spinner only while the coordinator is active to avoid idle animation churn.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.176...v0.2.177
