## @just-every/code v0.2.170

This update modernizes the TUI event pipeline and shores up notifications, quotas, and account workflows.

### Changes

- TUI/History: drive exec, assistant, explore, and rate-limit cells from domain events for consistent streaming.
- TUI/Notifications: add an OSC toggle command and harden slash routing, persistence, and filters so alerts stay accurate.
- Usage/Rate-limits: compact persisted stats, relog after resets, and persist reset state to keep quotas current.
- TUI/Accounts: prioritize ChatGPT accounts in login flows and restore the label prefix for clarity.
- UX: show a session resume hint on exit, surface Zed model selection, and restore Option+Enter newline plus Cmd+Z undo.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.169...v0.2.170
