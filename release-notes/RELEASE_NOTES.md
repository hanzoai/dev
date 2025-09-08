## @just-every/code v0.2.94

This release improves access mode UX, adds a clear footer indicator, and persists your choice per project.

### Changes

- TUI: add footer access‑mode indicator; Shift+Tab cycles Read Only / Approval / Full Access.
- TUI: show access‑mode status as a background event early; update Help with shortcut.
- Core: persist per‑project access mode in config.toml and apply on startup.
- Core: clarify read‑only write denials and block writes immediately in RO mode.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.93...v0.2.94

