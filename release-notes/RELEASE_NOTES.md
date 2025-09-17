## @just-every/code v0.2.152

This release adds a terminal overlay, richer explore summaries, and safer execution guards.

### Changes
- TUI: add terminal overlay and agent install flow.
- TUI/Explore: enrich run summaries with pipeline context; polish explore labels.
- Core/Exec: enforce dry-run guard for formatter commands.
- Explore: support read-only git commands.
- TUI: add plan names and sync terminal title.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.151...v0.2.152
