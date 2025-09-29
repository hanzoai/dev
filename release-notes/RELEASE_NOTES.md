## @just-every/code v0.2.173

This release tightens the TUI browser loop so failures hand off to Code with clearer diagnostics.

### Changes

- TUI/Browser: auto hand off /browser startup failures to Code so sessions self-heal.
- TUI/Browser: sanitize and surface error details when handoff triggers for faster diagnosis.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.172...v0.2.173
