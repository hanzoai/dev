## @just-every/code v0.2.168

This release expands limits history coverage, hardens CLI startup, and keeps assistant context intact.

### Changes

- TUI/Limits: restore the 6 month history view with expanded layout, spacing, and weekday labels.
- TUI/History: persist assistant stream records so prior reasoning stays available after reloads.
- TUI/Worktrees: stop deleting other PID checkouts and clean up stale directories after EINTR interruptions.
- TUI/Chat: keep manual file search queries synced for repeat lookups.
- CLI: adopt the pre-main hardening hook to align with tighter runtime protections.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.167...v0.2.168
