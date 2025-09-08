## @just-every/code v0.2.88

This release refines git behavior in worktrees and adds helpful TUI polish across footer hints, input shortcuts, and branch workflows.

### Changes

- Core/Git: ensure 'origin' exists in new worktrees and set origin/HEAD for default branch to improve git UX.
- TUI/Footer: show one-time Shift+Up/Down history hint on first scroll.
- TUI/Input: support macOS Command-key shortcuts in the composer.
- TUI/Branch: add hidden preface for auto-submitted confirm/merge-and-cleanup flow; prefix with '[branch created]' for clarity.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.87...v0.2.88

