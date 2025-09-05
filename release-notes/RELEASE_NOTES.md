## @just-every/code v0.2.67

This release improves Windows typing reliability and refines our issue triage automation.

### Changes

- TUI: prevent doubled characters on Windows by ignoring Repeat/Release for printable keys.
- CI: issue triage improves comment-mode capture, writes DECISION.json, and adds token fallbacks for comment/assign/close steps.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.66...v0.2.67
