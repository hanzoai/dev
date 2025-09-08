## @just-every/code v0.2.89

This release adds an in-app help overlay and improves composer editing with undo and line delete, plus smoother branch finalize handling.

### Changes

- TUI/Help: add Ctrl+H help overlay with key summary; update footer hint.
- TUI/Input: add Ctrl+Z undo in composer and route it to Chat correctly.
- TUI/Input: map Ctrl+Backspace to delete the current line in composer.
- TUI/Branch: treat "nothing to commit" as success on finalize and continue cleanup.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.88...v0.2.89
