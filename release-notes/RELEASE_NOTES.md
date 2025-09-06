## @just-every/code v0.2.69

This release improves the TUI resume workflow and timer readability, fixes Windows key handling, and updates config/env behavior.

### Changes
- TUI: add session resume picker (--resume) and quick resume (--continue).
- TUI: show minutes/hours in thinking timer.
- Fix: skip release key events on Windows.
- Core: respect model family overrides from config.
- Breaking: stop loading project .env files.

### Install
```
npm install -g @just-every/code@latest
code
```

### Thanks
Thanks to @pakrym-oai and @jif-oai for contributions!

Compare: https://github.com/just-every/code/compare/v0.2.68...v0.2.69

