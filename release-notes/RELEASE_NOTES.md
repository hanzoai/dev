## @just-every/code v0.4.1

This release polishes Auto Drive progress feedback and restores the CLI prompt label for smoother command runs.

### Changes
- Auto Drive: show in-progress summaries in the card so runs surface status while they execute.
- Auto Drive: refresh gradients and status colors to clarify automation progress states.
- TUI: restore the CLI send prompt label and stabilize vt100 rendering.
- Core/Debug: capture outgoing headers and order usage logs for clearer traces.

### Install
```bash
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.4.0...v0.4.1
