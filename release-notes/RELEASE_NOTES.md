## @just-every/code v0.2.175

This release hardens auto-drive with smarter recovery and new developer tooling for rehearsing faults.

### Changes

- TUI/Auto-drive: add retry/backoff orchestration so coordinator runs recover after transient failures.
- TUI/Auto-drive: honor rate-limit reset hints and jittered buffers to resume safely after 429 responses.
- Docs: outline dev fault injection knobs for rehearsing auto-drive failure scenarios.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.174...v0.2.175
