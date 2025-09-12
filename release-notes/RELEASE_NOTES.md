## @just-every/code v0.2.138

This release improves spinner creation reliability by honoring session auth and avoiding unnecessary retries.

### Changes

- TUI/Spinner: honor active auth (ChatGPT vs API key) for custom spinner generation to avoid 401s.
- Auth: prevent background AuthManager resets and align request shape with harness to stop retry loops.
- Stability: reduce spinner‑creation failures by matching session auth preferences.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.137...v0.2.138

