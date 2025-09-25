## @just-every/code v0.2.164

This release sharpens rate limit visibility and keeps postinstall flows resilient.

### Changes

- TUI/Limits: track API reset timers across core and TUI so rate windows stay accurate.
- CLI/Postinstall: restore shim detector and avoid overwriting existing code shim so installs stay intact.
- Core/Config: allow overriding OpenAI wire API and support OpenRouter routing metadata for custom deployments.
- Core/Agents: cap agent previews and handle updated truncation tuple to stay within API limits.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.163...v0.2.164
