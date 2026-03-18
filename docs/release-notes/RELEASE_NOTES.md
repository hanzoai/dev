## @hanzo/dev v0.6.82

This release fixes TLS connectivity to api.hanzo.ai, strips inline thinking tags from zen model output, and improves assistant message styling.

### Changes

- Transport: force rustls TLS backend on all reqwest clients — fixes "bad protocol version" errors when connecting to api.hanzo.ai on macOS (SecureTransport incompatibility with Go crypto/tls).
- TUI/Rendering: strip `<think>...</think>` blocks from zen model streaming output instead of rendering them as raw text.
- TUI/Rendering: boost assistant message background tint from 5% to 12% for visible contrast on dark terminals.
- TUI/Rendering: extend assistant message background to full terminal width (edge-to-edge).

### Install

```bash
npm install -g @hanzo/dev@latest
dev
```

Compare: https://github.com/hanzoai/dev/compare/v0.6.76...v0.6.77
