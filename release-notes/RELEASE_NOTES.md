## @just-every/code v0.2.178

This release clarifies automation transcripts, improves rate-limit feedback, and keeps colors consistent across terminals.

### Changes

- Auto-drive: restructure coordinator transcript to clarify CLI roles and context.
- Auto-drive: show coordinator summary while CLI commands execute so guidance stays visible.
- Auto-drive: require mandatory observer fields to avoid partial telemetry updates.
- TUI: round rate-limit windows with local reset times for accurate throttling feedback.
- TUI/Theme: preserve assistant tint across palettes to keep colors consistent across terminals.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.177...v0.2.178
