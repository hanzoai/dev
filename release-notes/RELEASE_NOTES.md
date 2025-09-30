## @just-every/code v0.2.179

This release refines auto-drive visibility while keeping transcripts accurate and colors aligned across terminals.

### Changes

- Auto-drive: persist conversation between turns and retain the raw coordinator transcript so context carries forward.
- Auto-drive: restore streaming reasoning titles and tidy decision summaries by removing stray ellipses.
- Auto-drive: surface spinner status when the composer is hidden, show progress in the title, and refresh the footer CTA styling.
- Auto-drive: expand coordinator guidance with AUTO_AGENTS instructions to keep automation setups aligned.
- TUI/Theme: reuse a shared RGB mapping for ANSI fallbacks to make colors consistent across terminals.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.178...v0.2.179
