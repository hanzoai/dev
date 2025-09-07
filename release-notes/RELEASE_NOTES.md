## @just-every/code v0.2.83

This release improves theme-aware JSON output in the TUI and fixes a stability issue in the embedded apply_patch scanner.

### Changes
- TUI: theme-aware JSON preview in Exec output; use UI-matched highlighting and avoid white backgrounds.
- TUI: apply UI-themed JSON highlighting for stdout; clear ANSI backgrounds so output inherits theme.
- Core: replace fragile tree-sitter query with a heredoc scanner in embedded apply_patch to prevent panics.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.82...v0.2.83

