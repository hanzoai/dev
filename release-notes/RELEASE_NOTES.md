## @just-every/code v0.2.118

This release introduces AI-powered custom themes with a full creation flow, along with reliability and UX improvements across the TUI.

### Changes

- TUI/Theme: add AI-powered custom theme creation with live preview, named themes, and save without switching.
- Theme Create: stream reasoning/output for live UI; salvage first JSON object; show clear errors with raw output for debugging.
- Theme Persist: apply custom colors only when using Custom; clear colors/label when switching to built-ins.
- TUI: improve readability and input — high-contrast loading/input text; accept Shift-modified characters.
- TUI: capitalize Overview labels; adjust "[ Close ]" spacing and navigation/height.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.117...v0.2.118
