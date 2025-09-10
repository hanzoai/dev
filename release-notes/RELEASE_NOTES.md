## @just-every/code v0.2.108

This release refines the theme/spinner experience in the TUI and clarifies exec output.

### Changes
- TUI: Add /theme Overview→Detail flow with live previews for Theme and Spinner selection.
- TUI: Bundle full cli-spinners set and allow choosing your loading spinner; 'diamond' stays default.
- TUI: Improve scrolling with anchored 9-row viewport; keep selector visible and dark-theme friendly.
- Core: Split stdout/stderr in Exec output and add ERROR divider on failures for clarity.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.107...v0.2.108
