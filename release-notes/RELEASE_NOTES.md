## @just-every/code v0.2.150

This release adds a merge workflow, polishes history and agents editor UX, and restores jq search and alt‑screen scrolling.

### Changes
- TUI/Branch: add /merge command and show diff summary in merge handoff.
- TUI/Agents: refine editor UX and persistence; keep instructions/buttons visible and tidy spacing.
- TUI/History: render exec status separately, keep gutter icon, and refine short-command and path labels.
- Core/TUI: restore jq search and alt-screen scrolling; treat jq filters as searches.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.149...v0.2.150
