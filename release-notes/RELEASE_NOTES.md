## @just-every/code v0.2.59

A stability and UX polish release: strict per‑turn ordering, instant cancels, and safer output rendering. Your terminal stays tidy and snappy.

### Changes
- TUI: enforce strict global ordering and require stream IDs for stable per‑turn history.
- TUI/Core: make cancel/exit immediate during streaming; kill child process on abort to avoid orphans.
- TUI: sanitize diff/output (expand tabs; strip OSC/DCS/C1/zero‑width) for safe rendering.
- TUI: add WebFetch tool cell with preview; preserve first line during streaming.
- TUI: restore typing on Git Bash/mintty by normalizing key event kind (Windows).

### Install
```
npm install -g @just-every/code
code
```

Compare: v0.2.56...v0.2.59

