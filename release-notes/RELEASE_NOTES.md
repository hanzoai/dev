## @just-every/code v0.4.0

A landmark release for Code: Auto Drive graduates into a multi-agent powerhouse, the TUI gains a card-based activity stream, and every setting now lives in a single overlay.

### Massive upgrades to Auto Drive
- Auto Drive now orchestrates complex projects end to end, coordinating agents, running self-checks, and recovering from transient failures.
- `/auto` sessions can be left unattended — plan a run, hand control to Auto Drive, and come back to finished work.
- Multi-agent stacks get smarter the more helpers you configure; we already rely on Auto Drive in ~70% of internal sessions.

### All `/settings` in one place
- A new two-level settings overlay gathers limits, themes, automation toggles, and CLI integrations under a single `/settings` command.
- Quickly inspect which features are enabled, tweak model routing, and return to coding without losing context.

### Card-based UX refresh
- Agents, Browser sessions, Web Search, and Auto Drive now render as compact cards in history, with overlays for full detail when you need it.
- Grouped actions highlight overall progress while preserving deep logs for debugging.

### Huge performance improvements
- CPU and memory hotspots uncovered during large agent runs are fixed, keeping Code responsive even with heavy automation.
- Stream rendering and history updates are smoother, trimming scroll jank on long sessions.

### Agent upgrades
- `/plan`, `/code`, and other commands can target different orchestrator CLIs, letting you mix fast research models (e.g., `gemini-2.5-flash`) with heavyweight builders (`claude-sonnet-4.5`).
- Future releases will let you register additional CLIs directly from the new settings hub.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.188...v0.4.0
