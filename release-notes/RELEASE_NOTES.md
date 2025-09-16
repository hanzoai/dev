## @just-every/code v0.2.149

This release refines the Agents editor, restores model presets, polishes reasoning UI, and improves exec/resume reliability.

### Changes
- TUI/Agents: redesign editor and list; keep Save/Cancel visible, add Delete, better navigation and scrolling.
- TUI/Model: restore /model selector and presets; persist model defaults; default local agent is "code".
- TUI/Reasoning: show reasoning level in header; keep reasoning cell visible; polish run cells and log claims.
- Exec/Resume: detect absolute bash and flag risky paths; fix race in unified exec; show abort and header when resuming.
- UX: skip animations on small terminals, update splash, and refine onboarding messaging.

### Install
```
npm install -g @just-every/code@latest
code
```

### Thanks
- Thanks to @ae and @jimmyfraiture2 for contributions!

Compare: https://github.com/just-every/code/compare/v0.2.148...v0.2.149
