## @just-every/code v0.2.187

This release locks in reliable history playback and keeps session resumes steady for enterprise teams.

### Changes

- TUI: Maintain strict streaming order and stable scrollback so history stays put while answers land.
- CLI: Prefer rollout `.jsonl` transcripts when resuming sessions so `code resume` stays reliable after snapshots.
- Core/Auth: Automatically use stored API keys for enterprise ChatGPT plans and honor retry hints from rate-limit errors.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.186...v0.2.187
