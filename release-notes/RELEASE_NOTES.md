## @just-every/code v0.2.105

This release refines triage behavior and restores preview build reliability.

### Changes
- Triage: make agent failures non-fatal; capture exit code and disable git prompts.
- Triage: forbid agent git commits; treat agent-made commits as changes; allow branch/push even when clean.
- Preview: fix code-fence array string and YAML error to restore builds.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.104...v0.2.105
