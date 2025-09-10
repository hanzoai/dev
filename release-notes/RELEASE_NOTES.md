## @just-every/code v0.2.110

This release refines automated issue comments and tidies build output.

### Changes
- Automation: update issue comments — remove direct download links, add LLM template and user mentions; keep commit summary
- Triage: defer user messaging to issue-comment workflow; remove queue acknowledgement
- TUI: remove unused imports to silence build warnings

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.109...v0.2.110
