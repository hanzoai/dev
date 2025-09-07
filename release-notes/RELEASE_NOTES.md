## @just-every/code v0.2.84

This release improves session accounting and streamlines our release tooling.

### Changes

- Core: move token usage/context accounting to session level for accurate per-session totals.
- Release: create_github_release accepts either --publish-alpha or --publish-release to avoid conflicting flags.
- Release: switch tooling to use gh, fresh temp clone, and Python rewrite for reliability.
- Repo: remove upstream-only workflows and TUI files to align with fork policy.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.83...v0.2.84

