## @just-every/code v0.2.78

Small maintenance release focused on CI reliability and repository hygiene.

### Changes

- CI: harden upstream-merge flow, fix PR step order, install jq; expand cleanup to purge nested Cargo caches for more reliable releases.
- Repo: broaden .gitignore to exclude Cargo caches and local worktrees, preventing accidental files in commits.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.77...v0.2.78
