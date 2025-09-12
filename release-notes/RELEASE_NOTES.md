## @just-every/code v0.2.140

This release contains CI improvements with no user-facing changes.

### Changes

- No user-facing changes; maintenance-only release with CI cache prewarming and policy hardening.
- CI: prewarm Rust build cache via ./build-fast.sh to speed upstream-merge and issue-code agents.
- CI: align cache home with enforced CARGO_HOME and enable cache-on-failure for more reliable runs.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.139...v0.2.140
