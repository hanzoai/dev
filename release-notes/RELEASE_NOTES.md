## @just-every/code v0.2.103

This release improves build ergonomics and stabilizes CI/triage behavior.

### Changes
- Build: add STRICT_CARGO_HOME to enforce CARGO_HOME; default stays repo-local when unset.
- Triage/Agent: standardize CARGO_HOME and share with rust-cache; prevent env overrides and unintended cargo updates.
- CI/Upstream-merge: fix YAML quoting and no-op outputs; split precheck and gate heavy work at job level for reliability.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.102...v0.2.103
