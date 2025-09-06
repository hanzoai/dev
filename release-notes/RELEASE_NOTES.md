## @just-every/code v0.2.73

Maintenance release improving sandboxed builds and exec reliability.

### Changes

- CI/Build: default CARGO_HOME and CARGO_TARGET_DIR to workspace; use sparse registry; precreate dirs for sandboxed runs.
- CI/Exec: enable network for workspace-write exec runs; keep git writes opt-in.
- CLI/Fix: remove invalid '-a never' in 'code exec'; verified locally.
- CI: pass flags after subcommand so Exec receives them; fix heredoc quoting and cache mapping; minor formatting cleanups.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.72...v0.2.73
