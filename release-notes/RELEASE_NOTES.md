## @just-every/code v0.2.79

Small maintenance release improving upstream sync stability.

### Changes

- CI: harden upstream merge strategy to prefer local changes and reduce conflicts during sync for more stable releases.
- Build: smarter cleanup of reintroduced crates to avoid transient workspace breaks during upstream sync.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.78...v0.2.79
