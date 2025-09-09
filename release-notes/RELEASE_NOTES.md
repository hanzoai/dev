## @just-every/code v0.2.100

This release improves build speed and release reliability, and includes a core date parsing fix.

### Changes
- Core: fix date parsing in rollout preflight to compile.
- Build: speed up build-fast via sccache; keep env passthrough for agents.
- Release: add preflight E2E tests and post-build smoke checks to improve publish reliability.
- Upstream-merge: refine branding guard to check only user-facing strings.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.99...v0.2.100
