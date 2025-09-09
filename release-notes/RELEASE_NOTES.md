## @just-every/code v0.2.106

This release improves CLI preview UX and stabilizes preview builds.

### Changes
- CLI/Preview: save downloads under ~/.code/bin by default; suffix binaries with PR id.
- CLI/Preview: run preview binary directly (no --help) for simpler testing.
- Preview build: use gh -R and upload only files; avoid .git dependency.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.105...v0.2.106
