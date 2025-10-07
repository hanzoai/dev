## @just-every/code v0.2.188

Small release to tighten MCP integration checks and make automation safer during publishing.

### Changes
- MCP: Validate stdio tool commands on PATH and surface clearer spawn errors during setup.
- Release: Guard release notes generation so headers always match the published version.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.187...v0.2.188
