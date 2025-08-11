# Setting up "code" - A Fork of OpenAI Codex

## Overview
This document outlines the changes needed to rebrand and publish this fork of OpenAI Codex as "code" under the @just-every npm scope.

## Required Changes

### 1. Binary Renaming
- [ ] Rename binary from `codex` to `code` in:
  - `codex-rs/cli/Cargo.toml` - Change `[[bin]] name = "codex"` to `name = "code"`
  - `codex-rs/tui/Cargo.toml` - Change binary name to `code-tui`
  - `codex-rs/exec/Cargo.toml` - Change binary name to `code-exec`

### 2. Package Configuration
- [ ] Update `codex-cli/package.json`:
  - Change name from `@openai/codex` to `@just-every/code`
  - Update repository URL to `https://github.com/just-every/code.git`
  - Update bin field from `"codex": "bin/codex.js"` to `"code": "bin/code.js"`
  - Set initial version (e.g., "0.1.0")

- [ ] Rename `codex-cli/bin/codex.js` to `codex-cli/bin/code.js`
- [ ] Update the shebang and binary detection logic in `code.js`

### 3. GitHub Actions Updates
- [ ] Modify `.github/workflows/rust-release.yml`:
  - Update artifact names from `codex-*` to `code-*`
  - Change npm package staging to use new name
  - Add npm publish step after GitHub release

- [ ] Create `.github/workflows/npm-publish.yml` for automated npm publishing:
  ```yaml
  name: Publish to npm
  on:
    release:
      types: [published]
  
  jobs:
    publish:
      runs-on: ubuntu-latest
      steps:
        - uses: actions/checkout@v4
        - uses: actions/setup-node@v4
          with:
            node-version: '20'
            registry-url: 'https://registry.npmjs.org'
        - name: Download release assets
          # Download the npm package from release
        - name: Publish to npm
          run: npm publish
          env:
            NODE_AUTH_TOKEN: ${{ secrets.NPM_TOKEN }}
  ```

### 4. Rust Code Updates
- [ ] Update all references to "codex" in:
  - Error messages
  - Help text
  - Documentation strings
  - Environment variables (e.g., `CODEX_HOME` → `CODE_HOME`)

### 5. Configuration Updates
- [ ] Update config paths from `~/.codex/` to `~/.code/`
- [ ] Update environment variable names
- [ ] Update default config values

### 6. Build Scripts
- [ ] Update build scripts to produce `code` binaries
- [ ] Update install scripts to use new binary names
- [ ] Update the staging script at `codex-cli/scripts/stage_rust_release.py`

### 7. Documentation
- [ ] Update README.md with new project name and installation instructions
- [ ] Update all documentation to reference `code` instead of `codex`
- [ ] Add attribution to original OpenAI Codex project

## GitHub Secrets Required
Add these secrets to your GitHub repository:
- `NPM_TOKEN`: Your npm authentication token for publishing

## Release Process
1. Update version in `codex-rs/Cargo.toml`
2. Update version in `codex-cli/package.json`
3. Commit changes
4. Create and push a tag: `git tag -a v0.1.0 -m "Release 0.1.0"`
5. Push tag: `git push origin v0.1.0`
6. GitHub Actions will automatically:
   - Build binaries for all platforms
   - Create GitHub release
   - Publish to npm

## Testing Locally
```bash
# Build the Rust binaries
cd codex-rs
cargo build --release --bin code

# Test the CLI
./target/release/code --version

# Test npm package locally
cd ../codex-cli
npm link
code --version
```

## npm Publishing Setup
1. Create npm account if needed
2. Create organization @just-every on npm
3. Generate npm access token (automation type)
4. Add token as GitHub secret NPM_TOKEN

## Maintaining the Fork
- Regularly sync with upstream OpenAI Codex for bug fixes
- Keep a clear changelog of fork-specific features
- Maintain compatibility where possible