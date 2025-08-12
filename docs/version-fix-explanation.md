# Fix for CARGO_PKG_VERSION Not Being Set During Releases

## Problem

The `CARGO_PKG_VERSION` environment variable was always showing "0.0.0" in release builds because:

1. The workspace `Cargo.toml` had `version = "0.0.0"` hardcoded
2. All packages inherited this version with `version = { workspace = true }`
3. The release process updated the version only in a tag/branch, not in the actual build environment

This caused issues with:
- Version checks in the application
- Update notifications showing incorrect versions
- MCP server version reporting

## Solution

The fix involves updating the GitHub Actions workflow to dynamically set the version from the release tag during the build process.

### Changes Made

#### 1. Updated GitHub Actions Workflow (`.github/workflows/rust-release.yml`)

The workflow now:
- Extracts the version from the tag name (e.g., `rust-v0.21.0` → `0.21.0`)
- Updates the `version` field in `codex-rs/Cargo.toml` before building
- Ensures all binaries are built with the correct version embedded

Key changes:
- Modified `tag-check` job to output the version
- Added "Update version in Cargo.toml" step in the `build` job
- Version is updated using `sed` before compilation

#### 2. Simplified Release Script (`codex-rs/scripts/create_github_release.sh`)

The script now:
- Only creates and pushes a tag (no branch or commit needed)
- Validates the version format
- Provides clear feedback about what will happen
- The actual version update happens in GitHub Actions

### How It Works

1. **Developer runs release script:**
   ```bash
   ./codex-rs/scripts/create_github_release.sh 0.21.0
   ```

2. **Script creates and pushes tag:**
   - Validates version format
   - Creates tag `rust-v0.21.0`
   - Pushes to GitHub

3. **GitHub Actions workflow triggers:**
   - Extracts version from tag (`0.21.0`)
   - Updates `Cargo.toml`: `version = "0.21.0"`
   - Builds all binaries with correct version
   - Creates GitHub release with artifacts

4. **Binary contains correct version:**
   - `env!("CARGO_PKG_VERSION")` returns `"0.21.0"`
   - Version checks work correctly
   - Update notifications show proper versions

### Testing

To test the version embedding locally:

```bash
# Run the test script
./test-version-embedding.sh
```

This script simulates what the GitHub Actions workflow does and verifies that `CARGO_PKG_VERSION` gets set correctly.

### Benefits

1. **No manual version updates needed** - Version is derived from the tag
2. **Single source of truth** - The tag name determines the version
3. **Consistent versioning** - All binaries in a release have the same version
4. **Simpler release process** - No need to commit version changes

### Important Notes

- The workspace `Cargo.toml` remains at `version = "0.0.0"` in the main branch
- The version is only updated during the release build process
- Local development builds will show "0.0.0" (this is expected)
- Release binaries will show the correct version from the tag

### Rollback

If you need to revert these changes:

1. Restore the original `create_github_release.sh` that updates and commits the version
2. Modify the workflow to expect the version to already be in `Cargo.toml`
3. Remove the "Update version in Cargo.toml" step from the workflow

### Future Improvements

Consider:
- Using a version management tool like `cargo-release`
- Implementing version bumping based on conventional commits
- Adding version validation to prevent accidental downgrades