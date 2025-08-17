#!/usr/bin/env bash
# Fast build script for local development - optimized for speed
set -euo pipefail

# Change to the Rust project root directory
cd codex-rs

# Use dev-fast profile by default for quick iteration
# Can override with: PROFILE=release ./build-fast.sh
PROFILE="${PROFILE:-dev-fast}"

# Determine the correct binary path based on profile
if [ "$PROFILE" = "dev-fast" ]; then
    BIN_PATH="./target/dev-fast/code"
elif [ "$PROFILE" = "dev" ]; then
    BIN_PATH="./target/debug/code"
else
    BIN_PATH="./target/${PROFILE}/code"
fi

echo "Building code binary (${PROFILE} mode)..."

# Build for native target (no --target flag) for maximum speed
# This reuses the host stdlib and normal cache

# In fast dev profile, suppress compiler warnings for a clean output while
# still surfacing errors. This does not affect other profiles.
if [ "$PROFILE" = "dev-fast" ]; then
    export RUSTFLAGS="${RUSTFLAGS:-} -Awarnings"
fi

cargo build --profile "${PROFILE}" --bin code --bin code-tui --bin code-exec

# Check if build succeeded
if [ $? -eq 0 ]; then
    echo "✅ Build successful!"
    echo "Binary location: ${BIN_PATH}"
    
    # Keep old symlink locations working for compatibility
    # Create symlink in target/release for npm wrapper expectations
    mkdir -p ./target/release
    if [ -e "./target/release/code" ]; then
        rm -f ./target/release/code
    fi
    ln -sf "../${PROFILE}/code" "./target/release/code"
    
    # Update the symlink in codex-cli/bin for npm wrapper
    CODEX_CLI_BIN_CODE="../codex-cli/bin/code-aarch64-apple-darwin"
    mkdir -p "$(dirname "$CODEX_CLI_BIN_CODE")"
    # Update symlinks for code (primary) and coder (fallback)
    for LINK in code-aarch64-apple-darwin coder-aarch64-apple-darwin; do
      DEST="../codex-cli/bin/$LINK"
      [ -e "$DEST" ] && rm -f "$DEST"
      ln -sf "../../codex-rs/target/${PROFILE}/code" "$DEST"
    done
    
    echo "✅ Symlinks updated"
    echo ""
    echo "You can now run: code"
    echo "Binary size: $(du -h ${BIN_PATH} | cut -f1)"
    echo ""
    echo "Build profile: ${PROFILE}"
    if [ "$PROFILE" = "dev-fast" ]; then
        echo "  → Optimized for fast iteration (incremental builds enabled)"
        echo "  → For production build, use: PROFILE=release-prod ./build-fast.sh"
    fi
else
    echo "❌ Build failed"
    exit 1
fi
