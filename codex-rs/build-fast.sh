#!/usr/bin/env bash
# Fast build script for local development - optimized for speed
set -euo pipefail

# Use dev-fast profile by default for quick iteration
# Can override with: PROFILE=release ./build-fast.sh
PROFILE="${PROFILE:-dev-fast}"

# Determine the correct binary path based on profile
if [ "$PROFILE" = "dev-fast" ]; then
    BIN_PATH="./target/dev-fast/coder"
elif [ "$PROFILE" = "dev" ]; then
    BIN_PATH="./target/debug/coder"
else
    BIN_PATH="./target/${PROFILE}/coder"
fi

echo "Building coder binary (${PROFILE} mode)..."

# Build for native target (no --target flag) for maximum speed
# This reuses the host stdlib and normal cache
cargo build --profile "${PROFILE}" --bin coder

# Check if build succeeded
if [ $? -eq 0 ]; then
    echo "✅ Build successful!"
    echo "Binary location: ${BIN_PATH}"
    
    # Keep old symlink locations working for compatibility
    # Create symlink in target/release for npm wrapper expectations
    mkdir -p ./target/release
    if [ -e "./target/release/coder" ]; then
        rm -f ./target/release/coder
    fi
    ln -sf "../${PROFILE}/coder" "./target/release/coder"
    
    # Update the symlink in codex-cli/bin for npm wrapper
    CODEX_CLI_BIN="./codex-cli/bin/coder-aarch64-apple-darwin"
    mkdir -p "$(dirname "$CODEX_CLI_BIN")"
    if [ -e "$CODEX_CLI_BIN" ]; then
        rm -f "$CODEX_CLI_BIN"
    fi
    # Create relative symlink from codex-cli/bin to the actual binary
    ln -sf "../../target/${PROFILE}/coder" "$CODEX_CLI_BIN"
    
    echo "✅ Symlinks updated"
    echo ""
    echo "You can now run: coder"
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
