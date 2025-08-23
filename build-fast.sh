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

# Check Cargo.lock validity (fast, non-blocking check)
if ! cargo metadata --locked --format-version 1 >/dev/null 2>&1; then
    echo "⚠️  Warning: Cargo.lock appears out of date or inconsistent"
    echo "  This might mean:"
    echo "  • You've modified Cargo.toml dependencies"
    echo "  • You've changed workspace crate versions"
    echo "  • The lockfile is missing entries"
    echo ""
    echo "  Run 'cargo update' to update all dependencies, or"
    echo "  Run 'cargo update -p <crate-name>' to update specific crates"
    echo ""
    echo "  Continuing with unlocked build for development..."
    echo ""
    USE_LOCKED=""
else
    # Lockfile is valid, use it for consistent builds
    USE_LOCKED="--locked"
fi

# Select the cargo/rustc toolchain to match deploy
# Prefer rustup with the toolchain pinned in rust-toolchain.toml or $RUSTUP_TOOLCHAIN
USE_CARGO="cargo"
if command -v rustup >/dev/null 2>&1; then
  # Determine desired toolchain
  if [ -n "${RUSTUP_TOOLCHAIN:-}" ]; then
    TOOLCHAIN="$RUSTUP_TOOLCHAIN"
  else
    # Try parse channel from rust-toolchain.toml in repo root
    if [ -f "rust-toolchain.toml" ]; then
      TOOLCHAIN=$(sed -n 's/^channel\s*=\s*"\(.*\)"/\1/p' rust-toolchain.toml | head -n1)
    fi
    # Fallback to active default if none found
    TOOLCHAIN="${TOOLCHAIN:-$(rustup show active-toolchain 2>/dev/null | awk '{print $1}')}"
  fi

  if [ -n "$TOOLCHAIN" ]; then
    # Ensure toolchain is installed; if not, attempt install quietly
    if ! rustup toolchain list | awk '{print $1}' | grep -qx "$TOOLCHAIN"; then
      echo "rustup: installing toolchain $TOOLCHAIN ..."
      rustup toolchain install "$TOOLCHAIN" >/dev/null 2>&1 || true
    fi
    USE_CARGO="rustup run $TOOLCHAIN cargo"
    echo "Using rustup toolchain: $TOOLCHAIN"
    rustup run "$TOOLCHAIN" rustc --version || true
  else
    echo "rustup found but no toolchain detected; using system cargo"
  fi
else
  echo "Error: rustup is required for consistent builds."
  echo "Please install rustup: https://rustup.rs/"
  exit 1
fi

# Build for native target (no --target flag) for maximum speed
# This reuses the host stdlib and normal cache

# Build with or without --locked based on lockfile validity
${USE_CARGO} build ${USE_LOCKED} --profile "${PROFILE}" --bin code --bin code-tui --bin code-exec

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
    
    # If lockfile was out of date, remind user
    if [ -z "$USE_LOCKED" ]; then
        echo ""
        echo "⚠️  Remember: Built without --locked due to Cargo.lock issues"
        echo "  Consider running 'cargo update' and committing the changes"
    fi
else
    echo "❌ Build failed"
    exit 1
fi