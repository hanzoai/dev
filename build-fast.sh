#!/usr/bin/env bash
# Fast build script for local development - optimized for speed
set -euo pipefail

# Change to the Rust project root directory
cd codex-rs

# Compute repository root (one level up from codex-rs)
REPO_ROOT="$(cd .. && pwd)"

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
    if ! rustup which rustc --toolchain "$TOOLCHAIN" >/dev/null 2>&1; then
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

# Optional trace mode for diagnosing environment differences
if [ "${TRACE_BUILD:-}" = "1" ]; then
  echo "--- TRACE_BUILD environment ---"
  echo "whoami: $(whoami)"
  echo "pwd: $(pwd)"
  echo "SHELL: ${SHELL:-}"
  bash --version | head -n1 || true
  if [ -n "${TOOLCHAIN:-}" ]; then
    echo "TOOLCHAIN: ${TOOLCHAIN}"
    rustup run "$TOOLCHAIN" rustc -vV || true
    rustup run "$TOOLCHAIN" cargo -vV || true
  fi
  echo "Filtered env (CARGO|RUST*|PROFILE|CODE_HOME|CODEX_HOME):"
  env | egrep '^(CARGO|RUST|RUSTUP|PROFILE|CODE_HOME|CODEX_HOME)=' | sort || true
  echo "--------------------------------"
fi

# Build for native target (no --target flag) for maximum speed
# This reuses the host stdlib and normal cache

# Build with or without --locked based on lockfile validity
# Merge cargo's stderr into stdout so CI/harnesses that only capture stdout
# still show all compilation lines and warnings just like an interactive shell.
${USE_CARGO} build ${USE_LOCKED} --profile "${PROFILE}" --bin code --bin code-tui --bin code-exec 2>&1

# Check if build succeeded
if [ $? -eq 0 ]; then
    echo "✅ Build successful!"
    echo "Binary location: ${BIN_PATH}"
    # Compute absolute path and SHA256 for clarity
    ABS_BIN_PATH="$(cd "$(dirname "${BIN_PATH}")" && pwd)/$(basename "${BIN_PATH}")"
    if command -v shasum >/dev/null 2>&1; then
      BIN_SHA="$(shasum -a 256 "${ABS_BIN_PATH}" | awk '{print $1}')"
    elif command -v sha256sum >/dev/null 2>&1; then
      BIN_SHA="$(sha256sum "${ABS_BIN_PATH}" | awk '{print $1}')"
    else
      BIN_SHA=""
    fi
    
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
    echo "You can run the CLI directly:"
    if [ -n "$BIN_SHA" ]; then
      echo "  ${ABS_BIN_PATH} (sha256: ${BIN_SHA})"
    else
      echo "  ${ABS_BIN_PATH}"
    fi
    echo "Binary size: $(du -h ${BIN_PATH} | cut -f1)"
    echo ""
    echo "Build profile: ${PROFILE}"
    if [ "$PROFILE" = "dev-fast" ]; then
        echo "  → Optimized for fast iteration (incremental builds enabled)"
        echo "  → For production build, use: PROFILE=release-prod ./build-fast.sh"
    fi
    
    # PATH guidance and collision warning
    REPO_BIN_DIR="${REPO_ROOT}/codex-cli/bin"
    CODE_PATH_ON_PATH="$(command -v code 2>/dev/null || true)"
    if [ -n "$CODE_PATH_ON_PATH" ] && [[ "$CODE_PATH_ON_PATH" != "${REPO_BIN_DIR}/code" && "$CODE_PATH_ON_PATH" != *"/codex-cli/bin/"* ]]; then
      echo ""
      echo "⚠️  PATH notice: 'code' currently resolves to: $CODE_PATH_ON_PATH"
      echo "    That is likely Visual Studio Code, not this CLI."
      echo "    To use this binary via 'code', prepend the repo bin directory to PATH:"
      echo "      export PATH=\"${REPO_BIN_DIR}:\$PATH\""
      echo "    Then verify with:"
      echo "      which code"
    fi

    if [ "${TRACE_BUILD:-}" = "1" ] && [ -n "$BIN_SHA" ]; then
      echo "--- TRACE_BUILD artifact ---"
      echo "ABS_BIN_PATH: ${ABS_BIN_PATH}"
      echo "SHA256: ${BIN_SHA}"
      echo "--------------------------------"
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
