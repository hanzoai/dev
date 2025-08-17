#!/usr/bin/env bash
# Production build script - optimized for size and performance
set -euo pipefail

cd codex-rs

echo "Building code binary (production release mode)..."
echo "This will take longer but produce the most optimized binary."

# Build with the release-prod profile for maximum optimization
cargo build --profile release-prod --bin code --target aarch64-apple-darwin

# Check if build succeeded
if [ $? -eq 0 ]; then
    echo "✅ Production build successful!"
    echo "Binary location: ./target/aarch64-apple-darwin/release-prod/code"
    
    # Update symlinks for production binary
    mkdir -p ./target/release
    if [ -e "./target/release/code" ]; then
        rm -f ./target/release/code
    fi
    ln -sf ./aarch64-apple-darwin/release-prod/code ./target/release/code
    
    # Update the symlink in codex-cli/bin for npm wrapper
    CODEX_CLI_BIN_CODE="../codex-cli/bin/code-aarch64-apple-darwin"
    if [ -e "$CODEX_CLI_BIN_CODE" ]; then
        rm -f "$CODEX_CLI_BIN_CODE"
    fi
    ln -sf ../../codex-rs/target/aarch64-apple-darwin/release-prod/code "$CODEX_CLI_BIN_CODE"
    
    echo "✅ Symlinks updated"
    echo ""
    echo "Production binary ready: code"
    echo "Binary size: $(du -h ./target/aarch64-apple-darwin/release-prod/code | cut -f1)"
    echo ""
    echo "This build is optimized for distribution with:"
    echo "  → Maximum link-time optimization (LTO=fat)"
    echo "  → Single codegen unit for best performance"
    echo "  → Symbols stripped for smaller size"
else
    echo "❌ Build failed"
    exit 1
fi
