#!/usr/bin/env bash
# Production build script - optimized for size and performance
set -euo pipefail

cd codex-rs

echo "Building coder binary (production release mode)..."
echo "This will take longer but produce the most optimized binary."

# Build with the release-prod profile for maximum optimization
cargo build --profile release-prod --bin coder --target aarch64-apple-darwin

# Check if build succeeded
if [ $? -eq 0 ]; then
    echo "✅ Production build successful!"
    echo "Binary location: ./target/aarch64-apple-darwin/release-prod/coder"
    
    # Update symlinks for production binary
    mkdir -p ./target/release
    if [ -e "./target/release/coder" ]; then
        rm -f ./target/release/coder
    fi
    ln -sf ./aarch64-apple-darwin/release-prod/coder ./target/release/coder
    
    # Update the symlink in codex-cli/bin for npm wrapper
    CODEX_CLI_BIN="../codex-cli/bin/coder-aarch64-apple-darwin"
    if [ -e "$CODEX_CLI_BIN" ]; then
        rm -f "$CODEX_CLI_BIN"
    fi
    ln -sf ../../codex-rs/target/aarch64-apple-darwin/release-prod/coder "$CODEX_CLI_BIN"
    
    echo "✅ Symlinks updated"
    echo ""
    echo "Production binary ready: coder"
    echo "Binary size: $(du -h ./target/aarch64-apple-darwin/release-prod/coder | cut -f1)"
    echo ""
    echo "This build is optimized for distribution with:"
    echo "  → Maximum link-time optimization (LTO=fat)"
    echo "  → Single codegen unit for best performance"
    echo "  → Symbols stripped for smaller size"
else
    echo "❌ Build failed"
    exit 1
fi