#!/bin/bash

# Test script to verify that CARGO_PKG_VERSION gets set correctly
# This simulates what the GitHub Actions workflow does

set -euo pipefail

echo "🧪 Testing version embedding process..."
echo ""

# Save current state
ORIGINAL_VERSION=$(grep "^version = " codex-rs/Cargo.toml | sed 's/version = "\(.*\)"/\1/')
echo "Current version in Cargo.toml: $ORIGINAL_VERSION"

# Test version
TEST_VERSION="9.9.9-test"
echo "Test version to embed: $TEST_VERSION"
echo ""

# Change to codex-rs directory
cd codex-rs

# Update version (like the workflow does)
echo "📝 Updating version in Cargo.toml..."
sed -i.bak "s/^version = \".*\"/version = \"$TEST_VERSION\"/" Cargo.toml

# Verify the update
echo "Updated Cargo.toml:"
grep "^version = " Cargo.toml
echo ""

# Update Cargo.lock
echo "🔄 Updating Cargo.lock..."
cargo update --workspace

# Build a small test binary to check version embedding
echo "🔨 Building test binary to verify version embedding..."
cat > test-version/src/main.rs << 'EOF'
fn main() {
    println!("CARGO_PKG_VERSION: {}", env!("CARGO_PKG_VERSION"));
}
EOF

# Create a minimal test crate if it doesn't exist
if [ ! -f test-version/Cargo.toml ]; then
    mkdir -p test-version/src
    cat > test-version/Cargo.toml << EOF
[package]
name = "test-version"
version = { workspace = true }
edition = "2024"
EOF
fi

# Build the test binary
cargo build --manifest-path test-version/Cargo.toml

# Run it to see the embedded version
echo ""
echo "✅ Testing embedded version:"
./target/debug/test-version

# Clean up - restore original version
echo ""
echo "🧹 Restoring original version..."
sed -i.bak "s/^version = \".*\"/version = \"$ORIGINAL_VERSION\"/" Cargo.toml
cargo update --workspace
rm -rf test-version

echo ""
echo "✅ Test complete!"
echo ""
echo "If the test showed 'CARGO_PKG_VERSION: $TEST_VERSION', then the version"
echo "embedding process is working correctly and will work in GitHub Actions."