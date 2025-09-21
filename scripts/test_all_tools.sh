#!/bin/bash
# Test all 118 MCP tools

cd /Users/z/work/hanzo/dev/src/rs

echo "Building with complete tool registry..."
cargo build --package dev-mcp-server --release

echo ""
echo "Counting available tools..."
tool_count=$(./target/release/dev mcp list-tools | grep "^[a-z_]*:" | wc -l)
echo "Found $tool_count tools"

if [ "$tool_count" -eq 30 ]; then
    echo "✅ Currently using unified.rs with 30 tools"
    echo "❌ NOT using unified_complete.rs with 118 tools"
    echo ""
    echo "To activate all 118 tools, we need to:"
    echo "1. Update lib.rs to use CompleteToolRegistry"
    echo "2. Update message_processor.rs to use CompleteToolRegistry"
    echo "3. Wire up all category handlers"
else
    echo "✅ Using complete tool registry with $tool_count tools!"
fi

echo ""
echo "Testing core tools..."
echo "1. Bash tool:"
./target/release/dev mcp call bash --params '{"command": "echo Test successful"}'

echo ""
echo "2. Git status tool:"
./target/release/dev mcp call git_status --params '{}'

echo ""
echo "3. Read file tool:"
./target/release/dev mcp call read_file --params '{"path": "/Users/z/work/hanzo/dev/README.md"}' | head -20

echo ""
echo "Tool categories available:"
./target/release/dev mcp list-tools | grep "Category:" | sort -u