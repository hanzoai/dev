#!/bin/bash

# Simple test for debug logging

echo "Testing debug logging..."

# Set the debug log directory
DEBUG_LOG_DIR="$HOME/.codex/debug_logs"

# Clean up any existing debug logs
if [ -d "$DEBUG_LOG_DIR" ]; then
    echo "Cleaning up old debug logs..."
    rm -rf "$DEBUG_LOG_DIR"
fi

# Run coder exec with debug flag
cd /Users/zemaj/www/just-every/coder
echo "Running: ./codex-rs/target/dev-fast/coder exec --debug 'What is 2+2?'"
OUTPUT=$(./codex-rs/target/dev-fast/coder exec --debug "What is 2+2?" 2>&1)

echo "Command output:"
echo "$OUTPUT" | head -20

# Check if debug logs were created
echo ""
if [ -d "$DEBUG_LOG_DIR" ]; then
    echo "✅ Debug logs directory created: $DEBUG_LOG_DIR"
    
    # Count log files
    LOG_COUNT=$(find "$DEBUG_LOG_DIR" -type f | wc -l)
    echo "Number of log files created: $LOG_COUNT"
    
    # List log files
    echo ""
    echo "Log files:"
    ls -la "$DEBUG_LOG_DIR" | tail -n +2 | head -10
    
    # Show a sample request log
    echo ""
    REQUEST_LOG=$(find "$DEBUG_LOG_DIR" -name "*request*.json" | head -1)
    if [ -n "$REQUEST_LOG" ]; then
        echo "Sample request log (first 30 lines of $(basename "$REQUEST_LOG")):"
        cat "$REQUEST_LOG" | head -30
    fi
else
    echo "❌ Debug logs directory NOT created - debug logging may not be working"
fi