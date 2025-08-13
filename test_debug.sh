#!/bin/bash

# Test the debug flag

echo "Testing debug flag..."

# Check if debug logs directory gets created
DEBUG_LOG_DIR="$HOME/.codex/debug_logs"

# Remove old debug logs if they exist
if [ -d "$DEBUG_LOG_DIR" ]; then
    echo "Removing old debug logs..."
    rm -rf "$DEBUG_LOG_DIR"
fi

# Run coder with debug flag in exec mode (non-interactive)
echo "Running coder with --debug flag..."
./target/dev-fast/coder exec --debug "What is 2+2?" 2>&1 | head -20

# Check if debug logs were created
if [ -d "$DEBUG_LOG_DIR" ]; then
    echo ""
    echo "Debug logs directory created: $DEBUG_LOG_DIR"
    echo "Contents:"
    ls -la "$DEBUG_LOG_DIR" 2>/dev/null | head -10
    
    # Show a sample log file
    echo ""
    echo "Sample log file:"
    FIRST_LOG=$(ls "$DEBUG_LOG_DIR" 2>/dev/null | head -1)
    if [ -n "$FIRST_LOG" ]; then
        echo "Contents of $FIRST_LOG:"
        head -20 "$DEBUG_LOG_DIR/$FIRST_LOG"
    fi
else
    echo "Debug logs directory NOT created"
fi