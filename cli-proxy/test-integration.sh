#!/bin/bash
# Test @hanzo/dev with Claude CLI proxy

set -e

echo "=========================================="
echo "  @hanzo/dev + Claude CLI Integration Test"
echo "=========================================="

# Check if proxy is running
if ! curl -s http://localhost:9999/health > /dev/null 2>&1; then
    echo ""
    echo "⚠️  Proxy not running. Starting it..."
    cd "$(dirname "$0")"
    node claude-proxy.js &
    PROXY_PID=$!
    sleep 3
    echo "✅ Proxy started (PID: $PROXY_PID)"
else
    echo "✅ Proxy already running"
fi

echo ""
echo "Test 1: Direct proxy test"
echo "-------------------------"
RESPONSE=$(curl -s http://localhost:9999/v1/chat/completions \
  -X POST \
  -H 'Content-Type: application/json' \
  -d '{"model":"claude-cli","messages":[{"role":"user","content":"Say hello in one word"}]}')

CONTENT=$(echo "$RESPONSE" | grep -o '"content":"[^"]*"' | cut -d'"' -f4 | head -1)
if [ -n "$CONTENT" ]; then
    echo "✅ Proxy response: $CONTENT"
else
    echo "❌ Proxy test failed"
    echo "$RESPONSE"
    exit 1
fi

echo ""
echo "Test 2: @hanzo/dev exec mode"
echo "----------------------------"
cd "$(dirname "$0")/.."
echo "What is 2+2?" | timeout 60 ./codex-cli/bin/code-aarch64-apple-darwin exec --full-auto 2>&1 | tail -10

echo ""
echo "=========================================="
echo "  Integration Test Complete!"
echo "=========================================="
echo ""
echo "To use @hanzo/dev with Claude CLI:"
echo "  1. Start proxy: cd ~/work/hanzo/dev/cli-proxy && node claude-proxy.js"
echo "  2. Run code: code 'your prompt'"
echo ""
