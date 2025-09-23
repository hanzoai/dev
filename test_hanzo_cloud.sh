#!/bin/bash

echo "🚀 Hanzo Cloud Integration Test"
echo "================================"

# Check if HANZO_API_KEY is set
if [ -z "$HANZO_API_KEY" ]; then
    echo "⚠️  HANZO_API_KEY not set. Please set it to test cloud auth."
    echo "   export HANZO_API_KEY=your-api-key"
else
    echo "✅ HANZO_API_KEY is configured"
fi

# Test local services
echo -e "\n📡 Testing Local Services:"
echo "  Checking Hanzo Engine (port 36900)..."
curl -s -o /dev/null -w "    Status: %{http_code}\n" http://localhost:36900/health 2>/dev/null || echo "    Status: Not running"

echo "  Checking Hanzo Node (port 3690)..."
curl -s -o /dev/null -w "    Status: %{http_code}\n" http://localhost:3690/health 2>/dev/null || echo "    Status: Not running"

# Test cloud connectivity
echo -e "\n☁️  Testing Hanzo Cloud:"
if [ ! -z "$HANZO_API_KEY" ]; then
    echo "  Checking API endpoint..."
    curl -s -o /dev/null -w "    Status: %{http_code}\n" \
        -H "Authorization: Bearer $HANZO_API_KEY" \
        https://api.hanzo.ai/v1/health 2>/dev/null || echo "    Status: Unable to connect"

    echo "  Checking Auth service..."
    curl -s -o /dev/null -w "    Status: %{http_code}\n" \
        https://auth.hanzo.ai/health 2>/dev/null || echo "    Status: Unable to connect"
else
    echo "  Skipping - API key not set"
fi

echo -e "\n📝 Configuration Summary:"
echo "  Config file: hanzo_config.toml"
echo "  Priority chain:"
echo "    1. Hanzo Local Engine (localhost:36900)"
echo "    2. Hanzo Node (localhost:3690)"
echo "    3. Hanzo Cloud (api.hanzo.ai)"
echo "    4. OpenAI (fallback)"
echo "    5. Anthropic Claude (fallback)"

echo -e "\n✨ To run hanzod with config:"
echo "  cargo run --bin hanzod --release -- --config hanzo_config.toml"
echo "  OR"
echo "  ./target/release/hanzod --config hanzo_config.toml"

echo -e "\n🎯 Features enabled:"
echo "  • Qwen3 models (8B, 14B, 30B-A3B, 72B)"
echo "  • Qwen3 Reranker"
echo "  • Qwen2-VL for vision"
echo "  • Cloud sync"
echo "  • Thinking/reasoning mode"