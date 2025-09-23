#!/bin/bash

echo "=== Testing Hanzod Integration ==="
echo

echo "1. Health Check (HTTP Port 8080):"
curl -s http://localhost:8080/health | jq . || echo "Response: $(curl -s http://localhost:8080/health)"
echo

echo "2. Test Inference Endpoint:"
echo "   Sending: 'What is 2+2?'"
response=$(curl -s -X POST http://localhost:8080/v1/inference \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-4b-thinking-2507",
    "prompt": "What is 2+2?",
    "temperature": 0.7,
    "max_tokens": 50,
    "stream": false
  }')
echo "   Response: $response"
echo

echo "3. Test Embeddings Endpoint:"
response=$(curl -s -X POST http://localhost:8080/v1/embeddings \
  -H "Content-Type: application/json" \
  -d '{
    "text": "Hello world",
    "model": "nomic-embed-text"
  }')
echo "   Response length: ${#response} characters"
echo

echo "4. Check gRPC Port (50051):"
nc -zv localhost 50051 2>&1 | grep -E "(succeeded|open)" || echo "   gRPC port status: $(nc -zv localhost 50051 2>&1)"
echo

echo "5. Check Running Processes:"
ps aux | grep hanzod | grep -v grep | head -1
echo

echo "✅ Hanzod integration test complete!"