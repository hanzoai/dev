#!/bin/bash

echo "=== Testing Containerized Inference Setup ==="
echo

# Check if Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Docker is not running. Please start Docker first."
    exit 1
fi

echo "1. Building inference container..."
docker compose build inference

echo
echo "2. Starting inference container..."
docker compose up -d inference

echo
echo "3. Waiting for container to be ready..."
for i in {1..30}; do
    if curl -s http://localhost:8080/health > /dev/null 2>&1; then
        echo "✅ Container is ready!"
        break
    fi
    echo -n "."
    sleep 2
done

echo
echo "4. Testing health endpoint..."
curl -s http://localhost:8080/health | jq . || echo "Response: $(curl -s http://localhost:8080/health)"

echo
echo "5. Testing inference..."
curl -s -X POST http://localhost:8080/v1/inference \
  -H "Content-Type: application/json" \
  -d '{
    "model": "Qwen/Qwen2.5-3B-Instruct",
    "prompt": "What is 2+2? Answer in one word.",
    "temperature": 0.1,
    "max_tokens": 10,
    "stream": false
  }' | jq .

echo
echo "6. Testing chat completions..."
curl -s -X POST http://localhost:8080/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "Qwen/Qwen2.5-3B-Instruct",
    "messages": [
      {"role": "user", "content": "What is the capital of France?"}
    ],
    "temperature": 0.1,
    "max_tokens": 20,
    "stream": false
  }' | jq .

echo
echo "7. Container logs (last 10 lines):"
docker compose logs --tail=10 inference

echo
echo "✅ Test complete! Container is running on port 8080"
echo
echo "To stop the container: docker compose down"
echo "To view logs: docker compose logs -f inference"