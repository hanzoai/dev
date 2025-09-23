#!/bin/bash

# Test container operations with hanzod

echo "Testing Hanzod Container Operations"
echo "===================================="

# Check runtimes
echo -e "\n1. Checking available runtimes:"
curl -s http://localhost:3690/v1/runtimes | jq '.'

# Test health endpoint
echo -e "\n2. Health check:"
curl -s http://localhost:3690/health | jq '.'

# Test inference endpoint
echo -e "\n3. Testing inference endpoint:"
curl -X POST http://localhost:3690/v1/inference \
  -H "Content-Type: application/json" \
  -d '{
    "prompt": "What is the meaning of life?",
    "model": "llama3:8b",
    "max_tokens": 100
  }' | jq '.'

# Test embedding endpoint
echo -e "\n4. Testing embedding endpoint:"
curl -X POST http://localhost:3690/v1/embeddings \
  -H "Content-Type: application/json" \
  -d '{
    "text": "This is a test text for embedding generation",
    "model": "bge-large"
  }' | jq '.dimensions'

# Test vector search
echo -e "\n5. Testing vector search endpoint:"
curl -X POST http://localhost:3690/v1/vector_search \
  -H "Content-Type: application/json" \
  -d '{
    "embedding": [0.1, 0.2, 0.3],
    "limit": 5,
    "threshold": 0.8
  }' | jq '.'

# Check operation history
echo -e "\n6. Checking operation history:"
curl -s http://localhost:3690/v1/history | jq '.count'

echo -e "\nAll tests completed!"