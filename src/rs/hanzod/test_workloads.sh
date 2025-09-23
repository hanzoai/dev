#!/bin/bash

echo "Testing Hanzod Workload Management"
echo "==================================="

# Check health
echo -e "\n1. Health check:"
curl -s http://localhost:3690/health | jq '.'

# Check runtimes
echo -e "\n2. Available runtimes:"
curl -s http://localhost:3690/v1/runtimes | jq '.'

# List workloads (should be empty)
echo -e "\n3. Current workloads (empty):"
curl -s http://localhost:3690/v1/workloads | jq '.'

# Schedule a compute workload
echo -e "\n4. Scheduling a simple compute workload:"
curl -X POST http://localhost:3690/v1/workloads \
  -H "Content-Type: application/json" \
  -d '{
    "id": "test-workload-001",
    "workload_type": {
      "Compute": {
        "image": "alpine:latest",
        "command": ["echo", "Hello from Hanzod workload!"]
      }
    },
    "resources": {
      "memory_mb": 256,
      "cpu_cores": 0.5,
      "gpu": false
    }
  }' | jq '.'

# Schedule an inference workload
echo -e "\n5. Scheduling an inference workload:"
curl -X POST http://localhost:3690/v1/workloads \
  -H "Content-Type: application/json" \
  -d '{
    "id": "inference-workload-001",
    "workload_type": {
      "Inference": {
        "model": "llama3:8b",
        "engine": "ollama"
      }
    },
    "resources": {
      "memory_mb": 8192,
      "cpu_cores": 4.0,
      "gpu": false
    }
  }' | jq '.'

# Schedule a vector DB workload
echo -e "\n6. Scheduling a vector database workload:"
curl -X POST http://localhost:3690/v1/workloads \
  -H "Content-Type: application/json" \
  -d '{
    "id": "vectordb-workload-001",
    "workload_type": {
      "VectorDB": {
        "engine": "qdrant"
      }
    },
    "resources": {
      "memory_mb": 2048,
      "cpu_cores": 2.0,
      "storage_gb": 10
    }
  }' | jq '.'

# List all workloads
echo -e "\n7. List all workloads:"
curl -s http://localhost:3690/v1/workloads | jq '.'

# Check specific workload status
echo -e "\n8. Check specific workload status:"
curl -s http://localhost:3690/v1/workloads/test-workload-001 | jq '.'

echo -e "\nWorkload testing complete!"