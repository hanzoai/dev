# Qwen3 Integration with Hanzo Dev CLI

## Overview

This document describes the complete Qwen3 model integration with the Hanzo Dev CLI, including support for the Hanzo Node API, embedding models, and reranking capabilities.

## Features

### Qwen3 Model Support
- **Qwen3-8B**: 8 billion parameter model for general tasks
- **Qwen3-14B**: 14 billion parameter model for enhanced performance
- **Qwen3-30B-A3B**: 30B total with 3B active parameters (MoE architecture)
- **Qwen3-72B**: 72 billion parameter model for complex reasoning
- **Qwen3-235B**: 235 billion parameter model (future support)

### Specialized Models
- **Qwen3-Embedding-8B**: 4096-dimensional embeddings with 32K context window
- **Qwen3-Reranker-4B**: Specialized model for document reranking and retrieval improvement

### Multi-Provider Support
1. **Hanzo Engine** (Port 36900) - Local Rust-based inference
2. **LM Studio** - Local model management
3. **Ollama** - Local model runner
4. **OpenAI API** - Cloud fallback
5. **Anthropic Claude** - Cloud fallback
6. **Together AI** - Cloud fallback
7. **DashScope** - Alibaba Cloud integration

## Quick Start

### 1. Start the Hanzo Node

```bash
# Using the provided script
sh scripts/run_node_localhost.sh

# Or using Make
make hanzo-node-start
```

### 2. Check Service Status

```bash
make status
```

Expected output:
```
Service Status:
===============
✓ Hanzo Engine is running on port 36900
✓ Hanzo Node is running on port 3690
✓ Ollama is running on port 11434
```

### 3. Access the API

- **API Endpoint**: `http://localhost:3690`
- **Swagger UI**: `http://localhost:3690/v2/swagger-ui/`
- **P2P Network**: Port 3691

## Configuration

### Main Configuration (`hanzo_config.toml`)

```toml
[providers.hanzo_local]
enabled = true
type = "hanzo_engine"
url = "http://localhost:36900"
priority = 1

[models.qwen3]
variants = [
    "qwen3-8b",
    "qwen3-14b",
    "qwen3-30b-a3b",
    "qwen3-72b"
]
default = "qwen3-8b"
```

### Node API Configuration (`config/node_api_config.toml`)

```toml
[api]
base_url = "http://localhost:3690"
version = "v2"

[models.qwen3]
embedding_model = "qwen3-embedding-8b"
embedding_dimensions = 4096
reranker_model = "qwen3-reranker-4b"
```

## API Usage Examples

### Chat Completion

```bash
curl -X POST http://localhost:3690/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-8b",
    "messages": [
      {"role": "user", "content": "Explain quantum computing"}
    ],
    "temperature": 0.7,
    "thinking": true
  }'
```

### Embeddings

```bash
curl -X POST http://localhost:3690/v1/embeddings \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-embedding-8b",
    "input": ["Text to embed"],
    "dimensions": 4096
  }'
```

### Document Reranking

```bash
curl -X POST http://localhost:3690/v1/rerank \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-reranker-4b",
    "query": "quantum computing basics",
    "documents": [
      "Quantum computing uses qubits...",
      "Classical computing uses bits...",
      "Quantum mechanics principles..."
    ],
    "top_n": 3
  }'
```

## Rust Integration

### Using the Node Client

```rust
use codex_core::node_client::{NodeClient, create_qwen3_request};

#[tokio::main]
async fn main() -> Result<()> {
    // Create client
    let client = NodeClient::new(None, None)?;
    
    // Check health
    if client.health_check().await? {
        println!("Node is healthy!");
    }
    
    // Create chat request with thinking enabled
    let messages = vec![
        NodeClient::create_message("user", "Solve this problem step by step")
    ];
    let request = create_qwen3_request("qwen3-8b", messages, true);
    
    // Send request
    let response = client.chat_completion(request).await?;
    println!("Response: {:?}", response);
    
    Ok(())
}
```

### Creating Embeddings

```rust
use codex_core::node_client::{NodeClient, create_qwen3_embedding_request};

async fn create_embeddings(texts: Vec<String>) -> Result<Vec<Vec<f32>>> {
    let client = NodeClient::new(None, None)?;
    let request = create_qwen3_embedding_request(texts);
    let response = client.create_embeddings(request).await?;
    
    Ok(response.data.into_iter()
        .map(|d| d.embedding)
        .collect())
}
```

### Document Reranking

```rust
use codex_core::node_client::{NodeClient, create_qwen3_rerank_request};

async fn rerank_documents(query: String, docs: Vec<String>) -> Result<Vec<String>> {
    let client = NodeClient::new(None, None)?;
    let request = create_qwen3_rerank_request(query, docs, 5);
    let response = client.rerank_documents(request).await?;
    
    Ok(response.data.into_iter()
        .map(|r| r.document)
        .collect())
}
```

## Makefile Commands

### Service Management
- `make start` - Start all services (Engine, Node, Ollama)
- `make stop` - Stop all services
- `make restart` - Restart all services
- `make status` - Check service status
- `make logs` - View service logs

### Hanzo Engine
- `make engine-start` - Start Hanzo Engine
- `make engine-stop` - Stop Hanzo Engine
- `make engine-build` - Build Hanzo Engine
- `make engine-status` - Check Engine status

### Hanzo Node
- `make hanzo-node-start` - Start Node with Qwen3
- `make hanzo-node-stop` - Stop Node
- `make hanzo-node-build` - Build Node
- `make hanzo-node-status` - Check Node status

### Development
- `make dev` - Start development environment
- `make test` - Run all tests
- `make clean` - Clean build artifacts
- `make format` - Format all code
- `make lint` - Run linters

## Environment Variables

```bash
# Hanzo Configuration
export HANZO_ENGINE_URL="http://localhost:36900"
export HANZO_NODE_API_URL="http://localhost:3690"
export HANZO_ENABLE_QWEN3=true

# Model Selection
export QWEN3_DEFAULT_MODEL="qwen3-8b"
export QWEN3_EMBEDDING_MODEL="qwen3-embedding-8b"
export QWEN3_RERANKER_MODEL="qwen3-reranker-4b"

# API Keys (for fallback)
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        Hanzo Dev CLI                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐ │
│  │   Rust CLI   │───▶│  Node Client │───▶│   Hanzo API  │ │
│  │   (codex)    │    │  (port 3690) │    │   (Node.js)  │ │
│  └──────────────┘    └──────────────┘    └──────────────┘ │
│         │                    │                    │         │
│         ▼                    ▼                    ▼         │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              Provider Fallback Chain                 │  │
│  ├──────────────────────────────────────────────────────┤  │
│  │  1. Hanzo Engine (36900) - Qwen3 models              │  │
│  │  2. LM Studio (1234)     - Local models              │  │
│  │  3. Ollama (11434)       - Local models              │  │
│  │  4. OpenAI API           - Cloud fallback            │  │
│  │  5. Anthropic API        - Cloud fallback            │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │                  Qwen3 Capabilities                   │  │
│  ├──────────────────────────────────────────────────────┤  │
│  │  • Chat Completion (8B, 14B, 30B-A3B, 72B)           │  │
│  │  • Embeddings (4096 dims, 32K context)               │  │
│  │  • Document Reranking (4B specialized)               │  │
│  │  • Hybrid Reasoning (with thinking tokens)           │  │
│  │  • Multi-modal Support (future)                      │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Testing

Run the comprehensive test suite:

```bash
./test_qwen3_integration.sh
```

This will verify:
- All configuration files exist
- Services are properly configured
- Ports are available
- API keys are set
- Rust integration compiles
- Makefile targets work

## Troubleshooting

### Port Already in Use
```bash
# Find and kill process on port 3690
lsof -ti:3690 | xargs kill -9

# Or use Make
make stop
```

### Engine Not Starting
```bash
# Check engine logs
tail -f /tmp/hanzo-engine.log

# Rebuild engine
make engine-build
```

### Node Connection Failed
```bash
# Check node status
make hanzo-node-status

# Restart node
make hanzo-node-restart
```

### Model Not Found
```bash
# List available models
curl http://localhost:3690/v1/models | jq
```

## Performance Tips

1. **Use Local Models**: Prioritize Hanzo Engine for lowest latency
2. **Enable Caching**: Redis caching reduces redundant computations
3. **Batch Requests**: Use batch endpoints for multiple operations
4. **Optimize Context**: Keep context under 32K tokens for best performance
5. **Use Reranking**: Improve search quality with Qwen3-Reranker

## Security Considerations

1. **API Authentication**: Enable API keys in production
2. **Network Security**: Use firewall rules for ports 3690, 3691, 36900
3. **TLS/SSL**: Enable HTTPS for production deployments
4. **Rate Limiting**: Configure rate limits to prevent abuse
5. **Monitoring**: Enable metrics and logging for audit trails

## Future Enhancements

- [ ] Qwen3-235B model support
- [ ] Multi-modal capabilities (vision, audio)
- [ ] Distributed inference across multiple nodes
- [ ] Fine-tuning interface for custom models
- [ ] WebSocket support for real-time streaming
- [ ] GraphQL API endpoint
- [ ] Kubernetes deployment manifests
- [ ] Prometheus metrics integration

## Support

For issues or questions:
1. Check logs: `make logs`
2. Run tests: `./test_qwen3_integration.sh`
3. Review configuration: `cat hanzo_config.toml`
4. Check API docs: http://localhost:3690/v2/swagger-ui/

## License

This integration is part of the Hanzo Dev CLI project.
See LICENSE file for details.