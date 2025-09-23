# Hanzod Integration with Dev Project

## Overview
The Hanzo Dev project now uses **hanzod** as its unified compute layer, providing LLM inference, embeddings, vector search, GSPO training, and BitDelta personalization through a single daemon.

## Key Changes

### 1. Unified Compute Architecture
- Replaced separate services with single hanzod daemon
- All AI operations through unified endpoints (HTTP: 8080, gRPC: 50051)
- Post-quantum secure communication using HPKE

### 2. Configuration Updates
```rust
// New HanzoConfig structure
pub struct HanzoConfig {
    pub hanzod_path: Option<String>,
    pub http_port: u16,    // 8080
    pub grpc_port: u16,    // 50051
    pub inference_model: String,  // "qwen3:8b"
    pub enable_inference: bool,
    pub enable_embeddings: bool,
    pub enable_vector_search: bool,
    pub enable_gspo: bool,       // NEW: GSPO training
    pub enable_bitdelta: bool,   // NEW: BitDelta personalization
}
```

### 3. API Endpoints
- **HTTP (Port 8080)**:
  - `/health` - Service health check
  - `/v1/inference` - LLM inference (Qwen3 models)
  - `/v1/embeddings` - Text embedding generation
  - `/v1/vector_search` - Similarity search
  - `/v1/gspo/train` - GSPO preference optimization
  - `/v1/bitdelta` - BitDelta model personalization

- **gRPC (Port 50051)**:
  - High-performance versions of all above endpoints
  - Streaming inference support
  - Batch operations

### 4. Core Components

#### HanzoComputeEndpoint (`hanzo_inference.rs`)
- Manages hanzod daemon lifecycle
- Unified interface for all AI operations
- Health monitoring across services

#### Post-Quantum Security
- HPKE for secure channels
- ML-KEM-768 (NIST FIPS 203) for key encapsulation
- ChaCha20-Poly1305 for authenticated encryption
- PQSSHSession for container communication

## Usage

### Starting Hanzod
```bash
cd /Users/z/work/hanzo/hanzod
cargo build --release
./target/release/hanzod
```

### Using from Dev
```rust
use hanzo_inference::{HanzoManager, HanzoConfig};

// Initialize
let config = HanzoConfig::default();
let manager = HanzoManager::new(config);
manager.start_all().await?;

// Inference
let response = manager.compute.inference(
    "What is the meaning of life?".to_string(),
    Some(0.7),  // temperature
    Some(2048)  // max_tokens
).await?;

// Embeddings
let embedding = manager.embedding.embed("Hello world").await?;

// Vector Search
let results = manager.vector.search(
    query_vector,
    10,  // top_k
    Some(0.8)  // threshold
).await?;
```

### Health Check
```bash
curl http://localhost:8080/health
# {"status":"healthy","version":"0.1.0","mlx_enabled":true}
```

## Features

### MLX Acceleration (Apple Silicon)
- Automatic detection and enablement
- Optimized for M1/M2/M3 chips
- 2-3x faster inference

### GSPO (Group Sparse Preference Optimization)
- Train models on preference data
- Improve response quality
- Fine-tune for specific use cases

### BitDelta Personalization
- Efficient model personalization
- Minimal storage overhead
- Fast adaptation to user preferences

## Environment Variables
```bash
HANZOD_ENABLE_MLX=true       # Apple Silicon acceleration
HANZOD_MODEL=qwen3:8b        # Default model
HANZOD_ENABLE_GSPO=true      # Enable GSPO training
HANZOD_ENABLE_BITDELTA=true  # Enable BitDelta
```

## Migration Notes

### From Old Architecture
- Port 3690 → 8080 (HTTP)
- `HanzoInferenceEndpoint` → `HanzoComputeEndpoint`
- `manager.inference` → `manager.compute`
- Separate embedding/vector processes → Single hanzod daemon

### Benefits
1. **Simplified Architecture**: Single daemon instead of multiple services
2. **Better Performance**: Shared model loading, unified caching
3. **Enhanced Security**: Post-quantum cryptography throughout
4. **More Features**: GSPO and BitDelta capabilities
5. **Easier Deployment**: One process to manage

## Testing
```bash
# Build and run tests
cd /Users/z/work/hanzo/dev
cargo test --all

# Test inference
curl -X POST http://localhost:8080/v1/inference \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3:8b",
    "prompt": "Hello, world!",
    "temperature": 0.7,
    "max_tokens": 100
  }'
```

## Troubleshooting

### Port Conflicts
```bash
# Check if ports are in use
lsof -i :8080
lsof -i :50051

# Kill existing processes if needed
kill -9 <PID>
```

### Build Issues
```bash
# Clean build
cd /Users/z/work/hanzo/hanzod
cargo clean
cargo build --release
```

### Missing MLX Support
- Ensure you're on Apple Silicon (M1/M2/M3)
- MLX will automatically disable on Intel Macs

## Next Steps
1. Deploy to production environment
2. Set up monitoring and logging
3. Configure model caching
4. Implement rate limiting
5. Add authentication layer