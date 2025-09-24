# Testing Hanzo AI Blockchain with Lux Consensus

## Quick Start

### 1. Install Dependencies
```bash
# Ensure you have Rust installed
rustup update
cargo --version

# Install test dependencies
make deps
```

### 2. Run All Tests
```bash
# Run comprehensive test suite
make test

# Or run specific test categories:
make test-quick      # Unit tests only
make test-integration # Integration tests only
```

### 3. Start Local Node
```bash
# Start local test node
make run

# Or use the test script
./run_local_test.sh
```

### 4. Test Client
```bash
# In another terminal, run test client
make run-client

# Or test endpoints directly
make test-client
```

## Test Categories

### Unit Tests

Located in module files and `src/lib.rs`:

```rust
cargo test --lib
```

Tests include:
- ✅ Module exports
- ✅ Configuration defaults
- ✅ Basic functionality

### Consensus Tests

Located in `src/lux_consensus.rs`:

```rust
cargo test lux_consensus
```

Tests include:
- ✅ Consensus initialization
- ✅ Validator registration (2M LUX minimum)
- ✅ Snow consensus parameters (k=20, α=15, β=15/20)
- ✅ Signature operations
- ✅ Uptime tracking
- ✅ Consensus rounds

### Warp FFI Tests

Located in `src/warp_ffi.rs`:

```rust
cargo test warp_ffi
```

Tests include:
- ✅ Message creation
- ✅ Message ID generation
- ✅ Chain ID validation (32 bytes)
- ✅ Size limits (256 KiB max)
- ✅ Ed25519 signing/verification
- ✅ Rust protocol implementation

### Integration Tests

Located in `tests/comprehensive_tests.rs`:

```rust
cargo test --test comprehensive_tests
```

Tests include:
- ✅ End-to-end workflow
- ✅ Multiple validators
- ✅ Interchain messaging (Warp ICM)
- ✅ Asset transfers (Teleport zkBridge)
- ✅ RPC configuration
- ✅ Chain support (C-Chain, X-Chain, P-Chain)

## Manual Testing

### 1. Start Local Node

```bash
cargo run --example run_local
```

This starts:
- Chain ID: `hanzo-local-test`
- Network ID: `1337` (local)
- gRPC: `localhost:50051`
- HTTP: `localhost:8545`
- WebSocket: `localhost:8546`

### 2. Test HTTP Endpoints

```bash
# Health check
curl http://localhost:8545/health

# Status
curl http://localhost:8545/status

# Validators
curl http://localhost:8545/v1/validators

# Inference (POST)
curl -X POST http://localhost:8545/v1/inference \
  -H "Content-Type: application/json" \
  -d '{
    "model": "qwen3-next:8b",
    "prompt": "Explain blockchain consensus",
    "max_tokens": 100
  }'
```

### 3. Test gRPC (when proto is compiled)

```bash
# Use grpcurl or similar tool
grpcurl -plaintext localhost:50051 list

# Or use the generated client
cargo run --example grpc_client
```

## Performance Testing

```bash
# Run benchmarks
make bench

# Performance tests
make perf
```

## Test Coverage

```bash
# Generate coverage report
make coverage

# Opens HTML report in browser
```

## Expected Test Results

### Consensus Tests
- **Validator Registration**: Should require 2M LUX minimum stake
- **Snow Parameters**: k=20, α=15, β_virtuous=15, β_rogue=20
- **Node ID Format**: `NodeID-xxxxxxxxxxxx`
- **Staking Address**: `P-lux1xxxxxxxxxxxx`

### Warp ICM Tests
- **Message Size**: Max 256 KiB
- **Chain ID**: Exactly 32 bytes
- **Signature**: Ed25519 (64 bytes)
- **Network ID**: 43114 (mainnet), 1337 (local)

### Interchain Support
- **Protocols**: Warp (messaging), Teleport (zkBridge assets)
- **Chains**: C-Chain (EVM), X-Chain (DAG), P-Chain (Platform)

## Troubleshooting

### Build Issues
```bash
# Clean and rebuild
make clean
make build

# Check for missing dependencies
cargo check --all-features
```

### Port Conflicts
```bash
# Check if ports are in use
lsof -i :50051  # gRPC
lsof -i :8545   # HTTP
lsof -i :8546   # WebSocket

# Kill processes if needed
kill -9 <PID>
```

### Test Failures
```bash
# Run with verbose output
cargo test -- --nocapture --test-threads=1

# Run single test
cargo test test_name -- --exact --nocapture
```

## CI/CD Integration

### GitHub Actions

```yaml
name: Test Hanzo AI Blockchain

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - uses: actions-rs/toolchain@v1
      with:
        toolchain: stable
    - name: Build
      run: cargo build --verbose
    - name: Run tests
      run: cargo test --verbose
    - name: Check consensus
      run: cargo test lux_consensus -- --nocapture
    - name: Check Warp FFI
      run: cargo test warp_ffi -- --nocapture
```

## Summary

The Hanzo AI Blockchain test suite ensures:

1. **Lux Consensus**: Full compatibility with Snow protocol
2. **Node Operators**: Proper staking and key management
3. **Warp ICM**: Interchain messaging protocol
4. **Teleport**: zkBridge for asset transfers
5. **Qwen3 Support**: AI model integration
6. **KuzuDB**: Native ledger functionality

All tests should pass before deployment!