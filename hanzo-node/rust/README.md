# hanzo-node (Rust Crate)

RPC-based compute node for the Hanzo Platform, written in Rust.

## Features

- **P2P Networking**: libp2p-based peer discovery and gossipsub messaging
- **gRPC Server**: Full NodeService API for compute pool operations
- **Storage**: Persistent sled database for deployment state
- **Container Orchestration**: Deployment lifecycle management

## Building

```bash
cd rust
cargo build --release
```

## Usage

```bash
# Start with defaults
./target/release/hanzo-node

# Custom configuration
./target/release/hanzo-node \
  --grpc-addr 0.0.0.0:50051 \
  --http-addr 0.0.0.0:8080 \
  --network-id 43114

# With P2P bootstrap peers
./target/release/hanzo-node \
  --p2p-addr /ip4/0.0.0.0/tcp/9000 \
  --bootstrap /ip4/1.2.3.4/tcp/9000/p2p/QmPeerId
```

## CLI Options

```
OPTIONS:
    --node-id <ID>          Node identifier (auto-generated if not provided)
    --grpc-addr <ADDR>      gRPC server address [default: 0.0.0.0:50051]
    --http-addr <ADDR>      HTTP health check address [default: 0.0.0.0:8080]
    --p2p-addr <ADDR>       P2P listen address [default: /ip4/0.0.0.0/tcp/9000]
    --bootstrap <ADDR>      Bootstrap peer address (can be specified multiple times)
    --data-dir <PATH>       Data directory for storage
    --network-id <ID>       Network ID (43114=mainnet, 43113=testnet, 1337=local)
    --operator <ADDR>       Operator wallet address
    --mlx                   Enable MLX acceleration (macOS only)
    --log-level <LEVEL>     Log level (trace, debug, info, warn, error)
```

## Environment Variables

- `HANZO_NODE_ID` - Node identifier
- `HANZO_GRPC_ADDR` - gRPC server address
- `HANZO_HTTP_ADDR` - HTTP health check address
- `HANZO_DATA_DIR` - Data directory
- `HANZO_NETWORK_ID` - Network ID
- `HANZO_OPERATOR` - Operator wallet address
- `RUST_LOG` - Log level filter

## gRPC API

The node implements the `NodeService` defined in `proto/hanzo.node.v1.proto`:

### Health & Status
- `GetHealth` - Node health check
- `GetStatus` - Full node status
- `GetMetrics` - Streaming metrics

### Node Lifecycle
- `RegisterNode` - Register with compute pool
- `Heartbeat` - Periodic heartbeat
- `UpdateCapabilities` - Update node capabilities

### Deployment Management
- `Deploy` - Deploy a compose spec
- `Stop` - Stop a deployment
- `Remove` - Remove a deployment
- `Scale` - Scale deployment replicas

### Monitoring
- `GetLogs` - Stream container logs
- `GetEvents` - Stream node events
- `ListContainers` - List all containers
- `InspectContainer` - Get container details
- `ExecInContainer` - Execute command in container

## Architecture

```
hanzo-node/
  src/
    lib.rs          # Core library and HanzoNode struct
    error.rs        # Error types
    p2p/            # P2P networking (libp2p)
    rpc/            # gRPC server (tonic)
    compute/        # Container orchestration
    storage/        # Persistent storage (sled)
    bin/
      hanzo-node.rs # CLI binary
```

## Dependencies

- **tokio** - Async runtime
- **tonic** - gRPC framework
- **libp2p** - P2P networking
- **sled** - Embedded database
- **tracing** - Structured logging

## Testing

```bash
cargo test
```

## License

Apache-2.0
