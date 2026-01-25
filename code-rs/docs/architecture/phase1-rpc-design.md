# Platform Phase 1 - Replace Docker/SSH with Hanzo-Node RPC

**Status**: Design
**Author**: Architecture Review
**Date**: January 2026

## Executive Summary

This document proposes replacing the current Docker-over-SSH deployment model in Hanzo Platform with a native RPC interface to hanzo-node. This change eliminates SSH key management complexity, improves security posture, and provides a foundation for the unified compute layer vision described in LLM.md.

## Current State Analysis

### Existing Deployment Patterns

The Hanzo Platform currently uses two deployment mechanisms:

1. **Docker over SSH** (`/platform/pkg/platform/src/utils/servers/remote-docker.ts`)
   ```typescript
   const dockerode = new Dockerode({
     host: server.ipAddress,
     port: server.port,
     username: server.username,
     protocol: "ssh",
     sshOptions: {
       privateKey: server.sshKey?.privateKey,
     },
   });
   ```

2. **HTTP REST API to Blockchain Nodes** (`/platform/app/platform/lib/hanzo-blockchain-infra.ts`)
   ```typescript
   const response = await fetch(`${node.endpoint}/deploy`, {
     method: "POST",
     headers: {
       "Content-Type": "application/json",
       "Authorization": `Bearer ${process.env.HANZO_NODE_API_KEY}`,
     },
     body: JSON.stringify({ spec, options }),
   });
   ```

### Problems with Current Approach

| Issue | Impact | Severity |
|-------|--------|----------|
| SSH key management | Operational complexity, security risk | High |
| Dockerode limitations | No streaming, poor error handling | Medium |
| Inconsistent APIs | Two different deployment paths | Medium |
| No mTLS | Bearer tokens only, weak auth | High |
| No service mesh | No observability, no traffic control | Medium |

## Proposed Architecture

### High-Level Design

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Hanzo Platform Control Plane                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌──────────────┐    ┌────────────────────────────────────────┐    │
│  │ Platform API │────│        RPC Client Layer                 │    │
│  └──────────────┘    │  ┌─────────┐ ┌─────────┐ ┌─────────┐  │    │
│                      │  │ gRPC    │ │ HTTP/2  │ │ WebSocket│  │    │
│                      │  │ Client  │ │ Fallback│ │ Streams  │  │    │
│                      │  └─────────┘ └─────────┘ └─────────┘  │    │
│                      └────────────────────────────────────────┘    │
│                                      │                              │
└──────────────────────────────────────┼──────────────────────────────┘
                                       │ mTLS
                    ┌──────────────────▼──────────────────┐
                    │          Hanzo Node Cluster          │
                    ├──────────────────────────────────────┤
                    │  ┌────────┐  ┌────────┐  ┌────────┐ │
                    │  │ Node 1 │  │ Node 2 │  │ Node 3 │ │
                    │  │ :50051 │  │ :50051 │  │ :50051 │ │
                    │  │ (gRPC) │  │ (gRPC) │  │ (gRPC) │ │
                    │  └────────┘  └────────┘  └────────┘ │
                    │                                      │
                    │  Lux Consensus (k=20, α=15, β=15/20) │
                    └──────────────────────────────────────┘
```

### RPC Interface Definition

The hanzo-node RPC interface follows the Model Context Protocol (MCP) patterns established in code-rs, with extensions for node management.

#### Core Service Definition (Protocol Buffers)

```protobuf
syntax = "proto3";
package hanzo.node.v1;

// Node management service
service NodeService {
  // Health and status
  rpc GetHealth(GetHealthRequest) returns (GetHealthResponse);
  rpc GetStatus(GetStatusRequest) returns (GetStatusResponse);
  rpc GetMetrics(GetMetricsRequest) returns (stream MetricsResponse);

  // Deployment lifecycle
  rpc Deploy(DeployRequest) returns (DeployResponse);
  rpc Stop(StopRequest) returns (StopResponse);
  rpc Remove(RemoveRequest) returns (RemoveResponse);
  rpc Scale(ScaleRequest) returns (ScaleResponse);

  // Logs and monitoring
  rpc GetLogs(GetLogsRequest) returns (stream LogEntry);
  rpc GetEvents(GetEventsRequest) returns (stream NodeEvent);

  // Container management (replaces Docker API)
  rpc ListContainers(ListContainersRequest) returns (ListContainersResponse);
  rpc InspectContainer(InspectContainerRequest) returns (ContainerInfo);
  rpc ExecInContainer(stream ExecInput) returns (stream ExecOutput);

  // Network management
  rpc ListNetworks(ListNetworksRequest) returns (ListNetworksResponse);
  rpc CreateNetwork(CreateNetworkRequest) returns (CreateNetworkResponse);

  // Volume management
  rpc ListVolumes(ListVolumesRequest) returns (ListVolumesResponse);
  rpc CreateVolume(CreateVolumeRequest) returns (CreateVolumeResponse);
}

// Inference service (from hanzod integration)
service InferenceService {
  rpc Inference(InferenceRequest) returns (stream InferenceResponse);
  rpc Embed(EmbedRequest) returns (EmbedResponse);
  rpc Rerank(RerankRequest) returns (RerankResponse);
}

// Operator registration (Lux consensus)
service OperatorService {
  rpc RegisterOperator(RegisterOperatorRequest) returns (RegisterOperatorResponse);
  rpc GetOperatorStatus(GetOperatorStatusRequest) returns (OperatorStatus);
  rpc UpdateStake(UpdateStakeRequest) returns (UpdateStakeResponse);
}
```

#### Message Types

```protobuf
message DeployRequest {
  string deployment_id = 1;
  string name = 2;
  ComposeSpec spec = 3;
  map<string, string> environment = 4;
  DeploymentOptions options = 5;
}

message ComposeSpec {
  string version = 1;  // "3.0"
  string name = 2;
  map<string, ServiceSpec> services = 3;
  map<string, NetworkSpec> networks = 4;
  map<string, VolumeSpec> volumes = 5;
}

message ServiceSpec {
  string image = 1;
  repeated string command = 2;
  repeated string entrypoint = 3;
  map<string, string> environment = 4;
  repeated string ports = 5;
  repeated string volumes = 6;
  repeated string networks = 7;
  DeploySpec deploy = 8;
  HealthcheckSpec healthcheck = 9;
  string restart = 10;
}

message DeploymentOptions {
  string namespace = 1;
  string environment = 2;  // production, staging, development
  map<string, string> labels = 3;
  ResourceLimits resource_limits = 4;
}

message GetHealthResponse {
  HealthStatus status = 1;
  string version = 2;
  bool mlx_enabled = 3;
  repeated string models = 4;
  ConsensusStatus consensus = 5;
}

enum HealthStatus {
  HEALTHY = 0;
  DEGRADED = 1;
  UNHEALTHY = 2;
}

message ConsensusStatus {
  bool synced = 1;
  uint64 block_height = 2;
  string network_id = 3;  // 43114 mainnet, 43113 testnet, 1337 local
}
```

### Rust Implementation (code-rs)

New crate: `code-rs/hanzo-node-client`

```rust
//! Hanzo Node RPC Client
//!
//! Provides a type-safe client for communicating with hanzo-node instances.

use tonic::transport::{Channel, ClientTlsConfig, Certificate, Identity};
use tonic::codegen::InterceptedService;
use std::time::Duration;

pub mod proto {
    tonic::include_proto!("hanzo.node.v1");
}

use proto::{
    node_service_client::NodeServiceClient,
    inference_service_client::InferenceServiceClient,
    operator_service_client::OperatorServiceClient,
};

/// Configuration for connecting to a hanzo-node.
#[derive(Debug, Clone)]
pub struct NodeConfig {
    /// Node endpoint (e.g., "https://node1.hanzo.ai:50051")
    pub endpoint: String,
    /// mTLS client certificate
    pub client_cert: Option<Vec<u8>>,
    /// mTLS client key
    pub client_key: Option<Vec<u8>>,
    /// CA certificate for server verification
    pub ca_cert: Option<Vec<u8>>,
    /// Connection timeout
    pub connect_timeout: Duration,
    /// Request timeout
    pub request_timeout: Duration,
}

impl Default for NodeConfig {
    fn default() -> Self {
        Self {
            endpoint: "http://localhost:50051".to_string(),
            client_cert: None,
            client_key: None,
            ca_cert: None,
            connect_timeout: Duration::from_secs(10),
            request_timeout: Duration::from_secs(60),
        }
    }
}

/// Unified client for all hanzo-node services.
pub struct HanzoNodeClient {
    node: NodeServiceClient<InterceptedService<Channel, AuthInterceptor>>,
    inference: InferenceServiceClient<InterceptedService<Channel, AuthInterceptor>>,
    operator: OperatorServiceClient<InterceptedService<Channel, AuthInterceptor>>,
}

impl HanzoNodeClient {
    /// Create a new client with the given configuration.
    pub async fn connect(config: NodeConfig) -> Result<Self, NodeClientError> {
        let mut tls_config = ClientTlsConfig::new();

        if let Some(ca_cert) = &config.ca_cert {
            tls_config = tls_config.ca_certificate(Certificate::from_pem(ca_cert));
        }

        if let (Some(cert), Some(key)) = (&config.client_cert, &config.client_key) {
            tls_config = tls_config.identity(Identity::from_pem(cert, key));
        }

        let channel = Channel::from_shared(config.endpoint)?
            .tls_config(tls_config)?
            .connect_timeout(config.connect_timeout)
            .timeout(config.request_timeout)
            .connect()
            .await?;

        let interceptor = AuthInterceptor::new();

        Ok(Self {
            node: NodeServiceClient::with_interceptor(channel.clone(), interceptor.clone()),
            inference: InferenceServiceClient::with_interceptor(channel.clone(), interceptor.clone()),
            operator: OperatorServiceClient::with_interceptor(channel, interceptor),
        })
    }

    /// Deploy a compose spec to the node.
    pub async fn deploy(&mut self, request: DeployRequest) -> Result<DeployResponse, NodeClientError> {
        let response = self.node.deploy(request).await?;
        Ok(response.into_inner())
    }

    /// Stream logs from a deployment.
    pub async fn stream_logs(
        &mut self,
        deployment_id: &str,
        tail: u32,
    ) -> Result<impl Stream<Item = Result<LogEntry, Status>>, NodeClientError> {
        let request = GetLogsRequest {
            deployment_id: deployment_id.to_string(),
            tail,
            follow: true,
        };
        let response = self.node.get_logs(request).await?;
        Ok(response.into_inner())
    }

    /// Execute a command in a container (replaces SSH).
    pub async fn exec(
        &mut self,
        container_id: &str,
        command: &[String],
    ) -> Result<ExecSession, NodeClientError> {
        ExecSession::new(self.node.clone(), container_id, command).await
    }

    /// Run inference on the node's LLM.
    pub async fn inference(
        &mut self,
        prompt: &str,
        model: Option<&str>,
    ) -> Result<impl Stream<Item = Result<InferenceResponse, Status>>, NodeClientError> {
        let request = InferenceRequest {
            prompt: prompt.to_string(),
            model: model.map(String::from),
            ..Default::default()
        };
        let response = self.inference.inference(request).await?;
        Ok(response.into_inner())
    }
}

/// Interactive exec session (replaces SSH session).
pub struct ExecSession {
    tx: mpsc::Sender<ExecInput>,
    rx: Streaming<ExecOutput>,
}

impl ExecSession {
    /// Write input to the command.
    pub async fn write(&mut self, data: &[u8]) -> Result<(), NodeClientError> {
        self.tx.send(ExecInput { data: data.to_vec() }).await?;
        Ok(())
    }

    /// Read output from the command.
    pub async fn read(&mut self) -> Option<Result<ExecOutput, Status>> {
        self.rx.next().await
    }
}

#[derive(Debug, thiserror::Error)]
pub enum NodeClientError {
    #[error("Transport error: {0}")]
    Transport(#[from] tonic::transport::Error),
    #[error("RPC error: {0}")]
    Rpc(#[from] tonic::Status),
    #[error("Channel closed")]
    ChannelClosed,
}
```

### TypeScript Client (for Platform)

```typescript
// /platform/pkg/platform/src/utils/hanzo-node/client.ts

import { ChannelCredentials, credentials } from "@grpc/grpc-js";
import { NodeServiceClient } from "./proto/hanzo/node/v1/node_grpc_pb";
import type { DeployRequest, DeployResponse, GetHealthResponse } from "./proto/hanzo/node/v1/node_pb";

export interface NodeClientConfig {
  endpoint: string;
  clientCert?: Buffer;
  clientKey?: Buffer;
  caCert?: Buffer;
  connectTimeout?: number;
  requestTimeout?: number;
}

export class HanzoNodeClient {
  private client: NodeServiceClient;

  constructor(config: NodeClientConfig) {
    let creds: ChannelCredentials;

    if (config.clientCert && config.clientKey && config.caCert) {
      creds = credentials.createSsl(
        config.caCert,
        config.clientKey,
        config.clientCert
      );
    } else if (config.caCert) {
      creds = credentials.createSsl(config.caCert);
    } else {
      creds = credentials.createInsecure();
    }

    this.client = new NodeServiceClient(config.endpoint, creds);
  }

  async getHealth(): Promise<GetHealthResponse.AsObject> {
    return new Promise((resolve, reject) => {
      this.client.getHealth({}, (err, response) => {
        if (err) reject(err);
        else resolve(response.toObject());
      });
    });
  }

  async deploy(request: DeployRequest.AsObject): Promise<DeployResponse.AsObject> {
    return new Promise((resolve, reject) => {
      const req = new DeployRequest();
      // ... populate request ...
      this.client.deploy(req, (err, response) => {
        if (err) reject(err);
        else resolve(response.toObject());
      });
    });
  }

  streamLogs(deploymentId: string, tail: number = 100): AsyncIterable<LogEntry.AsObject> {
    const stream = this.client.getLogs({ deploymentId, tail, follow: true });
    return {
      async *[Symbol.asyncIterator]() {
        for await (const entry of stream) {
          yield entry.toObject();
        }
      }
    };
  }
}
```

## Migration Path

### Phase 1.1: Protocol Definition (Week 1)

1. Create proto definitions in `code-rs/hanzo-node-client/proto/`
2. Generate Rust and TypeScript bindings
3. Write unit tests for message serialization

### Phase 1.2: Rust Client Implementation (Week 2)

1. Implement `HanzoNodeClient` in `code-rs/hanzo-node-client/`
2. Add connection pooling and retry logic
3. Integrate with existing code-rs infrastructure
4. Write integration tests

### Phase 1.3: Node Server Implementation (Week 3)

1. Create hanzo-node RPC server skeleton
2. Implement NodeService handlers
3. Wire up to existing container runtime
4. Add mTLS support

### Phase 1.4: Platform Migration (Week 4)

1. Replace `getRemoteDocker()` with `HanzoNodeClient`
2. Update `HanzoBlockchainInfra` to use gRPC
3. Remove SSH key management code
4. Update deployment flows

### Phase 1.5: Testing and Rollout (Week 5-6)

1. Full integration testing
2. Performance benchmarking
3. Staged rollout to development nodes
4. Production deployment

## Security Considerations

### Authentication

| Method | Current | Proposed | Security Level |
|--------|---------|----------|----------------|
| SSH Keys | Ed25519 keys stored in DB | mTLS certificates | Higher |
| Bearer Tokens | Plain HTTP headers | gRPC metadata with mTLS | Higher |
| API Keys | Environment variables | Certificate-based identity | Higher |

### mTLS Configuration

```yaml
# Certificate hierarchy
hanzo-ca.crt           # Root CA (Hanzo PKI)
├── platform.crt       # Platform control plane
├── node-1.crt         # Node 1 server cert
├── node-2.crt         # Node 2 server cert
└── operator-*.crt     # Node operator certificates
```

### Network Security

1. **TLS 1.3** for all RPC connections
2. **Certificate pinning** for node connections
3. **IP allowlisting** for control plane
4. **Rate limiting** on all endpoints
5. **Audit logging** for all operations

### Post-Quantum Considerations

As documented in LLM.md, the system is designed for post-quantum security:

- **ML-KEM-768** (NIST FIPS 203) for key encapsulation
- **ChaCha20-Poly1305** for authenticated encryption
- Migration path to hybrid classical/PQ certificates

## Compatibility

### Backward Compatibility

The migration maintains backward compatibility by:

1. Supporting both SSH and RPC paths during transition
2. Feature flag `HANZO_USE_RPC_DEPLOYMENT=true`
3. Automatic fallback to SSH if RPC unavailable
4. Gradual deprecation of SSH path

### Forward Compatibility

The RPC interface is designed for future extensions:

1. **`non_exhaustive`** enums for all status types
2. **Reserved fields** in proto messages
3. **Version negotiation** in initial handshake
4. **Capability discovery** via GetHealth

## Monitoring and Observability

### Metrics

```rust
// Exposed via Prometheus
hanzo_node_rpc_requests_total{method="Deploy", status="success"}
hanzo_node_rpc_request_duration_seconds{method="Deploy"}
hanzo_node_rpc_connections_active
hanzo_node_rpc_errors_total{type="transport"}
```

### Tracing

Integration with existing OpenTelemetry setup in `code-rs/otel/`:

```rust
let span = tracing::info_span!("hanzo_node_rpc", method = "Deploy");
let _guard = span.enter();
```

### Health Checks

```
GET /health          -> {"status": "healthy", "consensus": {...}}
GET /readiness       -> {"ready": true}
GET /liveness        -> {"alive": true}
```

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| gRPC library bugs | Low | High | Comprehensive testing, gradual rollout |
| Certificate management | Medium | Medium | Automated rotation, monitoring |
| Network partitions | Low | High | Retry logic, fallback paths |
| Performance regression | Low | Medium | Benchmarking, connection pooling |
| Breaking changes | Medium | Low | Versioned protos, deprecation policy |

## Success Metrics

1. **Zero SSH keys** in production after Phase 1
2. **< 100ms P99 latency** for health checks
3. **< 500ms P99 latency** for deploy operations
4. **99.9% availability** for RPC endpoints
5. **100% mTLS coverage** for all node connections

## References

- [MCP Protocol Documentation](/Users/z/work/hanzo/dev/hanzo-dev/docs/codex_mcp_interface.md)
- [Protocol V1 Specification](/Users/z/work/hanzo/dev/hanzo-dev/docs/protocol_v1.md)
- [Phase 0 Baseline](/Users/z/work/hanzo/dev/code-rs/docs/architecture/phase0-baseline.md)
- [LLM.md - Hanzod Integration](/Users/z/work/hanzo/dev/LLM.md)
- [Platform Architecture](/Users/z/work/hanzo/platform/HANZO_PLATFORM_ARCHITECTURE.md)
