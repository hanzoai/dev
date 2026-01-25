# Hanzo Node API Documentation

This directory contains the OpenAPI 3.0 specification for the Hanzo Node RPC API.

## Files

- `openapi.yaml` - Complete OpenAPI 3.0 specification

## API Overview

The Hanzo Node API provides comprehensive endpoints for:

### Core Services

| Category | Description | Key Endpoints |
|----------|-------------|---------------|
| **Health** | Node health and status monitoring | `/health`, `/v1/status`, `/v1/metrics` |
| **Node Registration** | Register and manage compute nodes | `/v1/nodes/register`, `/v1/nodes/{id}/heartbeat` |
| **Compute Pool** | Compute resource allocation | `/v1/compute/pools`, `/v1/compute/allocate` |
| **QoS Challenges** | Quality of Service verification | `/v1/qos/challenges`, `/v1/qos/providers/{id}/score` |
| **Deployments** | Container deployment lifecycle | `/v1/deployments`, `/v1/deployments/{id}/scale` |
| **Containers** | Docker-compatible container mgmt | `/v1/containers`, `/v1/containers/{id}/exec` |
| **Inference** | LLM inference services | `/v1/inference`, `/v1/embed`, `/v1/rerank` |
| **Operator** | Staking and operator registration | `/v1/operators/register`, `/v1/operators/{addr}/stake` |

### Authentication

All endpoints require mTLS authentication with certificates signed by the Hanzo CA.
Bearer token authentication is supported as fallback for development environments.

### gRPC Mapping

This REST API mirrors the gRPC interface defined in `../rust/proto/hanzo.node.v1.proto`.
For streaming endpoints, WebSocket connections are available at `/ws/*`.

## Usage

### View with Swagger UI

```bash
# Using npx
npx @redocly/cli preview-docs openapi.yaml

# Or with Docker
docker run -p 8080:8080 -v $(pwd):/spec redocly/redoc-cli serve /spec/openapi.yaml
```

### Generate Client SDKs

```bash
# TypeScript
npx @openapitools/openapi-generator-cli generate \
  -i openapi.yaml \
  -g typescript-axios \
  -o ../clients/typescript

# Rust
npx @openapitools/openapi-generator-cli generate \
  -i openapi.yaml \
  -g rust \
  -o ../clients/rust

# Go
npx @openapitools/openapi-generator-cli generate \
  -i openapi.yaml \
  -g go \
  -o ../clients/go
```

### Validate Spec

```bash
npx @redocly/cli lint openapi.yaml
```

## QoS Challenge System

The QoS (Quality of Service) challenge system ensures compute providers maintain
advertised service levels through cryptographic challenges:

### Challenge Types

| Type | Description | Metrics |
|------|-------------|---------|
| `compute` | GPU/CPU performance | FP32/FP16/INT8 FLOPS, Memory BW |
| `latency` | Network latency | P50/P95/P99 RTT |
| `bandwidth` | Throughput | Upload/Download Mbps |
| `availability` | Uptime | Response time |
| `model` | AI capability | Tokens/second |

### Score Calculation

Composite QoS scores use weighted geometric mean:
- Compute: 30%
- Latency: 20%
- Bandwidth: 15%
- Availability: 25%
- Consistency: 10%

### Challenge Flow

```
1. POST /v1/qos/challenges          <- Issue challenge
2. GET  /v1/qos/challenges/{id}     <- Provider fetches challenge
3. POST /v1/qos/challenges/{id}/respond  <- Submit proof
4. POST /v1/qos/challenges/{id}/verify   <- Verifiers confirm
5. GET  /v1/qos/providers/{id}/score     <- Updated score
```

## Network IDs

| Network | ID | Description |
|---------|-----|-------------|
| Mainnet | `43114` | Production network |
| Testnet | `43113` | Fuji testnet |
| Local | `1337` | Development |

## Related Documentation

- [Phase 1 RPC Design](../../code-rs/docs/architecture/phase1-rpc-design.md)
- [QoS Challenge Crate](../../code-rs/qos-challenge/)
- [Proto Definitions](../rust/proto/hanzo.node.v1.proto)
