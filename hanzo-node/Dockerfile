# Hanzo Node - Multi-stage Docker Build
# Build the Rust binary with all dependencies, then create minimal runtime image

# =============================================================================
# Stage 1: Builder - Compile Rust binary
# =============================================================================
FROM rust:1.82-bookworm AS builder

# Install build dependencies (protobuf compiler for tonic-build)
RUN apt-get update && apt-get install -y \
    protobuf-compiler \
    libprotobuf-dev \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /build

# Copy Cargo files first for dependency caching
COPY rust/Cargo.toml rust/Cargo.lock* ./

# Create dummy src to build dependencies (cache layer optimization)
RUN mkdir -p src/bin src/rpc && \
    echo "fn main() {}" > src/bin/hanzo-node.rs && \
    echo "pub fn placeholder() {}" > src/lib.rs

# Build dependencies only (this layer is cached)
RUN cargo build --release || true

# Remove dummy files
RUN rm -rf src

# Copy actual source code
COPY rust/src ./src
COPY rust/proto ./proto
COPY rust/build.rs ./build.rs

# Create output directory for protobuf
RUN mkdir -p src/rpc

# Build the actual binary
RUN cargo build --release --bin hanzo-node

# =============================================================================
# Stage 2: Runtime - Minimal production image
# =============================================================================
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user for security
RUN groupadd -r hanzo && useradd -r -g hanzo hanzo

# Create directories
RUN mkdir -p /app/data /app/config && \
    chown -R hanzo:hanzo /app

WORKDIR /app

# Copy binary from builder
COPY --from=builder /build/target/release/hanzo-node /app/hanzo-node

# Set ownership
RUN chown hanzo:hanzo /app/hanzo-node && \
    chmod +x /app/hanzo-node

# Switch to non-root user
USER hanzo

# Environment variables with defaults
ENV HANZO_NODE_ID=""
ENV HANZO_GRPC_ADDR="0.0.0.0:9090"
ENV HANZO_HTTP_ADDR="0.0.0.0:8080"
ENV HANZO_P2P_ADDR="/ip4/0.0.0.0/tcp/9000"
ENV HANZO_DATA_DIR="/app/data"
ENV HANZO_NETWORK_ID="1337"
ENV HANZO_OPERATOR=""
ENV HANZO_MLX="false"
ENV RUST_LOG="hanzo_node=info,tonic=info,libp2p=warn"

# Expose ports
# 9090 - gRPC/RPC server (main API) - user requested port
# 8080 - HTTP health check endpoint
# 9000 - P2P libp2p networking
EXPOSE 9090 8080 9000

# Health check - uses HTTP health endpoint
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Volume for persistent data
VOLUME ["/app/data"]

# Default command
CMD ["/app/hanzo-node"]
