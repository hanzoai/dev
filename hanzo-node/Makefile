# Hanzo Node - Makefile
# Common operations for building and running the node

.PHONY: help build run dev test lint clean docker-build docker-up docker-down docker-logs

# Default target
help:
	@echo "Hanzo Node - Build and Run Commands"
	@echo ""
	@echo "Development:"
	@echo "  make build        Build the Rust binary"
	@echo "  make run          Run the node locally"
	@echo "  make dev          Run with debug logging"
	@echo "  make test         Run all tests"
	@echo "  make lint         Run clippy linter"
	@echo "  make clean        Clean build artifacts"
	@echo ""
	@echo "Docker:"
	@echo "  make docker-build Build Docker image"
	@echo "  make docker-up    Start node with Docker Compose"
	@echo "  make docker-down  Stop Docker containers"
	@echo "  make docker-logs  View container logs"
	@echo "  make docker-shell Shell into running container"
	@echo ""
	@echo "Monitoring:"
	@echo "  make monitor-up   Start with Prometheus + Grafana"
	@echo "  make monitor-down Stop monitoring stack"
	@echo ""
	@echo "Release:"
	@echo "  make release      Build optimized release binary"
	@echo "  make docker-push  Push image to registry"

# =============================================================================
# Development
# =============================================================================

build:
	cd rust && cargo build

release:
	cd rust && cargo build --release

run:
	cd rust && cargo run --bin hanzo-node

dev:
	cd rust && RUST_LOG=hanzo_node=debug,tonic=debug cargo run --bin hanzo-node

test:
	cd rust && cargo test

lint:
	cd rust && cargo clippy -- -D warnings

fmt:
	cd rust && cargo fmt

fmt-check:
	cd rust && cargo fmt -- --check

clean:
	cd rust && cargo clean
	rm -rf target

# =============================================================================
# Docker
# =============================================================================

docker-build:
	docker build -t hanzoai/hanzo-node:latest .

docker-up:
	docker compose up -d

docker-down:
	docker compose down

docker-logs:
	docker compose logs -f hanzo-node

docker-shell:
	docker compose exec hanzo-node /bin/sh

docker-restart:
	docker compose restart hanzo-node

# =============================================================================
# Monitoring Stack
# =============================================================================

monitor-up:
	docker compose --profile monitoring up -d

monitor-down:
	docker compose --profile monitoring down

# =============================================================================
# Release
# =============================================================================

docker-push:
	docker push hanzoai/hanzo-node:latest

# Tag and push a specific version
# Usage: make docker-release VERSION=0.1.0
docker-release:
	docker tag hanzoai/hanzo-node:latest hanzoai/hanzo-node:$(VERSION)
	docker push hanzoai/hanzo-node:$(VERSION)
	docker push hanzoai/hanzo-node:latest

# =============================================================================
# Utilities
# =============================================================================

# Check health endpoint
health:
	curl -s http://localhost:8080/health | jq .

# gRPC health check (requires grpcurl)
grpc-health:
	grpcurl -plaintext localhost:9090 hanzo.node.v1.NodeService/GetHealth

# Show running containers
status:
	docker compose ps
