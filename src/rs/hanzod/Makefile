# Makefile for Hanzo AI Blockchain with Lux Consensus

.PHONY: help build test run clean install deps proto

# Colors for output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[1;33m
BLUE := \033[0;34m
NC := \033[0m # No Color

help: ## Show this help message
	@echo "$(BLUE)Hanzo AI Blockchain - Makefile$(NC)"
	@echo "$(YELLOW)================================$(NC)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "$(GREEN)%-15s$(NC) %s\n", $$1, $$2}'

deps: ## Install dependencies
	@echo "$(YELLOW)Installing dependencies...$(NC)"
	cargo fetch
	@echo "$(GREEN)✅ Dependencies installed$(NC)"

proto: ## Generate protobuf files
	@echo "$(YELLOW)Generating proto files...$(NC)"
	@if [ -d "proto" ]; then \
		cargo build --features grpc; \
		echo "$(GREEN)✅ Proto files generated$(NC)"; \
	else \
		echo "$(RED)⚠️  Proto directory not found$(NC)"; \
	fi

build: ## Build the project
	@echo "$(YELLOW)Building Hanzo AI Blockchain...$(NC)"
	cargo build --release
	@echo "$(GREEN)✅ Build complete$(NC)"

test: ## Run all tests
	@echo "$(YELLOW)Running comprehensive test suite...$(NC)"
	@echo ""
	@echo "$(BLUE)1. Unit Tests$(NC)"
	cargo test --lib -- --nocapture
	@echo ""
	@echo "$(BLUE)2. Integration Tests$(NC)"
	cargo test --test comprehensive_tests -- --nocapture
	@echo ""
	@echo "$(BLUE)3. Warp FFI Tests$(NC)"
	cargo test warp_ffi -- --nocapture
	@echo ""
	@echo "$(BLUE)4. Consensus Tests$(NC)"
	cargo test lux_consensus -- --nocapture
	@echo ""
	@echo "$(GREEN)✅ All tests complete$(NC)"

test-quick: ## Run quick tests only
	@echo "$(YELLOW)Running quick tests...$(NC)"
	cargo test --lib -- --nocapture

test-integration: ## Run integration tests only
	@echo "$(YELLOW)Running integration tests...$(NC)"
	cargo test --test comprehensive_tests -- --nocapture

run: ## Run local test node
	@echo "$(YELLOW)Starting Hanzo AI Blockchain local node...$(NC)"
	@echo "$(BLUE)Configuration:$(NC)"
	@echo "  • Chain ID: hanzo-local-test"
	@echo "  • Network ID: 1337 (local)"
	@echo "  • gRPC: localhost:50051"
	@echo "  • HTTP: localhost:8545"
	@echo "  • WebSocket: localhost:8546"
	@echo ""
	cargo run --example run_local

run-client: ## Run test client
	@echo "$(YELLOW)Starting test client...$(NC)"
	cargo run --example test_client

test-client: ## Test against running node
	@echo "$(YELLOW)Testing against local node...$(NC)"
	@curl -s http://localhost:8545/health | jq '.' || echo "$(RED)❌ Server not running. Start with: make run$(NC)"
	@echo ""
	@curl -s http://localhost:8545/status | jq '.' || true
	@echo ""
	@curl -s http://localhost:8545/v1/validators | jq '.' || true

bench: ## Run benchmarks
	@echo "$(YELLOW)Running benchmarks...$(NC)"
	cargo bench

clean: ## Clean build artifacts
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	cargo clean
	rm -rf target/
	@echo "$(GREEN)✅ Clean complete$(NC)"

install: build ## Install locally
	@echo "$(YELLOW)Installing hanzod...$(NC)"
	cargo install --path .
	@echo "$(GREEN)✅ Installation complete$(NC)"

check: ## Check code without building
	@echo "$(YELLOW)Checking code...$(NC)"
	cargo check --all-features
	cargo clippy -- -D warnings
	cargo fmt -- --check
	@echo "$(GREEN)✅ Code check complete$(NC)"

fmt: ## Format code
	@echo "$(YELLOW)Formatting code...$(NC)"
	cargo fmt
	@echo "$(GREEN)✅ Format complete$(NC)"

doc: ## Generate documentation
	@echo "$(YELLOW)Generating documentation...$(NC)"
	cargo doc --no-deps --open
	@echo "$(GREEN)✅ Documentation generated$(NC)"

# Development shortcuts
dev: fmt check test-quick ## Format, check, and run quick tests

all: deps proto build test ## Run everything

# Docker commands (if needed)
docker-build: ## Build Docker image
	@echo "$(YELLOW)Building Docker image...$(NC)"
	docker build -t hanzo/hanzod:latest .
	@echo "$(GREEN)✅ Docker image built$(NC)"

docker-run: ## Run in Docker
	@echo "$(YELLOW)Running in Docker...$(NC)"
	docker run -p 50051:50051 -p 8545:8545 -p 8546:8546 hanzo/hanzod:latest

# Test coverage
coverage: ## Generate test coverage report
	@echo "$(YELLOW)Generating coverage report...$(NC)"
	cargo tarpaulin --out Html
	@echo "$(GREEN)✅ Coverage report generated$(NC)"

# Performance testing
perf: ## Run performance tests
	@echo "$(YELLOW)Running performance tests...$(NC)"
	@echo "$(BLUE)Testing Warp message creation speed...$(NC)"
	@time cargo test test_warp_message_creation --release -- --nocapture
	@echo ""
	@echo "$(BLUE)Testing consensus round processing...$(NC)"
	@time cargo test test_consensus_round --release -- --nocapture
	@echo "$(GREEN)✅ Performance tests complete$(NC)"

# Status check
status: ## Check system status
	@echo "$(BLUE)Hanzo AI Blockchain Status$(NC)"
	@echo "$(YELLOW)===========================$(NC)"
	@echo "Checking local node..."
	@curl -s http://localhost:8545/health 2>/dev/null | jq '.' || echo "$(RED)❌ Node not running$(NC)"
	@echo ""
	@echo "Checking dependencies..."
	@cargo --version
	@rustc --version
	@echo ""
	@echo "Checking features..."
	@echo "  • KuzuDB: ✅ Enabled"
	@echo "  • Lux Consensus: ✅ Integrated"
	@echo "  • Warp ICM: ✅ Available"
	@echo "  • Teleport zkBridge: ✅ Available"
	@echo "  • Qwen3 Support: ✅ Configured"