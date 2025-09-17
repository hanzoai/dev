# Hanzo Dev - Universal Makefile
# Automates common tasks across all project types in the Hanzo ecosystem

# Default shell
SHELL := /bin/bash

# Colors for output
RED := $(shell printf '\033[0;31m')
GREEN := $(shell printf '\033[0;32m')
YELLOW := $(shell printf '\033[1;33m')
BLUE := $(shell printf '\033[0;34m')
CYAN := $(shell printf '\033[0;36m')
NC := $(shell printf '\033[0m') # No Color

# Version info
VERSION := $(shell git describe --tags --always --dirty 2>/dev/null || echo "dev")
COMMIT := $(shell git rev-parse --short HEAD 2>/dev/null || echo "unknown")
DATE := $(shell date -u +"%Y-%m-%dT%H:%M:%SZ")

# Directories
ROOT_DIR := $(shell pwd)
RUST_DIR := $(ROOT_DIR)/src/rs
CODEX_RS_DIR := $(ROOT_DIR)/codex-rs
TS_DIR := $(ROOT_DIR)/src/ts
SDK_DIR := $(ROOT_DIR)/../sdk
CLI_DIR := $(ROOT_DIR)/../cli
ENGINE_DIR := $(ROOT_DIR)/../engine
NODE_DIR := $(ROOT_DIR)/../node

# Binary names
HANZO_BIN := hanzo
CODEX_BIN := codex
ENGINE_BIN := hanzo-engine
NODE_BIN := hanzod

# Ports
ENGINE_PORT := 36900
NODE_PORT := 3690
OLLAMA_PORT := 11434

# Build flags
RUST_FLAGS := RUSTFLAGS="-C target-cpu=native"
CARGO_FLAGS := --release
NODE_FLAGS := NODE_ENV=production

# Python version
PYTHON := python3
UV := uv

.PHONY: all help clean build test install dev setup lint format check

# Default target
all: help

# Help target
help:
	@echo "$(CYAN)╔════════════════════════════════════════════════════════════════╗$(NC)"
	@echo "$(CYAN)║               Hanzo Dev - Universal Makefile                  ║$(NC)"
	@echo "$(CYAN)╠════════════════════════════════════════════════════════════════╣$(NC)"
	@echo "$(CYAN)║ Core Commands:                                                 ║$(NC)"
	@echo "$(CYAN)║$(NC)  $(GREEN)make setup$(NC)        - Initial setup for all components"
	@echo "$(CYAN)║$(NC)  $(GREEN)make build$(NC)        - Build all components"
	@echo "$(CYAN)║$(NC)  $(GREEN)make test$(NC)         - Run all tests"
	@echo "$(CYAN)║$(NC)  $(GREEN)make dev$(NC)          - Start development environment"
	@echo "$(CYAN)║$(NC)  $(GREEN)make clean$(NC)        - Clean all build artifacts"
	@echo "$(CYAN)║$(NC)  $(GREEN)make install$(NC)      - Install all components"
	@echo "$(CYAN)║                                                                ║$(NC)"
	@echo "$(CYAN)║ Component-Specific:                                           ║$(NC)"
	@echo "$(CYAN)║$(NC)  $(BLUE)make rust-*$(NC)       - Rust/Codex commands"
	@echo "$(CYAN)║$(NC)  $(BLUE)make node-*$(NC)       - Node.js/TypeScript commands"
	@echo "$(CYAN)║$(NC)  $(BLUE)make python-*$(NC)     - Python commands"
	@echo "$(CYAN)║$(NC)  $(BLUE)make engine-*$(NC)     - Hanzo Engine commands"
	@echo "$(CYAN)║$(NC)  $(BLUE)make hanzo-node-*$(NC) - Hanzo Node commands"
	@echo "$(CYAN)║$(NC)  $(BLUE)make docker-*$(NC)     - Docker commands"
	@echo "$(CYAN)║                                                                ║$(NC)"
	@echo "$(CYAN)║ Services:                                                      ║$(NC)"
	@echo "$(CYAN)║$(NC)  $(YELLOW)make start$(NC)        - Start all services"
	@echo "$(CYAN)║$(NC)  $(YELLOW)make stop$(NC)         - Stop all services"
	@echo "$(CYAN)║$(NC)  $(YELLOW)make status$(NC)       - Check service status"
	@echo "$(CYAN)║$(NC)  $(YELLOW)make logs$(NC)         - View service logs"
	@echo "$(CYAN)║                                                                ║$(NC)"
	@echo "$(CYAN)║ Quality:                                                       ║$(NC)"
	@echo "$(CYAN)║$(NC)  $(GREEN)make lint$(NC)         - Run all linters"
	@echo "$(CYAN)║$(NC)  $(GREEN)make format$(NC)       - Format all code"
	@echo "$(CYAN)║$(NC)  $(GREEN)make check$(NC)        - Run all checks"
	@echo "$(CYAN)║$(NC)  $(GREEN)make audit$(NC)        - Security audit"
	@echo "$(CYAN)╚════════════════════════════════════════════════════════════════╝$(NC)"

# ============================================================================
# Setup Commands
# ============================================================================

setup: setup-deps setup-rust setup-node setup-python setup-git
	@echo "$(GREEN)✓ Setup complete!$(NC)"

setup-deps:
	@echo "$(BLUE)Installing system dependencies...$(NC)"
	@if command -v brew >/dev/null 2>&1; then \
		brew install rust node python uv git-lfs protobuf; \
	elif command -v apt-get >/dev/null 2>&1; then \
		sudo apt-get update && sudo apt-get install -y build-essential git curl wget; \
		curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y; \
		curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -; \
		sudo apt-get install -y nodejs; \
	fi

setup-rust:
	@echo "$(BLUE)Setting up Rust environment...$(NC)"
	@rustup default stable
	@rustup component add rustfmt clippy rust-analyzer
	@cargo install cargo-watch cargo-edit cargo-audit cargo-tarpaulin sccache
	@echo "$(GREEN)✓ Rust setup complete$(NC)"

setup-node:
	@echo "$(BLUE)Setting up Node.js environment...$(NC)"
	@npm install -g pnpm bun tsx typescript @biomejs/biome
	@if [ -f package.json ]; then pnpm install; fi
	@echo "$(GREEN)✓ Node.js setup complete$(NC)"

setup-python:
	@echo "$(BLUE)Setting up Python environment...$(NC)"
	@if ! command -v uv >/dev/null 2>&1; then \
		curl -LsSf https://astral.sh/uv/install.sh | sh; \
	fi
	@uv venv
	@uv pip install --upgrade pip
	@uv pip install ruff mypy pytest pytest-cov black isort
	@echo "$(GREEN)✓ Python setup complete$(NC)"

setup-git:
	@echo "$(BLUE)Setting up Git hooks...$(NC)"
	@git config core.hooksPath .githooks
	@chmod +x .githooks/*
	@git lfs install
	@echo "$(GREEN)✓ Git setup complete$(NC)"

# ============================================================================
# Build Commands
# ============================================================================

build: build-rust build-node build-python
	@echo "$(GREEN)✓ All components built successfully!$(NC)"

build-rust: rust-build
build-node: node-build  
build-python: python-build

# Rust/Codex Targets
rust-build:
	@echo "$(BLUE)Building Rust/Codex components...$(NC)"
	@cd $(RUST_DIR) && $(RUST_FLAGS) cargo build $(CARGO_FLAGS)
	@if [ -d "$(CODEX_RS_DIR)" ]; then \
		cd $(CODEX_RS_DIR) && $(RUST_FLAGS) cargo build $(CARGO_FLAGS); \
	fi
	@echo "$(GREEN)✓ Rust build complete$(NC)"

rust-dev:
	@echo "$(BLUE)Starting Rust dev mode...$(NC)"
	@cd $(RUST_DIR) && cargo watch -x run

rust-test:
	@echo "$(BLUE)Running Rust tests...$(NC)"
	@cd $(RUST_DIR) && cargo test --all-features

rust-lint:
	@echo "$(BLUE)Linting Rust code...$(NC)"
	@cd $(RUST_DIR) && cargo clippy --all-targets --all-features -- -D warnings

rust-format:
	@echo "$(BLUE)Formatting Rust code...$(NC)"
	@cd $(RUST_DIR) && cargo fmt --all

rust-clean:
	@echo "$(YELLOW)Cleaning Rust artifacts...$(NC)"
	@if [ -d "$(RUST_DIR)" ]; then cd $(RUST_DIR) && cargo clean 2>/dev/null || true; fi
	@if [ -d "$(CODEX_RS_DIR)" ]; then cd $(CODEX_RS_DIR) && cargo clean 2>/dev/null || true; fi

rust-release:
	@echo "$(BLUE)Building Rust release...$(NC)"
	@cd $(RUST_DIR) && $(RUST_FLAGS) cargo build --release
	@strip target/release/$(HANZO_BIN) 2>/dev/null || true
	@echo "$(GREEN)✓ Release build complete: target/release/$(HANZO_BIN)$(NC)"

# Node.js/TypeScript Targets
node-build:
	@echo "$(BLUE)Building Node.js/TypeScript components...$(NC)"
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm install && pnpm build; \
	fi
	@echo "$(GREEN)✓ Node.js build complete$(NC)"

node-dev:
	@echo "$(BLUE)Starting Node.js dev mode...$(NC)"
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm dev; \
	else \
		echo "$(RED)No package.json found in $(TS_DIR)$(NC)"; \
	fi

node-test:
	@echo "$(BLUE)Running Node.js tests...$(NC)"
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm test; \
	fi

node-lint:
	@echo "$(BLUE)Linting TypeScript code...$(NC)"
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm lint; \
	fi

node-format:
	@echo "$(BLUE)Formatting TypeScript code...$(NC)"
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm format; \
	fi

node-clean:
	@echo "$(YELLOW)Cleaning Node.js artifacts...$(NC)"
	@rm -rf node_modules dist build .next .turbo
	@find . -name "node_modules" -type d -prune -exec rm -rf {} \; 2>/dev/null || true

# Python Targets
python-build:
	@echo "$(BLUE)Building Python components...$(NC)"
	@if [ -f "pyproject.toml" ]; then \
		uv build; \
	elif [ -f "setup.py" ]; then \
		$(PYTHON) setup.py build; \
	fi
	@echo "$(GREEN)✓ Python build complete$(NC)"

python-dev:
	@echo "$(BLUE)Starting Python dev mode...$(NC)"
	@if [ -f "main.py" ]; then \
		uv run python main.py; \
	elif [ -f "app.py" ]; then \
		uv run python app.py; \
	fi

python-test:
	@echo "$(BLUE)Running Python tests...$(NC)"
	@uv run pytest -v --cov

python-lint:
	@echo "$(BLUE)Linting Python code...$(NC)"
	@uv run ruff check .
	@uv run mypy .

python-format:
	@echo "$(BLUE)Formatting Python code...$(NC)"
	@uv run ruff format .
	@uv run isort .

python-clean:
	@echo "$(YELLOW)Cleaning Python artifacts...$(NC)"
	@rm -rf build dist *.egg-info __pycache__ .pytest_cache .coverage .mypy_cache .ruff_cache
	@find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pyc" -delete 2>/dev/null || true

# ============================================================================
# Hanzo Engine Commands
# ============================================================================

engine-build:
	@echo "$(BLUE)Building Hanzo Engine...$(NC)"
	@if [ -d "$(ENGINE_DIR)" ]; then \
		cd $(ENGINE_DIR) && cargo build --release --features cuda,flash-attn,cudnn; \
		echo "$(GREEN)✓ Hanzo Engine built$(NC)"; \
	else \
		echo "$(YELLOW)Hanzo Engine not found at $(ENGINE_DIR)$(NC)"; \
	fi

engine-start:
	@echo "$(BLUE)Starting Hanzo Engine on port $(ENGINE_PORT)...$(NC)"
	@if [ -f "$(ENGINE_DIR)/target/release/$(ENGINE_BIN)" ]; then \
		$(ENGINE_DIR)/target/release/$(ENGINE_BIN) serve --port $(ENGINE_PORT) & \
		echo $$! > .engine.pid; \
		echo "$(GREEN)✓ Hanzo Engine started (PID: $$(cat .engine.pid))$(NC)"; \
	else \
		echo "$(RED)Hanzo Engine binary not found. Run 'make engine-build' first$(NC)"; \
	fi

engine-stop:
	@echo "$(YELLOW)Stopping Hanzo Engine...$(NC)"
	@if [ -f .engine.pid ]; then \
		kill $$(cat .engine.pid) 2>/dev/null || true; \
		rm .engine.pid; \
		echo "$(GREEN)✓ Hanzo Engine stopped$(NC)"; \
	fi

engine-status:
	@if curl -s http://localhost:$(ENGINE_PORT)/health >/dev/null 2>&1; then \
		echo "$(GREEN)✓ Hanzo Engine is running on port $(ENGINE_PORT)$(NC)"; \
	else \
		echo "$(RED)✗ Hanzo Engine is not running$(NC)"; \
	fi

# ============================================================================
# Hanzo Node Commands
# ============================================================================

hanzo-node-build:
	@echo "$(BLUE)Building Hanzo Node...$(NC)"
	@if [ -d "$(NODE_DIR)" ]; then \
		cd $(NODE_DIR) && cargo build --release; \
		echo "$(GREEN)✓ Hanzo Node built$(NC)"; \
	else \
		echo "$(YELLOW)Hanzo Node not found at $(NODE_DIR)$(NC)"; \
	fi

hanzo-node-start:
	@echo "$(BLUE)Starting Hanzo Node with Qwen3 support...$(NC)"
	@echo "$(CYAN)API Port: $(NODE_PORT), P2P Port: 3691$(NC)"
	@if [ -f "./scripts/run_node_localhost.sh" ]; then \
		./scripts/run_node_localhost.sh & \
		echo $$! > .node.pid; \
		echo "$(GREEN)✓ Hanzo Node started (PID: $$(cat .node.pid))$(NC)"; \
		echo "$(GREEN)✓ Swagger UI: http://localhost:$(NODE_PORT)/v2/swagger-ui/$(NC)"; \
	elif [ -f "$(NODE_DIR)/target/release/$(NODE_BIN)" ]; then \
		HANZO_ENABLE_QWEN3=true \
		QWEN3_MODELS="qwen3-8b,qwen3-14b,qwen3-72b" \
		QWEN3_EMBEDDING_MODEL="qwen3-embedding-8b" \
		QWEN3_RERANKER_MODEL="qwen3-reranker-4b" \
		NODE_API_PORT=$(NODE_PORT) NODE_PORT=3691 \
		$(NODE_DIR)/target/release/$(NODE_BIN) & \
		echo $$! > .node.pid; \
		echo "$(GREEN)✓ Hanzo Node started with Qwen3 (PID: $$(cat .node.pid))$(NC)"; \
	else \
		echo "$(RED)Hanzo Node not found. Run 'make hanzo-node-build' first$(NC)"; \
	fi

hanzo-node-stop:
	@echo "$(YELLOW)Stopping Hanzo Node...$(NC)"
	@if [ -f .node.pid ]; then \
		kill $$(cat .node.pid) 2>/dev/null || true; \
		rm .node.pid; \
		echo "$(GREEN)✓ Hanzo Node stopped$(NC)"; \
	fi

hanzo-node-status:
	@if curl -s http://localhost:$(NODE_PORT)/health >/dev/null 2>&1; then \
		echo "$(GREEN)✓ Hanzo Node is running on port $(NODE_PORT)$(NC)"; \
	else \
		echo "$(RED)✗ Hanzo Node is not running$(NC)"; \
	fi

# ============================================================================
# Docker Commands
# ============================================================================

docker-build:
	@echo "$(BLUE)Building Docker images...$(NC)"
	@docker compose build

docker-up:
	@echo "$(BLUE)Starting Docker services...$(NC)"
	@docker compose up -d
	@echo "$(GREEN)✓ Docker services started$(NC)"

docker-down:
	@echo "$(YELLOW)Stopping Docker services...$(NC)"
	@docker compose down
	@echo "$(GREEN)✓ Docker services stopped$(NC)"

docker-logs:
	@docker compose logs -f

docker-clean:
	@echo "$(YELLOW)Cleaning Docker resources...$(NC)"
	@docker compose down -v 2>/dev/null || true
	@docker system prune -f 2>/dev/null || true
	@echo "$(GREEN)✓ Docker resources cleaned$(NC)"

docker-status:
	@docker compose ps

# ============================================================================
# Service Management
# ============================================================================

start: start-services
	@echo "$(GREEN)✓ All services started!$(NC)"
	@make status

start-services: engine-start hanzo-node-start
	@echo "$(BLUE)Starting additional services...$(NC)"
	@if command -v ollama >/dev/null 2>&1; then \
		ollama serve >/dev/null 2>&1 & \
		echo "$(GREEN)✓ Ollama started$(NC)"; \
	fi

stop: stop-services
	@echo "$(GREEN)✓ All services stopped!$(NC)"

stop-services: engine-stop hanzo-node-stop
	@echo "$(YELLOW)Stopping additional services...$(NC)"
	@pkill ollama 2>/dev/null || true

status:
	@echo "$(CYAN)Service Status:$(NC)"
	@echo "==============="
	@make -s engine-status
	@make -s hanzo-node-status
	@if lsof -i :$(OLLAMA_PORT) >/dev/null 2>&1; then \
		echo "$(GREEN)✓ Ollama is running on port $(OLLAMA_PORT)$(NC)"; \
	else \
		echo "$(RED)✗ Ollama is not running$(NC)"; \
	fi

logs:
	@if [ -f .engine.pid ]; then \
		echo "$(CYAN)Hanzo Engine logs:$(NC)"; \
		tail -f ~/.hanzo/engine.log 2>/dev/null || echo "No logs available"; \
	fi
	@if [ -f .node.pid ]; then \
		echo "$(CYAN)Hanzo Node logs:$(NC)"; \
		tail -f ~/.hanzo/node.log 2>/dev/null || echo "No logs available"; \
	fi

# ============================================================================
# Development Commands
# ============================================================================

dev: dev-setup
	@echo "$(BLUE)Starting development environment...$(NC)"
	@make -j3 dev-rust dev-node dev-services

dev-setup:
	@echo "$(BLUE)Setting up development environment...$(NC)"
	@cp hanzo_config.toml ~/.hanzo/config.toml 2>/dev/null || true
	@mkdir -p ~/.hanzo/logs

dev-rust:
	@cd $(RUST_DIR) && cargo watch -x run

dev-node:
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm dev; \
	fi

dev-services:
	@make start-services

dev-stop:
	@make stop
	@pkill cargo-watch 2>/dev/null || true
	@pkill node 2>/dev/null || true

# ============================================================================
# Testing Commands
# ============================================================================

test: test-unit test-integration
	@echo "$(GREEN)✓ All tests passed!$(NC)"

test-unit: rust-test node-test python-test
	@echo "$(GREEN)✓ Unit tests complete$(NC)"

test-integration:
	@echo "$(BLUE)Running integration tests...$(NC)"
	@if [ -f "tests/integration.sh" ]; then \
		./tests/integration.sh; \
	fi
	@echo "$(GREEN)✓ Integration tests complete$(NC)"

test-e2e:
	@echo "$(BLUE)Running end-to-end tests...$(NC)"
	@if [ -f "tests/e2e.sh" ]; then \
		./tests/e2e.sh; \
	fi

test-coverage:
	@echo "$(BLUE)Generating test coverage...$(NC)"
	@cd $(RUST_DIR) && cargo tarpaulin --out Html
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm test:coverage; \
	fi
	@uv run pytest --cov --cov-report=html

# ============================================================================
# Quality Commands
# ============================================================================

lint: rust-lint node-lint python-lint
	@echo "$(GREEN)✓ All linting passed!$(NC)"

format: rust-format node-format python-format
	@echo "$(GREEN)✓ All code formatted!$(NC)"

check: lint test
	@echo "$(GREEN)✓ All checks passed!$(NC)"

audit:
	@echo "$(BLUE)Running security audit...$(NC)"
	@cd $(RUST_DIR) && cargo audit
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm audit; \
	fi
	@uv pip audit
	@echo "$(GREEN)✓ Security audit complete$(NC)"

# ============================================================================
# Release Commands
# ============================================================================

release: check
	@echo "$(BLUE)Building release...$(NC)"
	@make rust-release
	@make node-build
	@make python-build
	@echo "$(GREEN)✓ Release build complete!$(NC)"

dist: release
	@echo "$(BLUE)Creating distribution packages...$(NC)"
	@mkdir -p dist
	@cp target/release/$(HANZO_BIN) dist/
	@tar -czf dist/hanzo-dev-$(VERSION)-$(shell uname -s)-$(shell uname -m).tar.gz -C dist $(HANZO_BIN)
	@echo "$(GREEN)✓ Distribution package created: dist/hanzo-dev-$(VERSION)-$(shell uname -s)-$(shell uname -m).tar.gz$(NC)"

publish: dist
	@echo "$(BLUE)Publishing packages...$(NC)"
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm publish; \
	fi
	@if [ -f "pyproject.toml" ]; then \
		uv publish; \
	fi
	@echo "$(GREEN)✓ Packages published!$(NC)"

# ============================================================================
# Installation Commands
# ============================================================================

install: build
	@echo "$(BLUE)Installing Hanzo Dev...$(NC)"
	@sudo cp target/release/$(HANZO_BIN) /usr/local/bin/ 2>/dev/null || \
		cp target/release/$(HANZO_BIN) ~/.local/bin/
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && npm link; \
	fi
	@if [ -f "pyproject.toml" ]; then \
		uv pip install -e .; \
	fi
	@echo "$(GREEN)✓ Hanzo Dev installed!$(NC)"

uninstall:
	@echo "$(YELLOW)Uninstalling Hanzo Dev...$(NC)"
	@sudo rm -f /usr/local/bin/$(HANZO_BIN)
	@rm -f ~/.local/bin/$(HANZO_BIN)
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && npm unlink; \
	fi
	@if [ -f "pyproject.toml" ]; then \
		uv pip uninstall hanzo-dev; \
	fi
	@echo "$(GREEN)✓ Hanzo Dev uninstalled$(NC)"

# ============================================================================
# Utility Commands
# ============================================================================

clean: rust-clean node-clean python-clean docker-clean
	@echo "$(YELLOW)Cleaning build artifacts...$(NC)"
	@rm -rf dist target build
	@rm -f .*.pid
	@echo "$(GREEN)✓ Clean complete!$(NC)"

deps:
	@echo "$(BLUE)Updating dependencies...$(NC)"
	@cd $(RUST_DIR) && cargo update
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm update; \
	fi
	@uv pip install --upgrade pip
	@echo "$(GREEN)✓ Dependencies updated$(NC)"

info:
	@echo "$(CYAN)Hanzo Dev Information$(NC)"
	@echo "====================="
	@echo "Version: $(VERSION)"
	@echo "Commit: $(COMMIT)"
	@echo "Date: $(DATE)"
	@echo ""
	@echo "Environment:"
	@echo "  Rust: $$(rustc --version)"
	@echo "  Node: $$(node --version)"
	@echo "  Python: $$($(PYTHON) --version)"
	@echo "  UV: $$(uv --version)"
	@echo ""
	@echo "Paths:"
	@echo "  Root: $(ROOT_DIR)"
	@echo "  Rust: $(RUST_DIR)"
	@echo "  TypeScript: $(TS_DIR)"
	@echo "  Engine: $(ENGINE_DIR)"
	@echo "  Node: $(NODE_DIR)"

watch:
	@echo "$(BLUE)Watching for changes...$(NC)"
	@fswatch -o . | xargs -n1 -I{} make build

benchmark:
	@echo "$(BLUE)Running benchmarks...$(NC)"
	@cd $(RUST_DIR) && cargo bench
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm bench; \
	fi

docs:
	@echo "$(BLUE)Building documentation...$(NC)"
	@cd $(RUST_DIR) && cargo doc --open
	@if [ -f "$(TS_DIR)/package.json" ]; then \
		cd $(TS_DIR) && pnpm docs; \
	fi

update-llm:
	@echo "$(BLUE)Updating LLM.md...$(NC)"
	@./scripts/update_llm.sh

# ============================================================================
# Git Commands
# ============================================================================

commit:
	@echo "$(BLUE)Committing changes...$(NC)"
	@git add -A
	@git commit -m "feat: $(shell read -p 'Commit message: ' msg; echo $$msg)"

push:
	@echo "$(BLUE)Pushing to remote...$(NC)"
	@git push origin main

pull:
	@echo "$(BLUE)Pulling from remote...$(NC)"
	@git pull origin main

sync: pull push
	@echo "$(GREEN)✓ Synced with remote$(NC)"

# ============================================================================
# Special Targets
# ============================================================================

.SILENT: help info status
.IGNORE: clean stop

# Keep intermediate files
.SECONDARY:

# Export all variables
.EXPORT_ALL_VARIABLES: