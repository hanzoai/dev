# Hanzo IDE Development Makefile
SHELL := /bin/bash

# ANSI color codes
GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
BLUE := \033[0;34m
RESET := \033[0m

# Default values
PYTHON := python3
POETRY := poetry
DOCKER := docker
DOCKER_COMPOSE := docker compose

# Docker image names
RUNTIME_IMAGE := ghcr.io/hanzoai/ide-runtime:latest
APP_IMAGE := ghcr.io/hanzoai/ide:latest

.PHONY: help
help: ## Show this help message
	@echo "$(BLUE)Hanzo IDE Development Commands$(RESET)"
	@echo ""
	@echo "$(GREEN)Production:$(RESET)"
	@echo "  make up              - Start production environment with Docker"
	@echo "  make down            - Stop production environment"
	@echo "  make logs            - View production logs"
	@echo ""
	@echo "$(GREEN)Development:$(RESET)"
	@echo "  make dev             - Start development environment locally"
	@echo "  make install         - Install all dependencies"
	@echo "  make test            - Run test suite"
	@echo "  make lint            - Run linting"
	@echo "  make format          - Format code"
	@echo ""
	@echo "$(GREEN)Docker:$(RESET)"
	@echo "  make build-docker    - Build Docker images"
	@echo "  make push-docker     - Push Docker images"
	@echo ""
	@echo "$(GREEN)Utilities:$(RESET)"
	@echo "  make clean           - Clean build artifacts"
	@echo "  make check           - Check dependencies"

# ========== Production Commands ==========

.PHONY: up
up: check-docker ## Start production environment with Docker
	@echo "$(GREEN)Starting production environment...$(RESET)"
	@if [ ! -f ../../docker-compose.yml ]; then \
		echo "$(RED)docker-compose.yml not found in root directory$(RESET)"; \
		exit 1; \
	fi
	@cd ../.. && $(DOCKER_COMPOSE) up -d
	@echo "$(GREEN)Production environment started!$(RESET)"
	@echo "Access the application at: http://localhost:8888"

.PHONY: down
down: ## Stop production environment
	@echo "$(YELLOW)Stopping production environment...$(RESET)"
	@cd ../.. && $(DOCKER_COMPOSE) down
	@echo "$(GREEN)Production environment stopped.$(RESET)"

.PHONY: logs
logs: ## View production logs
	@cd ../.. && $(DOCKER_COMPOSE) logs -f

# ========== Development Commands ==========

.PHONY: dev
dev: check install ## Start development environment locally
	@echo "$(GREEN)Starting development environment...$(RESET)"
	@$(POETRY) run hanzo-dev

.PHONY: install
install: check-python check-poetry ## Install all dependencies
	@echo "$(GREEN)Installing dependencies...$(RESET)"
	@$(POETRY) install --with dev
	@echo "$(GREEN)Dependencies installed successfully!$(RESET)"

.PHONY: test
test: ## Run test suite
	@echo "$(GREEN)Running tests...$(RESET)"
	@cd ../../ && PYTHONPATH=./app/dev/ $(POETRY) run pytest app/dev/tests/ -v

.PHONY: test-unit
test-unit: ## Run unit tests only
	@echo "$(GREEN)Running unit tests...$(RESET)"
	@$(POETRY) run pytest tests/unit/ -v

.PHONY: test-bash
test-bash: ## Run bash session tests
	@echo "$(GREEN)Running bash session tests...$(RESET)"
	@$(POETRY) run pytest tests/unit/test_bash_session.py -v

.PHONY: lint
lint: ## Run linting
	@echo "$(YELLOW)Running linter...$(RESET)"
	@$(POETRY) run ruff check .

.PHONY: format
format: ## Format code
	@echo "$(YELLOW)Formatting code...$(RESET)"
	@$(POETRY) run ruff format .
	@echo "$(GREEN)Code formatted successfully!$(RESET)"

.PHONY: typecheck
typecheck: ## Run type checking
	@echo "$(YELLOW)Running type checker...$(RESET)"
	@$(POETRY) run mypy .

# ========== Docker Commands ==========

.PHONY: build-docker
build-docker: ## Build Docker images
	@echo "$(GREEN)Building Docker images...$(RESET)"
	@echo "$(YELLOW)Building runtime image...$(RESET)"
	@$(DOCKER) build -f containers/runtime/Dockerfile -t $(RUNTIME_IMAGE) ../..
	@echo "$(YELLOW)Building app image...$(RESET)"
	@$(DOCKER) build -f containers/app/Dockerfile -t $(APP_IMAGE) ../..
	@echo "$(GREEN)Docker images built successfully!$(RESET)"

.PHONY: push-docker
push-docker: ## Push Docker images to registry
	@echo "$(GREEN)Pushing Docker images...$(RESET)"
	@$(DOCKER) push $(RUNTIME_IMAGE)
	@$(DOCKER) push $(APP_IMAGE)
	@echo "$(GREEN)Docker images pushed successfully!$(RESET)"

# ========== Utility Commands ==========

.PHONY: clean
clean: ## Clean build artifacts
	@echo "$(YELLOW)Cleaning build artifacts...$(RESET)"
	@find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name ".ruff_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name ".mypy_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pyc" -delete
	@rm -rf build/ dist/ *.egg-info
	@echo "$(GREEN)Clean completed!$(RESET)"

.PHONY: check
check: check-system check-python check-poetry check-docker check-tmux ## Check all dependencies

.PHONY: check-system
check-system:
	@echo "$(YELLOW)Checking system...$(RESET)"
	@echo "OS: $$(uname -s)"
	@echo "Architecture: $$(uname -m)"

.PHONY: check-python
check-python:
	@echo "$(YELLOW)Checking Python...$(RESET)"
	@if command -v $(PYTHON) &> /dev/null; then \
		echo "$(GREEN)✓ Python found: $$($(PYTHON) --version)$(RESET)"; \
	else \
		echo "$(RED)✗ Python not found. Please install Python 3.11 or 3.13$(RESET)"; \
		exit 1; \
	fi

.PHONY: check-poetry
check-poetry:
	@echo "$(YELLOW)Checking Poetry...$(RESET)"
	@if command -v $(POETRY) &> /dev/null; then \
		echo "$(GREEN)✓ Poetry found: $$($(POETRY) --version)$(RESET)"; \
	else \
		echo "$(RED)✗ Poetry not found. Install with: curl -sSL https://install.python-poetry.org | python3 -$(RESET)"; \
		exit 1; \
	fi

.PHONY: check-docker
check-docker:
	@echo "$(YELLOW)Checking Docker...$(RESET)"
	@if command -v $(DOCKER) &> /dev/null; then \
		echo "$(GREEN)✓ Docker found: $$($(DOCKER) --version)$(RESET)"; \
	else \
		echo "$(YELLOW)⚠ Docker not found. Docker is required for production mode.$(RESET)"; \
	fi

.PHONY: check-tmux
check-tmux:
	@echo "$(YELLOW)Checking tmux...$(RESET)"
	@if command -v tmux &> /dev/null; then \
		echo "$(GREEN)✓ tmux found: $$(tmux -V)$(RESET)"; \
	else \
		echo "$(YELLOW)⚠ tmux not found. Required for bash session functionality.$(RESET)"; \
	fi

# ========== Setup Commands ==========

.PHONY: setup
setup: install ## Complete setup for development
	@echo "$(GREEN)Setting up development environment...$(RESET)"
	@echo "$(YELLOW)Creating .env file...$(RESET)"
	@if [ ! -f .env ]; then \
		cp ../../config.template.toml config.toml; \
		echo "$(GREEN)✓ Created config.toml from template$(RESET)"; \
		echo "$(YELLOW)Please edit config.toml with your API keys$(RESET)"; \
	else \
		echo "$(BLUE)config.toml already exists$(RESET)"; \
	fi
	@echo "$(GREEN)Setup completed!$(RESET)"

.PHONY: run-frontend
run-frontend: ## Run frontend separately (for development)
	@echo "$(GREEN)Starting frontend...$(RESET)"
	@cd frontend && npm run dev

.PHONY: run-backend
run-backend: ## Run backend separately (for development)
	@echo "$(GREEN)Starting backend...$(RESET)"
	@$(POETRY) run uvicorn ide.server.app:app --reload --port 8888