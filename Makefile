SHELL=/usr/bin/env bash
# Makefile for Dev project using uv

# Variables
BACKEND_HOST ?= "127.0.0.1"
BACKEND_PORT = 3000
BACKEND_HOST_PORT = "$(BACKEND_HOST):$(BACKEND_PORT)"
FRONTEND_PORT = 3001
DEFAULT_WORKSPACE_DIR = "./workspace"
DEFAULT_MODEL = "gpt-4o"
CONFIG_FILE = config.toml
PRE_COMMIT_CONFIG_PATH = "./dev_config/python/.pre-commit-config.yaml"
PYTHON_VERSION = 3.12

# Docker image variables (customize these)
IMAGE_NAME ?= yourusername/hanzo-dev
IMAGE_TAG ?= latest

# ANSI color codes
GREEN=$(shell tput -Txterm setaf 2)
YELLOW=$(shell tput -Txterm setaf 3)
RED=$(shell tput -Txterm setaf 1)
BLUE=$(shell tput -Txterm setaf 6)
RESET=$(shell tput -Txterm sgr0)

### Default target: install, build package, then run tests.
all: install build-package test
	@echo "$(GREEN)All tasks completed.$(RESET)"

### Installation targets
install: check-dependencies install-python-dependencies install-frontend-dependencies install-pre-commit-hooks
	@echo "$(GREEN)Installation complete.$(RESET)"

### Build Python package (CLI) distribution
build-package: install
	@echo "$(YELLOW)Building package (Python CLI)...$(RESET)"
	@uv build
	@echo "$(GREEN)Package built. Distribution files are in 'dist/'$(RESET)"

### Publish package to PyPI (requires twine)
publish: build-package
	@echo "$(YELLOW)Publishing package...$(RESET)"
	@twine upload dist/*
	@echo "$(GREEN)Package published.$(RESET)"

### Build project (frontend)
build: install
	@echo "$(YELLOW)Building project...$(RESET)"
	@$(MAKE) build-frontend
	@echo "$(GREEN)Build complete.$(RESET)"

### Docker targets
docker-build:
	@echo "$(YELLOW)Building Docker image...$(RESET)"
	@docker build -t $(IMAGE_NAME):$(IMAGE_TAG) .
	@echo "$(GREEN)Docker image built: $(IMAGE_NAME):$(IMAGE_TAG)$(RESET)"

docker-push: docker-build
	@echo "$(YELLOW)Pushing Docker image...$(RESET)"
	@docker push $(IMAGE_NAME):$(IMAGE_TAG)
	@echo "$(GREEN)Docker image pushed: $(IMAGE_NAME):$(IMAGE_TAG)$(RESET)"

### System & dependency checks
check-uv:
	@echo "$(YELLOW)Checking uv installation...$(RESET)"
	@if ! command -v uv > /dev/null; then \
		echo "$(YELLOW)uv not found. Installing uv...$(RESET)"; \
		pip install uv || { echo "$(RED)Failed to install uv. Please install it manually.$(RESET)"; exit 1; }; \
		echo "$(BLUE)uv installed successfully.$(RESET)"; \
	else \
		echo "$(BLUE)uv is installed.$(RESET)"; \
	fi

check-dependencies:
	@echo "$(YELLOW)Checking dependencies...$(RESET)"
	@$(MAKE) -s check-system
	@$(MAKE) -s check-uv
	@$(MAKE) -s check-python
	@$(MAKE) -s check-nodejs
	@$(MAKE) -s check-npm
ifeq ($(INSTALL_DOCKER),)
	@$(MAKE) -s check-docker
endif
	@echo "$(GREEN)Dependencies checked successfully.$(RESET)"

check-system:
	@echo "$(YELLOW)Checking system...$(RESET)"
	@if [ "$$(uname)" = "Darwin" ]; then \
		echo "$(BLUE)macOS detected.$(RESET)"; \
	elif [ "$$(uname)" = "Linux" ]; then \
		if [ -f "/etc/manjaro-release" ]; then \
			echo "$(BLUE)Manjaro Linux detected.$(RESET)"; \
		else \
			echo "$(BLUE)Linux detected.$(RESET)"; \
		fi; \
	elif [ "$$(uname -r | grep -i microsoft)" ]; then \
		echo "$(BLUE)Windows Subsystem for Linux detected.$(RESET)"; \
	else \
		echo "$(RED)Unsupported system detected. Please use macOS, Linux, or WSL.$(RESET)"; \
		exit 1; \
	fi

check-python:
	@echo "$(YELLOW)Checking Python $(PYTHON_VERSION) installation...$(RESET)"
	@if ! command -v python$(PYTHON_VERSION) > /dev/null; then \
		echo "$(YELLOW)Python $(PYTHON_VERSION) not found. Installing it using uv...$(RESET)"; \
		uv python install $(PYTHON_VERSION) || { echo "$(RED)Failed to install Python $(PYTHON_VERSION).$(RESET)"; exit 1; }; \
		echo "$(BLUE)Python $(PYTHON_VERSION) installed.$(RESET)"; \
	else \
		echo "$(BLUE)$$(python$(PYTHON_VERSION) --version) is installed.$(RESET)"; \
	fi

check-npm:
	@echo "$(YELLOW)Checking npm installation...$(RESET)"
	@if command -v npm > /dev/null; then \
		echo "$(BLUE)npm $$(npm --version) is installed.$(RESET)"; \
	else \
		echo "$(RED)npm is not installed. Please install Node.js.$(RESET)"; \
		exit 1; \
	fi

check-nodejs:
	@echo "$(YELLOW)Checking Node.js installation...$(RESET)"
	@if command -v node > /dev/null; then \
		NODE_VERSION=$$(node --version | sed -E 's/v//g'); \
		IFS='.' read -r -a ver <<< "$$NODE_VERSION"; \
		if [ "$${ver[0]}" -ge 22 ]; then \
			echo "$(BLUE)Node.js $$NODE_VERSION is installed.$(RESET)"; \
		else \
			echo "$(RED)Node.js 22.x or later is required.$(RESET)"; \
			exit 1; \
		fi; \
	else \
		echo "$(RED)Node.js is not installed. Please install Node.js.$(RESET)"; \
		exit 1; \
	fi

check-docker:
	@echo "$(YELLOW)Checking Docker installation...$(RESET)"
	@if command -v docker > /dev/null; then \
		echo "$(BLUE)$$(docker --version) is installed.$(RESET)"; \
	else \
		echo "$(RED)Docker is not installed. Please install Docker.$(RESET)"; \
		exit 1; \
	fi

### Python dependency installation & virtual environment
install-python-dependencies:
	@echo "$(GREEN)Installing Python dependencies...$(RESET)"
	@if [ -z "$${TZ}" ]; then \
		echo "Defaulting TZ (timezone) to UTC"; \
		export TZ="UTC"; \
	fi; \
	if [ ! -d ".venv" ]; then \
		echo "$(BLUE)No virtual environment found. Creating one using 'uv venv --python=python$(PYTHON_VERSION) .venv'...$(RESET)"; \
		uv venv --python=python$(PYTHON_VERSION) .venv; \
	else \
		VENV_PY_VERSION=`.venv/bin/python --version 2>&1 | awk '{print $$2}'`; \
		echo "$(BLUE)Found virtual environment Python version: $$VENV_PY_VERSION$(RESET)"; \
		if echo "$$VENV_PY_VERSION" | grep -q "^$(PYTHON_VERSION)"; then \
			echo "$(BLUE)Virtual environment matches target version $(PYTHON_VERSION).$(RESET)"; \
		else \
			echo "$(YELLOW)Virtual environment version ($$VENV_PY_VERSION) does not match target $(PYTHON_VERSION). Recreating...$(RESET)"; \
			rm -rf .venv; \
			uv venv --python=python$(PYTHON_VERSION) .venv; \
		fi; \
	fi; \
	echo "$(BLUE)Upgrading pip, setuptools, and wheel...$(RESET)"; \
	source .venv/bin/activate && pip install --upgrade pip setuptools wheel; \
	echo "$(BLUE)Activating virtual environment and installing project dependencies via pip...$(RESET)"; \
	source .venv/bin/activate && pip install .; \
	echo "$(GREEN)Python dependencies installed.$(RESET)"

install-frontend-dependencies:
	@echo "$(YELLOW)Setting up frontend environment...$(RESET)"
	@cd frontend && node ./scripts/detect-node-version.js
	@echo "$(BLUE)Installing frontend dependencies with npm...$(RESET)"
	@cd frontend && npm install
	@echo "$(GREEN)Frontend dependencies installed.$(RESET)"

install-pre-commit-hooks:
	@echo "$(YELLOW)Installing pre-commit hooks...$(RESET)"
	@git config --unset-all core.hooksPath || true; \
	if [ -d ".venv" ]; then \
		echo "$(BLUE)Activating virtual environment...$(RESET)"; \
		source .venv/bin/activate && uv run pre-commit install --config $(PRE_COMMIT_CONFIG_PATH); \
	else \
		echo "$(RED)Virtual environment not found. Please run 'make install-python-dependencies' first.$(RESET)"; \
		exit 1; \
	fi; \
	echo "$(GREEN)Pre-commit hooks installed.$(RESET)"

### Lint and test targets
lint-backend:
	@echo "$(YELLOW)Running backend linters...$(RESET)"
	@if [ -d ".venv" ]; then \
		source .venv/bin/activate && uv run pre-commit run --files dev/**/* agenthub/**/* evaluation/**/* --show-diff-on-failure --config $(PRE_COMMIT_CONFIG_PATH); \
	else \
		echo "$(RED)Virtual environment not found. Please run 'make install-python-dependencies' first.$(RESET)"; \
		exit 1; \
	fi

lint-frontend:
	@echo "$(YELLOW)Running frontend linters...$(RESET)"
	@cd frontend && npm run lint

lint:
	@$(MAKE) lint-frontend
	@$(MAKE) lint-backend

test-frontend:
	@echo "$(YELLOW)Running frontend tests...$(RESET)"
	@cd frontend && npm run test

test:
	@echo "$(YELLOW)Running tests...$(RESET)"
	@$(MAKE) test-frontend
	@echo "$(GREEN)Tests passed.$(RESET)"

build-frontend:
	@echo "$(YELLOW)Building frontend...$(RESET)"
	@cd frontend && npm run build

start-backend:
	@echo "$(YELLOW)Starting backend...$(RESET)"
	@uvicorn dev.server.listen:app --host $(BACKEND_HOST) --port $(BACKEND_PORT) --reload --reload-exclude "./workspace"

start-frontend:
	@echo "$(YELLOW)Starting frontend...$(RESET)"
	@cd frontend && VITE_BACKEND_HOST=$(BACKEND_HOST_PORT) VITE_FRONTEND_PORT=$(FRONTEND_PORT) npm run dev -- --port $(FRONTEND_PORT) --host $(BACKEND_HOST)

_run_setup:
	@if [ "$$(uname)" = "MINGW"* ]; then \
		echo "$(RED)Windows is not supported, use WSL instead.$(RESET)"; \
		exit 1; \
	fi; \
	mkdir -p logs; \
	echo "$(YELLOW)Starting backend server...$(RESET)"; \
	uvicorn dev.server.listen:app --host $(BACKEND_HOST) --port $(BACKEND_PORT) &; \
	echo "$(YELLOW)Waiting for backend to start...$(RESET)"; \
	until nc -z localhost $(BACKEND_PORT); do sleep 0.1; done; \
	echo "$(GREEN)Backend started.$(RESET)"

run:
	@echo "$(YELLOW)Running the application...$(RESET)"
	@$(MAKE) _run_setup
	@$(MAKE) start-frontend
	@echo "$(GREEN)Application started.$(RESET)"

docker-run: WORKSPACE_BASE ?= $(PWD)/workspace
docker-run:
	@if [ -f /.dockerenv ]; then \
		echo "Running inside Docker. Exiting..."; \
		exit 0; \
	else \
		echo "$(YELLOW)Running the app in Docker $(OPTIONS)...$(RESET)"; \
		export WORKSPACE_BASE=${WORKSPACE_BASE}; \
		export SANDBOX_USER_ID=$$(id -u); \
		export DATE=$$(date +%Y%m%d%H%M%S); \
		docker compose up $(OPTIONS); \
	fi

run-wsl:
	@echo "$(YELLOW)Running the app in WSL mode...$(RESET)"
	@$(MAKE) _run_setup
	@cd frontend && npm run dev_wsl -- --port $(FRONTEND_PORT)
	@echo "$(GREEN)Application started in WSL mode.$(RESET)"

setup-config:
	@echo "$(YELLOW)Setting up $(CONFIG_FILE)...$(RESET)"
	@$(MAKE) setup-config-prompts
	@mv $(CONFIG_FILE).tmp $(CONFIG_FILE)
	@echo "$(GREEN)Config setup completed.$(RESET)"

setup-config-prompts:
	@echo "[core]" > $(CONFIG_FILE).tmp
	@read -p "Enter your workspace directory (absolute path) [default: $(DEFAULT_WORKSPACE_DIR)]: " ws_dir; \
	 ws_dir=$${ws_dir:-$(DEFAULT_WORKSPACE_DIR)}; \
	 echo "workspace_base=\"$$ws_dir\"" >> $(CONFIG_FILE).tmp; \
	echo "" >> $(CONFIG_FILE).tmp; \
	echo "[llm]" >> $(CONFIG_FILE).tmp; \
	read -p "Enter your LLM model name [default: $(DEFAULT_MODEL)]: " llm_model; \
	llm_model=$${llm_model:-$(DEFAULT_MODEL)}; \
	echo "model=\"$$llm_model\"" >> $(CONFIG_FILE).tmp; \
	read -p "Enter your LLM api key: " llm_api_key; \
	echo "api_key=\"$$llm_api_key\"" >> $(CONFIG_FILE).tmp; \
	read -p "Enter your LLM base URL (leave blank if not needed): " llm_url; \
	if [ -n "$$llm_url" ]; then echo "base_url=\"$$llm_url\"" >> $(CONFIG_FILE).tmp; fi

docker-dev:
	@if [ -f /.dockerenv ]; then \
		echo "Running inside Docker. Exiting..."; \
		exit 0; \
	else \
		echo "$(YELLOW)Building and running in Docker $(OPTIONS)...$(RESET)"; \
		./containers/dev/dev.sh $(OPTIONS); \
	fi

clean:
	@echo "$(YELLOW)Cleaning caches...$(RESET)"
	@rm -rf dev/.cache
	@echo "$(GREEN)Caches cleaned.$(RESET)"

help:
	@echo "$(BLUE)Usage: make [target]$(RESET)"
	@echo "Targets:"
	@echo "  $(GREEN)all$(RESET)                 - Install dependencies, build package, and run tests."
	@echo "  $(GREEN)install$(RESET)             - Install dependencies."
	@echo "  $(GREEN)build-package$(RESET)       - Build Python package (CLI) distribution."
	@echo "  $(GREEN)publish$(RESET)             - Publish package to PyPI."
	@echo "  $(GREEN)build$(RESET)               - Build project (frontend)."
	@echo "  $(GREEN)test$(RESET)                - Run tests."
	@echo "  $(GREEN)docker-build$(RESET)        - Build Docker image."
	@echo "  $(GREEN)docker-push$(RESET)         - Push Docker image."
	@echo "  $(GREEN)docker-run$(RESET)          - Run application in Docker."
	@echo "  $(GREEN)run$(RESET)                 - Run full application."
	@echo "  $(GREEN)docker-dev$(RESET)          - Run development in Docker."
	@echo "  $(GREEN)setup-config$(RESET)        - Setup configuration."
	@echo "  $(GREEN)clean$(RESET)               - Clean caches."
	@echo "  $(GREEN)help$(RESET)                - Show this help message."

.PHONY: all install build-package publish build test docker-build docker-push docker-run run docker-dev run-wsl setup-config setup-config-prompts clean help check-dependencies check-uv check-python check-npm check-nodejs check-docker
