# Hanzo Dev Makefile - Platform-aware builds
# Automatically detects platform and uses appropriate acceleration

# Detect OS
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Windows detection (Git Bash, WSL, MSYS2, etc.)
ifeq ($(OS),Windows_NT)
    PLATFORM := Windows
else
    PLATFORM := $(UNAME_S)
endif

# Set platform-specific features
ifeq ($(PLATFORM),Darwin)
    # macOS - use Metal/MLX
    CARGO_FEATURES := metal
    ACCEL_NAME := Metal/MLX
    ENGINE_FEATURES := --no-default-features --features metal
    HANZOD_FEATURES := --no-default-features --features metal
else ifeq ($(PLATFORM),Linux)
    # Linux - check for CUDA, then ROCm, then use CPU
    CUDA_CHECK := $(shell which nvcc 2>/dev/null)
    ROCM_CHECK := $(shell which rocminfo 2>/dev/null)
    ifneq ($(CUDA_CHECK),)
        # CUDA available
        CARGO_FEATURES := cuda flash-attn
        ACCEL_NAME := CUDA
        ENGINE_FEATURES := --features cuda,flash-attn
        HANZOD_FEATURES := --features cuda
    else ifneq ($(ROCM_CHECK),)
        # ROCm available (AMD GPUs)
        CARGO_FEATURES := rocm
        ACCEL_NAME := ROCm/AMD
        ENGINE_FEATURES := --no-default-features --features rocm
        HANZOD_FEATURES := --no-default-features --features rocm
    else
        # No GPU, use CPU with MKL
        CARGO_FEATURES := mkl
        ACCEL_NAME := CPU/MKL
        ENGINE_FEATURES := --no-default-features --features mkl
        HANZOD_FEATURES := --no-default-features
    endif
else ifeq ($(PLATFORM),Windows)
    # Windows - check for various acceleration options
    CUDA_CHECK := $(shell where nvcc 2>NUL)
    DIRECTML_CHECK := $(shell powershell -Command "Get-WmiObject Win32_VideoController | Select-String -Pattern 'Intel|AMD|NVIDIA'" 2>NUL)
    ifneq ($(CUDA_CHECK),)
        # CUDA available on Windows
        CARGO_FEATURES := cuda
        ACCEL_NAME := CUDA/Windows
        ENGINE_FEATURES := --features cuda
        HANZOD_FEATURES := --features cuda
    else ifneq ($(DIRECTML_CHECK),)
        # DirectML available (works with all GPUs on Windows)
        CARGO_FEATURES := directml onnx
        ACCEL_NAME := DirectML/ONNX
        ENGINE_FEATURES := --no-default-features --features directml,onnx
        HANZOD_FEATURES := --no-default-features --features onnx
    else
        # Fallback to ONNX Runtime with CPU
        CARGO_FEATURES := onnx
        ACCEL_NAME := ONNX/CPU
        ENGINE_FEATURES := --no-default-features --features onnx
        HANZOD_FEATURES := --no-default-features --features onnx
    endif
else
    # Unknown platform - use safe defaults
    CARGO_FEATURES :=
    ACCEL_NAME := CPU
    ENGINE_FEATURES := --no-default-features
    HANZOD_FEATURES := --no-default-features
endif

# Paths
HANZO_ENGINE_PATH := ~/work/hanzo/engine
HANZO_DEV_PATH := ~/work/hanzo/dev
HANZOD_RS_PATH := $(HANZO_DEV_PATH)/src/rs

# Build targets
.PHONY: build info clean help diagnose test run

info:
	@echo "========================================="
	@echo "Hanzo Build Configuration"
	@echo "========================================="
	@echo "Platform: $(UNAME_S) $(UNAME_M)"
	@echo "Acceleration: $(ACCEL_NAME)"
	@echo "Features: $(CARGO_FEATURES)"
	@echo "========================================="

# Build Hanzo Engine with platform-specific acceleration
build-engine:
	@echo "\n🚀 Building Hanzo Engine with $(ACCEL_NAME) support..."
	cd $(HANZO_ENGINE_PATH) && \
	cargo build --release $(ENGINE_FEATURES)
	@echo "✅ Hanzo Engine built successfully"
	@echo "📍 Binary: $(HANZO_ENGINE_PATH)/target/release/hanzoai"

# Build hanzod daemon with embedded Hanzo Engine
build-hanzod:
	@echo "\n🔧 Building hanzod with embedded Hanzo Engine ($(ACCEL_NAME))..."
	cd $(HANZOD_RS_PATH) && \
	cargo build --bin hanzod --release --features metal
	@echo "✅ hanzod built successfully with embedded inference engine"
	@echo "📍 Binary: $(HANZOD_RS_PATH)/target/release/hanzod"
	@echo "🚀 Engine: Embedded Hanzo Engine with Metal/MLX support"

# Run Hanzo Engine
run-engine:
	@echo "\n🌟 Starting Hanzo Engine with $(ACCEL_NAME)..."
	@echo "  Using Phi-3.5-mini model for testing..."
	$(HANZO_ENGINE_PATH)/target/release/hanzoai \
		--port 36900 \
		plain \
		--model-id microsoft/Phi-3.5-mini-instruct

# Run hanzod with config
run-hanzod: build-hanzod
	@echo "\n🎯 Starting hanzod on port 3690..."
	cd $(HANZO_DEV_PATH) && \
	$(HANZOD_RS_PATH)/target/release/hanzod \
		--config hanzo_config.toml \
		--port 3690

# Run both services
run-all:
	@echo "\n🚀 Starting all Hanzo services..."
	@$(MAKE) -f Makefile.hanzo run-engine &
	@sleep 5
	@$(MAKE) -f Makefile.hanzo run-hanzod

# Test connectivity
test:
	@echo "\n☁️  Testing Hanzo Cloud integration..."
	cd $(HANZO_DEV_PATH) && bash test_hanzo_cloud.sh

# Docker infrastructure
up-supabase:
	@echo "\n🚀 Starting Supabase stack..."
	docker compose -f docker-compose.full.yml up -d postgres supabase-auth supabase-storage kong postgrest redis
	@echo "✅ Supabase running at http://localhost:8000"

up-llm:
	@echo "\n🤖 Starting LLM servers..."
	docker compose -f docker-compose.full.yml up -d ollama
	@echo "✅ Ollama running at http://localhost:11434"

up-vllm:
	@echo "\n⚡ Starting vLLM (GPU required)..."
	docker compose -f docker-compose.full.yml --profile gpu up -d vllm
	@echo "✅ vLLM running at http://localhost:8100"

up-all: up-supabase up-llm
	@echo "\n🎯 Starting hanzod with full stack..."
	@sleep 5
	@$(MAKE) run-hanzod
	@echo "\n✅ Full stack running:"
	@echo "  • Supabase: http://localhost:8000"
	@echo "  • Ollama: http://localhost:11434"
	@echo "  • Hanzod: http://localhost:3690"

down:
	@echo "\n🛑 Stopping all services..."
	docker compose -f docker-compose.full.yml down
	@pkill -f hanzod || true
	@echo "✅ All services stopped"

# Model management
load-model:
	@echo "\n📦 Loading model into Ollama..."
	docker exec hanzo-ollama ollama pull qwen2.5:0.5b
	@echo "✅ Model loaded"

serve-mlx:
	@echo "\n🍎 Starting MLX server (Apple Silicon)..."
	cd ~/work/hanzo/engine && \
	mlx_lm.server --model mlx-community/Qwen3-4B-Instruct-2507-4bit --port 8300

serve-vllm-local:
	@echo "\n⚡ Starting vLLM locally..."
	python -m vllm.entrypoints.openai.api_server \
		--model facebook/opt-125m \
		--port 8100 \
		--trust-remote-code

# Development setup
setup:
	@echo "\n📦 Setting up Hanzo development environment..."
ifeq ($(UNAME_S),Darwin)
	@echo "Installing MLX for macOS..."
	@pip install -q mlx mlx-lm mlx-metal 2>/dev/null || true
endif
	@echo "✅ Development environment ready"

# Clean build artifacts
clean:
	@echo "\n🧹 Cleaning build artifacts..."
	cd $(HANZO_ENGINE_PATH) && cargo clean
	cd $(HANZOD_RS_PATH) && cargo clean
	@echo "✅ Clean complete"

# Platform diagnostic
diagnose:
	@echo "\n🔍 Platform Diagnostics"
	@echo "========================================="
	@echo "OS: $(UNAME_S)"
	@echo "Arch: $(UNAME_M)"
	@echo ""
ifeq ($(UNAME_S),Darwin)
	@echo "Metal Support:"
	@system_profiler SPDisplaysDataType | grep -i "metal" | head -5 || echo "  Metal info not available"
	@echo ""
	@echo "MLX Python packages:"
	@pip list | grep -i mlx || echo "  No MLX packages installed"
else ifeq ($(UNAME_S),Linux)
	@echo "CUDA Support:"
	@nvcc --version 2>/dev/null || echo "  CUDA not found"
	@echo ""
	@echo "GPU Info:"
	@lspci | grep -i nvidia || echo "  No NVIDIA GPU found"
endif
	@echo "========================================="

# Default target
.DEFAULT_GOAL := build

# Main build target
build: build-engine build-hanzod

# Run everything
run: run-all

# Help
help:
	@echo "Hanzo Dev Makefile - Platform-aware builds"
	@echo ""
	@echo "Usage: make [target]"
	@echo ""
	@echo "Primary targets:"
	@echo "  build        - Build everything (default)"
	@echo "  run          - Run all services"
	@echo "  test         - Test cloud connectivity"
	@echo "  clean        - Clean build artifacts"
	@echo ""
	@echo "Diagnostic targets:"
	@echo "  info         - Show build configuration"
	@echo "  diagnose     - Show platform diagnostics"
	@echo ""
	@echo "Platform Detection:"
	@echo "  - macOS: Metal/MLX acceleration"
	@echo "  - Linux + CUDA: CUDA acceleration"
	@echo "  - Linux + ROCm: AMD GPU acceleration"
	@echo "  - Linux: MKL/CPU acceleration"
	@echo "  - Windows + CUDA: CUDA acceleration"
	@echo "  - Windows + GPU: DirectML/ONNX acceleration"
	@echo "  - Windows: ONNX Runtime CPU"