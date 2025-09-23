#!/bin/bash

# Hanzo LLM Stack Deployment Script
# Automatically detects environment and deploys optimal LLM serving infrastructure

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Platform detection
PLATFORM=$(uname -s)
ARCH=$(uname -m)

echo -e "${BLUE}============================================${NC}"
echo -e "${BLUE}     Hanzo LLM Stack Deployment${NC}"
echo -e "${BLUE}============================================${NC}"
echo ""

# Detect capabilities
echo -e "${YELLOW}🔍 Detecting system capabilities...${NC}"
echo "Platform: $PLATFORM"
echo "Architecture: $ARCH"

# Check for GPU availability
GPU_TYPE="none"
if [[ "$PLATFORM" == "Darwin" ]]; then
    # macOS - check for Metal
    if system_profiler SPDisplaysDataType | grep -q "Metal"; then
        GPU_TYPE="metal"
        echo -e "${GREEN}✓ Metal GPU detected (Apple Silicon)${NC}"
    fi
elif command -v nvidia-smi &> /dev/null; then
    # NVIDIA GPU
    GPU_TYPE="cuda"
    GPU_NAME=$(nvidia-smi --query-gpu=name --format=csv,noheader | head -n1)
    echo -e "${GREEN}✓ NVIDIA GPU detected: $GPU_NAME${NC}"
elif command -v rocm-smi &> /dev/null; then
    # AMD GPU
    GPU_TYPE="rocm"
    echo -e "${GREEN}✓ AMD GPU detected (ROCm)${NC}"
else
    echo -e "${YELLOW}⚠ No GPU detected, will use CPU${NC}"
fi

# Check Docker/Container runtime
CONTAINER_RUNTIME=""
if command -v docker &> /dev/null; then
    CONTAINER_RUNTIME="docker"
    echo -e "${GREEN}✓ Docker detected${NC}"
elif command -v podman &> /dev/null; then
    CONTAINER_RUNTIME="podman"
    echo -e "${GREEN}✓ Podman detected${NC}"
else
    echo -e "${YELLOW}⚠ No container runtime detected${NC}"
fi

# Check for Ollama
if command -v ollama &> /dev/null; then
    echo -e "${GREEN}✓ Ollama installed${NC}"
    OLLAMA_INSTALLED=true
else
    echo -e "${YELLOW}⚠ Ollama not installed${NC}"
    OLLAMA_INSTALLED=false
fi

echo ""
echo -e "${BLUE}📋 Deployment Options:${NC}"
echo "1) Full Stack (Supabase + LLM servers)"
echo "2) LLM Servers Only"
echo "3) Native MLX Server (Apple Silicon only)"
echo "4) vLLM Server (NVIDIA GPU required)"
echo "5) Ollama Server (CPU/GPU)"
echo "6) Custom Configuration"

read -p "Select option [1-6]: " option

case $option in
    1)
        echo -e "\n${BLUE}🚀 Deploying Full Stack...${NC}"
        
        # Check if docker-compose.full.yml exists
        if [ ! -f "docker-compose.full.yml" ]; then
            echo -e "${RED}Error: docker-compose.full.yml not found${NC}"
            exit 1
        fi
        
        # Start Supabase
        echo -e "${YELLOW}Starting Supabase stack...${NC}"
        docker compose -f docker-compose.full.yml up -d \
            postgres \
            supabase-auth \
            supabase-storage \
            supabase-realtime \
            kong \
            postgrest \
            redis
        
        # Wait for services
        echo -e "${YELLOW}Waiting for services to be ready...${NC}"
        sleep 10
        
        # Start LLM servers based on capability
        if [[ "$GPU_TYPE" == "cuda" ]]; then
            echo -e "${YELLOW}Starting vLLM for NVIDIA GPU...${NC}"
            docker compose -f docker-compose.full.yml --profile gpu up -d vllm
            LLM_URL="http://localhost:8100"
        elif [[ "$GPU_TYPE" == "metal" ]]; then
            echo -e "${YELLOW}Starting MLX server for Apple Silicon...${NC}"
            # MLX runs best natively, not in container
            if command -v mlx_lm.server &> /dev/null; then
                nohup mlx_lm.server \
                    --model mlx-community/Qwen3-4B-Instruct-2507-4bit \
                    --port 8300 > mlx.log 2>&1 &
                echo $! > mlx.pid
                LLM_URL="http://localhost:8300"
            else
                echo -e "${YELLOW}MLX not installed, falling back to Ollama${NC}"
                docker compose -f docker-compose.full.yml up -d ollama
                LLM_URL="http://localhost:11434"
            fi
        else
            echo -e "${YELLOW}Starting Ollama for CPU...${NC}"
            docker compose -f docker-compose.full.yml up -d ollama
            LLM_URL="http://localhost:11434"
        fi
        
        # Start hanzod
        echo -e "${YELLOW}Starting hanzod...${NC}"
        if [ -f "./src/rs/target/release/hanzod" ]; then
            ./src/rs/target/release/hanzod --config hanzo_config.toml --port 3690 &
            echo $! > hanzod.pid
        else
            echo -e "${YELLOW}hanzod binary not found, building...${NC}"
            make build-hanzod
            ./src/rs/target/release/hanzod --config hanzo_config.toml --port 3690 &
            echo $! > hanzod.pid
        fi
        
        echo -e "\n${GREEN}✅ Full stack deployed!${NC}"
        echo -e "${BLUE}Services:${NC}"
        echo "  • Supabase: http://localhost:8000"
        echo "  • PostgreSQL: localhost:5432"
        echo "  • LLM Server: $LLM_URL"
        echo "  • Hanzod: http://localhost:3690"
        ;;
        
    2)
        echo -e "\n${BLUE}🤖 Deploying LLM Servers Only...${NC}"
        
        if [[ "$GPU_TYPE" == "cuda" ]]; then
            echo -e "${YELLOW}Starting vLLM...${NC}"
            docker run -d \
                --gpus all \
                --name hanzo-vllm \
                -p 8100:8000 \
                -v ~/.cache/huggingface:/root/.cache/huggingface \
                vllm/vllm-openai:latest \
                --model facebook/opt-125m \
                --trust-remote-code
            echo -e "${GREEN}✅ vLLM running at http://localhost:8100${NC}"
        else
            echo -e "${YELLOW}Starting Ollama...${NC}"
            docker run -d \
                --name hanzo-ollama \
                -p 11434:11434 \
                -v ollama:/root/.ollama \
                ollama/ollama:latest
            echo -e "${GREEN}✅ Ollama running at http://localhost:11434${NC}"
        fi
        ;;
        
    3)
        if [[ "$PLATFORM" != "Darwin" ]] || [[ "$ARCH" != "arm64" ]]; then
            echo -e "${RED}Error: MLX only works on Apple Silicon Macs${NC}"
            exit 1
        fi
        
        echo -e "\n${BLUE}🍎 Starting MLX Server...${NC}"
        
        # Install MLX if needed
        if ! pip show mlx-lm &> /dev/null; then
            echo -e "${YELLOW}Installing MLX...${NC}"
            pip install mlx mlx-lm
        fi
        
        # Start MLX server
        echo -e "${YELLOW}Starting MLX server with Qwen3-4B...${NC}"
        mlx_lm.server \
            --model mlx-community/Qwen3-4B-Instruct-2507-4bit \
            --port 8300 &
        
        echo -e "${GREEN}✅ MLX server running at http://localhost:8300${NC}"
        ;;
        
    4)
        if [[ "$GPU_TYPE" != "cuda" ]]; then
            echo -e "${RED}Error: vLLM requires NVIDIA GPU${NC}"
            exit 1
        fi
        
        echo -e "\n${BLUE}⚡ Starting vLLM Server...${NC}"
        
        # Get model selection
        echo "Select model:"
        echo "1) facebook/opt-125m (small, fast)"
        echo "2) mistralai/Mistral-7B-Instruct-v0.2"
        echo "3) meta-llama/Llama-2-7b-chat-hf"
        echo "4) Custom model"
        read -p "Choice [1-4]: " model_choice
        
        case $model_choice in
            1) MODEL="facebook/opt-125m" ;;
            2) MODEL="mistralai/Mistral-7B-Instruct-v0.2" ;;
            3) MODEL="meta-llama/Llama-2-7b-chat-hf" ;;
            4) read -p "Enter model name: " MODEL ;;
            *) MODEL="facebook/opt-125m" ;;
        esac
        
        echo -e "${YELLOW}Starting vLLM with $MODEL...${NC}"
        
        docker run -d \
            --gpus all \
            --name hanzo-vllm \
            -p 8100:8000 \
            -v ~/.cache/huggingface:/root/.cache/huggingface \
            -e HF_TOKEN=${HF_TOKEN:-} \
            vllm/vllm-openai:latest \
            --model $MODEL \
            --trust-remote-code \
            --gpu-memory-utilization 0.95 \
            --max-model-len 4096
        
        echo -e "${GREEN}✅ vLLM running at http://localhost:8100${NC}"
        echo "OpenAI-compatible endpoint: http://localhost:8100/v1"
        ;;
        
    5)
        echo -e "\n${BLUE}🦙 Starting Ollama Server...${NC}"
        
        if [[ "$OLLAMA_INSTALLED" == "true" ]]; then
            echo -e "${YELLOW}Starting native Ollama...${NC}"
            ollama serve &
            echo $! > ollama.pid
            sleep 3
            
            # Pull a model
            echo -e "${YELLOW}Pulling Qwen2.5 model...${NC}"
            ollama pull qwen2.5:0.5b
        else
            echo -e "${YELLOW}Starting Ollama in Docker...${NC}"
            docker run -d \
                --name hanzo-ollama \
                -p 11434:11434 \
                -v ollama:/root/.ollama \
                ollama/ollama:latest
            
            sleep 5
            docker exec hanzo-ollama ollama pull qwen2.5:0.5b
        fi
        
        echo -e "${GREEN}✅ Ollama running at http://localhost:11434${NC}"
        ;;
        
    6)
        echo -e "\n${BLUE}🔧 Custom Configuration${NC}"
        echo "Edit docker-compose.full.yml and run:"
        echo "  docker compose -f docker-compose.full.yml up"
        exit 0
        ;;
        
    *)
        echo -e "${RED}Invalid option${NC}"
        exit 1
        ;;
esac

# Test the deployment
echo ""
echo -e "${BLUE}🧪 Testing deployment...${NC}"

# Test hanzod if running
if [ -f "hanzod.pid" ] && kill -0 $(cat hanzod.pid) 2>/dev/null; then
    if curl -s http://localhost:3690/health > /dev/null; then
        echo -e "${GREEN}✓ Hanzod is healthy${NC}"
    else
        echo -e "${YELLOW}⚠ Hanzod is running but not responding${NC}"
    fi
fi

# Test LLM server
if [ ! -z "$LLM_URL" ]; then
    if curl -s $LLM_URL > /dev/null; then
        echo -e "${GREEN}✓ LLM server is accessible${NC}"
    else
        echo -e "${YELLOW}⚠ LLM server not responding yet${NC}"
    fi
fi

echo ""
echo -e "${GREEN}============================================${NC}"
echo -e "${GREEN}      Deployment Complete!${NC}"
echo -e "${GREEN}============================================${NC}"
echo ""
echo "To stop services:"
echo "  make down"
echo ""
echo "To view logs:"
echo "  docker compose -f docker-compose.full.yml logs -f"
echo ""
echo "To run inference test:"
echo "  python test_hf_inference.py"