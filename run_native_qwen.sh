#!/bin/bash
# Run Qwen models natively through Hanzo Engine

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

ENGINE_DIR="${HANZO_ENGINE_DIR:-$HOME/work/hanzo/engine}"
MODEL_DIR="${HANZO_MODEL_DIR:-$HOME/.cache/huggingface/hub}"
ENGINE_PORT="${HANZO_ENGINE_PORT:-36900}"

echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}         Hanzo Engine - Native Qwen Model Support              ${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo ""

# Check if engine directory exists
if [ ! -d "$ENGINE_DIR" ]; then
    echo -e "${RED}Error: Hanzo Engine directory not found at $ENGINE_DIR${NC}"
    exit 1
fi

cd "$ENGINE_DIR"

# Check if engine is built
if [ ! -f "target/release/hanzo-engine" ]; then
    echo -e "${YELLOW}Building Hanzo Engine with Metal support...${NC}"
    cargo build --release --no-default-features --features metal
fi

# Create model configuration for Qwen
cat > qwen_config.toml << 'EOF'
[model]
# Qwen model configuration
model_id = "Qwen/Qwen2.5-7B-Instruct"
model_type = "qwen2"
use_flash_attn = false
load_format = "safetensors"
dtype = "auto"
device = "metal"  # Use Metal for Mac

[server]
host = "0.0.0.0"
port = 36900
max_batch_size = 32
max_sequence_length = 32768

[cache]
cache_dir = "~/.cache/huggingface/hub"
use_cache = true

[download]
# Hugging Face configuration
use_auth_token = false
revision = "main"
force_download = false

[quantization]
# Use Q4_K_M quantization for better performance
method = "q4_k_m"
bits = 4
EOF

echo -e "${BLUE}Configuration created for Qwen models${NC}"

# Function to download model
download_model() {
    local model_id=$1
    echo -e "${YELLOW}Downloading model: $model_id${NC}"
    
    python3 << EOF
import os
from huggingface_hub import snapshot_download

model_id = "$model_id"
cache_dir = os.path.expanduser("~/.cache/huggingface/hub")

print(f"Downloading {model_id} to {cache_dir}...")
local_dir = snapshot_download(
    repo_id=model_id,
    cache_dir=cache_dir,
    ignore_patterns=["*.bin", "*.pth"],  # Only download safetensors
    resume_download=True
)
print(f"Model downloaded to: {local_dir}")
EOF
}

# Check if model exists, download if not
MODEL_ID="Qwen/Qwen2.5-7B-Instruct"
MODEL_PATH="$HOME/.cache/huggingface/hub/models--Qwen--Qwen2.5-7B-Instruct"

if [ ! -d "$MODEL_PATH" ]; then
    echo -e "${YELLOW}Model not found locally. Downloading...${NC}"
    download_model "$MODEL_ID"
else
    echo -e "${GREEN}✓ Model found at $MODEL_PATH${NC}"
fi

# Start the engine with Qwen model
echo -e "${BLUE}Starting Hanzo Engine with Qwen 2.5 7B...${NC}"
echo -e "${CYAN}Configuration:${NC}"
echo "  Model: Qwen2.5-7B-Instruct"
echo "  Port: $ENGINE_PORT"
echo "  Device: Metal (Mac GPU)"
echo "  Quantization: Q4_K_M (4-bit)"
echo ""

# Run the engine
if [ -f "target/release/hanzo-engine" ]; then
    ./target/release/hanzo-engine \
        --model-id "Qwen/Qwen2.5-7B-Instruct" \
        --model-type qwen2 \
        --port $ENGINE_PORT \
        --device metal \
        --dtype auto \
        --max-tokens 32768 \
        --temperature 0.7 \
        --top-p 0.95 \
        --quantization q4_k_m
else
    echo -e "${RED}Engine binary not found!${NC}"
    echo -e "${YELLOW}Trying mistralrs binary...${NC}"
    
    # Try mistralrs CLI if available
    if [ -f "target/release/mistralrs" ]; then
        ./target/release/mistralrs \
            --model-id "Qwen/Qwen2.5-7B-Instruct" \
            --arch qwen2 \
            --port $ENGINE_PORT \
            --device metal \
            --from-huggingface
    else
        echo -e "${RED}No engine binary found. Please build first.${NC}"
        exit 1
    fi
fi