#!/bin/bash
# Script to run Hanzo Node with Qwen3 support on localhost

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
NODE_DIR="${HANZO_NODE_DIR:-$HOME/work/hanzo/node}"
ENGINE_DIR="${HANZO_ENGINE_DIR:-$HOME/work/hanzo/engine}"
API_PORT="${HANZO_API_PORT:-3690}"
NODE_PORT="${HANZO_NODE_PORT:-3691}"
ENGINE_PORT="${HANZO_ENGINE_PORT:-36900}"

echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}║           Hanzo Node with Qwen3 Support Launcher           ║${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

# Check if node directory exists
if [ ! -d "$NODE_DIR" ]; then
    echo -e "${RED}Error: Hanzo Node directory not found at $NODE_DIR${NC}"
    echo -e "${YELLOW}Please set HANZO_NODE_DIR environment variable or ensure the node is at ~/work/hanzo/node${NC}"
    exit 1
fi

# Check if engine directory exists
if [ ! -d "$ENGINE_DIR" ]; then
    echo -e "${YELLOW}Warning: Hanzo Engine directory not found at $ENGINE_DIR${NC}"
    echo -e "${YELLOW}Qwen3 models will not be available locally${NC}"
fi

# Function to check if port is in use
check_port() {
    local port=$1
    if lsof -Pi :$port -sTCP:LISTEN -t >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Check if services are already running
echo -e "${BLUE}Checking service status...${NC}"

if check_port $API_PORT; then
    echo -e "${YELLOW}⚠ API port $API_PORT is already in use${NC}"
    read -p "Stop existing service and continue? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo -e "${YELLOW}Stopping existing service...${NC}"
        lsof -ti:$API_PORT | xargs kill -9 2>/dev/null || true
    else
        echo -e "${RED}Exiting...${NC}"
        exit 1
    fi
fi

if check_port $NODE_PORT; then
    echo -e "${YELLOW}⚠ Node port $NODE_PORT is already in use${NC}"
    read -p "Stop existing service and continue? (y/N) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo -e "${YELLOW}Stopping existing service...${NC}"
        lsof -ti:$NODE_PORT | xargs kill -9 2>/dev/null || true
    else
        echo -e "${RED}Exiting...${NC}"
        exit 1
    fi
fi

# Start Hanzo Engine if available
if [ -d "$ENGINE_DIR" ] && [ ! "$(check_port $ENGINE_PORT)" ]; then
    echo -e "${BLUE}Starting Hanzo Engine with Qwen3 models...${NC}"
    cd "$ENGINE_DIR"
    
    # Check if engine binary exists
    if [ -f "target/release/hanzo-engine" ]; then
        echo -e "${GREEN}Starting engine on port $ENGINE_PORT...${NC}"
        ./target/release/hanzo-engine \
            --port $ENGINE_PORT \
            --model qwen3-8b \
            --enable-embeddings \
            --enable-reranker \
            > /tmp/hanzo-engine.log 2>&1 &
        ENGINE_PID=$!
        echo -e "${GREEN}✓ Engine started (PID: $ENGINE_PID)${NC}"
    elif [ -f "hanzo-engine" ]; then
        ./hanzo-engine \
            --port $ENGINE_PORT \
            --model qwen3-8b \
            --enable-embeddings \
            --enable-reranker \
            > /tmp/hanzo-engine.log 2>&1 &
        ENGINE_PID=$!
        echo -e "${GREEN}✓ Engine started (PID: $ENGINE_PID)${NC}"
    else
        echo -e "${YELLOW}Engine binary not found. Building...${NC}"
        cargo build --release
        if [ -f "target/release/hanzo-engine" ]; then
            ./target/release/hanzo-engine \
                --port $ENGINE_PORT \
                --model qwen3-8b \
                --enable-embeddings \
                --enable-reranker \
                > /tmp/hanzo-engine.log 2>&1 &
            ENGINE_PID=$!
            echo -e "${GREEN}✓ Engine built and started (PID: $ENGINE_PID)${NC}"
        fi
    fi
fi

# Start Hanzo Node
echo -e "${BLUE}Starting Hanzo Node...${NC}"
cd "$NODE_DIR"

# Set environment variables for Qwen3 support
export HANZO_ENGINE_URL="http://localhost:$ENGINE_PORT"
export HANZO_API_PORT=$API_PORT
export HANZO_NODE_PORT=$NODE_PORT
export HANZO_ENABLE_QWEN3=true
export HANZO_ENABLE_EMBEDDINGS=true
export HANZO_ENABLE_RERANKER=true
export HANZO_MODEL_PROVIDERS="hanzo-engine,lm-studio,ollama,openai,anthropic"

# Model configuration
export QWEN3_MODELS="qwen3-8b,qwen3-14b,qwen3-30b-a3b,qwen3-72b"
export QWEN3_EMBEDDING_MODEL="qwen3-embedding-8b"
export QWEN3_RERANKER_MODEL="qwen3-reranker-4b"
export QWEN3_EMBEDDING_DIMS=4096
export QWEN3_MAX_CONTEXT=32768

# Check if node binary exists
if [ -f "hanzod" ]; then
    NODE_BIN="./hanzod"
elif [ -f "target/release/hanzod" ]; then
    NODE_BIN="./target/release/hanzod"
elif [ -f "target/debug/hanzod" ]; then
    NODE_BIN="./target/debug/hanzod"
else
    echo -e "${YELLOW}Node binary not found. Building...${NC}"
    if [ -f "Cargo.toml" ]; then
        cargo build --release
        NODE_BIN="./target/release/hanzod"
    elif [ -f "package.json" ]; then
        npm install
        npm run build
        NODE_BIN="node dist/index.js"
    else
        echo -e "${RED}Error: Cannot determine how to build the node${NC}"
        exit 1
    fi
fi

# Start the node
echo -e "${GREEN}Starting Hanzo Node...${NC}"
echo -e "${BLUE}Configuration:${NC}"
echo -e "  API Port: ${GREEN}$API_PORT${NC}"
echo -e "  Node Port: ${GREEN}$NODE_PORT${NC}"
echo -e "  Engine Port: ${GREEN}$ENGINE_PORT${NC}"
echo -e "  Qwen3 Models: ${GREEN}$QWEN3_MODELS${NC}"
echo -e "  Embedding Model: ${GREEN}$QWEN3_EMBEDDING_MODEL${NC}"
echo -e "  Reranker Model: ${GREEN}$QWEN3_RERANKER_MODEL${NC}"
echo ""

# Run the node
$NODE_BIN \
    --api-port $API_PORT \
    --node-port $NODE_PORT \
    --enable-swagger \
    --enable-metrics \
    --log-level info

# Cleanup on exit
trap "echo -e '${YELLOW}Shutting down...${NC}'; kill $ENGINE_PID 2>/dev/null || true" EXIT