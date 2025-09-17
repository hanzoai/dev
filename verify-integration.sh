#!/usr/bin/env bash
# Comprehensive verification script for Hanzo Dev integration

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║           Hanzo Dev Integration Verification                  ║${NC}"
echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Function to check command exists
check_command() {
    if command -v $1 &> /dev/null; then
        echo -e "${GREEN}✓ $1 found${NC}"
        return 0
    else
        echo -e "${RED}✗ $1 not found${NC}"
        return 1
    fi
}

# Function to check port
check_port() {
    if lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo -e "${GREEN}✓ Port $1 is active${NC}"
        return 0
    else
        echo -e "${YELLOW}○ Port $1 is not active${NC}"
        return 1
    fi
}

# Function to test API endpoint
test_endpoint() {
    local url=$1
    local name=$2
    if curl -s -f -o /dev/null "$url" 2>/dev/null; then
        echo -e "${GREEN}✓ $name is responding${NC}"
        return 0
    else
        echo -e "${YELLOW}○ $name is not responding${NC}"
        return 1
    fi
}

echo -e "${BLUE}1. Checking System Requirements${NC}"
echo "================================="
check_command cargo
check_command python3
check_command ollama
check_command jq
check_command curl
echo ""

echo -e "${BLUE}2. Checking Service Ports${NC}"
echo "========================="
check_port 36900 && NATIVE_ENGINE=true || NATIVE_ENGINE=false
check_port 11434 && OLLAMA_RUNNING=true || OLLAMA_RUNNING=false
check_port 3690 && NODE_API=true || NODE_API=false
check_port 3691 && P2P_PORT=true || P2P_PORT=false
echo ""

echo -e "${BLUE}3. Checking API Endpoints${NC}"
echo "========================="
if [ "$NATIVE_ENGINE" = "true" ]; then
    test_endpoint "http://localhost:36900/health" "Native Engine"
fi
if [ "$OLLAMA_RUNNING" = "true" ]; then
    test_endpoint "http://localhost:11434" "Ollama"
fi
if [ "$NODE_API" = "true" ]; then
    test_endpoint "http://localhost:3690/health" "Node API"
fi
echo ""

echo -e "${BLUE}4. Checking Ollama Models${NC}"
echo "========================="
if [ "$OLLAMA_RUNNING" = "true" ]; then
    models=$(ollama list 2>/dev/null | grep -c "qwen" || echo "0")
    echo -e "Found ${GREEN}$models${NC} Qwen models"
    ollama list 2>/dev/null | grep "qwen" || true
else
    echo -e "${YELLOW}Ollama not running${NC}"
fi
echo ""

echo -e "${BLUE}5. Checking Native Engine Binary${NC}"
echo "================================"
ENGINE_BIN="$HOME/work/hanzo/engine/target/release/mistralrs-server"
if [ -f "$ENGINE_BIN" ]; then
    echo -e "${GREEN}✓ Engine binary exists${NC}"
    echo "  Path: $ENGINE_BIN"
    echo "  Size: $(du -h "$ENGINE_BIN" | cut -f1)"
else
    echo -e "${YELLOW}○ Engine binary not found${NC}"
    echo "  Expected at: $ENGINE_BIN"
    # Check if build is in progress
    if pgrep -f "cargo build.*hanzo/engine" > /dev/null; then
        echo -e "${YELLOW}  Build in progress...${NC}"
    fi
fi
echo ""

echo -e "${BLUE}6. Testing hanzo-dev CLI${NC}"
echo "========================"
if [ -f "./hanzo-dev" ]; then
    echo -e "${GREEN}✓ hanzo-dev CLI found${NC}"
    
    # Test status command
    echo -e "\n${CYAN}Running: ./hanzo-dev status${NC}"
    ./hanzo-dev status || true
    
    # Test chat if any provider is available
    if [ "$NATIVE_ENGINE" = "true" ] || [ "$OLLAMA_RUNNING" = "true" ]; then
        echo -e "\n${CYAN}Running: ./hanzo-dev chat 'Hello, are you working?'${NC}"
        ./hanzo-dev chat "Hello, are you working?" || true
    fi
else
    echo -e "${RED}✗ hanzo-dev CLI not found${NC}"
fi
echo ""

echo -e "${BLUE}7. Summary${NC}"
echo "=========="
if [ "$NATIVE_ENGINE" = "true" ]; then
    echo -e "${GREEN}✓ Native Engine is running${NC}"
    provider="Native Engine"
elif [ "$OLLAMA_RUNNING" = "true" ]; then
    echo -e "${GREEN}✓ Ollama is available as fallback${NC}"
    provider="Ollama"
else
    echo -e "${YELLOW}⚠ No AI provider is currently running${NC}"
    provider="None"
fi

echo -e "\nCurrent AI Provider: ${CYAN}$provider${NC}"

if [ "$provider" = "None" ]; then
    echo -e "\n${YELLOW}To start services:${NC}"
    echo "  1. For Native Engine: ./hanzo-dev start"
    echo "  2. For Ollama: ollama serve"
fi

echo -e "\n${GREEN}Verification complete!${NC}"