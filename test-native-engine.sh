#!/usr/bin/env bash
# Test script for native engine integration

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

ENGINE_DIR="${HANZO_ENGINE_DIR:-$HOME/work/hanzo/engine}"
ENGINE_BIN="$ENGINE_DIR/target/release/mistralrs-server"

echo -e "${BLUE}Testing Native Hanzo Engine Integration${NC}"
echo "========================================="

# Check if engine binary exists
if [ ! -f "$ENGINE_BIN" ]; then
    echo -e "${YELLOW}Engine binary not found at: $ENGINE_BIN${NC}"
    echo -e "${YELLOW}Please wait for the build to complete or run:${NC}"
    echo "cd $ENGINE_DIR && cargo build --release --no-default-features --features metal"
    exit 1
fi

echo -e "${GREEN}✓ Engine binary found${NC}"

# Try to start the engine
echo -e "${BLUE}Starting native engine...${NC}"
./hanzo-dev start

# Check status
echo -e "\n${BLUE}Checking service status...${NC}"
./hanzo-dev status

# Run test
echo -e "\n${BLUE}Running test...${NC}"
./hanzo-dev test

# Try a chat
echo -e "\n${BLUE}Testing chat functionality...${NC}"
./hanzo-dev chat "What is the capital of France?"

echo -e "\n${GREEN}✓ All tests completed${NC}"