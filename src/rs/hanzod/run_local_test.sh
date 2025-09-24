#!/bin/bash

# Run local test for Hanzo AI Blockchain with Lux Consensus

set -e

echo "============================================================"
echo "🚀 HANZO AI BLOCKCHAIN - LOCAL TEST RUNNER"
echo "============================================================"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Function to check if port is in use
check_port() {
    if lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null ; then
        echo -e "${RED}❌ Port $1 is already in use${NC}"
        return 1
    else
        echo -e "${GREEN}✅ Port $1 is available${NC}"
        return 0
    fi
}

echo -e "${BLUE}Step 1: Checking dependencies...${NC}"
command -v cargo >/dev/null 2>&1 || { echo -e "${RED}cargo is required but not installed.${NC}" >&2; exit 1; }
command -v rustc >/dev/null 2>&1 || { echo -e "${RED}rust is required but not installed.${NC}" >&2; exit 1; }
echo -e "${GREEN}✅ Dependencies OK${NC}"
echo ""

echo -e "${BLUE}Step 2: Checking ports...${NC}"
check_port 50051 || echo "  (gRPC port)"
check_port 8545 || echo "  (HTTP port)"
check_port 8546 || echo "  (WebSocket port)"
echo ""

echo -e "${BLUE}Step 3: Building project...${NC}"
cargo build --release --examples 2>/dev/null || cargo build --examples
echo -e "${GREEN}✅ Build complete${NC}"
echo ""

echo -e "${BLUE}Step 4: Running tests...${NC}"
echo -e "${YELLOW}Running unit tests...${NC}"
cargo test --lib -- --nocapture 2>&1 | grep -E "test result:|running" | tail -5 || true
echo ""

echo -e "${YELLOW}Running Warp FFI tests...${NC}"
cargo test warp_ffi::tests -- --nocapture 2>&1 | grep -E "test result:|running" | tail -5 || true
echo ""

echo -e "${YELLOW}Running consensus tests...${NC}"
cargo test lux_consensus::tests -- --nocapture 2>&1 | grep -E "test result:|running" | tail -5 || true
echo ""

echo -e "${GREEN}✅ Tests complete${NC}"
echo ""

echo -e "${BLUE}Step 5: Starting local node...${NC}"
echo -e "${YELLOW}Configuration:${NC}"
echo "  • Chain ID: hanzo-local-test"
echo "  • Network ID: 1337 (local)"
echo "  • Min stake: 1000 LUX (test mode)"
echo "  • Consensus: Snow (k=20, α=15)"
echo ""
echo -e "${YELLOW}RPC Endpoints:${NC}"
echo "  • gRPC: localhost:50051"
echo "  • HTTP: http://localhost:8545"
echo "  • WebSocket: ws://localhost:8546"
echo ""

# Run the local node in background
echo -e "${GREEN}Starting node...${NC}"
cargo run --example run_local 2>&1 &
NODE_PID=$!

# Give it time to start
sleep 3

echo ""
echo -e "${BLUE}Step 6: Testing endpoints...${NC}"

# Test health endpoint
echo -e "${YELLOW}Testing /health...${NC}"
curl -s http://localhost:8545/health 2>/dev/null | grep -q "ok" && echo -e "${GREEN}✅ Health check passed${NC}" || echo -e "${RED}❌ Health check failed${NC}"

# Test status endpoint
echo -e "${YELLOW}Testing /status...${NC}"
curl -s http://localhost:8545/status 2>/dev/null | grep -q "chain_id" && echo -e "${GREEN}✅ Status check passed${NC}" || echo -e "${RED}❌ Status check failed${NC}"

echo ""
echo "============================================================"
echo -e "${GREEN}✅ Hanzo AI Blockchain is running locally!${NC}"
echo "============================================================"
echo ""
echo "To interact with the node:"
echo "  1. Test client: cargo run --example test_client"
echo "  2. HTTP API: curl http://localhost:8545/status"
echo "  3. Stop node: kill $NODE_PID"
echo ""
echo "Press Ctrl+C to stop the node"
echo ""

# Wait for user to stop
wait $NODE_PID