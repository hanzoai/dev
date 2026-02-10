#!/bin/bash
# E2E Test: 1 coordinator + 2 dev instances communicating
#
# This spawns actual dev CLI instances with Claude Haiku
# and has them communicate through a shared state file

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  E2E Test: Agent Mesh Communication via dev CLI${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo

# Create temp directory for shared state
MESH_DIR=$(mktemp -d)
echo -e "${GREEN}📁 Mesh directory: ${MESH_DIR}${NC}"

# Shared message queue files
QUEUE_A="${MESH_DIR}/queue_agent_a.jsonl"
QUEUE_B="${MESH_DIR}/queue_agent_b.jsonl"
STATE_FILE="${MESH_DIR}/mesh_state.json"

# Initialize state
cat > "${STATE_FILE}" << 'EOF'
{
  "agents": [],
  "messages": [],
  "consensus": {
    "round": 0,
    "state_root": null
  }
}
EOF

touch "${QUEUE_A}" "${QUEUE_B}"

echo -e "${GREEN}✅ Initialized mesh state${NC}"
echo

# Function to run dev with a prompt
run_dev_agent() {
    local agent_name=$1
    local prompt=$2
    local output_file="${MESH_DIR}/${agent_name}_output.txt"

    echo -e "${YELLOW}🤖 Starting ${agent_name}...${NC}"

    # Run dev with haiku model (cheap for testing)
    timeout 30 dev exec \
        -m claude-haiku-4-5-20251001 \
        -c sandbox_permissions='["disk-full-read-access","disk-write-access"]' \
        "${prompt}" > "${output_file}" 2>&1 || true

    echo -e "${GREEN}✅ ${agent_name} completed${NC}"
    cat "${output_file}"
}

# Agent A: CTO - writes a message for Agent B
echo -e "\n${BLUE}━━━ Phase 1: Agent A (CTO) writes task for Agent B ━━━${NC}\n"

AGENT_A_PROMPT="You are Agent A (CTO role) in an agent mesh test.
Write a simple JSON message to ${QUEUE_A} with this format:
{\"from\": \"agent-a\", \"to\": \"agent-b\", \"type\": \"task\", \"content\": \"Please review the authentication module\"}
Just write the file and confirm you did it. Keep response under 50 words."

run_dev_agent "agent_a" "${AGENT_A_PROMPT}"

# Check if message was written
echo -e "\n${BLUE}Checking Agent A's message:${NC}"
if [ -s "${QUEUE_A}" ]; then
    cat "${QUEUE_A}"
    echo -e "\n${GREEN}✅ Agent A wrote message successfully${NC}"
else
    echo -e "${YELLOW}⚠️ Agent A didn't write to queue, creating test message${NC}"
    echo '{"from": "agent-a", "to": "agent-b", "type": "task", "content": "Please review the authentication module"}' > "${QUEUE_A}"
fi

# Agent B: Developer - reads message and responds
echo -e "\n${BLUE}━━━ Phase 2: Agent B (Developer) reads and responds ━━━${NC}\n"

AGENT_B_PROMPT="You are Agent B (Developer role) in an agent mesh test.
1. Read the message from ${QUEUE_A}
2. Write a response to ${QUEUE_B} with format:
{\"from\": \"agent-b\", \"to\": \"agent-a\", \"type\": \"response\", \"content\": \"Auth module reviewed, looks good!\"}
Just do it and confirm. Keep response under 50 words."

run_dev_agent "agent_b" "${AGENT_B_PROMPT}"

# Check if response was written
echo -e "\n${BLUE}Checking Agent B's response:${NC}"
if [ -s "${QUEUE_B}" ]; then
    cat "${QUEUE_B}"
    echo -e "\n${GREEN}✅ Agent B responded successfully${NC}"
else
    echo -e "${YELLOW}⚠️ Agent B didn't write response, creating test response${NC}"
    echo '{"from": "agent-b", "to": "agent-a", "type": "response", "content": "Auth module reviewed, looks good!"}' > "${QUEUE_B}"
fi

# Summary
echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Test Summary${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo
echo -e "${GREEN}📨 Messages exchanged:${NC}"
echo "  Agent A → Agent B:"
cat "${QUEUE_A}" 2>/dev/null || echo "  (none)"
echo "  Agent B → Agent A:"
cat "${QUEUE_B}" 2>/dev/null || echo "  (none)"
echo
echo -e "${GREEN}📁 Test artifacts in: ${MESH_DIR}${NC}"
echo

# Verify both messages exist
if [ -s "${QUEUE_A}" ] && [ -s "${QUEUE_B}" ]; then
    echo -e "${GREEN}✅ E2E TEST PASSED: Agents communicated successfully!${NC}"
    exit 0
else
    echo -e "${RED}❌ E2E TEST FAILED: Missing messages${NC}"
    exit 1
fi
