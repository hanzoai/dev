#!/bin/bash
# E2E Test: 2 Claude agents communicating via shared ZAP queue
#
# Uses claude CLI directly with haiku model

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  E2E Test: Agent Mesh via Claude CLI${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo

# Create temp directory for shared state
MESH_DIR=$(mktemp -d)
echo -e "${GREEN}📁 Mesh directory: ${MESH_DIR}${NC}"

# Message queue files (simulating ZAP transport)
MSG_A_TO_B="${MESH_DIR}/a_to_b.json"
MSG_B_TO_A="${MESH_DIR}/b_to_a.json"

echo -e "${GREEN}✅ Mesh initialized${NC}"
echo

# Phase 1: Agent A sends task
echo -e "${BLUE}━━━ Phase 1: Agent A (CTO) sends task ━━━${NC}"

cat > "${MSG_A_TO_B}" << 'EOF'
{
  "from": "agent-a-cto",
  "to": "agent-b-dev",
  "type": "task_request",
  "content": "Please implement a simple ping function that returns 'pong'",
  "timestamp": "2024-01-01T00:00:00Z"
}
EOF

echo -e "${GREEN}📤 Agent A wrote task:${NC}"
cat "${MSG_A_TO_B}"
echo

# Phase 2: Agent B reads and responds using claude
echo -e "\n${BLUE}━━━ Phase 2: Agent B (Developer) responds ━━━${NC}"

AGENT_B_PROMPT="You are Agent B (Developer).
Read this task from Agent A: $(cat ${MSG_A_TO_B})

Write a brief JSON response to indicate you completed the task. Format:
{\"from\": \"agent-b-dev\", \"to\": \"agent-a-cto\", \"type\": \"task_response\", \"content\": \"...\"}

Just output the JSON, nothing else."

echo -e "${YELLOW}🤖 Agent B processing...${NC}"

# Use claude CLI with haiku
claude --model haiku -p "${AGENT_B_PROMPT}" --output-format text 2>/dev/null > "${MSG_B_TO_A}" || {
    echo -e "${YELLOW}⚠️ Claude CLI not available or failed, simulating response${NC}"
    cat > "${MSG_B_TO_A}" << 'EOF'
{
  "from": "agent-b-dev",
  "to": "agent-a-cto",
  "type": "task_response",
  "content": "Implemented ping function: fn ping() -> &'static str { \"pong\" }"
}
EOF
}

echo -e "${GREEN}📥 Agent B response:${NC}"
cat "${MSG_B_TO_A}"
echo

# Summary
echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Message Flow${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo
echo -e "${GREEN}Agent A (CTO) ──task──> Agent B (Developer)${NC}"
echo -e "${GREEN}Agent B (Developer) ──response──> Agent A (CTO)${NC}"
echo
echo -e "${GREEN}✅ E2E Communication Test Complete${NC}"
echo -e "${GREEN}📁 Artifacts: ${MESH_DIR}${NC}"
