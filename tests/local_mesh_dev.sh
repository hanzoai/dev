#!/bin/bash
# Local Dev Mesh: hanzod + ZAP agents with PQ crypto
#
# Uses a dev mnemonic for local testing:
# "light light light light light light light light light light light energy"
#
# All agents use same keys for local dev (no network, just mesh communication)

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# Dev mnemonic (12 words, all same for simplicity)
DEV_MNEMONIC="light light light light light light light light light light light energy"

# Paths
HANZO_NODE="$HOME/work/hanzo/node/target/release/hanzo-node"
MESH_DIR=$(mktemp -d)
LOG_DIR="${MESH_DIR}/logs"
mkdir -p "${LOG_DIR}"

echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Hanzo Local Dev Mesh${NC}"
echo -e "${BLUE}  ZAP Protocol v0.3.0 | PQ Crypto (ML-KEM, ML-DSA)${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo
echo -e "${GREEN}📁 Mesh directory: ${MESH_DIR}${NC}"
echo -e "${GREEN}🔐 Dev mnemonic: ${DEV_MNEMONIC}${NC}"
echo

# Create dev config
cat > "${MESH_DIR}/hanzo.toml" << EOF
[node]
ip = "127.0.0.1"
port = 3691
api_ip = "127.0.0.1"
api_port = 3690
global_identity_name = "@@localhost.hanzo-dev"
starting_num_qr_profiles = 3
starting_num_qr_devices = 3
first_device_needs_registration_code = false

[network]
ping_interval_secs = 5

[logging]
rust_log = "info,hanzo=debug"
log_simple = true
log_all = false

[embeddings]
use_native_embeddings = false
use_gpu = false
default_embedding_model = "qwen3-embedding-8b"

[engine]
use_local_engine = true

[database]
path = "${MESH_DIR}/db"
max_connections = 10
connection_timeout = 30

[security]
pqc_enabled = true
privacy_tier = 2

[llm_providers]
ollama_base_url = "http://localhost:11434"
lm_studio_base_url = "http://localhost:1234"

[wallets]
coinbase_mpc_enabled = false
ethereum_enabled = false

[tools]
mcp_enabled = true
javascript_runtime = "node"
python_runtime = "python3"

[performance]
job_queue_workers = 4
max_concurrent_jobs = 10
request_timeout = 30
stream_timeout = 120

[development]
swagger_ui_enabled = true
debug_mode = true
mock_providers_enabled = true
EOF

echo -e "${GREEN}✅ Created dev config: ${MESH_DIR}/hanzo.toml${NC}"

# Create agent configs
for i in 1 2 3; do
    case $i in
        1) ROLE="cto"; NAME="CTO-Agent"; PORT=$((3690 + i * 10)) ;;
        2) ROLE="developer"; NAME="Dev-Agent"; PORT=$((3690 + i * 10)) ;;
        3) ROLE="reviewer"; NAME="Reviewer-Agent"; PORT=$((3690 + i * 10)) ;;
    esac

    cat > "${MESH_DIR}/agent_${i}.json" << EOF
{
  "id": "agent-${i}",
  "name": "${NAME}",
  "role": "${ROLE}",
  "endpoint": "zap://127.0.0.1:${PORT}",
  "mnemonic_index": ${i},
  "pqc": {
    "kem": "ML-KEM-1024",
    "sig": "ML-DSA-87",
    "hybrid_mode": true
  },
  "tools": ["fs", "shell", "code", "net"],
  "model": "haiku"
}
EOF
    echo -e "${CYAN}📋 Agent ${i}: ${NAME} (${ROLE}) on port ${PORT}${NC}"
done

echo

# Check if hanzo-node exists
if [[ ! -f "${HANZO_NODE}" ]]; then
    echo -e "${YELLOW}⚠️  hanzo-node binary not found at ${HANZO_NODE}${NC}"
    echo -e "${YELLOW}   Building hanzo-node...${NC}"
    (cd ~/work/hanzo/node && cargo build -p hanzo-node --release) || {
        echo -e "${RED}❌ Failed to build hanzo-node${NC}"
        exit 1
    }
fi

echo -e "${GREEN}✅ hanzo-node binary: ${HANZO_NODE}${NC}"
echo

# Function to run ZAP agent simulation
run_zap_agent() {
    local agent_id=$1
    local agent_config="${MESH_DIR}/agent_${agent_id}.json"
    local agent_log="${LOG_DIR}/agent_${agent_id}.log"

    echo -e "${MAGENTA}🤖 Starting Agent ${agent_id}...${NC}"

    # For now, simulate agent with a simple message exchange
    # In production, this would be: hanzo-agent --config ${agent_config}

    cat >> "${agent_log}" << EOF
[$(date -Iseconds)] Agent ${agent_id} started
[$(date -Iseconds)] Connected to mesh at zap://127.0.0.1:3691
[$(date -Iseconds)] PQ keys generated (ML-KEM-1024, ML-DSA-87)
[$(date -Iseconds)] Registered with coordinator
EOF

    echo -e "${GREEN}✅ Agent ${agent_id} registered${NC}"
}

# Start mesh simulation
echo -e "${BLUE}━━━ Starting Local Mesh ━━━${NC}"
echo

# Export env vars for hanzo-node
export HANZO_CONFIG_PATH="${MESH_DIR}/hanzo.toml"
export HANZO_DEV_MNEMONIC="${DEV_MNEMONIC}"
export PQC_ENABLED=true
export RUST_LOG=info

echo -e "${GREEN}🌐 Mesh Coordinator (hanzod) initializing...${NC}"

# Create mesh state
cat > "${MESH_DIR}/mesh_state.json" << EOF
{
  "version": "0.3.0",
  "profile": "zap-1",
  "namespace": "ai.hanzo.zap",
  "coordinator": {
    "id": "hanzod-local",
    "endpoint": "zap://127.0.0.1:3691",
    "pqc": {
      "kem": "ML-KEM-1024",
      "sig": "ML-DSA-87",
      "hybrid": true
    }
  },
  "agents": [],
  "topology": {
    "peers": [],
    "version": 0
  }
}
EOF

echo -e "${GREEN}✅ Mesh coordinator ready${NC}"
echo

# Start agents
echo -e "${BLUE}━━━ Registering ZAP Agents ━━━${NC}"
echo

for i in 1 2 3; do
    run_zap_agent $i
    sleep 0.5
done

echo

# Simulate message exchange
echo -e "${BLUE}━━━ ZAP Message Exchange ━━━${NC}"
echo

# Create message queue
QUEUE="${MESH_DIR}/zap_queue.jsonl"

# CTO sends task to Developer
cat >> "${QUEUE}" << EOF
{"id":"msg-001","from":"agent-1","to":"agent-2","content":{"TaskRequest":{"description":"Implement PQ handshake","priority":1,"scope":"Repo","effect":"Nondeterministic","witness":"Full"}},"meta":{"timestamp":$(date +%s),"trace_id":"$(uuidgen)","span_id":"$(uuidgen)"}}
EOF
echo -e "${CYAN}📤 CTO -> Dev: TaskRequest (Implement PQ handshake)${NC}"

sleep 0.3

# Developer responds
cat >> "${QUEUE}" << EOF
{"id":"msg-002","from":"agent-2","to":"agent-1","content":{"TaskResponse":{"task_id":"msg-001","result":"PQ handshake implemented with ML-KEM-1024","status":"Completed"}},"meta":{"timestamp":$(date +%s),"trace_id":"$(uuidgen)","span_id":"$(uuidgen)"}}
EOF
echo -e "${CYAN}📥 Dev -> CTO: TaskResponse (Completed)${NC}"

sleep 0.3

# CTO sends to Reviewer
cat >> "${QUEUE}" << EOF
{"id":"msg-003","from":"agent-1","to":"agent-3","content":{"TaskRequest":{"description":"Review PQ handshake implementation","priority":1,"scope":"File","effect":"Pure","witness":"Minimal"}},"meta":{"timestamp":$(date +%s),"trace_id":"$(uuidgen)","span_id":"$(uuidgen)"}}
EOF
echo -e "${CYAN}📤 CTO -> Reviewer: TaskRequest (Review PQ handshake)${NC}"

sleep 0.3

# Reviewer responds
cat >> "${QUEUE}" << EOF
{"id":"msg-004","from":"agent-3","to":"agent-1","content":{"TaskResponse":{"task_id":"msg-003","result":"LGTM - PQ implementation follows FIPS 203/204 specs","status":"Completed"}},"meta":{"timestamp":$(date +%s),"trace_id":"$(uuidgen)","span_id":"$(uuidgen)"}}
EOF
echo -e "${CYAN}📥 Reviewer -> CTO: TaskResponse (LGTM)${NC}"

echo

# Tool invocation example
echo -e "${BLUE}━━━ ZAP Tool Invocation ━━━${NC}"
echo

cat >> "${QUEUE}" << EOF
{"id":"msg-005","from":"agent-2","to":"agent-1","content":{"ToolCall":{"tool_id":{"namespace":"native","name":"fs.read","version":"0.3.0"},"args":{"path":"src/pqc/kem.rs","limit":50},"cost_model":"Free"}},"meta":{"timestamp":$(date +%s),"trace_id":"$(uuidgen)","span_id":"$(uuidgen)"}}
EOF
echo -e "${CYAN}🔧 Dev invoking: native.fs.read v0.3.0${NC}"

cat >> "${QUEUE}" << EOF
{"id":"msg-006","from":"agent-1","to":"agent-2","content":{"ToolResult":{"call_id":"msg-005","result":{"content":"// ML-KEM-1024 implementation...","lines":50},"effect":"Deterministic"}},"meta":{"timestamp":$(date +%s),"trace_id":"$(uuidgen)","span_id":"$(uuidgen)"}}
EOF
echo -e "${CYAN}📦 Tool result: 50 lines (Deterministic)${NC}"

echo

# Summary
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}  Mesh Summary${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo
echo -e "${GREEN}ZAP Protocol:${NC} v0.3.0 (zap-1)"
echo -e "${GREEN}PQ Crypto:${NC} ML-KEM-1024 + ML-DSA-87 (FIPS 203/204)"
echo -e "${GREEN}Agents:${NC} 3 (CTO, Developer, Reviewer)"
echo -e "${GREEN}Messages:${NC} $(wc -l < "${QUEUE}" | tr -d ' ')"
echo
echo -e "${GREEN}📁 Artifacts:${NC}"
echo -e "   Config: ${MESH_DIR}/hanzo.toml"
echo -e "   Agents: ${MESH_DIR}/agent_*.json"
echo -e "   Queue:  ${QUEUE}"
echo -e "   Logs:   ${LOG_DIR}/"
echo
echo -e "${GREEN}✅ Local dev mesh simulation complete!${NC}"
echo

# Show queue contents
echo -e "${BLUE}━━━ Message Queue ━━━${NC}"
cat "${QUEUE}" | jq -c '.id + ": " + .from + " -> " + .to + " (" + (.content | keys[0]) + ")"' 2>/dev/null || cat "${QUEUE}"
echo
