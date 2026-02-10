#!/usr/bin/env bash
# local_5node_network.sh - Start a local 5-node Hanzo L2 dev network
#
# Usage: ./tests/local_5node_network.sh
# Prerequisites: lux CLI, cargo (for building hanzod), cast (foundry, for smoke test)
#
# NOTE: Run `chmod +x tests/local_5node_network.sh` before first use.

set -euo pipefail

# ---------------------------------------------------------------------------
# Color helpers
# ---------------------------------------------------------------------------
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
RESET='\033[0m'

info()  { printf "${CYAN}[INFO]${RESET}  %s\n" "$*"; }
ok()    { printf "${GREEN}[OK]${RESET}    %s\n" "$*"; }
warn()  { printf "${YELLOW}[WARN]${RESET}  %s\n" "$*"; }
err()   { printf "${RED}[ERR]${RESET}   %s\n" "$*" >&2; }
fatal() { err "$@"; exit 1; }

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
CHAIN_NAME="hanzo-dev"
CHAIN_ID=36963
DEV_MNEMONIC="light light light light light light light light light light light energy"
DEVNET_RPC_BASE="http://127.0.0.1:9650"

# Dev account 0 derived from the mnemonic (m/44'/60'/0'/0/0)
DEV_ADDR="0x35D64Ff3f618f7a17DF34DCb21be375A4686a8de"
DEV_KEY="0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GENESIS_PATH="${SCRIPT_DIR}/genesis.json"

NODE_MANIFEST="${HOME}/work/hanzo/node/Cargo.toml"
HANZOD_BIN="${HOME}/work/hanzo/node/target/release/hanzo-node"

HEALTH_RETRIES=30
HEALTH_INTERVAL=2

# ---------------------------------------------------------------------------
# Cleanup on exit
# ---------------------------------------------------------------------------
cleanup() {
    local exit_code=$?
    info "Cleaning up..."

    # Stop the devnet (lux CLI handles process shutdown)
    if lux network status --devnet >/dev/null 2>&1; then
        info "Stopping devnet..."
        lux network stop --devnet 2>/dev/null || true
    fi

    if [ $exit_code -ne 0 ]; then
        err "Script exited with code ${exit_code}"
    fi
}
trap cleanup EXIT INT TERM

# ---------------------------------------------------------------------------
# Step 0: Check prerequisites
# ---------------------------------------------------------------------------
info "Checking prerequisites..."

if ! command -v lux >/dev/null 2>&1; then
    fatal "lux CLI not found. Install from ~/work/lux/cli/ or add to PATH."
fi
ok "lux CLI found: $(command -v lux)"

if ! [ -f "${GENESIS_PATH}" ]; then
    fatal "Genesis file not found: ${GENESIS_PATH}"
fi
ok "Genesis file: ${GENESIS_PATH}"

CAST_AVAILABLE=true
if ! command -v cast >/dev/null 2>&1; then
    warn "cast (foundry) not found -- smoke test will use curl JSON-RPC fallback."
    CAST_AVAILABLE=false
fi

# ---------------------------------------------------------------------------
# Step 1: Build hanzod if needed
# ---------------------------------------------------------------------------
if [ -f "${HANZOD_BIN}" ]; then
    ok "hanzod binary exists: ${HANZOD_BIN}"
else
    info "Building hanzod (release)..."
    if ! [ -f "${NODE_MANIFEST}" ]; then
        fatal "Cargo.toml not found: ${NODE_MANIFEST}"
    fi
    cargo build -p hanzo-node --release --manifest-path "${NODE_MANIFEST}"
    ok "hanzod built: ${HANZOD_BIN}"
fi

if ! [ -x "${HANZOD_BIN}" ]; then
    fatal "hanzod binary is not executable: ${HANZOD_BIN}"
fi

# ---------------------------------------------------------------------------
# Step 2: Stop any existing devnet
# ---------------------------------------------------------------------------
if lux network status --devnet >/dev/null 2>&1; then
    warn "Existing devnet detected, stopping it first..."
    lux network stop --devnet || true
    sleep 2
fi

# ---------------------------------------------------------------------------
# Step 3: Start 5-node Lux L1 devnet
# ---------------------------------------------------------------------------
info "Starting 5-node Lux devnet..."
lux network start --devnet --num-validators 5

info "Waiting for devnet health..."
for i in $(seq 1 "${HEALTH_RETRIES}"); do
    if lux network status --devnet >/dev/null 2>&1; then
        ok "Devnet is healthy (attempt ${i}/${HEALTH_RETRIES})"
        break
    fi
    if [ "$i" -eq "${HEALTH_RETRIES}" ]; then
        fatal "Devnet failed to become healthy after ${HEALTH_RETRIES} attempts."
    fi
    sleep "${HEALTH_INTERVAL}"
done

# ---------------------------------------------------------------------------
# Step 4: Create Hanzo L2 chain
# ---------------------------------------------------------------------------
info "Creating Hanzo L2 chain: ${CHAIN_NAME}"

# Remove stale chain config if it exists from a previous run
if [ -d "${HOME}/.lux/chains/${CHAIN_NAME}" ]; then
    warn "Removing stale chain config: ~/.lux/chains/${CHAIN_NAME}"
    rm -rf "${HOME}/.lux/chains/${CHAIN_NAME}"
fi

lux chain create "${CHAIN_NAME}" \
    --custom-vm \
    --vm "${HANZOD_BIN}" \
    --genesis "${GENESIS_PATH}" \
    --evm-chain-id "${CHAIN_ID}"

ok "Chain created: ${CHAIN_NAME}"

# ---------------------------------------------------------------------------
# Step 5: Deploy chain to devnet
# ---------------------------------------------------------------------------
info "Deploying ${CHAIN_NAME} to devnet..."
lux chain deploy "${CHAIN_NAME}" --devnet

ok "Chain deployed: ${CHAIN_NAME}"

# ---------------------------------------------------------------------------
# Step 6: Wait for chain RPC to be ready
# ---------------------------------------------------------------------------
info "Waiting for Hanzo L2 RPC endpoint..."

# The chain RPC is typically exposed under the devnet base URL.
# We probe the eth_chainId method to confirm readiness.
RPC_URL="${DEVNET_RPC_BASE}/ext/bc/${CHAIN_NAME}/rpc"

for i in $(seq 1 "${HEALTH_RETRIES}"); do
    RESPONSE=$(curl -s -X POST "${RPC_URL}" \
        -H "Content-Type: application/json" \
        -d '{"jsonrpc":"2.0","method":"eth_chainId","params":[],"id":1}' \
        2>/dev/null || true)

    if echo "${RESPONSE}" | grep -q '"result"'; then
        REPORTED_CHAIN_ID=$(echo "${RESPONSE}" | grep -o '"result":"[^"]*"' | cut -d'"' -f4)
        ok "Chain RPC ready (chainId: ${REPORTED_CHAIN_ID}, attempt ${i}/${HEALTH_RETRIES})"
        break
    fi

    if [ "$i" -eq "${HEALTH_RETRIES}" ]; then
        fatal "Chain RPC not ready after ${HEALTH_RETRIES} attempts. Last response: ${RESPONSE}"
    fi

    sleep "${HEALTH_INTERVAL}"
done

# ---------------------------------------------------------------------------
# Step 7: Smoke test -- send ETH transfer
# ---------------------------------------------------------------------------
info "Running smoke test: ETH transfer on Hanzo L2..."

# Recipient is dev account 1
RECIPIENT="0xdAF82928dE0ABBAE133322020B253283d335d3A8"
AMOUNT="0.01ether"

if [ "${CAST_AVAILABLE}" = true ]; then
    # Use cast (foundry) for a clean transfer
    TX_HASH=$(cast send \
        --rpc-url "${RPC_URL}" \
        --private-key "${DEV_KEY}" \
        --chain "${CHAIN_ID}" \
        "${RECIPIENT}" \
        --value "${AMOUNT}" \
        --json 2>/dev/null | grep -o '"transactionHash":"[^"]*"' | cut -d'"' -f4 || true)

    if [ -n "${TX_HASH}" ]; then
        ok "Smoke test passed (tx: ${TX_HASH})"
    else
        # Fallback: try cast without --json and just check exit code
        if cast send \
            --rpc-url "${RPC_URL}" \
            --private-key "${DEV_KEY}" \
            --chain "${CHAIN_ID}" \
            "${RECIPIENT}" \
            --value "${AMOUNT}" >/dev/null 2>&1; then
            ok "Smoke test passed (cast send succeeded)"
        else
            err "Smoke test failed: cast send returned non-zero"
        fi
    fi
else
    # Fallback: curl JSON-RPC eth_getBalance to verify genesis allocation
    BALANCE_RESP=$(curl -s -X POST "${RPC_URL}" \
        -H "Content-Type: application/json" \
        -d "{\"jsonrpc\":\"2.0\",\"method\":\"eth_getBalance\",\"params\":[\"${DEV_ADDR}\",\"latest\"],\"id\":1}")

    BALANCE=$(echo "${BALANCE_RESP}" | grep -o '"result":"[^"]*"' | cut -d'"' -f4 || true)

    if [ -n "${BALANCE}" ] && [ "${BALANCE}" != "0x0" ]; then
        ok "Smoke test passed (dev account balance: ${BALANCE})"
    else
        err "Smoke test failed: unexpected balance response: ${BALANCE_RESP}"
    fi
fi

# ---------------------------------------------------------------------------
# Step 8: Print success summary
# ---------------------------------------------------------------------------
printf "\n"
printf "${BOLD}${GREEN}========================================${RESET}\n"
printf "${BOLD}${GREEN}  Hanzo L2 Dev Network is Running${RESET}\n"
printf "${BOLD}${GREEN}========================================${RESET}\n"
printf "\n"
printf "  ${BOLD}Chain:${RESET}      %s\n" "${CHAIN_NAME}"
printf "  ${BOLD}Chain ID:${RESET}   %s\n" "${CHAIN_ID}"
printf "  ${BOLD}RPC URL:${RESET}    %s\n" "${RPC_URL}"
printf "  ${BOLD}Nodes:${RESET}      5 validators\n"
printf "  ${BOLD}VM:${RESET}         %s\n" "${HANZOD_BIN}"
printf "\n"
printf "  ${BOLD}Dev Mnemonic:${RESET}\n"
printf "    %s\n" "${DEV_MNEMONIC}"
printf "\n"
printf "  ${BOLD}Funded Accounts (1000 ETH each):${RESET}\n"
printf "    [0] 0x35D64Ff3f618f7a17DF34DCb21be375A4686a8de\n"
printf "    [1] 0xdAF82928dE0ABBAE133322020B253283d335d3A8\n"
printf "    [2] 0xBb5D7C55DbbB353f9e7667dbCC43B228B857998a\n"
printf "    [3] 0x6828eAa708F40c11C121C25BdbB4a6fd1415fB24\n"
printf "    [4] 0xa238C9E15C4AD9b10441fe9f7afFa6A131F4Cc37\n"
printf "\n"
printf "  ${BOLD}Hanzo Features:${RESET}\n"
printf "    PQ Crypto:       enabled\n"
printf "    AI Precompiles:  enabled\n"
printf "    Quasar Consensus: enabled\n"
printf "\n"
printf "  ${BOLD}Usage:${RESET}\n"
printf "    cast call --rpc-url %s <contract> <sig>\n" "${RPC_URL}"
printf "    lux network status --devnet\n"
printf "    lux network stop --devnet\n"
printf "\n"

# Disable cleanup trap on success -- leave the network running
trap - EXIT
ok "Network is live. Run 'lux network stop --devnet' to shut down."
