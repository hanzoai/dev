#!/bin/bash

# Hanzo Dev Startup Script with Automatic Provider Detection
# This script detects available AI providers and starts Hanzo Dev with the best option

set -e

echo "🚀 Hanzo Dev - Intelligent Provider Detection"
echo "============================================"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
HANZO_ENGINE_PORT=36900
HANZO_NODE_PORT=3690
HANZO_NODE_ALT_PORT=3691

# Function to check if a port is open
check_port() {
    local host=$1
    local port=$2
    nc -z -w 1 "$host" "$port" 2>/dev/null
}

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Initialize provider list
PROVIDERS=()
DEFAULT_MODEL=""

echo -e "\n${BLUE}Checking available providers...${NC}\n"

# 1. Check Hanzo Engine (highest priority)
if check_port localhost $HANZO_ENGINE_PORT; then
    echo -e "${GREEN}✓${NC} Hanzo Engine detected on port $HANZO_ENGINE_PORT (Qwen3 models available)"
    PROVIDERS+=("hanzo-engine")
    DEFAULT_MODEL="qwen3-8b"
    export HANZO_ENGINE_URL="http://localhost:$HANZO_ENGINE_PORT"
elif command_exists hanzo-engine; then
    echo -e "${YELLOW}⚠${NC} Hanzo Engine installed but not running. Starting..."
    hanzo-engine serve --port $HANZO_ENGINE_PORT &
    sleep 3
    if check_port localhost $HANZO_ENGINE_PORT; then
        echo -e "${GREEN}✓${NC} Hanzo Engine started successfully"
        PROVIDERS+=("hanzo-engine")
        DEFAULT_MODEL="qwen3-8b"
        export HANZO_ENGINE_URL="http://localhost:$HANZO_ENGINE_PORT"
    fi
fi

# 2. Check Hanzo Node
if check_port localhost $HANZO_NODE_PORT; then
    echo -e "${GREEN}✓${NC} Hanzo Node detected on port $HANZO_NODE_PORT"
    PROVIDERS+=("hanzo-node")
    export HANZO_NODE_URL="http://localhost:$HANZO_NODE_PORT"
elif check_port localhost $HANZO_NODE_ALT_PORT; then
    echo -e "${GREEN}✓${NC} Hanzo Node detected on port $HANZO_NODE_ALT_PORT"
    PROVIDERS+=("hanzo-node")
    export HANZO_NODE_URL="http://localhost:$HANZO_NODE_ALT_PORT"
elif command_exists hanzod; then
    echo -e "${YELLOW}⚠${NC} Hanzo Node (hanzod) installed but not running. Starting..."
    hanzod --port $HANZO_NODE_PORT &
    sleep 3
    if check_port localhost $HANZO_NODE_PORT; then
        echo -e "${GREEN}✓${NC} Hanzo Node started successfully"
        PROVIDERS+=("hanzo-node")
        export HANZO_NODE_URL="http://localhost:$HANZO_NODE_PORT"
    fi
fi

# 3. Check OpenAI
if [ -n "$OPENAI_API_KEY" ]; then
    echo -e "${GREEN}✓${NC} OpenAI API key detected"
    PROVIDERS+=("openai")
    [ -z "$DEFAULT_MODEL" ] && DEFAULT_MODEL="gpt-4o"
fi

# 4. Check Anthropic Claude
if [ -n "$ANTHROPIC_API_KEY" ]; then
    echo -e "${GREEN}✓${NC} Anthropic API key detected"
    PROVIDERS+=("claude")
    [ -z "$DEFAULT_MODEL" ] && DEFAULT_MODEL="claude-3-opus-20240229"
fi

# 5. Check for Claude Code CLI
if command_exists claude; then
    echo -e "${GREEN}✓${NC} Claude Code CLI detected"
    PROVIDERS+=("claude-cli")
fi

# 6. Check for other Codex tools
if command_exists codex; then
    echo -e "${GREEN}✓${NC} OpenAI Codex CLI detected"
    PROVIDERS+=("codex-cli")
fi

# 7. Check DashScope/Qwen API
if [ -n "$DASHSCOPE_API_KEY" ] || [ -n "$QWEN_API_KEY" ]; then
    echo -e "${GREEN}✓${NC} DashScope/Qwen API key detected"
    PROVIDERS+=("dashscope")
    [ -z "$DEFAULT_MODEL" ] && DEFAULT_MODEL="qwen-max"
fi

# 8. Check Ollama
if check_port localhost 11434; then
    echo -e "${GREEN}✓${NC} Ollama detected on port 11434"
    PROVIDERS+=("ollama")
    [ -z "$DEFAULT_MODEL" ] && DEFAULT_MODEL="llama2"
fi

# Summary
echo -e "\n${BLUE}Provider Summary:${NC}"
echo "==================="

if [ ${#PROVIDERS[@]} -eq 0 ]; then
    echo -e "${RED}✗${NC} No AI providers detected!"
    echo ""
    echo "Please ensure one of the following:"
    echo "  1. Start Hanzo Engine: hanzo-engine serve --port $HANZO_ENGINE_PORT"
    echo "  2. Start Hanzo Node: hanzod --port $HANZO_NODE_PORT"
    echo "  3. Set OPENAI_API_KEY environment variable"
    echo "  4. Set ANTHROPIC_API_KEY environment variable"
    echo "  5. Install and run Ollama"
    exit 1
fi

echo -e "Found ${GREEN}${#PROVIDERS[@]}${NC} provider(s):"
for provider in "${PROVIDERS[@]}"; do
    echo "  • $provider"
done

echo -e "\nDefault model: ${GREEN}$DEFAULT_MODEL${NC}"

# Offer to use specific provider if multiple available
if [ ${#PROVIDERS[@]} -gt 1 ]; then
    echo -e "\n${BLUE}Multiple providers available. Select preference:${NC}"
    echo "  1) Use Hanzo Local (Qwen3) - Recommended"
    echo "  2) Use OpenAI"
    echo "  3) Use Claude"
    echo "  4) Use first available (auto)"
    echo -n "Choice [1-4, default=1]: "
    read -r choice
    
    case $choice in
        2)
            if [[ " ${PROVIDERS[@]} " =~ " openai " ]]; then
                DEFAULT_MODEL="gpt-4o"
                echo -e "Using ${GREEN}OpenAI${NC}"
            fi
            ;;
        3)
            if [[ " ${PROVIDERS[@]} " =~ " claude " ]]; then
                DEFAULT_MODEL="claude-3-opus-20240229"
                echo -e "Using ${GREEN}Claude${NC}"
            fi
            ;;
        4)
            echo -e "Using ${GREEN}auto-detection${NC}"
            ;;
        *)
            if [[ " ${PROVIDERS[@]} " =~ " hanzo-engine " ]] || [[ " ${PROVIDERS[@]} " =~ " hanzo-node " ]]; then
                DEFAULT_MODEL="qwen3-8b"
                echo -e "Using ${GREEN}Hanzo Local (Qwen3)${NC}"
            fi
            ;;
    esac
fi

# Export configuration
export HANZO_PROVIDERS="${PROVIDERS[*]}"
export HANZO_DEFAULT_MODEL="$DEFAULT_MODEL"

# Check if config file should be used
CONFIG_FILE=""
if [ -f "hanzo_config.toml" ]; then
    CONFIG_FILE="--config hanzo_config.toml"
    echo -e "\n${GREEN}✓${NC} Using hanzo_config.toml"
elif [ -f "~/.hanzo/config.toml" ]; then
    CONFIG_FILE="--config ~/.hanzo/config.toml"
    echo -e "\n${GREEN}✓${NC} Using ~/.hanzo/config.toml"
fi

# Start Hanzo Dev
echo -e "\n${BLUE}Starting Hanzo Dev...${NC}"
echo "====================="

# Determine which command to use
if command_exists hanzo; then
    echo "Running: hanzo --model $DEFAULT_MODEL $CONFIG_FILE $@"
    exec hanzo --model "$DEFAULT_MODEL" $CONFIG_FILE "$@"
elif [ -f "./target/release/hanzo" ]; then
    echo "Running: ./target/release/hanzo --model $DEFAULT_MODEL $CONFIG_FILE $@"
    exec ./target/release/hanzo --model "$DEFAULT_MODEL" $CONFIG_FILE "$@"
elif [ -f "./target/debug/hanzo" ]; then
    echo "Running: ./target/debug/hanzo --model $DEFAULT_MODEL $CONFIG_FILE $@"
    exec ./target/debug/hanzo --model "$DEFAULT_MODEL" $CONFIG_FILE "$@"
else
    echo -e "${YELLOW}⚠${NC} Hanzo Dev binary not found. Building..."
    cargo build --release
    exec ./target/release/hanzo --model "$DEFAULT_MODEL" $CONFIG_FILE "$@"
fi