#!/bin/bash
# Run Hanzo Dev with Ollama models including Qwen

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}          Hanzo Dev with Ollama/Qwen Integration              ${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo ""

# Check Ollama is running
if ! curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
    echo -e "${YELLOW}Starting Ollama...${NC}"
    ollama serve &
    sleep 3
fi

# Pull Qwen models if not already available
echo -e "${BLUE}Checking Qwen models...${NC}"
if ! ollama list | grep -q "qwen2.5:7b"; then
    echo -e "${YELLOW}Pulling Qwen 2.5 7B model...${NC}"
    ollama pull qwen2.5:7b
fi

# List available models
echo -e "\n${GREEN}Available models:${NC}"
ollama list

# Configure environment for Ollama
export HANZO_PROVIDER=ollama
export OLLAMA_BASE_URL=http://localhost:11434
export HANZO_MODEL=qwen2.5:7b

# Test the model
echo -e "\n${BLUE}Testing Qwen model...${NC}"
echo "Explain quantum computing in one sentence" | ollama run qwen2.5:7b --verbose 2>&1 | head -5

echo -e "\n${GREEN}✓ Ollama with Qwen models is ready!${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo -e "\n${YELLOW}To use Qwen models:${NC}"
echo "  1. Chat: ${CYAN}ollama run qwen2.5:7b${NC}"
echo "  2. API: ${CYAN}curl http://localhost:11434/api/generate -d '{\"model\":\"qwen2.5:7b\", \"prompt\":\"Hello\"}'${NC}"
echo "  3. Embeddings: ${CYAN}curl http://localhost:11434/api/embeddings -d '{\"model\":\"qwen2.5:7b\", \"prompt\":\"Test\"}'${NC}"

# Create a simple API wrapper for compatibility
echo -e "\n${BLUE}Creating OpenAI-compatible API wrapper...${NC}"
cat > /tmp/ollama_wrapper.py << 'EOF'
#!/usr/bin/env python3
from flask import Flask, request, jsonify, Response
import requests
import json

app = Flask(__name__)
OLLAMA_URL = "http://localhost:11434"

@app.route('/v1/chat/completions', methods=['POST'])
def chat_completions():
    data = request.json
    model = data.get('model', 'qwen2.5:7b')
    messages = data.get('messages', [])
    
    # Convert to Ollama format
    prompt = "\n".join([f"{m['role']}: {m['content']}" for m in messages])
    
    ollama_data = {
        "model": model,
        "prompt": prompt,
        "stream": data.get('stream', False)
    }
    
    resp = requests.post(f"{OLLAMA_URL}/api/generate", json=ollama_data)
    
    # Convert response to OpenAI format
    if resp.status_code == 200:
        ollama_resp = resp.json()
        return jsonify({
            "id": "chatcmpl-123",
            "object": "chat.completion",
            "created": 1677858242,
            "model": model,
            "choices": [{
                "index": 0,
                "message": {
                    "role": "assistant",
                    "content": ollama_resp.get('response', '')
                },
                "finish_reason": "stop"
            }],
            "usage": {
                "prompt_tokens": len(prompt.split()),
                "completion_tokens": len(ollama_resp.get('response', '').split()),
                "total_tokens": len(prompt.split()) + len(ollama_resp.get('response', '').split())
            }
        })
    return jsonify({"error": "Failed to generate response"}), 500

@app.route('/v1/embeddings', methods=['POST'])
def embeddings():
    data = request.json
    model = data.get('model', 'qwen2.5:7b')
    input_text = data.get('input', [''])[0] if isinstance(data.get('input'), list) else data.get('input', '')
    
    ollama_data = {
        "model": model,
        "prompt": input_text
    }
    
    resp = requests.post(f"{OLLAMA_URL}/api/embeddings", json=ollama_data)
    
    if resp.status_code == 200:
        ollama_resp = resp.json()
        return jsonify({
            "object": "list",
            "data": [{
                "object": "embedding",
                "embedding": ollama_resp.get('embedding', []),
                "index": 0
            }],
            "model": model,
            "usage": {
                "prompt_tokens": len(input_text.split()),
                "total_tokens": len(input_text.split())
            }
        })
    return jsonify({"error": "Failed to generate embeddings"}), 500

@app.route('/v1/models', methods=['GET'])
def list_models():
    resp = requests.get(f"{OLLAMA_URL}/api/tags")
    if resp.status_code == 200:
        ollama_models = resp.json().get('models', [])
        return jsonify({
            "object": "list",
            "data": [
                {
                    "id": m['name'],
                    "object": "model",
                    "created": 1677858242,
                    "owned_by": "ollama",
                    "capabilities": {
                        "max_tokens": 32768,
                        "supports_thinking": "qwen" in m['name'].lower(),
                        "supports_vision": False,
                        "supports_function_calling": True,
                        "embedding_dimensions": 4096 if "qwen" in m['name'].lower() else 1536
                    }
                }
                for m in ollama_models
            ]
        })
    return jsonify({"error": "Failed to list models"}), 500

@app.route('/health', methods=['GET'])
def health():
    return jsonify({"status": "healthy", "provider": "ollama"})

if __name__ == '__main__':
    print("Starting OpenAI-compatible API wrapper on port 3690...")
    app.run(host='0.0.0.0', port=3690, debug=False)
EOF

# Start the wrapper
echo -e "${BLUE}Starting API wrapper on port 3690...${NC}"
python3 /tmp/ollama_wrapper.py &
WRAPPER_PID=$!

echo -e "${GREEN}✓ API wrapper started (PID: $WRAPPER_PID)${NC}"
echo -e "${GREEN}✓ OpenAI-compatible API available at http://localhost:3690${NC}"

# Wait for interrupt
trap "kill $WRAPPER_PID 2>/dev/null; exit" INT
echo -e "\n${YELLOW}Press Ctrl+C to stop${NC}"
wait