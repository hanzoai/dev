# Claude CLI to OpenAI-compatible API Proxy

This proxy enables **@hanzo/dev** (and other OpenAI-compatible clients) to use your authenticated **Claude Hanzo Dev CLI** as a backend, without needing API keys.

## How It Works

```
┌─────────────┐     HTTP/JSON      ┌─────────────┐     stdin/stdout    ┌─────────────┐
│ @hanzo/dev  │ ─────────────────► │   Proxy     │ ──────────────────► │ Claude CLI  │
│   CLI       │ ◄───────────────── │  :9999      │ ◄────────────────── │  (claude)   │
└─────────────┘  OpenAI-compat     └─────────────┘      text           └─────────────┘
```

## Quick Start

### 1. Start the Proxy

```bash
cd ~/work/hanzo/dev/cli-proxy
PORT=9999 node claude-proxy.js
```

### 2. Configure @hanzo/dev

Add to `~/.hanzo/config.toml`:

```toml
model_provider = "claude-cli"
model = "claude-cli"

[model_providers.claude-cli]
name = "Claude CLI"
base_url = "http://localhost:9999/v1"
wire_api = "chat"
```

### 3. Use @hanzo/dev

```bash
# Interactive mode
dev

# Exec mode
echo "What is 2+2?" | dev exec --full-auto
```

## Testing

Direct proxy test with curl:

```bash
curl -s http://localhost:9999/v1/chat/completions \
  -X POST \
  -H "Content-Type: application/json" \
  -d '{"model":"claude-cli","messages":[{"role":"user","content":"What is 2+2?"}]}' \
  | jq .
```

Expected response:
```json
{
  "choices": [{
    "message": {
      "role": "assistant",
      "content": "4"
    }
  }]
}
```

## Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Health check |
| `/v1/models` | GET | List available models |
| `/v1/chat/completions` | POST | Chat completions (main endpoint) |

## Model Mapping

| Request Model | Claude Model |
|--------------|--------------|
| `claude-cli` | Default (Sonnet) |
| `claude-opus` | claude-opus-4-5-20251101 |
| `claude-sonnet` | claude-sonnet-4-20250514 |
| `claude-haiku` | claude-haiku-4-5-20251001 |
| `gpt-4*` | Default |

## Features

- **Zero API Keys**: Uses your authenticated Claude Hanzo Dev CLI
- **OpenAI Compatible**: Works with any OpenAI-compatible client
- **Multi-Message Support**: Handles system/user/assistant roles
- **Telemetry Filtering**: Cleans JSON artifacts from CLI output
- **CORS Enabled**: Works with browser-based clients

## Requirements

- Node.js 16+
- Claude Hanzo Dev CLI installed and authenticated (`claude --version`)

## Troubleshooting

### Port Already in Use

```bash
PORT=8080 node claude-proxy.js  # Use different port
```

### Claude CLI Not Found

Ensure Claude Hanzo Dev CLI is installed:
```bash
npm install -g @anthropic-ai/claude-dev
claude auth login
```

### Testing the Integration

Run the integration test:
```bash
./test-integration.sh
```

## License

MIT - Part of the @hanzo/dev CLI orchestration project.
