#!/usr/bin/env node
/**
 * Claude CLI to OpenAI-compatible API Proxy
 *
 * This proxy allows @hanzo/dev to use Claude Code CLI as a backend.
 * It accepts OpenAI-compatible chat completion requests and invokes
 * the Claude CLI under the hood.
 *
 * Usage:
 *   node claude-proxy.js [--port 8080]
 *
 * Then configure ~/.code/config.toml:
 *   [model_providers.claude-cli]
 *   name = "Claude CLI"
 *   base_url = "http://localhost:8080/v1"
 *   wire_api = "chat"
 */

const http = require("http");
const { spawn } = require("child_process");

const PORT = parseInt(
  process.env.PORT ||
    process.argv.find((a) => a.startsWith("--port="))?.split("=")[1] ||
    "8080",
);

/**
 * Extract non-JSON text from Claude CLI output (filter telemetry)
 */
function extractResponse(output) {
  const lines = output.split("\n");
  const textLines = [];
  let inJson = false;
  let braceCount = 0;

  for (const line of lines) {
    const trimmed = line.trim();

    // Skip empty lines at start
    if (!trimmed && textLines.length === 0) continue;

    // Track JSON blocks
    if (trimmed.startsWith("{")) {
      inJson = true;
      braceCount = 1;
      continue;
    }

    if (inJson) {
      braceCount += (trimmed.match(/{/g) || []).length;
      braceCount -= (trimmed.match(/}/g) || []).length;
      if (braceCount <= 0) {
        inJson = false;
      }
      continue;
    }

    // Skip lines that are just JSON artifacts (brackets, commas)
    if (/^[\[\]\{\},\s]*$/.test(trimmed)) {
      continue;
    }

    // Not in JSON block - this is actual content
    if (trimmed && !trimmed.startsWith("}")) {
      textLines.push(line);
    }
  }

  // Clean up any trailing bracket artifacts
  let result = textLines.join("\n").trim();
  result = result.replace(/\s*[\[\]\{\}]+\s*$/g, "").trim();

  return result;
}

/**
 * Map model names to Claude CLI supported models
 */
function mapModel(model) {
  const modelMap = {
    "claude-cli": null, // Use default
    "claude-opus": "claude-opus-4-5-20251101",
    "claude-sonnet": "claude-sonnet-4-20250514",
    "claude-haiku": "claude-haiku-4-5-20251001",
    "gpt-4": null, // Use default
    "gpt-4o": null,
    "gpt-4o-mini": null,
  };

  // If model is in map, use mapped value (null means use default)
  if (model in modelMap) {
    return modelMap[model];
  }

  // If model starts with 'claude-', pass it through
  if (model && model.startsWith("claude-")) {
    return model;
  }

  // Default: use Claude's default model
  return null;
}

/**
 * Invoke Claude CLI and return the response
 */
function invokeClaude(prompt, model) {
  return new Promise((resolve, reject) => {
    const args = ["-p", "--output-format", "text"];

    // Map model name to Claude CLI format
    const mappedModel = mapModel(model);
    if (mappedModel) {
      args.push("--model", mappedModel);
    }

    // Create clean env without ANTHROPIC_API_KEY to force subscription mode
    const cleanEnv = { ...process.env };
    delete cleanEnv.ANTHROPIC_API_KEY;
    delete cleanEnv.ANTHROPIC_AUTH_TOKEN;
    cleanEnv.OTEL_SDK_DISABLED = "true";

    const claude = spawn("claude", args, {
      stdio: ["pipe", "pipe", "pipe"],
      env: cleanEnv,
    });

    let stdout = "";
    let stderr = "";

    claude.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    claude.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    claude.on("close", (code) => {
      if (code !== 0 && !stdout) {
        reject(new Error(`Claude CLI exited with code ${code}: ${stderr}`));
      } else {
        const response = extractResponse(stdout);
        resolve(response || stdout);
      }
    });

    claude.on("error", (err) => {
      reject(err);
    });

    // Send prompt via stdin
    claude.stdin.write(prompt);
    claude.stdin.end();
  });
}

/**
 * Handle incoming HTTP requests
 */
async function handleRequest(req, res) {
  const url = new URL(req.url, `http://localhost:${PORT}`);

  // CORS headers
  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");

  if (req.method === "OPTIONS") {
    res.writeHead(200);
    res.end();
    return;
  }

  // Health check
  if (url.pathname === "/health" || url.pathname === "/") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ status: "ok", provider: "claude-cli" }));
    return;
  }

  // Chat completions endpoint
  if (url.pathname === "/v1/chat/completions" && req.method === "POST") {
    let body = "";

    req.on("data", (chunk) => {
      body += chunk.toString();
    });

    req.on("end", async () => {
      try {
        const request = JSON.parse(body);

        // Extract prompt from messages - combine all for context
        const messages = request.messages || [];
        const model = request.model;

        // Build a combined prompt with role prefixes for context
        let prompt = messages
          .map((msg) => {
            const role = msg.role || "user";
            const content = msg.content || "";
            if (role === "system") {
              return `[System]: ${content}`;
            } else if (role === "assistant") {
              return `[Assistant]: ${content}`;
            } else {
              return content; // User messages without prefix
            }
          })
          .join("\n\n");

        console.log(
          `[${new Date().toISOString()}] Request: model=${model}, prompt=${prompt.substring(0, 50)}...`,
        );

        // Invoke Claude CLI
        const response = await invokeClaude(prompt, model);

        console.log(
          `[${new Date().toISOString()}] Response: ${response.substring(0, 100)}...`,
        );

        // Return OpenAI-compatible response
        const completion = {
          id: `chatcmpl-${Date.now()}`,
          object: "chat.completion",
          created: Math.floor(Date.now() / 1000),
          model: model || "claude-cli",
          choices: [
            {
              index: 0,
              message: {
                role: "assistant",
                content: response,
              },
              finish_reason: "stop",
            },
          ],
          usage: {
            prompt_tokens: Math.ceil(prompt.length / 4),
            completion_tokens: Math.ceil(response.length / 4),
            total_tokens: Math.ceil((prompt.length + response.length) / 4),
          },
        };

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(JSON.stringify(completion));
      } catch (error) {
        console.error(`[${new Date().toISOString()}] Error:`, error.message);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(
          JSON.stringify({
            error: {
              message: error.message,
              type: "server_error",
              code: "internal_error",
            },
          }),
        );
      }
    });

    return;
  }

  // Models endpoint (for compatibility)
  if (url.pathname === "/v1/models" && req.method === "GET") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(
      JSON.stringify({
        object: "list",
        data: [
          {
            id: "claude-cli",
            object: "model",
            created: Date.now(),
            owned_by: "anthropic",
          },
          {
            id: "claude-opus",
            object: "model",
            created: Date.now(),
            owned_by: "anthropic",
          },
          {
            id: "claude-sonnet",
            object: "model",
            created: Date.now(),
            owned_by: "anthropic",
          },
          {
            id: "claude-haiku",
            object: "model",
            created: Date.now(),
            owned_by: "anthropic",
          },
        ],
      }),
    );
    return;
  }

  // 404 for unknown endpoints
  res.writeHead(404, { "Content-Type": "application/json" });
  res.end(
    JSON.stringify({ error: { message: "Not found", type: "not_found" } }),
  );
}

// Create server
const server = http.createServer(handleRequest);

server.listen(PORT, () => {
  console.log(`
╔══════════════════════════════════════════════════════════════╗
║          Claude CLI → OpenAI API Proxy                       ║
╠══════════════════════════════════════════════════════════════╣
║  Listening on: http://localhost:${PORT}                         ║
║  Endpoint:     http://localhost:${PORT}/v1/chat/completions     ║
╠══════════════════════════════════════════════════════════════╣
║  Configure ~/.code/config.toml:                              ║
║                                                              ║
║  [model_providers.claude-cli]                                ║
║  name = "Claude CLI"                                         ║
║  base_url = "http://localhost:${PORT}/v1"                       ║
║  wire_api = "chat"                                           ║
║                                                              ║
║  Then: code -c 'model_provider="claude-cli"'                 ║
╚══════════════════════════════════════════════════════════════╝
`);
});

// Graceful shutdown
process.on("SIGINT", () => {
  console.log("\nShutting down...");
  server.close(() => {
    process.exit(0);
  });
});
