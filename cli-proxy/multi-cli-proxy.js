#!/usr/bin/env node
/**
 * Multi-CLI to OpenAI-compatible API Proxy
 *
 * Supports multiple AI CLI backends (uses subscription/login, NOT API keys):
 * - Claude Code CLI (claude) - Anthropic subscription
 * - Codex CLI (codex) - OpenAI subscription
 * - Gemini CLI (gemini) - Google account
 * - Mistral Vibe CLI (vibe) - Mistral subscription
 * - Qwen CLI (qwen) - Alibaba account
 * - Ollama (ollama) - Local models
 *
 * Usage:
 *   node multi-cli-proxy.js [--port 9999]
 *
 * Configure ~/.code/config.toml with any supported provider.
 */

const http = require("http");
const { spawn } = require("child_process");

const PORT = parseInt(
  process.env.PORT ||
    process.argv.find((a) => a.startsWith("--port="))?.split("=")[1] ||
    "9999",
);

/**
 * API keys to clean per backend (forces subscription/login mode)
 */
const API_KEYS_TO_CLEAN = {
  claude: ["ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN"],
  codex: ["OPENAI_API_KEY", "OPENAI_ORG_ID"],
  gemini: ["GOOGLE_API_KEY", "GEMINI_API_KEY"],
  vibe: ["MISTRAL_API_KEY"],
  qwen: ["DASHSCOPE_API_KEY", "QWEN_API_KEY"],
};

/**
 * CLI Backend Configurations
 * All backends use subscription/login mode by default (API keys removed from env)
 */
const CLI_BACKENDS = {
  // Claude Code CLI - uses Anthropic subscription
  claude: {
    command: "claude",
    args: (prompt, model) => {
      const args = ["-p", "--output-format", "text"];
      if (model && model !== "claude-cli" && model.startsWith("claude-")) {
        args.push("--model", model);
      }
      return args;
    },
    env: { OTEL_SDK_DISABLED: "true" },
    inputMode: "stdin",
    models: ["claude-cli", "claude-opus", "claude-sonnet", "claude-haiku"],
  },

  // Codex CLI (OpenAI) - uses OpenAI subscription
  codex: {
    command: "codex",
    args: (prompt, model) => {
      const args = ["-q", "--full-auto"]; // -q for quiet, final output only
      if (model && model !== "codex" && model.startsWith("codex-")) {
        args.push("-m", model);
      }
      args.push(prompt);
      return args;
    },
    env: { OTEL_SDK_DISABLED: "true" },
    inputMode: "args",
    models: [
      "codex",
      "codex-mini",
      "codex-mini-latest",
      "gpt-4o",
      "gpt-4o-mini",
      "o1",
      "o3-mini",
    ],
  },

  // Google Gemini CLI - uses Google account
  gemini: {
    command: "gemini",
    args: (prompt, model) => {
      const args = ["-p", prompt, "-y"]; // -y for auto-approve
      if (model && model !== "gemini" && model.startsWith("gemini-")) {
        args.push("-m", model);
      }
      return args;
    },
    env: {},
    inputMode: "args",
    models: ["gemini", "gemini-2.5-pro", "gemini-2.0-flash", "gemini-1.5-pro"],
  },

  // Mistral Vibe CLI - uses Mistral subscription
  vibe: {
    command: "vibe",
    args: (prompt, model) => [
      "-p",
      prompt,
      "--output",
      "text",
      "--auto-approve",
    ],
    env: {},
    inputMode: "args",
    models: ["vibe", "mistral", "mistral-large", "mistral-small"],
  },

  // Qwen CLI - uses Alibaba account
  qwen: {
    command: "qwen",
    args: (prompt, model) => ["-p", prompt],
    env: { OTEL_SDK_DISABLED: "true" },
    inputMode: "args",
    models: ["qwen", "qwen-cli", "qwen-plus", "qwen-turbo", "qwen3"],
  },

  // Ollama (local models) - no API keys needed
  ollama: {
    command: "ollama",
    args: (prompt, model) => ["run", model || "llama3.2", prompt],
    env: {},
    inputMode: "args",
    models: ["ollama", "llama3.2", "llama3.1", "codellama", "mixtral", "phi3"],
  },

  // GitHub Copilot - uses GitHub subscription via gh extension
  copilot: {
    command: "gh",
    args: (prompt, model) => ["copilot", "suggest", "--type", "shell", prompt],
    env: {},
    inputMode: "args",
    models: ["copilot", "gh-copilot", "github-copilot"],
  },
};

/**
 * Map model name to CLI backend
 */
function getBackend(model) {
  if (!model) return { ...CLI_BACKENDS["claude"], name: "claude" };

  // Direct backend match
  for (const [name, backend] of Object.entries(CLI_BACKENDS)) {
    if (backend.models.includes(model)) {
      return { ...backend, name };
    }
  }

  // Prefix matching
  if (model.startsWith("claude"))
    return { ...CLI_BACKENDS["claude"], name: "claude" };
  if (
    model.startsWith("codex") ||
    model.startsWith("gpt") ||
    model.startsWith("o1") ||
    model.startsWith("o3")
  ) {
    return { ...CLI_BACKENDS["codex"], name: "codex" };
  }
  if (model.startsWith("gemini"))
    return { ...CLI_BACKENDS["gemini"], name: "gemini" };
  if (model.startsWith("vibe") || model.startsWith("mistral"))
    return { ...CLI_BACKENDS["vibe"], name: "vibe" };
  if (model.startsWith("qwen"))
    return { ...CLI_BACKENDS["qwen"], name: "qwen" };
  if (
    model.startsWith("llama") ||
    model.startsWith("ollama") ||
    model.startsWith("phi")
  ) {
    return { ...CLI_BACKENDS["ollama"], name: "ollama" };
  }
  if (model.startsWith("copilot") || model.startsWith("gh-copilot")) {
    return { ...CLI_BACKENDS["copilot"], name: "copilot" };
  }

  // Default to Claude
  return { ...CLI_BACKENDS["claude"], name: "claude" };
}

/**
 * Extract clean text from CLI output (filter telemetry/JSON artifacts)
 */
function extractResponse(output) {
  const lines = output.split("\n");
  const textLines = [];
  let inJson = false;
  let braceCount = 0;

  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed && textLines.length === 0) continue;

    if (trimmed.startsWith("{")) {
      inJson = true;
      braceCount = 1;
      continue;
    }

    if (inJson) {
      braceCount += (trimmed.match(/{/g) || []).length;
      braceCount -= (trimmed.match(/}/g) || []).length;
      if (braceCount <= 0) inJson = false;
      continue;
    }

    if (/^[\[\]\{\},\s]*$/.test(trimmed)) continue;
    if (trimmed && !trimmed.startsWith("}")) {
      textLines.push(line);
    }
  }

  let result = textLines.join("\n").trim();
  result = result.replace(/\s*[\[\]\{\}]+\s*$/g, "").trim();
  return result;
}

/**
 * Invoke CLI backend and return response
 */
function invokeCLI(prompt, model) {
  return new Promise((resolve, reject) => {
    const backend = getBackend(model);
    const args = backend.args(prompt, model);

    console.log(
      `[${new Date().toISOString()}] Backend: ${backend.name}, Command: ${backend.command} ${args.slice(0, 2).join(" ")}...`,
    );

    // Build environment - clean API keys to force subscription/login mode
    let spawnEnv = { ...process.env, ...backend.env };
    const keysToClean = API_KEYS_TO_CLEAN[backend.name] || [];
    for (const key of keysToClean) {
      delete spawnEnv[key];
    }

    const proc = spawn(backend.command, args, {
      stdio: ["pipe", "pipe", "pipe"],
      env: spawnEnv,
    });

    let stdout = "";
    let stderr = "";

    proc.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    proc.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    proc.on("close", (code) => {
      if (code !== 0 && !stdout) {
        reject(
          new Error(`${backend.name} CLI exited with code ${code}: ${stderr}`),
        );
      } else {
        const response = extractResponse(stdout);
        resolve(response || stdout.trim());
      }
    });

    proc.on("error", (err) => {
      reject(new Error(`Failed to spawn ${backend.name}: ${err.message}`));
    });

    // Send prompt via stdin if backend uses stdin mode
    if (backend.inputMode === "stdin") {
      proc.stdin.write(prompt);
      proc.stdin.end();
    }
  });
}

/**
 * Build prompt from messages array
 */
function buildPrompt(messages) {
  return messages
    .map((msg) => {
      const role = msg.role || "user";
      const content = msg.content || "";
      if (role === "system") return `[System]: ${content}`;
      if (role === "assistant") return `[Assistant]: ${content}`;
      return content;
    })
    .join("\n\n");
}

/**
 * Handle HTTP requests
 */
async function handleRequest(req, res) {
  const url = new URL(req.url, `http://localhost:${PORT}`);

  // CORS
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
    res.end(
      JSON.stringify({
        status: "ok",
        backends: Object.keys(CLI_BACKENDS),
        default: "claude",
      }),
    );
    return;
  }

  // Chat completions
  if (url.pathname === "/v1/chat/completions" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => {
      body += chunk.toString();
    });
    req.on("end", async () => {
      try {
        const request = JSON.parse(body);
        const messages = request.messages || [];
        const model = request.model;
        const prompt = buildPrompt(messages);

        console.log(
          `[${new Date().toISOString()}] Request: model=${model}, prompt=${prompt.substring(0, 50)}...`,
        );

        const response = await invokeCLI(prompt, model);

        console.log(
          `[${new Date().toISOString()}] Response: ${response.substring(0, 100)}...`,
        );

        const completion = {
          id: `chatcmpl-${Date.now()}`,
          object: "chat.completion",
          created: Math.floor(Date.now() / 1000),
          model: model || "claude-cli",
          choices: [
            {
              index: 0,
              message: { role: "assistant", content: response },
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

  // Models endpoint
  if (url.pathname === "/v1/models" && req.method === "GET") {
    const allModels = [];
    for (const [name, backend] of Object.entries(CLI_BACKENDS)) {
      for (const model of backend.models) {
        allModels.push({
          id: model,
          object: "model",
          created: Date.now(),
          owned_by: name,
        });
      }
    }
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ object: "list", data: allModels }));
    return;
  }

  // 404
  res.writeHead(404, { "Content-Type": "application/json" });
  res.end(
    JSON.stringify({ error: { message: "Not found", type: "not_found" } }),
  );
}

// Start server
const server = http.createServer(handleRequest);

server.listen(PORT, () => {
  const backends = Object.keys(CLI_BACKENDS).join(", ");
  console.log(`
╔══════════════════════════════════════════════════════════════════╗
║     Multi-CLI → OpenAI API Proxy (Subscription Mode)             ║
╠══════════════════════════════════════════════════════════════════╣
║  Listening:  http://localhost:${PORT}                               ║
║  Endpoint:   http://localhost:${PORT}/v1/chat/completions           ║
║  Backends:   ${backends.padEnd(48)}║
╠══════════════════════════════════════════════════════════════════╣
║  All CLIs use subscription/login (API keys are stripped):        ║
║    claude-cli, claude-opus → Claude (Anthropic subscription)     ║
║    codex, gpt-4o, o1       → Codex (OpenAI subscription)         ║
║    gemini, gemini-2.5-pro  → Gemini (Google account)             ║
║    vibe, mistral           → Vibe (Mistral subscription)         ║
║    qwen, qwen-plus         → Qwen (Alibaba account)              ║
║    ollama, llama3.2        → Ollama (local, no keys needed)      ║
╚══════════════════════════════════════════════════════════════════╝
`);
});

process.on("SIGINT", () => {
  console.log("\nShutting down...");
  server.close(() => process.exit(0));
});
