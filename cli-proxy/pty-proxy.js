#!/usr/bin/env node
/**
 * Claude CLI PTY Proxy
 *
 * Uses PTY to interact with Claude CLI in interactive mode,
 * which uses your Claude subscription instead of API credits.
 */

const http = require("http");
const pty = require("node-pty");

const PORT = parseInt(process.env.PORT || "9999");

// Track active PTY sessions
const sessions = new Map();

/**
 * Send a message to Claude via PTY and get response
 */
function sendToClaude(prompt) {
  return new Promise((resolve, reject) => {
    const timeout = 120000; // 2 minutes
    let output = "";
    let responseStarted = false;
    let lastDataTime = Date.now();

    // Spawn Claude in interactive mode
    const proc = pty.spawn("claude", ["--dangerously-skip-permissions"], {
      name: "xterm-256color",
      cols: 120,
      rows: 40,
      cwd: process.cwd(),
      env: {
        ...process.env,
        TERM: "xterm-256color",
        NO_COLOR: "1", // Disable colors for easier parsing
      },
    });

    const timeoutId = setTimeout(() => {
      proc.kill();
      reject(new Error("Timeout waiting for Claude response"));
    }, timeout);

    // Detect when Claude is ready for input
    let ready = false;
    let sentPrompt = false;

    proc.onData((data) => {
      lastDataTime = Date.now();
      output += data;

      // Debug logging
      if (process.env.DEBUG) {
        console.log("[PTY DATA]:", JSON.stringify(data.substring(0, 100)));
      }

      // Detect ready state (Claude shows prompt)
      if (!ready && (output.includes(">") || output.includes("Claude"))) {
        ready = true;
      }

      // Send prompt once ready
      if (ready && !sentPrompt) {
        sentPrompt = true;
        setTimeout(() => {
          proc.write(prompt + "\n");
        }, 500);
      }

      // Detect response completion (new prompt appears after response)
      if (sentPrompt && output.split(prompt).length > 1) {
        // Look for completion indicators
        const afterPrompt = output.split(prompt).pop();
        if (afterPrompt && afterPrompt.includes(">")) {
          // Response complete
          clearTimeout(timeoutId);
          proc.kill();

          // Extract response between prompt and next >
          const response = extractResponse(output, prompt);
          resolve(response);
        }
      }
    });

    proc.onExit(({ exitCode }) => {
      clearTimeout(timeoutId);
      if (exitCode !== 0 && !output) {
        reject(new Error(`Claude exited with code ${exitCode}`));
      } else {
        const response = extractResponse(output, prompt);
        resolve(response || "No response received");
      }
    });
  });
}

/**
 * Extract Claude's response from PTY output
 */
function extractResponse(output, prompt) {
  // Remove ANSI escape codes
  const clean = output
    .replace(/\x1B\[[0-9;]*[a-zA-Z]/g, "")
    .replace(/\x1B\][^\x07]*\x07/g, "")
    .replace(/[\x00-\x1F]/g, " ");

  // Find the response after the prompt
  const promptIndex = clean.indexOf(prompt);
  if (promptIndex === -1) return clean.trim();

  let response = clean.substring(promptIndex + prompt.length);

  // Remove trailing prompt indicator
  response = response.replace(/>\s*$/, "").trim();

  // Remove any leading/trailing whitespace and common artifacts
  response = response
    .split("\n")
    .filter(
      (line) =>
        line.trim() && !line.includes("Claude") && !line.startsWith(">"),
    )
    .join("\n")
    .trim();

  return response || "Response received";
}

/**
 * Build prompt from messages
 */
function buildPrompt(messages) {
  const lastUserMsg = messages.filter((m) => m.role === "user").pop();
  return lastUserMsg?.content || messages[messages.length - 1]?.content || "";
}

/**
 * HTTP request handler
 */
async function handleRequest(req, res) {
  const url = new URL(req.url, `http://localhost:${PORT}`);

  res.setHeader("Access-Control-Allow-Origin", "*");
  res.setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
  res.setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");

  if (req.method === "OPTIONS") {
    res.writeHead(200);
    res.end();
    return;
  }

  if (url.pathname === "/health" || url.pathname === "/") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(JSON.stringify({ status: "ok", mode: "pty-interactive" }));
    return;
  }

  if (url.pathname === "/v1/chat/completions" && req.method === "POST") {
    let body = "";
    req.on("data", (chunk) => {
      body += chunk.toString();
    });
    req.on("end", async () => {
      try {
        const request = JSON.parse(body);
        const prompt = buildPrompt(request.messages || []);

        console.log(
          `[${new Date().toISOString()}] PTY Request: ${prompt.substring(0, 50)}...`,
        );

        const response = await sendToClaude(prompt);

        console.log(
          `[${new Date().toISOString()}] PTY Response: ${response.substring(0, 100)}...`,
        );

        res.writeHead(200, { "Content-Type": "application/json" });
        res.end(
          JSON.stringify({
            id: `chatcmpl-${Date.now()}`,
            object: "chat.completion",
            created: Math.floor(Date.now() / 1000),
            model: request.model || "claude-interactive",
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
          }),
        );
      } catch (error) {
        console.error(`[${new Date().toISOString()}] Error:`, error.message);
        res.writeHead(500, { "Content-Type": "application/json" });
        res.end(
          JSON.stringify({
            error: { message: error.message, type: "server_error" },
          }),
        );
      }
    });
    return;
  }

  res.writeHead(404, { "Content-Type": "application/json" });
  res.end(JSON.stringify({ error: { message: "Not found" } }));
}

const server = http.createServer(handleRequest);

server.listen(PORT, () => {
  console.log(`
╔══════════════════════════════════════════════════════════════╗
║       Claude CLI PTY Proxy (Interactive Mode)                ║
╠══════════════════════════════════════════════════════════════╣
║  Listening: http://localhost:${PORT}                            ║
║  Mode:      PTY Interactive (uses subscription, not API)     ║
╠══════════════════════════════════════════════════════════════╣
║  This proxy spawns Claude CLI in interactive mode via PTY,   ║
║  which uses your Claude subscription instead of API credits. ║
╚══════════════════════════════════════════════════════════════╝
`);
});

process.on("SIGINT", () => {
  console.log("\nShutting down...");
  server.close(() => process.exit(0));
});
