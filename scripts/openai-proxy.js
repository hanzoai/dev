#!/usr/bin/env node
// Local OpenAI proxy replicating the upstream-merge workflow behavior.
//
// Features:
// - Injects real OPENAI_API_KEY server-side; clients can use a dummy key
// - Allows only /v1/chat/completions and /v1/responses
// - Adds OpenAI-Beta: responses=experimental for Responses API
// - Sets Accept: text/event-stream by default to stabilize streaming
// - Scrubs sensitive headers from logs; logs are structured JSON
// - Reuses connections and sets generous timeouts for SSE
//
// Usage:
//   # 1) Export your real API key for the proxy process
//   export OPENAI_API_KEY="sk-..."
//   # 2) Start the proxy (default port 5055)
//   node scripts/openai-proxy.js
//   #    or choose a port / upstream base
//   PORT=5055 OPENAI_BASE_URL=https://api.openai.com/v1 node scripts/openai-proxy.js
//
//   # 3) Run the local Codex binary with the proxy (dummy key to client)
//   #    Build the binary first: ./build-fast.sh
//   OPENAI_API_KEY="x" OPENAI_BASE_URL="http://127.0.0.1:${PORT:-5055}/v1" \
//     ./codex-rs/target/dev-fast/code llm request \
//       --developer "Say 'pong' as plain text" \
//       --message "ping" \
//       --format-type json_schema \
//       --schema-json '{"type":"object","properties":{},"additionalProperties":false}' \
//       --model gpt-4o-mini
//
//   # Or run your usual `code` subcommand (Exec/TUI) with the same env vars.

const http = require('http');
const https = require('https');
const crypto = require('crypto');
const { URL } = require('url');

const PORT = process.env.PORT || 5055;
const API_KEY = process.env.OPENAI_API_KEY || '';
const UPSTREAM = new URL(process.env.OPENAI_BASE_URL || 'https://api.openai.com/v1');
const ALLOWED = ['/v1/chat/completions', '/v1/responses'];

if (!API_KEY) {
  console.error('[fatal] OPENAI_API_KEY missing');
  process.exit(1);
}

function redactHeaders(h) {
  const out = {};
  for (const [k, v] of Object.entries(h || {})) {
    const key = k.toLowerCase();
    if (key === 'authorization' || key === 'proxy-authorization' || key === 'cookie' || key === 'set-cookie') {
      out[key] = 'REDACTED';
    } else {
      out[key] = Array.isArray(v) ? v.map(() => '<omitted>') : String(v);
    }
  }
  return out;
}

function log(ev) {
  try { console.log(JSON.stringify({ ts: new Date().toISOString(), ...ev })); } catch {}
}

const server = http.createServer((req, res) => {
  const rid = crypto.randomUUID();
  if (!ALLOWED.some(p => req.url.startsWith(p))) {
    res.writeHead(403, { 'content-type': 'application/json' });
    res.end(JSON.stringify({ error: 'blocked', path: req.url }));
    return;
  }

  const chunks = [];
  let size = 0;
  req.on('data', c => { chunks.push(c); size += c.length; if (size > 2 * 1024 * 1024) { req.destroy(new Error('body too large')); } });
  req.on('error', err => {
    log({ level: 'error', rid, msg: 'client request error', err: String(err) });
    if (!res.headersSent) res.writeHead(400, { 'content-type': 'application/json' });
    res.end(JSON.stringify({ error: 'bad_request' }));
  });
  req.on('end', () => {
    const body = Buffer.concat(chunks);
    const up = new URL(req.url, UPSTREAM);
    const incoming = { ...req.headers };
    if (!incoming['content-type']) incoming['content-type'] = 'application/json';
    if (!incoming['accept']) incoming['accept'] = 'text/event-stream';
    incoming['authorization'] = `Bearer ${API_KEY}`; // replace with real key
    if (up.pathname.startsWith('/v1/responses')) {
      if (!incoming['openai-beta']) incoming['openai-beta'] = 'responses=experimental';
    }
    if (!incoming['originator']) incoming['originator'] = 'codex_cli_rs';
    delete incoming['host']; incoming['host'] = up.host;
    incoming['connection'] = 'keep-alive';
    if (incoming['content-length']) incoming['content-length'] = String(body.length);
    delete incoming['proxy-connection'];
    delete incoming['proxy-authorization'];

    log({ level: 'info', rid, phase: 'request', method: req.method, url: up.toString(), headers: redactHeaders(incoming), body_bytes: body.length });

    const opts = {
      protocol: up.protocol,
      hostname: up.hostname,
      port: up.port || (up.protocol === 'https:' ? 443 : 80),
      path: up.pathname + up.search,
      method: req.method,
      headers: incoming,
      servername: up.hostname,
      agent: (up.protocol === 'https:'
        ? new https.Agent({ keepAlive: true, maxSockets: 64, servername: up.hostname })
        : new http.Agent({ keepAlive: true, maxSockets: 64 }))
    };

    const upstream = (up.protocol === 'https:' ? https : http).request(opts, (upr) => {
      const resHeaders = { ...upr.headers };
      res.writeHead(upr.statusCode || 500, resHeaders);
      log({ level: 'info', rid, phase: 'response_head', status: upr.statusCode, headers: redactHeaders(resHeaders) });
      let total = 0;
      upr.on('data', (chunk) => { total += chunk.length; });
      upr.on('end', () => { log({ level: 'info', rid, phase: 'response_end', status: upr.statusCode, bytes: total, request_id: resHeaders['x-request-id'] || null }); });
      upr.on('error', (e) => { log({ level: 'error', rid, phase: 'response_error', err: String(e) }); });
      upr.pipe(res);
    });
    upstream.setTimeout(600000, () => { upstream.destroy(new Error('upstream timeout')); });
    upstream.on('error', (e) => {
      log({ level: 'error', rid, phase: 'upstream_error', err: String(e) });
      if (!res.headersSent) res.writeHead(502, { 'content-type': 'application/json' });
      res.end(JSON.stringify({ error: 'upstream_error' }));
    });
    upstream.end(body);
  });
});
server.headersTimeout = 650000;
server.keepAliveTimeout = 650000;
server.listen(PORT, '127.0.0.1', () => { log({ level: 'info', msg: 'proxy listening', addr: '127.0.0.1', port: PORT }); });

