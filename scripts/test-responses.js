#!/usr/bin/env node
/*
Quick Responses API test harness using credentials from $CODEX_HOME/auth.json.

Usage examples:
  # Auto-detect ChatGPT vs API key from auth.json; run all tests non-streaming
  node scripts/test-responses.js --model gpt-4o-mini

  # Force ChatGPT auth and only run the json-schema test (non-streaming)
  node scripts/test-responses.js --auth chatgpt --tests json-schema --model gpt-4o-mini

  # Force API key auth via env; override base URL (e.g., proxy)
  OPENAI_API_KEY=x OPENAI_BASE_URL=http://127.0.0.1:5055/v1 \
    node scripts/test-responses.js --auth api-key --tests base,json-schema --model gpt-4o-mini

Flags:
  --auth chatgpt|api-key|auto   Default: auto (prefer ChatGPT if tokens exist)
  --model <slug>                Default: gpt-4o-mini
  --tests <csv>                 One or more of: base,json-schema,tools-web-search,tools-web-search-preview,store
                                Default: base,json-schema
  --stream true|false           Use streaming SSE (true) or JSON (false). Default: false
  --store true|false            Force store flag; default depends on test case

Notes:
  - ChatGPT auth uses https://chatgpt.com/backend-api/codex/responses
  - API-key auth uses $OPENAI_BASE_URL or https://api.openai.com/v1/responses
  - Sends OpenAI-Beta: responses=v1; Accept: application/json (stream=false) or text/event-stream (stream=true)
  - Prints status, x-request-id, and first 400 chars of error text for failures
*/

const fs = require('fs');
const path = require('path');
const { randomUUID } = require('crypto');

const argv = parseArgs(process.argv.slice(2));

function parseArgs(args) {
  const out = { auth: 'auto', model: 'gpt-4o-mini', tests: ['base','json-schema'], stream: false, store: undefined };
  for (let i=0; i<args.length; i++) {
    const a = args[i];
    const next = () => args[++i];
    if (a === '--auth') out.auth = String(next()||'auto');
    else if (a === '--model') out.model = String(next()||'gpt-4o-mini');
    else if (a === '--tests') out.tests = String(next()||'').split(',').map(s=>s.trim()).filter(Boolean);
    else if (a === '--stream') out.stream = /^true$/i.test(String(next()||''));
    else if (a === '--store') out.store = /^true$/i.test(String(next()||''));
  }
  return out;
}

function codexHome() {
  return process.env.CODEX_HOME || process.env.CODE_HOME || path.join(require('os').homedir(), '.codex');
}

function readAuthJson() {
  const p = path.join(codexHome(), 'auth.json');
  try { return JSON.parse(fs.readFileSync(p,'utf8')); } catch { return null; }
}

function getChatGptToken(auth) {
  const t = auth && auth.tokens;
  return t && t.access_token ? t.access_token : null;
}

function getAccountId(auth) {
  return auth && auth.tokens && auth.tokens.account_id || null;
}

function getApiKey(auth) {
  // Prefer env var to mirror production behavior
  if (process.env.OPENAI_API_KEY && process.env.OPENAI_API_KEY.trim()) return process.env.OPENAI_API_KEY.trim();
  return auth && auth.OPENAI_API_KEY || null;
}

function pickAuth(mode, auth) {
  if (mode === 'chatgpt') {
    const tok = getChatGptToken(auth);
    if (!tok) throw new Error('No ChatGPT token in auth.json');
    return { kind: 'chatgpt', token: tok, accountId: getAccountId(auth) };
  }
  if (mode === 'api-key') {
    const key = getApiKey(auth);
    if (!key) throw new Error('No OPENAI_API_KEY in env or auth.json');
    return { kind: 'api-key', token: key };
  }
  // auto
  const tok = getChatGptToken(auth);
  if (tok) return { kind: 'chatgpt', token: tok, accountId: getAccountId(auth) };
  const key = getApiKey(auth);
  if (key) return { kind: 'api-key', token: key };
  throw new Error('No usable credentials found');
}

function baseFor(authKind) {
  if (authKind === 'chatgpt') {
    return (process.env.CHATGPT_BASE_URL || 'https://chatgpt.com/backend-api/codex').replace(/\/$/, '');
  }
  return (process.env.OPENAI_BASE_URL || 'https://api.openai.com/v1').replace(/\/$/, '');
}

function makePayload(kind, model, stream, store, variant) {
  const input = [
    { type: 'message', role: 'user', content: [{ type: 'input_text', text: 'Say ok.' } ] }
  ];
  let tools = undefined;
  if (variant === 'tools-web-search') {
    tools = [ { type: 'web_search', name: 'web_search', description: 'Search the web' } ];
  } else if (variant === 'tools-web-search-preview') {
    tools = [ { type: 'web_search_preview', name: 'web_search_preview', description: 'Search the web (preview)' } ];
  }
  let text = undefined;
  if (variant === 'json-schema') {
    text = {
      format: {
        type: 'json_schema',
        name: 'ok_schema',
        strict: true,
        schema: { type: 'object', additionalProperties: false, properties: { ok: { type: 'boolean' } }, required: ['ok'] }
      }
    };
  }
  // For ChatGPT, server rejects some combos; keep store=false by default unless explicitly asked
  const useStore = typeof store === 'boolean' ? store : false;
  return {
    model,
    instructions: 'You are a test harness.',
    input,
    tools,
    tool_choice: 'auto',
    parallel_tool_calls: true,
    reasoning: null,
    text,
    store: useStore,
    stream,
    include: [],
    prompt_cache_key: randomUUID(),
  };
}

async function sendOnce(auth, opts) {
  const base = baseFor(auth.kind);
  const url = base + '/responses';
  const headers = {
    'Authorization': `Bearer ${auth.token}`,
    'OpenAI-Beta': 'responses=v1',
  };
  if (opts.stream) headers['Accept'] = 'text/event-stream';
  if (auth.kind === 'chatgpt') headers['session_id'] = randomUUID();
  if (auth.kind === 'chatgpt' && auth.accountId) headers['chatgpt-account-id'] = auth.accountId;
  headers['Content-Type'] = 'application/json';
  headers['originator'] = 'codex_cli_rs';
  headers['version'] = '0.0.0';

  const res = await fetch(url, { method: 'POST', headers, body: JSON.stringify(opts.payload) });
  const rid = res.headers.get('x-request-id');
  const info = { status: res.status, ok: res.ok, requestId: rid };
  if (res.ok) {
    if (opts.stream) {
      console.log(`[ok] ${opts.caseName}: ${res.status} (request-id: ${rid || '-'})`);
      return info;
    }
    const data = await res.json();
    console.log(`[ok] ${opts.caseName}: ${res.status} (request-id: ${rid || '-'})`);
    if (process.env.VERBOSE) console.log(JSON.stringify(data).slice(0, 800));
    return info;
  } else {
    let body = '';
    try { body = await res.text(); } catch {}
    const excerpt = body.length > 400 ? body.slice(0,400)+'…' : body;
    console.log(`[fail] ${opts.caseName}: ${res.status} (request-id: ${rid || '-'})\n${excerpt}`);
    return info;
  }
}

async function main() {
  const authJson = readAuthJson();
  const auth = pickAuth(argv.auth, authJson);
  const tests = new Set(argv.tests);
  const stream = !!argv.stream;
  const model = argv.model;

  const cases = [];
  if (tests.has('base')) cases.push({ name: 'base', variant: 'base' });
  if (tests.has('json-schema')) cases.push({ name: 'json-schema', variant: 'json-schema' });
  if (tests.has('tools-web-search')) cases.push({ name: 'tools-web-search', variant: 'tools-web-search' });
  if (tests.has('tools-web-search-preview')) cases.push({ name: 'tools-web-search-preview', variant: 'tools-web-search-preview' });
  if (tests.has('store')) cases.push({ name: 'store=true', variant: 'base', store: true });

  for (const c of cases) {
    const payload = makePayload(auth.kind, model, stream, c.store, c.variant);
    await sendOnce(auth, { payload, stream, caseName: c.name });
  }
}

main().catch(e => { console.error(e.stack||String(e)); process.exit(1); });

