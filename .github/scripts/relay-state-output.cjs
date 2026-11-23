#!/usr/bin/env node
// Reads the deployed relay stack state from Alchemy and writes client tracing
// config to $RUNNER_TEMP/relay-client-tracing.env. Fails closed (non-zero,
// writes no config) when the CLI fails or the JSON is missing/incomplete.
"use strict";

const { spawnSync } = require("node:child_process");
const { mkdtempSync, rmSync, writeFileSync } = require("node:fs");
const { tmpdir } = require("node:os");
const { join } = require("node:path");

const argv = process.argv.slice(2);
const runnerTemp = argv[0] || process.env.RUNNER_TEMP || mkdtempSync(join(tmpdir(), "hanzo-relay-state-"));
const ownRunnerTemp = argv.length === 0;

function fail(message) {
  console.error(message);
  if (ownRunnerTemp) rmSync(runnerTemp, { recursive: true, force: true });
  process.exit(1);
}

// Client tracing is optional: with no deployed relay stack the build ships
// without it. But when we do run the CLI, we must fail closed on a failure
// or missing/incomplete JSON rather than writing a broken config.
const output = spawnSync("npx", ["alchemy", "state", "read", "HanzoDevRelay/prod/output", "--backend", "cloudflare", "--log-level", "error", "--no-input"], {
  encoding: "utf8",
  cwd: "infra/relay",
  env: process.env,
});

if (output.status !== 0 || !output.stdout || output.stdout.trim() === "") {
  console.warn("::warning::No production relay stack state; building without relay client tracing.");
  if (ownRunnerTemp) rmSync(runnerTemp, { recursive: true, force: true });
  process.exit(0);
}

// Vercel/Alchemy can print progress lines before the JSON. Strip everything
// up to the first `{`.
const jsonStart = output.stdout.indexOf("{");
if (jsonStart === -1) fail("Relay stack output is missing JSON");
const jsonText = output.stdout.slice(jsonStart);

let config;
try {
  config = JSON.parse(jsonText);
} catch {
  fail("Relay stack output is not valid JSON");
}

function field(key) {
  const value = config[key];
  if (value === null || value === undefined || value === "") {
    fail(`Relay stack output is missing ${key}`);
  }
  // Redacted values are persisted as {"__redacted__": "<value>"}.
  return typeof value === "object" ? value.__redacted__ : value;
}

const url = field("clientTracingUrl");
const dataset = field("clientTracingDataset");
const token = field("clientTracingToken");

if (!url || !dataset || !token) {
  fail("Relay stack output is missing a required tracing field");
}

const envPath = join(runnerTemp, "relay-client-tracing.env");
writeFileSync(
  envPath,
  `HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_URL=${url}\n` +
    `HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_DATASET=${dataset}\n` +
    `HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_TOKEN=${token}\n`,
);
process.stdout.write(`::add-mask::${token}\n`);
process.exit(0);
