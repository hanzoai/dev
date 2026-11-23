const assert = require("node:assert/strict");
const { spawnSync } = require("node:child_process");
const { mkdtempSync, readFileSync, rmSync, writeFileSync, existsSync } = require("node:fs");
const { tmpdir } = require("node:os");
const { join } = require("node:path");
const { test } = require("node:test");

const script = join(__dirname, "relay-state-output.cjs");

const config = {
  clientTracingUrl: "https://example.invalid/traces",
  clientTracingDataset: "fixture-dataset",
  clientTracingToken: { __redacted__: "fixture-token" },
};
const json = JSON.stringify(config, null, 2);

function runStep(stdout, exitCode = 0) {
  const runnerTemp = mkdtempSync(join(tmpdir(), "hanzo-relay-state-test-"));
  const binDir = mkdtempSync(join(tmpdir(), "hanzo-relay-state-bin-"));
  try {
    // Fake `npx` on PATH that prints FIXTURE_STDOUT and exits FIXTURE_EXIT.
    writeFileSync(
      join(binDir, "npx"),
      `#!/bin/sh\nprintf '%s\\n' "$FIXTURE_STDOUT"\nexit "$FIXTURE_EXIT"\n`,
      { mode: 0o755 },
    );
    const result = spawnSync(process.execPath, [script, runnerTemp], {
      encoding: "utf8",
      env: {
        PATH: `${binDir}:${process.env.PATH}`,
        RUNNER_TEMP: runnerTemp,
        FIXTURE_STDOUT: stdout,
        FIXTURE_EXIT: String(exitCode),
      },
    });
    assert.ifError(result.error);
    const envPath = join(runnerTemp, "relay-client-tracing.env");
    return {
      ...result,
      envFile: existsSync(envPath) ? readFileSync(envPath, "utf8") : undefined,
    };
  } finally {
    rmSync(runnerTemp, { recursive: true, force: true });
    rmSync(binDir, { recursive: true, force: true });
  }
}

for (const prefix of [
  "",
  "• Refreshing Cloudflare State Store credentials\n✓ Refreshing Cloudflare State Store credentials\n",
]) {
  test(`extracts tracing config ${prefix ? "after progress output" : "from plain JSON"}`, () => {
    const result = runStep(prefix + json);
    assert.equal(result.status, 0, result.stderr);
    assert.equal(result.stdout, "::add-mask::fixture-token\n");
    assert.equal(
      result.envFile,
      "HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_URL=https://example.invalid/traces\n" +
        "HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_DATASET=fixture-dataset\n" +
        "HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_TOKEN=fixture-token\n",
    );
  });
}

for (const [name, stdout, exitCode] of [
  ["failed CLI even with valid JSON", json, 1],
  ["missing JSON", "Refreshing credentials...", 0],
  ["malformed JSON", "{not JSON", 0],
  ["missing token", JSON.stringify({ ...config, clientTracingToken: null }, null, 2), 0],
]) {
  test(`rejects ${name} without writing config`, () => {
    const result = runStep(stdout, exitCode);
    assert.notEqual(result.status, 0);
    assert.equal(result.envFile, undefined);
  });
}
