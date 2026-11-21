// @effect-diagnostics nodeBuiltinImport:off globalDate:off
import { assert, describe, it } from "@effect/vitest";
import * as NodeFSP from "node:fs/promises";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";

import { defaultUsageSettings } from "@hanzodev/usage";

import { createUsageRuntime } from "./UsageRuntime.ts";

const tmp = () => NodeFSP.mkdtemp(NodePath.join(NodeOS.tmpdir(), "usage-runtime-"));

describe("UsageRuntime", () => {
  it("persists settings privately and serves a tray model without credentials", async () => {
    const stateDir = await tmp();
    const home = await tmp();
    const runtime = createUsageRuntime(stateDir, { platform: "linux", home, env: {} });

    await runtime.saveSettings({
      ...defaultUsageSettings,
      providers: {
        hanzo: { enabled: true },
        claude: { enabled: false },
        codex: { enabled: false },
        copilot: { enabled: false },
      },
    });
    const file = NodePath.join(stateDir, "usage-settings.json");
    assert.strictEqual((await NodeFSP.stat(file)).mode & 0o777, 0o600);

    const again = createUsageRuntime(stateDir, { platform: "linux", home, env: {} });
    assert.strictEqual((await again.settings()).providers.hanzo?.enabled, true);

    const { tray, alerts } = await again.tray(
      async () => ({ local: [], cost: [] }),
      new Date("2026-11-20T12:00:00Z"),
    );
    assert.deepStrictEqual(alerts, []);
    assert.match(tray.lines[0] ?? "", /^Hanzo: /);
    assert.strictEqual(tray.title, "");
  });
});
