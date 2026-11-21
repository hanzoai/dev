// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
import { assert, describe, it } from "@effect/vitest";
import * as Schema from "effect/Schema";

import { CATALOG } from "./catalog.ts";
import type { UsageEnv } from "./env.ts";
import { FETCHERS, parseCopilot, parseHanzo } from "./fetchers.ts";
import { countdown, evaluate } from "./notify.ts";
import { ProviderSnapshot, UsageReport } from "./schema.ts";
import { createUsageService } from "./service.ts";
import { fetchStatus } from "./status.ts";
import { trayModel } from "./tray.ts";

const isSnapshot = Schema.is(ProviderSnapshot);
const isReport = Schema.is(UsageReport);
const NOW = new Date("2026-11-20T12:00:00Z");

const fakeEnv = (
  files: Record<string, string>,
  routes: Record<string, unknown>,
  env: Record<string, string> = {},
): UsageEnv => ({
  fetch: (async (url: string | URL | Request) => {
    const body = routes[String(url)];
    return body === undefined
      ? new Response("no", { status: 404 })
      : new Response(JSON.stringify(body), { status: 200 });
  }) as typeof fetch,
  home: "/h",
  env,
  now: () => NOW,
  readText: async (f) => files[f],
  listFiles: async (d, s) => Object.keys(files).filter((f) => f.startsWith(d) && f.endsWith(s)),
  keychain: async () => undefined,
});

describe("catalog", () => {
  it("lists 115 unique providers including hanzo", () => {
    assert.strictEqual(CATALOG.length, 115);
    assert.strictEqual(new Set(CATALOG.map((p) => p.id)).size, 115);
    assert.isTrue(CATALOG.some((p) => p.id === "hanzo"));
  });
  it("every fetcher has a catalog row", () => {
    for (const id of Object.keys(FETCHERS)) assert.isTrue(CATALOG.some((p) => p.id === id));
  });
});

describe("parsers", () => {
  it("copilot inverts percent_remaining and skips unlimited", () => {
    const r = parseCopilot({
      quota_reset_date: "2026-12-01",
      quota_snapshots: {
        premium_interactions: { percent_remaining: 25 },
        chat: { unlimited: true },
      },
    });
    assert.deepStrictEqual(
      r.windows.map((w) => [w.label, w.usedPercent]),
      [["Premium", 75]],
    );
  });
  it("hanzo balance is cents, available after holds", () => {
    const r = parseHanzo({ currency: "usd", balance: 2000, holds: 500, available: 1500 });
    assert.deepStrictEqual(r.credits, { currency: "USD", balance: 15, spent: 5 });
  });
});

describe("fetchers", () => {
  it("hanzo reads the IAM identity file and calls api.hanzo.ai only", async () => {
    const env = fakeEnv(
      {
        "/h/.hanzo/identities.json": JSON.stringify({
          active: "hanzo/z",
          identities: { "hanzo/z": { access_token: "tok" } },
        }),
      },
      {
        "https://api.hanzo.ai/v1/billing/balance": {
          currency: "usd",
          balance: 1000,
          holds: 0,
          available: 1000,
        },
      },
    );
    const s = await FETCHERS.hanzo!(env);
    assert.strictEqual(s.provider, "hanzo");
    assert.strictEqual(s.credits?.balance, 10);
    assert.isTrue(isSnapshot(s));
  });
  it("fails loudly when no credential is present", async () => {
    let msg = "";
    await FETCHERS.copilot!(fakeEnv({}, {})).catch((e: Error) => (msg = e.message));
    assert.match(msg, /GitHub Copilot/);
  });
  it("openrouter turns credits into a balance and a used percent", async () => {
    const env = fakeEnv(
      {},
      { "https://openrouter.ai/api/v1/credits": { data: { total_credits: 10, total_usage: 2.5 } } },
      { OPENROUTER_API_KEY: "k" },
    );
    const s = await FETCHERS.openrouter!(env);
    assert.strictEqual(s.windows[0]!.usedPercent, 25);
    assert.strictEqual(s.credits?.balance, 7.5);
  });
});

describe("status", () => {
  it("reads incidents and degrades to unknown", async () => {
    const p = CATALOG.find((x) => x.id === "claude")!;
    const ok = await fetchStatus(
      fakeEnv(
        {},
        {
          [`${p.statusPage}/api/v2/summary.json`]: {
            status: { indicator: "minor", description: "Degraded" },
            incidents: [{ name: "Elevated errors", impact: "minor" }],
          },
        },
      ),
      p,
    );
    assert.strictEqual(ok?.indicator, "minor");
    assert.strictEqual(ok?.incidents[0]?.name, "Elevated errors");
    assert.strictEqual((await fetchStatus(fakeEnv({}, {}), p))?.indicator, "unknown");
  });
});

describe("notifications", () => {
  const snap = (used: number): ProviderSnapshot => ({
    provider: "claude",
    fetchedAt: "x",
    source: "oauth",
    windows: [{ kind: "session", label: "Session", usedPercent: used }],
  });
  it("fires once per threshold and re-arms after reset", () => {
    const t = [80, 95];
    let r = evaluate(new Map(), [snap(70)], t);
    assert.strictEqual(r.alerts.length, 0);
    r = evaluate(r.state, [snap(82)], t);
    assert.deepStrictEqual(
      r.alerts.map((a) => a.threshold),
      [80],
    );
    r = evaluate(r.state, [snap(85)], t);
    assert.strictEqual(r.alerts.length, 0);
    r = evaluate(r.state, [snap(96)], t);
    assert.deepStrictEqual(
      r.alerts.map((a) => a.threshold),
      [95],
    );
    r = evaluate(r.state, [snap(3)], t);
    r = evaluate(r.state, [snap(81)], t);
    assert.deepStrictEqual(
      r.alerts.map((a) => a.threshold),
      [80],
    );
  });
  it("formats countdowns", () => {
    assert.strictEqual(countdown("2026-11-20T14:05:00Z", NOW), "2h 05m");
    assert.strictEqual(countdown("2026-11-23T16:00:00Z", NOW), "3d 4h");
    assert.strictEqual(countdown("2026-11-20T11:00:00Z", NOW), "now");
  });
});

describe("tray", () => {
  const s = (provider: string, used: number): ProviderSnapshot => ({
    provider,
    fetchedAt: "x",
    source: "oauth",
    windows: [{ kind: "session", label: "Session", usedPercent: used }],
  });
  it("merged shows the busiest provider; per-provider lists each", () => {
    const names = { claude: "Claude", codex: "Codex" };
    assert.strictEqual(
      trayModel([s("claude", 40), s("codex", 73)], names, "merged", NOW).title,
      "73%",
    );
    assert.strictEqual(
      trayModel([s("claude", 40), s("codex", 73)], names, "perProvider", NOW).title,
      "40 73",
    );
    assert.strictEqual(trayModel([], names, "merged", NOW).title, "");
  });
});

describe("service and report", () => {
  const local: ProviderSnapshot = {
    provider: "claude",
    fetchedAt: "x",
    source: "oauth",
    windows: [{ kind: "session", label: "Session", usedPercent: 50 }],
  };
  const cost = [
    {
      day: "2026-11-19",
      provider: "claude",
      model: "claude-opus-4-5",
      inputTokens: 10,
      outputTokens: 5,
      cacheTokens: 0,
      costUsd: 0.01,
    },
    {
      day: "2026-10-01",
      provider: "claude",
      model: "claude-opus-4-5",
      inputTokens: 1,
      outputTokens: 1,
      cacheTokens: 0,
    },
  ];
  it("merges host limits with fetched ones, alerts on crossings, and reports metadata only", async () => {
    const files = {
      "/h/.hanzo/identities.json": JSON.stringify({
        active: "a/b",
        identities: { "a/b": { access_token: "t" } },
      }),
    };
    const routes = {
      "https://api.hanzo.ai/v1/billing/balance": { currency: "usd", available: 1000 },
    };
    const svc = createUsageService(fakeEnv(files, routes), () => ({
      providers: {},
      trayMode: "merged",
      showInMenuBar: false,
      notifyAtPercent: [80],
      refreshSeconds: 300,
    }));
    const hot: ProviderSnapshot = {
      ...local,
      provider: "codex",
      windows: [{ kind: "weekly", label: "Weekly", usedPercent: 90 }],
    };
    const { summary, alerts } = await svc.refresh({ local: [local, hot], cost });
    assert.strictEqual(summary.snapshots.find((x) => x.provider === "hanzo")!.credits?.balance, 10);
    assert.strictEqual(
      summary.snapshots.find((x) => x.provider === "claude")!.windows[0]!.usedPercent,
      50,
    );
    assert.deepStrictEqual(
      alerts.map((a) => a.provider),
      ["codex"],
    );
    assert.isTrue(summary.snapshots.some((x) => x.provider === "copilot" && x.error));
    const report = svc.report("dev-1", new Date("2026-11-01T00:00:00Z"));
    assert.isTrue(isReport(report));
    assert.strictEqual(report.cost.length, 1);
  });
  it("keeps the last good windows when a refresh fails", async () => {
    const env = fakeEnv({}, {});
    const svc = createUsageService(env, () => ({
      providers: {
        hanzo: { enabled: true },
        claude: { enabled: false },
        codex: { enabled: false },
        copilot: { enabled: false },
      },
      trayMode: "merged",
      showInMenuBar: false,
      notifyAtPercent: [],
      refreshSeconds: 300,
    }));
    const ok: ProviderSnapshot = {
      provider: "hanzo",
      fetchedAt: "x",
      source: "oauth",
      windows: [{ kind: "monthly", label: "Monthly", usedPercent: 40 }],
    };
    await svc.refresh({ local: [ok], cost: [] });
    const again = await svc.refresh({ local: [], cost: [] });
    const h = again.summary.snapshots[0]!;
    assert.strictEqual(h.windows[0]!.usedPercent, 40);
    assert.isDefined(h.error);
  });
});
