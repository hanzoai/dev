// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
import { assert, describe, it } from "@effect/vitest";
import * as Schema from "effect/Schema";

import { CATALOG } from "./catalog.ts";
import { parseClaudeLog, parseCodexLog, priceOf, rollupDays } from "./cost.ts";
import type { UsageEnv } from "./env.ts";
import { FETCHERS, parseClaude, parseCodex, parseCopilot, parseHanzo } from "./fetchers.ts";
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
  it("claude windows and extra usage", () => {
    const r = parseClaude({
      five_hour: { utilization: 42, resets_at: "2026-11-20T15:00:00Z" },
      seven_day: { utilization: 10, resets_at: "2026-11-25T00:00:00Z" },
      extra_usage: { is_enabled: true, monthly_limit: 5000, used_credits: 1250 },
    });
    assert.deepStrictEqual(
      r.windows.map((w) => [w.label, w.usedPercent]),
      [
        ["Session", 42],
        ["Weekly", 10],
      ],
    );
    assert.deepStrictEqual(r.credits, { currency: "USD", limit: 50, spent: 12.5 });
  });
  it("codex windows, plan and credits", () => {
    const r = parseCodex({
      plan_type: "pro",
      rate_limit: {
        primary_window: { used_percent: 30, limit_window_seconds: 18000, reset_at: 1_800_000_000 },
        secondary_window: { used_percent: 5, limit_window_seconds: 604800 },
      },
      credits: { balance: "12.5" },
    });
    assert.strictEqual(r.plan, "pro");
    assert.strictEqual(r.windows[0]!.windowMinutes, 300);
    assert.strictEqual(r.windows[1]!.kind, "weekly");
    assert.strictEqual(r.credits?.balance, 12.5);
  });
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
  it("hanzo billing summary", () => {
    const r = parseHanzo({
      plan: "pro",
      monthly_used_cents: 500,
      monthly_limit_cents: 2000,
      credit_balance_cents: 1500,
    });
    assert.strictEqual(r.windows[0]!.usedPercent, 25);
    assert.deepStrictEqual(r.credits, { currency: "USD", balance: 15, spent: 5, limit: 20 });
  });
});

describe("fetchers", () => {
  it("hanzo reads the IAM identity file and calls api.hanzo.ai only", async () => {
    const env = fakeEnv(
      {
        "/h/.hanzo/identities.json": JSON.stringify({
          accounts: [{ id: "a", access_token: "tok" }],
        }),
      },
      {
        "https://api.hanzo.ai/v1/billing/summary": {
          plan: "pro",
          monthly_used_cents: 100,
          monthly_limit_cents: 1000,
        },
      },
    );
    const s = await FETCHERS.hanzo!(env);
    assert.strictEqual(s.provider, "hanzo");
    assert.strictEqual(s.windows[0]!.usedPercent, 10);
    assert.isTrue(isSnapshot(s));
  });
  it("fails loudly when no credential is present", async () => {
    let msg = "";
    await FETCHERS.codex!(fakeEnv({}, {})).catch((e: Error) => (msg = e.message));
    assert.match(msg, /codex login/);
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

describe("cost", () => {
  const claudeLine = (id: string, out: number) =>
    JSON.stringify({
      type: "assistant",
      timestamp: "2026-11-20T10:00:00Z",
      sessionId: "s",
      requestId: "r" + id,
      message: {
        id: "m" + id,
        model: "claude-sonnet-4-5",
        usage: {
          input_tokens: 1000,
          output_tokens: out,
          cache_read_input_tokens: 0,
          cache_creation_input_tokens: 0,
        },
      },
    });
  it("dedupes streamed claude messages and prices them", () => {
    const ev = parseClaudeLog(
      [claudeLine("1", 100), claudeLine("1", 100), claudeLine("2", 200)].join("\n"),
      "f",
    );
    assert.strictEqual(ev.length, 2);
    assert.closeTo(ev[0]!.costUsd!, (1000 * 3 + 100 * 15) / 1e6, 1e-9);
  });
  it("codex splits cached input and leaves unknown models unpriced", () => {
    const text = [
      JSON.stringify({ type: "turn_context", payload: { model: "gpt-5-codex" } }),
      JSON.stringify({
        type: "event_msg",
        timestamp: "2026-11-20T10:00:00Z",
        payload: {
          type: "token_count",
          info: {
            last_token_usage: { input_tokens: 1000, cached_input_tokens: 400, output_tokens: 50 },
          },
        },
      }),
    ].join("\n");
    const [e] = parseCodexLog(text, "f");
    assert.strictEqual(e!.inputTokens, 600);
    assert.strictEqual(e!.cacheReadTokens, 400);
    assert.isUndefined(
      priceOf({
        model: "mystery",
        inputTokens: 1,
        outputTokens: 1,
        cacheReadTokens: 0,
        cacheWriteTokens: 0,
      }),
    );
  });
  it("rolls up by day and model", () => {
    const ev = parseClaudeLog([claudeLine("1", 100), claudeLine("2", 200)].join("\n"), "f");
    const days = rollupDays(ev);
    assert.strictEqual(days.length, 1);
    assert.strictEqual(days[0]!.outputTokens, 300);
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
  it("refreshes enabled providers, keeps last good data on failure, and reports metadata only", async () => {
    const files = {
      "/h/.hanzo/identities.json": JSON.stringify({ accounts: [{ access_token: "t" }] }),
      "/h/.claude/projects/p/s.jsonl": JSON.stringify({
        type: "assistant",
        timestamp: "2026-11-19T10:00:00Z",
        message: {
          id: "a",
          model: "claude-opus-4-5",
          content: "SECRET PROMPT TEXT",
          usage: { input_tokens: 10, output_tokens: 5 },
        },
      }),
    };
    const routes = {
      "https://api.hanzo.ai/v1/billing/summary": {
        plan: "pro",
        monthly_used_cents: 900,
        monthly_limit_cents: 1000,
      },
    };
    const svc = createUsageService(fakeEnv(files, routes), () => ({
      providers: {},
      trayMode: "merged",
      notifyAtPercent: [80],
      refreshSeconds: 300,
    }));
    const { summary, alerts } = await svc.refresh();
    const hanzo = summary.snapshots.find((x) => x.provider === "hanzo")!;
    assert.strictEqual(hanzo.windows[0]!.usedPercent, 90);
    assert.deepStrictEqual(
      alerts.map((a) => a.provider),
      ["hanzo"],
    );
    assert.isTrue(summary.snapshots.some((x) => x.provider === "claude" && x.error));
    const report = svc.report("dev-1", new Date("2026-11-01T00:00:00Z"));
    assert.isTrue(isReport(report));
    assert.strictEqual(report.events.length, 1);
    assert.notInclude(JSON.stringify(report), "SECRET PROMPT TEXT");
  });
});
