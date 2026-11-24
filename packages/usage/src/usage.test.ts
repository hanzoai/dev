// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
import { assert, describe, it } from "@effect/vitest";
import * as Schema from "effect/Schema";

import { CATALOG } from "./catalog.ts";
import type { UsageEnv } from "./env.ts";
import { accountName } from "./account.ts";
import {
  parseAntigravity,
  parseClaude,
  parseCodex,
  parseCursor,
  parseHanzo,
  parseMuse,
  parseZai,
} from "./accounts.ts";
import { parseCopilot } from "./fetchers.ts";
import { CONNECTORS, FETCHERS } from "./readers.ts";
import { countdown, evaluate } from "./notify.ts";
import { ProviderSnapshot, UsageReport } from "./schema.ts";
import { createUsageService } from "./service.ts";
import { fetchStatus } from "./status.ts";
import { trayModel } from "./tray.ts";

const isSnapshot = Schema.is(ProviderSnapshot);
const isReport = Schema.is(UsageReport);
const NOW = new Date("2026-11-20T12:00:00Z");

const jwt = (claims: Record<string, unknown>) =>
  `h.${Buffer.from(JSON.stringify(claims)).toString("base64url")}.s`;
const IN_AN_HOUR = Math.floor(NOW.getTime() / 1000) + 3600;

const fakeEnv = (
  files: Record<string, string>,
  routes: Record<string, unknown>,
  env: Record<string, string> = {},
  more: Partial<UsageEnv> = {},
): UsageEnv => ({
  fetch: (async (url: string | URL | Request) => {
    const body = routes[String(url)];
    return body === undefined
      ? new Response("no", { status: 404 })
      : new Response(JSON.stringify(body), { status: 200 });
  }) as typeof fetch,
  platform: "darwin",
  home: "/h",
  env,
  now: () => NOW,
  readText: async (f) => files[f],
  listFiles: async (d, s) => Object.keys(files).filter((f) => f.startsWith(d) && f.endsWith(s)),
  listDir: async (d) => [
    ...new Set(
      Object.keys(files)
        .filter((f) => f.startsWith(`${d}/`))
        .map((f) => f.slice(d.length + 1).split("/")[0]!),
    ),
  ],
  sqliteValue: async () => undefined,
  exec: async () => undefined,
  withScratch: async (_files, run) => run("/scratch"),
  keychain: async () => undefined,
  ...more,
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
  it("hanzo reads every identity in the CLI store; the expired one says to sign in again", async () => {
    const files = {
      "/h/Library/Application Support/hanzo/credentials": JSON.stringify({
        "hanzo/admin/z": JSON.stringify({
          access_token: jwt({ iss: "https://hanzo.ai", exp: IN_AN_HOUR, email: "z@example.com" }),
        }),
        "hanzo/hanzo/z": JSON.stringify({
          access_token: jwt({ iss: "https://hanzo.ai", exp: 1, email: "z@example.com" }),
        }),
      }),
      "/h/Library/Application Support/hanzo/config.toml":
        '[auth]\nprovider = "hanzo"\n\n[auth.active]\nhanzo = "admin/z"\n',
    };
    const env = fakeEnv(files, {
      "https://hanzo.ai/.well-known/openid-configuration": { issuer: "https://hanzo.ai" },
      "https://api.hanzo.ai/v1/billing/balance": {
        currency: "usd",
        balance: 1000,
        holds: 0,
        available: 1000,
      },
      "https://api.hanzo.ai/v1/billing/tier": {
        tier: { displayName: "Pro" },
        planName: "Unlimited",
      },
    });
    const [active, stale] = await FETCHERS.hanzo!(env, []);
    assert.strictEqual(active!.label, "admin/z · active");
    assert.strictEqual(active!.account, "z@example.com");
    assert.strictEqual(active!.credits?.balance, 10);
    assert.strictEqual(active!.plan, "Pro · Unlimited");
    assert.strictEqual(stale!.error, "Sign in again.");
    assert.isTrue(isSnapshot(active) && isSnapshot(stale));
  });
  it("hanzo never sends a token the discovered issuer did not issue", async () => {
    const calls: string[] = [];
    const files = {
      "/h/Library/Application Support/hanzo/credentials": JSON.stringify({
        "hanzo/admin/z": {
          access_token: jwt({ iss: "https://elsewhere.example", exp: IN_AN_HOUR }),
        },
      }),
    };
    const env = fakeEnv(
      files,
      { "https://hanzo.ai/.well-known/openid-configuration": { issuer: "https://hanzo.ai" } },
      {},
      {
        fetch: (async (url: string | URL | Request) => {
          calls.push(String(url));
          return new Response(JSON.stringify({ issuer: "https://hanzo.ai" }), { status: 200 });
        }) as typeof fetch,
      },
    );
    const [r] = await FETCHERS.hanzo!(env, []);
    assert.match(r!.error ?? "", /not issued by Hanzo/);
    assert.deepStrictEqual(calls, ["https://hanzo.ai/.well-known/openid-configuration"]);
  });
  it("fails loudly when no credential is present", async () => {
    let msg = "";
    await FETCHERS.copilot!(fakeEnv({}, {}), []).catch((e: Error) => (msg = e.message));
    assert.match(msg, /GitHub Copilot/);
  });
  it("openrouter turns credits into a balance and a used percent", async () => {
    const env = fakeEnv(
      {},
      { "https://openrouter.ai/api/v1/credits": { data: { total_credits: 10, total_usage: 2.5 } } },
      { OPENROUTER_API_KEY: "k" },
    );
    const [s] = (await FETCHERS.openrouter!(env, [])) as [ProviderSnapshot];
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
    instance: "claudeAgent",
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
      "/h/Library/Application Support/hanzo/credentials": JSON.stringify({
        "hanzo/a/b": { access_token: jwt({ exp: IN_AN_HOUR }) },
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
    const { summary, alerts } = await svc.refresh({
      instances: [{ provider: "claude", id: "claudeAgent", name: "Claude" }],
      local: [local, { ...hot, instance: "codex" }],
      cost,
    });
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
  it("shows a row for a provider that is slow, without waiting for it", async () => {
    const token = ".gemini/antigravity-cli/antigravity-oauth-token";
    const svc = createUsageService(
      fakeEnv(
        { [`/h/${token}`]: JSON.stringify({ id_token_email: "a@b.c" }) },
        {},
        {},
        {
          exec: () => new Promise<string>(() => undefined),
        },
      ),
      () => ({
        providers: Object.fromEntries(
          CATALOG.map((p) => [p.id, { enabled: p.id === "antigravity" }]),
        ),
        trayMode: "merged",
        showInMenuBar: false,
        notifyAtPercent: [],
        refreshSeconds: 300,
      }),
      50,
    );
    const { summary } = await svc.refresh({ instances: [], local: [], cost: [] });
    const row = summary.snapshots.find((x) => x.provider === "antigravity")!;
    assert.isTrue(typeof row.error === "string" && row.error.length > 0);
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
    await svc.refresh({ instances: [], local: [ok], cost: [] });
    const again = await svc.refresh({ instances: [], local: [], cost: [] });
    const h = again.summary.snapshots[0]!;
    assert.strictEqual(h.windows[0]!.usedPercent, 40);
    assert.isDefined(h.error);
  });
});

describe("account readers", () => {
  it("parses Claude windows, scoped weekly limits and extra usage", () => {
    const r = parseClaude({
      five_hour: { utilization: 12.5, resets_at: "2026-11-20T15:00:00Z" },
      seven_day: { utilization: 71, resets_at: "2026-11-27T01:00:00Z" },
      seven_day_opus: null,
      limits: [
        { kind: "weekly_all", percent: 71 },
        {
          kind: "weekly_scoped",
          percent: 4,
          resets_at: "2026-11-27T01:00:00Z",
          scope: { model: { display_name: "Sonnet" } },
        },
      ],
      extra_usage: {
        is_enabled: true,
        monthly_limit: 2000,
        used_credits: 150,
        currency: "USD",
        decimal_places: 2,
      },
    });
    assert.deepStrictEqual(
      r.windows.map((w) => [w.label, w.usedPercent]),
      [
        ["Session", 12.5],
        ["Weekly", 71],
        ["Weekly · Sonnet", 4],
      ],
    );
    assert.deepStrictEqual(r.credits, { currency: "USD", spent: 1.5, limit: 20 });
  });
  it("parses Codex session and weekly windows", () => {
    const r = parseCodex({
      email: "z@example.com",
      plan_type: "plus",
      rate_limit: {
        primary_window: { used_percent: 1, limit_window_seconds: 18000, reset_at: 1795260928 },
        secondary_window: { used_percent: 57, limit_window_seconds: 604800, reset_at: 1795564295 },
      },
      credits: { has_credits: false, balance: "0" },
    });
    assert.deepStrictEqual(
      r.windows.map((w) => [w.kind, w.windowMinutes, w.usedPercent]),
      [
        ["session", 300, 1],
        ["weekly", 10080, 57],
      ],
    );
    assert.strictEqual(r.plan, "Plus");
    assert.strictEqual(r.account, "z@example.com");
    assert.isUndefined(r.credits);
  });
  it("parses the Antigravity /usage report and skips disabled buckets", () => {
    const out = JSON.stringify({
      command: {
        data: {
          groups: [
            {
              name: "Gemini Models",
              buckets: [
                { window: "weekly", remaining_fraction: 1, reset_time: "2026-11-28T09:07:13Z" },
                { window: "5h", remaining_fraction: 0.75, reset_time: "2026-11-21T14:07:13Z" },
              ],
            },
            {
              name: "Claude and GPT models",
              buckets: [
                { window: "weekly", remaining_fraction: 0, reset_time: "2026-11-25T19:48:40Z" },
                { window: "5h", disabled: true, remaining_fraction: 1 },
              ],
            },
          ],
        },
      },
    });
    assert.deepStrictEqual(
      parseAntigravity(out).windows.map((w) => [w.label, w.usedPercent]),
      [
        ["Gemini Models · Weekly", 0],
        ["Gemini Models · 5-hour", 25],
        ["Claude and GPT models · Weekly", 100],
      ],
    );
  });
  it("parses Cursor plan percentages and the billing cycle reset", () => {
    const r = parseCursor({
      membershipType: "pro",
      billingCycleEnd: "2026-12-08T08:20:42.412Z",
      individualUsage: { plan: { totalPercentUsed: 42, autoPercentUsed: 10, apiPercentUsed: 80 } },
    });
    assert.strictEqual(r.plan, "Pro");
    assert.deepStrictEqual(
      r.windows.map((w) => [w.label, w.usedPercent]),
      [
        ["Included", 42],
        ["Auto", 10],
        ["API", 80],
      ],
    );
    assert.strictEqual(r.windows[0]!.resetsAt, "2026-12-08T08:20:42.412Z");
  });
  it("parses z.ai quota limits by window length", () => {
    const r = parseZai({
      success: true,
      data: {
        planName: "Pro",
        limits: [
          {
            type: "TIME_LIMIT",
            unit: 5,
            number: 1,
            usage: 1000,
            remaining: 900,
            percentage: 10,
            nextResetTime: 1795564295000,
          },
          {
            type: "TOKENS_LIMIT",
            unit: 3,
            number: 5,
            percentage: 33,
            nextResetTime: 1795260928000,
          },
          { type: "SOMETHING_ELSE", unit: 1, number: 1, percentage: 99 },
        ],
      },
    });
    assert.strictEqual(r.plan, "Pro");
    assert.deepStrictEqual(
      r.windows.map((w) => [w.label, w.kind, w.usedPercent]),
      [
        ["5-hour", "session", 33],
        ["MCP", "monthly", 10],
      ],
    );
  });
  it("parses Muse subscription usage into session and weekly windows", () => {
    const r = parseMuse({
      subs_tier_name: "Muse Code Everyday Usage",
      user_email: "z@example.com",
      subs_usage: {
        window: { used_percent: 155, window_duration_mins: 300, resets_at: 1795272648 },
        weekly: { used_percent: 51, resets_at: 1795392000 },
      },
    });
    assert.deepStrictEqual(
      r.windows.map((w) => [w.kind, w.windowMinutes, w.usedPercent]),
      [
        ["session", 300, 100],
        ["weekly", 10080, 51],
      ],
    );
    assert.strictEqual(r.account, "z@example.com");
  });
  it("copilot skips a quota the plan has no entitlement to and keeps the account", () => {
    const r = parseCopilot({
      login: "octo",
      copilot_plan: "individual",
      quota_reset_date: "2026-12-01",
      quota_snapshots: {
        premium_interactions: { has_quota: false, percent_remaining: 0 },
        chat: { has_quota: true, percent_remaining: 100 },
        completions: { has_quota: true, percent_remaining: 75 },
      },
    });
    assert.deepStrictEqual(
      r.windows.map((w) => [w.label, w.usedPercent]),
      [
        ["Chat", 0],
        ["Completions", 25],
      ],
    );
    assert.deepStrictEqual([r.plan, r.account], ["Individual", "octo"]);
  });
  it("never reads muse's keychain item on a refresh, only when the user connects", async () => {
    const calls: string[] = [];
    let keychainReads = 0;
    const env = fakeEnv(
      {},
      {},
      {},
      {
        keychain: async () => {
          keychainReads++;
          return JSON.stringify({ access_token: "dca:abc" });
        },
        fetch: (async (url: string | URL | Request) => {
          calls.push(String(url));
          return new Response(
            JSON.stringify({ is_subs_active: true, subs_usage: { weekly: { used_percent: 5 } } }),
            { status: 200 },
          );
        }) as typeof fetch,
      },
    );
    let refused = "";
    await FETCHERS.muse!(env, []).catch((e: Error) => {
      refused = e.message;
    });
    assert.strictEqual(keychainReads, 0);
    assert.deepStrictEqual(calls, []);
    assert.include(refused, "Connect Muse");
    const [r] = await CONNECTORS.muse!(env, []);
    assert.strictEqual(keychainReads, 1);
    assert.deepStrictEqual(calls, ["https://api.meta.ai/muse-code/key"]);
    assert.strictEqual(r!.windows[0]!.usedPercent, 5);
  });
  it("parses hanzo tier into the plan", () => {
    assert.strictEqual(
      parseHanzo({ available: 100 }, { tier: { displayName: "Pro" }, planName: "Pro" }).plan,
      "Pro",
    );
  });
  it("names an account by instance and email", () => {
    assert.strictEqual(
      accountName({ label: "Work", account: "z@example.com" }, "Claude"),
      "Claude · Work (z@example.com)",
    );
    assert.strictEqual(
      accountName({ account: "z@example.com" }, "Cursor"),
      "Cursor (z@example.com)",
    );
    assert.strictEqual(accountName({}, "Codex"), "Codex");
  });
});

describe("accounts", () => {
  const credential = (expiresAt: number) =>
    JSON.stringify({
      claudeAiOauth: {
        accessToken: "a",
        expiresAt,
        subscriptionType: "max",
        rateLimitTier: "default_claude_max_20x",
      },
    });
  const profile = (email: string) => JSON.stringify({ oauthAccount: { emailAddress: email } });
  const usage = { seven_day: { utilization: 37, resets_at: "2026-11-27T01:00:00Z" } };

  it("reads each Claude instance on its own, and offers an unused config dir instead of merging it", async () => {
    const keychain = async (service: string) =>
      service === "Claude Code-credentials" ? credential(NOW.getTime() + 3_600_000) : undefined;
    const env = fakeEnv(
      {
        "/h/.claude.json": profile("work@example.com"),
        "/h/.claude-extra/.claude.json": profile("extra@example.com"),
        "/h/.claude-extra/.credentials.json": credential(1),
      },
      { "https://api.anthropic.com/api/oauth/usage": usage },
      {},
      { keychain },
    );
    const [work, extra] = await FETCHERS.claude!(env, [{ id: "claudeAgent", name: "Work" }]);
    assert.deepStrictEqual(
      [work!.label, work!.account, work!.plan, work!.windows[0]!.usedPercent],
      ["Work", "work@example.com", "Max 20x", 37],
    );
    assert.isUndefined(work!.unconfigured);
    assert.deepStrictEqual(
      [extra!.instance, extra!.account, extra!.unconfigured, extra!.error],
      ["dir:/h/.claude-extra", "extra@example.com", { path: "/h/.claude-extra" }, "Sign in again."],
    );
    assert.isTrue(isSnapshot(work) && isSnapshot(extra));
  });
  it("a configured instance with its own home is not offered again", async () => {
    const env = fakeEnv(
      {
        "/h/.claude-extra/.claude.json": profile("extra@example.com"),
        "/h/.claude-extra/.credentials.json": credential(NOW.getTime() + 1e6),
      },
      { "https://api.anthropic.com/api/oauth/usage": usage },
    );
    const rows = await FETCHERS.claude!(env, [
      { id: "claude_extra", name: "Extra", home: "~/.claude-extra" },
    ]);
    assert.strictEqual(rows.length, 1);
    assert.isUndefined(rows[0]!.unconfigured);
    assert.strictEqual(rows[0]!.instance, "claude_extra");
  });
  it("antigravity runs agy in a scratch HOME holding a copy of the token, never the real one", async () => {
    let staged: Record<string, string> = {};
    let ran: { file: string; home: string | undefined } | undefined;
    const report = JSON.stringify({
      command: {
        data: {
          groups: [
            { name: "Gemini Models", buckets: [{ window: "weekly", remaining_fraction: 0.5 }] },
          ],
        },
      },
    });
    const env = fakeEnv(
      {
        "/h/.gemini/jetski-standalone-oauth-token": JSON.stringify({
          id_token_email: "z@example.com",
          token: { access_token: "x" },
        }),
      },
      {},
      {},
      {
        withScratch: async (files, run) => {
          staged = { ...files };
          return run("/scratch");
        },
        exec: async (file, _args, o) => {
          ran = { file, home: o.env.HOME };
          return report;
        },
      },
    );
    const [r] = await FETCHERS.antigravity!(env, []);
    assert.strictEqual(r!.account, "z@example.com");
    assert.strictEqual(r!.windows[0]!.usedPercent, 50);
    assert.strictEqual(ran!.home, "/scratch");
    assert.isTrue(".gemini/antigravity-cli/antigravity-oauth-token" in staged);
  });
  it("antigravity without a sign-in says so", async () => {
    let msg = "";
    await FETCHERS.antigravity!(fakeEnv({}, {}), []).catch((e: Error) => (msg = e.message));
    assert.match(msg, /Sign in to Antigravity/);
  });
  it("cursor builds the session cookie from the app's own login and sends it to cursor.com", async () => {
    const token = jwt({ sub: "auth0|user_123", exp: IN_AN_HOUR });
    let cookie = "";
    const env = fakeEnv(
      {},
      {},
      {},
      {
        sqliteValue: async (_f, _q, key) =>
          key === "cursorAuth/accessToken" ? token : "z@example.com",
        fetch: (async (_u: string | URL | Request, init?: RequestInit) => {
          cookie = String((init!.headers as Record<string, string>).Cookie);
          return new Response(
            JSON.stringify({
              membershipType: "free",
              individualUsage: { plan: { totalPercentUsed: 0 } },
            }),
            { status: 200 },
          );
        }) as typeof fetch,
      },
    );
    const [r] = await FETCHERS.cursor!(env, []);
    assert.isTrue(cookie.startsWith("WorkosCursorSessionToken=user_123%3A%3A"));
    assert.deepStrictEqual(
      [r!.account, r!.plan, r!.windows[0]!.usedPercent],
      ["z@example.com", "Free", 0],
    );
  });
  it("z.ai reports a key it rejects in the body, in plain words", async () => {
    const env = fakeEnv(
      {},
      {
        "https://api.z.ai/api/monitor/usage/quota/limit": {
          code: 401,
          msg: "token expired or incorrect",
          success: false,
        },
      },
      { Z_AI_API_KEY: "k" },
    );
    const [r] = await FETCHERS.zai!(env, []);
    assert.match(r!.error ?? "", /rejected this API key/);
  });
  it("providers with no usage API say so", async () => {
    const svc = createUsageService(fakeEnv({}, {}), () => ({
      providers: { grok: { enabled: true } },
      trayMode: "merged",
      showInMenuBar: false,
      notifyAtPercent: [],
      refreshSeconds: 300,
    }));
    const { summary } = await svc.refresh({ instances: [], local: [], cost: [] });
    assert.match(summary.snapshots.find((x) => x.provider === "grok")!.error ?? "", /No usage API/);
  });
  it("reads provider API keys supplied via HostReadings.env", async () => {
    let authHeader = "";
    const env = fakeEnv(
      {},
      {},
      {},
      {
        fetch: (async (url, init) => {
          if (String(url).includes("/credits")) {
            authHeader = String(
              (init?.headers as Record<string, string> | undefined)?.Authorization,
            );
          }
          return new Response(JSON.stringify({ data: { total_credits: 50, total_usage: 10 } }), {
            status: 200,
          });
        }) as typeof fetch,
      },
    );
    const svc = createUsageService(env, () => ({
      providers: {
        openrouter: { enabled: true },
        claude: { enabled: false },
        codex: { enabled: false },
        copilot: { enabled: false },
      },
      trayMode: "merged",
      showInMenuBar: false,
      notifyAtPercent: [],
      refreshSeconds: 300,
    }));
    const { summary } = await svc.refresh({
      instances: [],
      local: [],
      cost: [],
      env: { OPENROUTER_API_KEY: "sk-from-instance" },
    });
    const or = summary.snapshots.find((x) => x.provider === "openrouter");
    assert.strictEqual(authHeader, "Bearer sk-from-instance");
    assert.strictEqual(or?.source, "api");
    assert.strictEqual(or?.windows[0]?.usedPercent, 20);
    assert.strictEqual(or?.credits?.balance, 40);
  });
});
