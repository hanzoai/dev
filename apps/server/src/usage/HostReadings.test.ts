import { assert, describe, it } from "@effect/vitest";
import {
  SecretProviderId,
  type ServerProvider,
  type ServerSettings,
  type UsageBucket,
} from "@hanzodev/contracts";

import { catalogId, collectUsageEnvFromSettings, costDays, snapshotOf } from "./HostReadings.ts";

const provider = (driver: string, windows: unknown[], unavailable?: unknown) =>
  ({
    driver,
    instanceId: `${driver}-work`,
    displayName: "Work",
    usageLimits: { checkedAt: "2026-11-20T00:00:00Z", windows, unavailable },
  }) as unknown as ServerProvider;

describe("HostReadings", () => {
  it("maps provider limit windows onto the usage model", () => {
    const s = snapshotOf(
      provider("claudeAgent", [
        {
          id: "a",
          kind: "session",
          label: "Session",
          usedPercent: 42,
          windowDurationMins: 300,
          resetsAt: "2026-11-20T05:00:00Z",
        },
      ]),
    );
    assert.strictEqual(s?.provider, "claude");
    assert.deepStrictEqual([s?.instance, s?.label], ["claudeAgent-work", "Work"]);
    assert.deepStrictEqual(s?.windows[0], {
      kind: "session",
      label: "Session",
      usedPercent: 42,
      windowMinutes: 300,
      resetsAt: "2026-11-20T05:00:00Z",
    });
    assert.isUndefined(snapshotOf(provider("codex", [])));
    assert.strictEqual(catalogId("codex"), "codex");
    assert.strictEqual(catalogId("hanzo"), "hanzo");
    assert.strictEqual(catalogId("hanzoGen"), "hanzo");
  });
  it("carries the driver's own reason when it has no windows", () => {
    const s = snapshotOf(
      provider("opencode", [], {
        reason: "probeFailed",
        message: "OpenCode Console sign-in expired.",
      }),
    );
    assert.strictEqual(s?.error, "OpenCode Console sign-in expired.");
    assert.strictEqual(s?.source, "none");
  });
  it("rolls transcript buckets into per-day model cost", () => {
    const b = (day: string, out: number, cost: number) =>
      ({
        day,
        provider: "claude",
        model: "m",
        totals: {
          uncachedInputTokens: 1,
          cachedInputTokens: 2,
          cacheCreationTokens: 3,
          outputTokens: out,
          reasoningTokens: 0,
        },
        costUsd: cost,
      }) as unknown as UsageBucket;
    const days = costDays([
      b("2026-11-19", 5, 0.5),
      b("2026-11-19", 7, 0.25),
      b("2026-11-18", 1, 0.1),
    ]);
    assert.strictEqual(days.length, 2);
    assert.deepStrictEqual(
      [days[0]!.day, days[0]!.outputTokens, days[0]!.cacheTokens, days[0]!.costUsd],
      ["2026-11-19", 12, 10, 0.75],
    );
  });
  it("extracts usage API keys and secret references from enabled provider instance environments", () => {
    const ref = {
      provider: SecretProviderId.make("onepassword"),
      reference: "op://vault/deepseek/credential",
    };
    const settings = {
      providerInstances: {
        disabled: {
          driver: "codex",
          enabled: false,
          environment: [{ name: "OPENROUTER_API_KEY", value: "sk-disabled", sensitive: true }],
        },
        work: {
          driver: "codex",
          enabled: true,
          environment: [
            { name: "OPENROUTER_API_KEY", value: "  sk-or-secret  ", sensitive: true },
            { name: "ZAI_API_KEY", value: "sk-zai", sensitive: true },
            { name: "DEEPSEEK_API_KEY", value: ref, sensitive: true },
            { name: "UNRELATED_SECRET", value: "should-not-leak", sensitive: true },
          ],
        },
      },
    } as unknown as ServerSettings;

    const { env, secretRefs } = collectUsageEnvFromSettings(settings);
    assert.deepStrictEqual(env, {
      OPENROUTER_API_KEY: "sk-or-secret",
      ZAI_API_KEY: "sk-zai",
    });
    assert.deepStrictEqual(secretRefs, [{ name: "DEEPSEEK_API_KEY", reference: ref }]);
  });
  it("keeps per-model token use from Hanzo driver transcripts in the Cost view", () => {
    const bucket = (providerName: string, model: string, input: number, output: number) =>
      ({
        day: "2026-11-20",
        provider: providerName,
        model,
        totals: {
          uncachedInputTokens: input,
          cachedInputTokens: 100,
          cacheCreationTokens: 0,
          outputTokens: output,
          reasoningTokens: 0,
        },
        costUsd: 0.42,
      }) as unknown as UsageBucket;
    const days = costDays([
      bucket("hanzo", "zen5", 1000, 250),
      bucket("hanzo", "claude-sonnet-4-6", 500, 120),
      bucket("hanzoGen", "zen5-mini", 300, 80),
    ]);
    assert.deepStrictEqual(
      days.map((d) => ({
        provider: d.provider,
        model: d.model,
        inputTokens: d.inputTokens,
        outputTokens: d.outputTokens,
      })),
      [
        { provider: "hanzo", model: "claude-sonnet-4-6", inputTokens: 500, outputTokens: 120 },
        { provider: "hanzo", model: "zen5", inputTokens: 1000, outputTokens: 250 },
        { provider: "hanzo", model: "zen5-mini", inputTokens: 300, outputTokens: 80 },
      ],
    );
  });
});
