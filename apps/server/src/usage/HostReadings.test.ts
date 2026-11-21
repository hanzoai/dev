import { assert, describe, it } from "@effect/vitest";
import type { ServerProvider, UsageBucket } from "@hanzodev/contracts";

import { catalogId, costDays, snapshotOf } from "./HostReadings.ts";

const provider = (driver: string, windows: unknown[]) =>
  ({
    driver,
    usageLimits: { checkedAt: "2026-11-20T00:00:00Z", windows },
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
    assert.deepStrictEqual(s?.windows[0], {
      kind: "session",
      label: "Session",
      usedPercent: 42,
      windowMinutes: 300,
      resetsAt: "2026-11-20T05:00:00Z",
    });
    assert.isUndefined(snapshotOf(provider("codex", [])));
    assert.strictEqual(catalogId("codex"), "codex");
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
});
