// @effect-diagnostics globalDate:off globalDateInEffect:off
import { UsageDay, type ServerProvider, type UsageBucket } from "@hanzodev/contracts";
import type { CostDay, HostReadings, ProviderSnapshot, UsageWindow } from "@hanzodev/usage";
import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";

import { ProviderRegistry } from "../provider/ProviderRegistry.ts";
import * as UsageService from "./UsageService.ts";

const COST_DAYS = 30;
const decodeDay = Schema.decodeSync(UsageDay);

/** The catalog id for a provider driver: `claudeAgent` and `claude` are both Claude. */
export const catalogId = (driver: string): string =>
  driver.toLowerCase().startsWith("claude") ? "claude" : driver.toLowerCase();

export const snapshotOf = (p: ServerProvider): ProviderSnapshot | undefined => {
  const limits = p.usageLimits;
  if (!limits || limits.windows.length === 0) return undefined;
  const windows: UsageWindow[] = limits.windows.map((w) => ({
    kind: w.kind,
    label: w.label,
    usedPercent: w.usedPercent,
    ...(w.windowDurationMins ? { windowMinutes: w.windowDurationMins } : {}),
    ...(w.resetsAt ? { resetsAt: w.resetsAt } : {}),
  }));
  return { provider: catalogId(p.driver), fetchedAt: limits.checkedAt, source: "local", windows };
};

export const costDays = (buckets: ReadonlyArray<UsageBucket>): CostDay[] => {
  const by = new Map<string, CostDay>();
  for (const b of buckets) {
    const key = `${b.day}|${b.provider}|${b.model}`;
    const prev = by.get(key);
    by.set(key, {
      day: b.day,
      provider: catalogId(b.provider),
      model: b.model,
      inputTokens: (prev?.inputTokens ?? 0) + b.totals.uncachedInputTokens,
      outputTokens: (prev?.outputTokens ?? 0) + b.totals.outputTokens,
      cacheTokens:
        (prev?.cacheTokens ?? 0) + b.totals.cachedInputTokens + b.totals.cacheCreationTokens,
      costUsd: (prev?.costUsd ?? 0) + b.costUsd,
    });
  }
  return [...by.values()].toSorted((a, b) =>
    a.day < b.day ? 1 : a.day > b.day ? -1 : a.model.localeCompare(b.model),
  );
};

/** Limit windows from the provider sessions this server runs, and per-day cost from the transcripts it already scans. */
export const readHostReadings = Effect.gen(function* () {
  const registry = yield* ProviderRegistry;
  const usage = yield* UsageService.UsageService;
  const providers = yield* registry.getProviders;
  const now = new Date();
  const summary = yield* usage
    .readSummary({
      sinceDay: decodeDay(
        new Date(now.getTime() - COST_DAYS * 86_400_000).toISOString().slice(0, 10),
      ),
      untilDay: decodeDay(now.toISOString().slice(0, 10)),
      timeZone: "UTC",
      provisional: true,
    })
    .pipe(Effect.orElseSucceed(() => undefined));
  return {
    local: providers.flatMap((p) => {
      const s = snapshotOf(p);
      return s ? [s] : [];
    }),
    cost: summary ? costDays(summary.buckets) : [],
  } satisfies HostReadings;
});
