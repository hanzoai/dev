// @effect-diagnostics globalDate:off globalDateInEffect:off
import {
  isSecretReference,
  UsageDay,
  type SecretReference,
  type ServerProvider,
  type ServerSettings,
  type UsageBucket,
} from "@hanzodev/contracts";
import * as SecretResolverCore from "@hanzodev/secret-provider-core/server/SecretResolver";
import {
  USAGE_ENV_KEYS,
  type CostDay,
  type HostReadings,
  type ProviderSnapshot,
  type UsageWindow,
} from "@hanzodev/usage";
import * as Effect from "effect/Effect";
import * as Option from "effect/Option";
import * as Schema from "effect/Schema";

import { sharedIdentityHost } from "../provider/hanzoLocalIdentity.ts";
import { hanzoAccessToken } from "../provider/hanzoSharedIdentityNode.ts";
import { ProviderRegistry } from "../provider/ProviderRegistry.ts";
import { ServerSettingsService } from "../serverSettings.ts";
import * as UsageService from "./UsageService.ts";

const COST_DAYS = 30;
const decodeDay = Schema.decodeSync(UsageDay);
const USAGE_ENV_KEY_SET = new Set<string>(USAGE_ENV_KEYS);

/**
 * Extracts known usage API key environment variables from enabled provider instances.
 * String values already include secrets hydrated from Dev's ServerSecretStore; external
 * SecretReferences are returned separately for batch resolution via SecretResolver.
 */
export const collectUsageEnvFromSettings = (
  settings: ServerSettings | undefined,
): {
  readonly env: Record<string, string>;
  readonly secretRefs: ReadonlyArray<{
    readonly name: string;
    readonly reference: SecretReference;
  }>;
} => {
  const env: Record<string, string> = {};
  const secretRefs: Array<{ readonly name: string; readonly reference: SecretReference }> = [];
  const seenRefs = new Set<string>();
  for (const instance of Object.values(settings?.providerInstances ?? {})) {
    if (instance.enabled === false) continue;
    for (const variable of instance.environment ?? []) {
      if (!USAGE_ENV_KEY_SET.has(variable.name)) continue;
      if (typeof variable.value === "string") {
        const trimmed = variable.value.trim();
        if (trimmed && !(variable.name in env)) {
          env[variable.name] = trimmed;
        }
      } else if (
        isSecretReference(variable.value) &&
        !(variable.name in env) &&
        !seenRefs.has(variable.name)
      ) {
        seenRefs.add(variable.name);
        secretRefs.push({ name: variable.name, reference: variable.value });
      }
    }
  }
  return { env, secretRefs };
};

/** The catalog id for a provider driver: `claudeAgent` -> `claude`, `hanzoGen` -> `hanzo`. */
export const catalogId = (driver: string): string => {
  const lower = driver.toLowerCase();
  if (lower.startsWith("claude")) return "claude";
  if (lower.startsWith("hanzo")) return "hanzo";
  return lower;
};

const who = (p: ServerProvider) => ({
  instance: p.instanceId,
  ...(p.displayName ? { label: p.displayName } : {}),
});

export const snapshotOf = (p: ServerProvider): ProviderSnapshot | undefined => {
  const limits = p.usageLimits;
  if (!limits) return undefined;
  // The driver's own reason ("not signed in", "no usage API") beats a generic one.
  if (limits.windows.length === 0)
    return limits.unavailable?.message
      ? {
          provider: catalogId(p.driver),
          ...who(p),
          fetchedAt: limits.checkedAt,
          source: "none",
          windows: [],
          error: limits.unavailable.message,
        }
      : undefined;
  const windows: UsageWindow[] = limits.windows.map((w) => ({
    kind: w.kind,
    label: w.label,
    usedPercent: w.usedPercent,
    ...(w.windowDurationMins ? { windowMinutes: w.windowDurationMins } : {}),
    ...(w.resetsAt ? { resetsAt: w.resetsAt } : {}),
  }));
  return {
    provider: catalogId(p.driver),
    ...who(p),
    fetchedAt: limits.checkedAt,
    source: "local",
    windows,
  };
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
  const settings = yield* (yield* ServerSettingsService).getSettings.pipe(
    Effect.orElseSucceed(() => undefined),
  );
  const { env, secretRefs } = collectUsageEnvFromSettings(settings);
  if (secretRefs.length > 0) {
    const resolver = yield* Effect.serviceOption(SecretResolverCore.SecretResolver);
    if (Option.isSome(resolver)) {
      const resolved = yield* resolver.value
        .read(secretRefs.map(({ reference }) => reference))
        .pipe(Effect.orElseSucceed((): ReadonlyArray<string | undefined> => []));
      for (let i = 0; i < secretRefs.length; i++) {
        const value = resolved[i]?.trim();
        const entry = secretRefs[i];
        if (value && entry && !(entry.name in env)) {
          env[entry.name] = value;
        }
      }
    }
  }
  const identityHost = yield* sharedIdentityHost();
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
    instances: providers.map((p) => {
      const config = settings?.providerInstances[p.instanceId]?.config;
      const text = (key: string) => {
        const v =
          typeof config === "object" && config !== null ? Reflect.get(config, key) : undefined;
        return typeof v === "string" && v.trim() ? v.trim() : undefined;
      };
      const home = p.runtimePaths?.homePath ?? text("homePath");
      const binary = text("binaryPath");
      return {
        provider: catalogId(p.driver),
        id: p.instanceId,
        name: p.displayName ?? p.instanceId,
        ...(home ? { home } : {}),
        ...(binary ? { binary } : {}),
      };
    }),
    local: providers.flatMap((p) => {
      const s = snapshotOf(p);
      return s ? [s] : [];
    }),
    cost: summary ? costDays(summary.buckets) : [],
    ...(Object.keys(env).length > 0 ? { env } : {}),
    hanzoToken: () => hanzoAccessToken(identityHost),
  } satisfies HostReadings;
});
