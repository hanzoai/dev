import { snapshotKey } from "./account.ts";
import { CATALOG, DEFAULT_ENABLED, providerById } from "./catalog.ts";
import type { UsageEnv } from "./env.ts";
import { NotConfigured, type Instance } from "./fetchers.ts";
import { CONNECTORS, FETCHERS } from "./readers.ts";
import { evaluate, type LimitAlert, type NotifyState } from "./notify.ts";
import type {
  CostDay,
  ProviderSnapshot,
  UsageReport,
  UsageSettings,
  UsageSummary,
} from "./schema.ts";
import { fetchStatus } from "./status.ts";

/** Shown before the user chooses: the defaults, and any provider the user configured an instance of. */
export const isEnabled = (settings: UsageSettings, id: string, configured = false): boolean =>
  settings.providers[id]?.enabled ?? (configured || DEFAULT_ENABLED.includes(id));

/**
 * What the host already knows without a network call here: the provider instances the user configured,
 * limit windows from the provider sessions it runs, and per-day cost from the transcripts it already scans.
 */
export interface HostReadings {
  readonly instances: ReadonlyArray<Instance & { readonly provider: string }>;
  readonly local: ReadonlyArray<ProviderSnapshot>;
  readonly cost: ReadonlyArray<CostDay>;
}

const failure = (env: UsageEnv, provider: string, error: string): ProviderSnapshot => ({
  provider,
  fetchedAt: env.now().toISOString(),
  source: "none",
  windows: [],
  error,
});

/** Directly read accounts win; a session's own numbers stand in where the direct read failed. */
const merge = (
  read: ReadonlyArray<ProviderSnapshot>,
  local: ReadonlyArray<ProviderSnapshot>,
): ProviderSnapshot[] => {
  const byKey = new Map(local.map((s) => [snapshotKey(s), s]));
  const out = read.map((s) => {
    const l = byKey.get(snapshotKey(s));
    byKey.delete(snapshotKey(s));
    return s.error && l && !l.error ? l : s;
  });
  return [...out, ...byKey.values()];
};

const refreshOne = async (
  env: UsageEnv,
  id: string,
  local: ReadonlyArray<ProviderSnapshot>,
  instances: ReadonlyArray<Instance>,
): Promise<ProviderSnapshot[]> => {
  const p = providerById(id);
  const fetcher = FETCHERS[id];
  const read = fetcher
    ? await fetcher(env, instances).catch((e: unknown) => [
        failure(env, id, e instanceof NotConfigured || e instanceof Error ? e.message : String(e)),
      ])
    : [];
  const snapshots = merge(read, local);
  if (snapshots.length === 0)
    snapshots.push(
      failure(
        env,
        id,
        p?.dashboard
          ? "No usage API for this provider; open its dashboard."
          : "No usage API for this provider.",
      ),
    );
  const status = p ? await fetchStatus(env, p) : undefined;
  return status ? snapshots.map((s) => ({ ...s, status })) : snapshots;
};

/** The longest the panel waits for one provider; a slower reader keeps running and lands on a later refresh. */
export const READ_BUDGET_MS = 20_000;

/** Holds the last good snapshot per provider so a failed refresh never blanks the panel. */
export const createUsageService = (
  env: UsageEnv,
  getSettings: () => UsageSettings,
  budgetMs: number = READ_BUDGET_MS,
) => {
  const last = new Map<string, ProviderSnapshot>();
  const reading = new Map<string, Promise<ProviderSnapshot[]>>();

  /** One read per provider at a time, so a slow provider never piles up processes. */
  const budgeted = (
    id: string,
    start: () => Promise<ProviderSnapshot[]>,
  ): Promise<ProviderSnapshot[]> => {
    let read = reading.get(id);
    if (!read) {
      read = start().finally(() => reading.delete(id));
      reading.set(id, read);
    }
    let timer: ReturnType<typeof setTimeout> | undefined;
    const late = new Promise<ProviderSnapshot[]>((resolve) => {
      // @effect-diagnostics-next-line globalTimers:off - a Promise read budget, outside any Effect runtime.
      timer = setTimeout(() => {
        const previous = [...last.values()].filter((s) => s.provider === id);
        resolve(
          previous.length > 0
            ? previous.map((s) => ({ ...s, error: "Still updating; showing the last reading." }))
            : [failure(env, id, "Still reading usage; this provider is slow to answer.")],
        );
      }, budgetMs);
    });
    return Promise.race([read, late]).finally(() => clearTimeout(timer));
  };
  let notified: NotifyState = new Map();
  let cost: ReadonlyArray<CostDay> = [];

  const refresh = async (
    host: HostReadings,
  ): Promise<{ summary: UsageSummary; alerts: LimitAlert[] }> => {
    const settings = getSettings();
    const ids = CATALOG.map((p) => p.id).filter((id) =>
      isEnabled(
        settings,
        id,
        host.instances.some((i) => i.provider === id),
      ),
    );
    const fresh = await Promise.all(
      ids.map((id) =>
        budgeted(id, () =>
          refreshOne(
            env,
            id,
            host.local.filter((s) => s.provider === id),
            host.instances.filter((i) => i.provider === id),
          ),
        ),
      ),
    );
    const seen = new Set<string>();
    for (const s of fresh.flat()) {
      const key = snapshotKey(s);
      seen.add(key);
      const prev = last.get(key);
      last.set(
        key,
        s.error && prev && prev.windows.length
          ? { ...prev, error: s.error, ...(s.status ? { status: s.status } : {}) }
          : s,
      );
    }
    // An account that is gone (instance removed, directory deleted) must not linger.
    for (const key of last.keys()) if (!seen.has(key)) last.delete(key);
    cost = host.cost;
    const snapshots = fresh.flat().map((s) => last.get(snapshotKey(s))!);
    const ev = evaluate(notified, snapshots, settings.notifyAtPercent);
    notified = ev.state;
    return {
      summary: { generatedAt: env.now().toISOString(), snapshots, cost },
      alerts: ev.alerts,
    };
  };

  /** The user's "Connect" for a provider whose login is in another app's keychain. */
  const connect = async (
    id: string,
    instances: ReadonlyArray<Instance>,
  ): Promise<ProviderSnapshot[]> => {
    const connector = CONNECTORS[id];
    if (!connector) return [failure(env, id, "This provider has no connect step.")];
    return [
      ...(await connector(env, instances).catch((e: unknown) => [
        failure(env, id, e instanceof Error ? e.message : String(e)),
      ])),
    ];
  };

  /** What a linked desktop reports to the cloud registry: metadata only. */
  const report = (deviceId: string, since: Date): UsageReport => ({
    version: 1,
    deviceId,
    from: since.toISOString(),
    to: env.now().toISOString(),
    cost: cost.filter((c) => c.day >= since.toISOString().slice(0, 10)),
    // Local paths and error text stay on this machine.
    snapshots: [...last.values()].map(({ error: _e, unconfigured: _u, instance, ...rest }) => ({
      ...rest,
      ...(instance && !instance.startsWith("dir:") ? { instance } : {}),
    })),
  });

  return { refresh, report, connect };
};
