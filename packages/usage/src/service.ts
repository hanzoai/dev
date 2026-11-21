import { snapshotKey } from "./account.ts";
import { CATALOG, DEFAULT_ENABLED, providerById } from "./catalog.ts";
import type { UsageEnv } from "./env.ts";
import { NotConfigured, type Instance } from "./fetchers.ts";
import { FETCHERS } from "./readers.ts";
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

/** Holds the last good snapshot per provider so a failed refresh never blanks the panel. */
export const createUsageService = (env: UsageEnv, getSettings: () => UsageSettings) => {
  const last = new Map<string, ProviderSnapshot>();
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
        refreshOne(
          env,
          id,
          host.local.filter((s) => s.provider === id),
          host.instances.filter((i) => i.provider === id),
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

  return { refresh, report };
};
