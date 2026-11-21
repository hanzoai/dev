// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
import { CATALOG, providerById } from "./catalog.ts";
import type { UsageEnv } from "./env.ts";
import { FETCHERS, NotConfigured } from "./fetchers.ts";
import { evaluate, type LimitAlert, type NotifyState } from "./notify.ts";
import type {
  CostDay,
  ProviderSnapshot,
  UsageReport,
  UsageSettings,
  UsageSummary,
} from "./schema.ts";
import { fetchStatus } from "./status.ts";

/** Providers shown before the user chooses: the ones Dev users link. */
export const DEFAULT_ENABLED: ReadonlyArray<string> = ["hanzo", "claude", "codex", "copilot"];

export const isEnabled = (settings: UsageSettings, id: string): boolean =>
  settings.providers[id]?.enabled ?? DEFAULT_ENABLED.includes(id);

/**
 * What the host already knows without a network call here: limit windows from the provider sessions it runs,
 * and per-day cost from the transcripts it already scans.
 */
export interface HostReadings {
  readonly local: ReadonlyArray<ProviderSnapshot>;
  readonly cost: ReadonlyArray<CostDay>;
}

const refreshOne = async (
  env: UsageEnv,
  id: string,
  local: ProviderSnapshot | undefined,
): Promise<ProviderSnapshot> => {
  const fetcher = FETCHERS[id];
  const p = providerById(id);
  const fetchedAt = env.now().toISOString();
  const usage: Promise<ProviderSnapshot> = local
    ? Promise.resolve(local)
    : fetcher
      ? fetcher(env).catch((e: unknown): ProviderSnapshot => ({
          provider: id,
          fetchedAt,
          source: "none",
          windows: [],
          error:
            e instanceof NotConfigured ? e.message : e instanceof Error ? e.message : String(e),
        }))
      : Promise.resolve({
          provider: id,
          fetchedAt,
          source: "none",
          windows: [],
          error: "No live reading yet; open the dashboard.",
        });
  const [snapshot, status] = await Promise.all([usage, p ? fetchStatus(env, p) : undefined]);
  return status ? { ...snapshot, status } : snapshot;
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
    const ids = CATALOG.map((p) => p.id).filter((id) => isEnabled(settings, id));
    const fresh = await Promise.all(
      ids.map((id) =>
        refreshOne(
          env,
          id,
          host.local.find((s) => s.provider === id),
        ),
      ),
    );
    for (const s of fresh) {
      const prev = last.get(s.provider);
      last.set(
        s.provider,
        s.error && prev && prev.windows.length
          ? { ...prev, error: s.error, ...(s.status ? { status: s.status } : {}) }
          : s,
      );
    }
    cost = host.cost;
    const snapshots = ids.flatMap((id) => (last.has(id) ? [last.get(id)!] : []));
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
    snapshots: [...last.values()].map(({ error: _e, ...rest }) => rest),
  });

  return { refresh, report };
};
