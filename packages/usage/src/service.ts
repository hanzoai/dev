// @effect-diagnostics nodeBuiltinImport:off globalDate:off extendsNativeError:off
import { CATALOG, providerById } from "./catalog.ts";
import { rollupDays, scanEvents } from "./cost.ts";
import type { UsageEnv } from "./env.ts";
import { FETCHERS, NotConfigured } from "./fetchers.ts";
import { evaluate, type LimitAlert, type NotifyState } from "./notify.ts";
import type {
  ProviderSnapshot,
  UsageEvent,
  UsageReport,
  UsageSettings,
  UsageSummary,
} from "./schema.ts";
import { fetchStatus } from "./status.ts";

/** Providers shown before the user chooses: the ones Dev users link. */
export const DEFAULT_ENABLED: ReadonlyArray<string> = ["hanzo", "claude", "codex", "copilot"];

export const isEnabled = (settings: UsageSettings, id: string): boolean =>
  settings.providers[id]?.enabled ?? DEFAULT_ENABLED.includes(id);

const refreshOne = async (env: UsageEnv, id: string): Promise<ProviderSnapshot> => {
  const fetcher = FETCHERS[id];
  const p = providerById(id);
  const fetchedAt = env.now().toISOString();
  if (!fetcher || !p) {
    return {
      provider: id,
      fetchedAt,
      source: "none",
      windows: [],
      error: "No live fetcher yet; open the dashboard.",
    };
  }
  const [usage, status] = await Promise.all([
    fetcher(env).catch((e: unknown): ProviderSnapshot => ({
      provider: id,
      fetchedAt,
      source: "none",
      windows: [],
      error: e instanceof NotConfigured ? e.message : e instanceof Error ? e.message : String(e),
    })),
    fetchStatus(env, p),
  ]);
  return status ? { ...usage, status } : usage;
};

/** Holds the last good snapshot per provider so a failed refresh never blanks the panel. */
export const createUsageService = (env: UsageEnv, getSettings: () => UsageSettings) => {
  const last = new Map<string, ProviderSnapshot>();
  let notified: NotifyState = new Map();
  let events: UsageEvent[] = [];

  const refresh = async (): Promise<{ summary: UsageSummary; alerts: LimitAlert[] }> => {
    const settings = getSettings();
    const ids = CATALOG.map((p) => p.id).filter((id) => isEnabled(settings, id));
    const fresh = await Promise.all(ids.map((id) => refreshOne(env, id)));
    for (const s of fresh) {
      const prev = last.get(s.provider);
      last.set(
        s.provider,
        s.error && prev && prev.windows.length
          ? { ...prev, error: s.error, ...(s.status ? { status: s.status } : {}) }
          : s,
      );
    }
    events = await scanEvents(env, new Date(env.now().getTime() - 30 * 86_400_000));
    const snapshots = ids.flatMap((id) => (last.has(id) ? [last.get(id)!] : []));
    const ev = evaluate(notified, snapshots, settings.notifyAtPercent);
    notified = ev.state;
    return {
      summary: { generatedAt: env.now().toISOString(), snapshots, cost: rollupDays(events) },
      alerts: ev.alerts,
    };
  };

  /** What a linked desktop reports to the cloud registry: metadata only. */
  const report = (deviceId: string, since: Date): UsageReport => ({
    version: 1,
    deviceId,
    from: since.toISOString(),
    to: env.now().toISOString(),
    events: events
      .filter((e) => Date.parse(e.at) >= since.getTime())
      .map(({ content: _c, ...rest }) => rest),
    snapshots: [...last.values()].map(({ error: _e, ...rest }) => rest),
  });

  return { refresh, report };
};
