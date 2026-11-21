// @effect-diagnostics nodeBuiltinImport:off globalDate:off
import * as NodeFSP from "node:fs/promises";
import * as NodePath from "node:path";

import {
  alertText,
  CATALOG,
  createUsageService,
  defaultUsageSettings,
  nodeEnv,
  trayModel,
  type HostFacts,
  type HostReadings,
  type LimitAlert,
  type ProviderSnapshot,
  type TrayModel,
  type UsageReport,
  type UsageSettings,
  type UsageSummary,
} from "@hanzodev/usage";

const STALE_MS = 60_000;

export interface UsageTray {
  readonly tray: TrayModel;
  /** Threshold crossings since the last read; reading drains them. */
  readonly alerts: ReadonlyArray<{ title: string; body: string }>;
}

const names = Object.fromEntries(CATALOG.map((p) => [p.id, p.name]));

/** One per state directory: holds settings, the last summary and the pending alerts. */
export const createUsageRuntime = (stateDir: string, host: HostFacts) => {
  const settingsPath = NodePath.join(stateDir, "usage-settings.json");
  let settings: UsageSettings = defaultUsageSettings;
  let loaded = false;
  let summary: UsageSummary | undefined;
  let pending: LimitAlert[] = [];
  let inflight: Promise<UsageSummary> | undefined;
  /** What the user's "Connect" read once, by provider: a reading, never the login it came from. */
  const connectedPath = NodePath.join(stateDir, "usage-connected.json");
  let connected: Record<string, ReadonlyArray<ProviderSnapshot>> | undefined;

  const service = createUsageService(nodeEnv(host), () => settings);

  const load = async () => {
    if (loaded) return;
    loaded = true;
    const text = await NodeFSP.readFile(settingsPath, "utf8").catch(() => undefined);
    if (text) {
      try {
        settings = { ...defaultUsageSettings, ...(JSON.parse(text) as Partial<UsageSettings>) };
      } catch {
        settings = defaultUsageSettings;
      }
    }
  };

  const loadConnected = async () => {
    if (connected) return connected;
    const text = await NodeFSP.readFile(connectedPath, "utf8").catch(() => undefined);
    try {
      connected = text ? (JSON.parse(text) as typeof connected) : {};
    } catch {
      connected = {};
    }
    return connected ?? {};
  };

  const refresh = (host: () => Promise<HostReadings>): Promise<UsageSummary> => {
    inflight ??= (async () => {
      await load();
      const readings = await host();
      // A connected provider's last reading stands in until a reader of its own succeeds.
      const kept = Object.values(await loadConnected()).flat();
      const r = await service.refresh({ ...readings, local: [...readings.local, ...kept] });
      summary = r.summary;
      pending.push(...r.alerts);
      return r.summary;
    })().finally(() => {
      inflight = undefined;
    });
    return inflight;
  };

  const current = async (host: () => Promise<HostReadings>): Promise<UsageSummary> =>
    summary && Date.now() - Date.parse(summary.generatedAt) < STALE_MS ? summary : refresh(host);

  return {
    settings: async () => {
      await load();
      return settings;
    },
    saveSettings: async (next: UsageSettings) => {
      await load();
      settings = next;
      await NodeFSP.mkdir(stateDir, { recursive: true });
      await NodeFSP.writeFile(settingsPath, JSON.stringify(next, null, 2), { mode: 0o600 });
      summary = undefined;
    },
    summary: current,
    refresh,
    connect: async (provider: string, host: () => Promise<HostReadings>): Promise<UsageSummary> => {
      const instances = (await host()).instances.filter((i) => i.provider === provider);
      const readings = await service.connect(provider, instances);
      const good = readings.filter((s) => s.windows.length > 0);
      const store = await loadConnected();
      if (good.length > 0) {
        connected = { ...store, [provider]: good };
        await NodeFSP.mkdir(stateDir, { recursive: true });
        await NodeFSP.writeFile(connectedPath, JSON.stringify(connected), { mode: 0o600 });
      }
      summary = undefined;
      const next = await refresh(host);
      // Why a connect failed is told once, in the row the user is looking at.
      const failure = good.length === 0 ? readings.find((s) => s.error) : undefined;
      return failure
        ? {
            ...next,
            snapshots: [...next.snapshots.filter((s) => s.provider !== provider), failure],
          }
        : next;
    },
    report: async (
      host: () => Promise<HostReadings>,
      deviceId: string,
      since: Date,
    ): Promise<UsageReport> => {
      await current(host);
      return service.report(deviceId, since);
    },
    tray: async (host: () => Promise<HostReadings>, now: Date): Promise<UsageTray> => {
      const s = await current(host);
      const alerts = pending.map((a) => alertText(a, names[a.provider] ?? a.provider, now));
      pending = [];
      const tray = trayModel(s.snapshots, names, settings.trayMode, now);
      return { tray: settings.showInMenuBar ? tray : { ...tray, title: "" }, alerts };
    },
  };
};

export type UsageRuntime = ReturnType<typeof createUsageRuntime>;

const runtimes = new Map<string, UsageRuntime>();
export const usageRuntimeFor = (stateDir: string, host: HostFacts): UsageRuntime => {
  let r = runtimes.get(stateDir);
  if (!r) {
    r = createUsageRuntime(stateDir, host);
    runtimes.set(stateDir, r);
  }
  return r;
};
