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

  const refresh = (host: () => Promise<HostReadings>): Promise<UsageSummary> => {
    inflight ??= (async () => {
      await load();
      const r = await service.refresh(await host());
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
