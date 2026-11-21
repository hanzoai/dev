import type { ProviderSnapshot } from "./schema.ts";

export interface LimitAlert {
  readonly provider: string;
  readonly label: string;
  readonly threshold: number;
  readonly usedPercent: number;
  readonly resetsAt?: string;
}

/** Highest threshold already announced, per provider window. Drops when usage falls below it (the window reset). */
export type NotifyState = ReadonlyMap<string, number>;

/** Pure: which thresholds did this refresh newly cross? */
export const evaluate = (
  state: NotifyState,
  snapshots: ReadonlyArray<ProviderSnapshot>,
  thresholds: ReadonlyArray<number>,
): { state: NotifyState; alerts: LimitAlert[] } => {
  const next = new Map<string, number>();
  const alerts: LimitAlert[] = [];
  const sorted = [...thresholds].sort((a, b) => a - b);
  for (const s of snapshots) {
    for (const w of s.windows) {
      const key = `${s.provider}/${w.label}`;
      const crossed = sorted.filter((t) => w.usedPercent >= t);
      const top = crossed.at(-1);
      const announced = state.get(key) ?? 0;
      if (top !== undefined) {
        next.set(key, top);
        if (top > announced) {
          alerts.push({
            provider: s.provider,
            label: w.label,
            threshold: top,
            usedPercent: w.usedPercent,
            ...(w.resetsAt ? { resetsAt: w.resetsAt } : {}),
          });
        }
      }
    }
  }
  return { state: next, alerts };
};

export const alertText = (
  a: LimitAlert,
  name: string,
  now: Date,
): { title: string; body: string } => ({
  title: `${name}: ${a.label} at ${Math.round(a.usedPercent)}%`,
  body: a.resetsAt ? `Resets in ${countdown(a.resetsAt, now)}.` : `Crossed ${a.threshold}%.`,
});

/** "2h 05m", "3d 4h", "now". */
export const countdown = (iso: string, now: Date): string => {
  const ms = Date.parse(iso) - now.getTime();
  if (!(ms > 0)) return "now";
  const m = Math.floor(ms / 60000);
  const d = Math.floor(m / 1440);
  const h = Math.floor((m % 1440) / 60);
  if (d > 0) return `${d}d ${h}h`;
  if (h > 0) return `${h}h ${String(m % 60).padStart(2, "0")}m`;
  return `${m}m`;
};
