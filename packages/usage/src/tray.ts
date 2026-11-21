import { countdown } from "./notify.ts";
import type { ProviderSnapshot, TrayMode } from "./schema.ts";

export interface TrayModel {
  /** Text beside the tray icon; empty when there is nothing to show. */
  readonly title: string;
  /** 0-100 fill for the icon bar; the busiest provider in merged mode. */
  readonly fill: number;
  readonly lines: ReadonlyArray<string>;
}

const primary = (s: ProviderSnapshot) =>
  s.windows.slice().sort((a, b) => b.usedPercent - a.usedPercent)[0];

/** One icon summarising every enabled provider (merged), or one line per provider. */
export const trayModel = (
  snapshots: ReadonlyArray<ProviderSnapshot>,
  names: Readonly<Record<string, string>>,
  mode: TrayMode,
  now: Date,
): TrayModel => {
  const rows = snapshots
    .map((s) => ({ s, w: primary(s) }))
    .filter((r): r is { s: ProviderSnapshot; w: NonNullable<ReturnType<typeof primary>> } => !!r.w);
  const lines = snapshots.map((s) => {
    const name = names[s.provider] ?? s.provider;
    const w = primary(s);
    if (s.error && !w) return `${name}: ${s.error}`;
    if (w)
      return `${name}: ${w.label} ${Math.round(w.usedPercent)}%${w.resetsAt ? ` (resets ${countdown(w.resetsAt, now)})` : ""}`;
    if (s.credits?.balance !== undefined)
      return `${name}: ${s.credits.balance.toFixed(2)} ${s.credits.currency}`;
    return `${name}: no data`;
  });
  const top = rows.reduce((m, r) => (r.w.usedPercent > m ? r.w.usedPercent : m), 0);
  if (!rows.length) return { title: "", fill: 0, lines };
  if (mode === "merged") return { title: `${Math.round(top)}%`, fill: top, lines };
  return { title: rows.map((r) => `${Math.round(r.w.usedPercent)}`).join(" "), fill: top, lines };
};
