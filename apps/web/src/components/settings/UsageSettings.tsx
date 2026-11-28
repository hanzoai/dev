import { accountName } from "@hanzodev/usage/account";
import { CATALOG, DEFAULT_ENABLED, LIVE_PROVIDERS, providerById } from "@hanzodev/usage/catalog";
import { countdown } from "@hanzodev/usage/notify";
import type { ProviderSnapshot, StatusIndicator, UsageWindow } from "@hanzodev/usage/schema";
import { Link } from "@tanstack/react-router";
import { useGT } from "gt-react";
import { useSyncExternalStore } from "react";

import { AddAccountButton } from "../usage/AddAccountButton";
import { Button } from "../ui/button";
import {
  getUsageState,
  refreshUsageNow,
  subscribeUsage,
  updateUsageSettings,
} from "../../usage/usageStore";
import { useEmbedSettingsHost } from "../../embed/settingsHost";
import { SettingsPageContainer, SettingsRow, SettingsSection } from "./settingsLayout";

const barColor = (percent: number) =>
  percent >= 95 ? "bg-red-500" : percent >= 80 ? "bg-amber-500" : "bg-emerald-500";

const STATUS_DOT: Record<StatusIndicator, string> = {
  none: "bg-emerald-500",
  minor: "bg-amber-500",
  major: "bg-orange-500",
  critical: "bg-red-500",
  maintenance: "bg-sky-500",
  unknown: "bg-muted-foreground/50",
};

function WindowBar({ window: w, now }: { readonly window: UsageWindow; readonly now: Date }) {
  const gt = useGT();
  const percent = Math.round(w.usedPercent);
  return (
    <div className="space-y-1" data-testid="usage-window">
      <div className="flex items-baseline justify-between gap-3 text-xs">
        <span className="font-medium text-foreground">{w.label}</span>
        <span className="text-muted-foreground tabular-nums">
          {w.resetsAt
            ? gt("{percent}% used · resets in {time}", {
                percent,
                time: countdown(w.resetsAt, now),
              })
            : gt("{percent}% used", { percent })}
        </span>
      </div>
      <div
        role="progressbar"
        aria-label={w.label}
        aria-valuenow={percent}
        aria-valuemin={0}
        aria-valuemax={100}
        className="h-1.5 w-full overflow-hidden rounded-full bg-muted"
      >
        <div
          className={`h-full rounded-full ${barColor(w.usedPercent)}`}
          style={{ width: `${Math.max(2, w.usedPercent)}%` }}
        />
      </div>
    </div>
  );
}

function money(value: number, currency: string) {
  return currency === "USD" ? `$${value.toFixed(2)}` : `${value.toFixed(2)} ${currency}`;
}

function ProviderCard({
  snapshot,
  now,
}: {
  readonly snapshot: ProviderSnapshot;
  readonly now: Date;
}) {
  const gt = useGT();
  const statusLabel: Record<StatusIndicator, string> = {
    none: gt("Operational"),
    minor: gt("Minor incident"),
    major: gt("Major incident"),
    critical: gt("Outage"),
    maintenance: gt("Maintenance"),
    unknown: gt("Status unknown"),
  };
  const descriptor = providerById(snapshot.provider);
  const status = snapshot.status;
  const credits = snapshot.credits;
  return (
    <div
      className="space-y-3 px-3 py-3 sm:px-4"
      data-testid={`usage-provider-${snapshot.provider}`}
      data-instance={snapshot.instance}
    >
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="flex min-w-0 items-center gap-2">
          <span
            className="size-2.5 shrink-0 rounded-full"
            style={{ background: descriptor?.color ?? "currentColor" }}
          />
          <span className="truncate text-sm font-semibold text-foreground">
            {accountName(snapshot, descriptor?.name ?? snapshot.provider)}
          </span>
          {snapshot.plan ? (
            <span className="rounded bg-muted px-1.5 py-0.5 text-2xs text-muted-foreground">
              {snapshot.plan}
            </span>
          ) : null}
        </div>
        {snapshot.unconfigured ? <AddAccountButton snapshot={snapshot} /> : null}
        {status ? (
          <a
            className="flex items-center gap-1.5 text-xs text-muted-foreground hover:text-foreground"
            href={descriptor?.statusPage ?? descriptor?.statusLink}
            target="_blank"
            rel="noreferrer noopener"
          >
            <span className={`size-2 rounded-full ${STATUS_DOT[status.indicator]}`} />
            {status.incidents.length > 0
              ? status.incidents[0]!.name
              : statusLabel[status.indicator]}
          </a>
        ) : null}
      </div>
      {snapshot.windows.map((w) => (
        <WindowBar key={`${w.kind}-${w.label}`} window={w} now={now} />
      ))}
      {credits ? (
        <div className="flex flex-wrap gap-x-4 text-xs text-muted-foreground">
          {credits.balance !== undefined ? (
            <span>
              {gt("Balance {amount}", { amount: money(credits.balance, credits.currency) })}
            </span>
          ) : null}
          {credits.spent !== undefined ? (
            <span>{gt("Spent {amount}", { amount: money(credits.spent, credits.currency) })}</span>
          ) : null}
          {credits.limit !== undefined ? (
            <span>{gt("Limit {amount}", { amount: money(credits.limit, credits.currency) })}</span>
          ) : null}
        </div>
      ) : null}
      {snapshot.error ? <p className="text-xs text-muted-foreground">{snapshot.error}</p> : null}
    </div>
  );
}

export function UsageSettingsPanel() {
  const gt = useGT();
  const host = useEmbedSettingsHost();
  const { summary, settings, error, refreshing } = useSyncExternalStore(
    subscribeUsage,
    getUsageState,
  );
  const now = new Date(summary?.generatedAt ?? 0);
  const update = updateUsageSettings;
  const isEnabled = (id: string) =>
    settings?.providers[id]?.enabled ?? DEFAULT_ENABLED.includes(id);

  const totalUsd = (summary?.cost ?? []).reduce((sum, d) => sum + (d.costUsd ?? 0), 0);

  return (
    <SettingsPageContainer>
      {host?.usageHeader ?? null}
      <SettingsSection
        id="usage"
        title={gt("Usage")}
        headerAction={
          <Button
            size="sm"
            variant="outline"
            disabled={refreshing}
            onClick={() => void refreshUsageNow()}
          >
            {gt("Refresh")}
          </Button>
        }
      >
        {error ? <p className="px-3 py-3 text-sm text-muted-foreground sm:px-4">{error}</p> : null}
        {summary?.snapshots.length === 0 ? (
          <p className="px-3 py-3 text-sm text-muted-foreground sm:px-4">
            {gt("No providers enabled. Turn one on below.")}
          </p>
        ) : null}
        {summary?.snapshots.map((s) => (
          <ProviderCard key={`${s.provider}|${s.instance ?? ""}`} snapshot={s} now={now} />
        ))}
      </SettingsSection>

      <SettingsSection id="usage-cost" title={gt("Local cost, last 30 days")}>
        <SettingsRow
          title={gt("Estimated total")}
          description={gt("Read from local session logs. Nothing leaves this machine.")}
          control={
            <div className="flex items-center gap-3">
              <span className="text-sm font-medium tabular-nums">${totalUsd.toFixed(2)}</span>
              {!host?.inline ? (
                <Button size="sm" variant="outline" render={<Link to="/usage" />}>
                  {gt("Details")}
                </Button>
              ) : null}
            </div>
          }
        />
      </SettingsSection>

      {settings ? (
        <SettingsSection id="usage-settings" title={gt("Menu bar and alerts")}>
          <SettingsRow
            title={gt("Menu bar icon")}
            description={gt(
              "One merged icon for the busiest provider, or one reading per provider.",
            )}
            control={
              <Button
                size="sm"
                variant="outline"
                onClick={() =>
                  void update({
                    ...settings,
                    trayMode: settings.trayMode === "merged" ? "perProvider" : "merged",
                  })
                }
              >
                {settings.trayMode === "merged" ? gt("Merged") : gt("Per provider")}
              </Button>
            }
          />
          <SettingsRow
            title={gt("Show usage in menu bar")}
            description={gt(
              "Put the reading beside the icon. Off, the menu bar shows only the icon.",
            )}
            control={
              <input
                type="checkbox"
                aria-label={gt("Show usage in menu bar")}
                checked={settings.showInMenuBar}
                onChange={(e) => void update({ ...settings, showInMenuBar: e.target.checked })}
              />
            }
          />
          <SettingsRow
            title={gt("Alert thresholds")}
            description={gt(
              "A notification fires the first time a window crosses each percentage.",
            )}
            control={
              <span className="text-sm tabular-nums">
                {settings.notifyAtPercent.map((p) => `${p}%`).join(", ") || gt("Off")}
              </span>
            }
          />
        </SettingsSection>
      ) : null}

      {settings ? (
        <SettingsSection id="usage-providers" title={gt("Providers")}>
          {CATALOG.map((p) => {
            const live = LIVE_PROVIDERS.includes(p.id);
            return (
              <SettingsRow
                key={p.id}
                title={p.name}
                description={
                  live
                    ? p.auth.includes("oauth")
                      ? gt("Reads the local sign-in this tool already has.")
                      : gt("Uses {env} from your environment.", { env: p.env?.[0] ?? "an API key" })
                    : gt("Links to the dashboard; live usage not yet supported.")
                }
                control={
                  <input
                    type="checkbox"
                    aria-label={p.name}
                    checked={isEnabled(p.id)}
                    onChange={(e) =>
                      void update({
                        ...settings,
                        providers: {
                          ...settings.providers,
                          [p.id]: { ...settings.providers[p.id], enabled: e.target.checked },
                        },
                      })
                    }
                  />
                }
              />
            );
          })}
        </SettingsSection>
      ) : null}
    </SettingsPageContainer>
  );
}
