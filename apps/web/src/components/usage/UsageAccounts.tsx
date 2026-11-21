import type { ServerProvider, ServerProviderUsageWindow } from "@hanzodev/contracts";
import { accountName } from "@hanzodev/usage/account";
import { providerById } from "@hanzodev/usage/catalog";
import type { ProviderSnapshot } from "@hanzodev/usage/schema";
import { useGT } from "gt-react";
import { useSyncExternalStore } from "react";

import { getUsageState, subscribeUsage } from "../../usage/usageStore";
import { AddAccountButton } from "./AddAccountButton";
import { LimitWindows } from "./UsageLimits";

const asWindows = (snapshot: ProviderSnapshot): ServerProviderUsageWindow[] =>
  snapshot.windows.map((w, index) => ({
    id: `${snapshot.provider}-${snapshot.instance ?? "default"}-${index}`,
    kind: w.kind,
    label: w.label,
    usedPercent: w.usedPercent,
    ...(w.windowMinutes ? { windowDurationMins: w.windowMinutes } : {}),
    ...(w.resetsAt ? { resetsAt: w.resetsAt } : {}),
  })) as ServerProviderUsageWindow[];

/**
 * Every account the usage module read on this machine: each provider instance and any login found on
 * disk. A provider session that already reports limits for an instance keeps its own row.
 */
export function UsageAccounts({
  providers,
  now,
}: {
  readonly providers: ReadonlyArray<ServerProvider>;
  readonly now: number;
}) {
  const gt = useGT();
  const { summary } = useSyncExternalStore(subscribeUsage, getUsageState);
  const covered = new Set<string>(
    providers.filter((p) => (p.usageLimits?.windows.length ?? 0) > 0).map((p) => p.instanceId),
  );
  const rows = (summary?.snapshots ?? []).filter(
    (s) => !(s.instance !== undefined && covered.has(s.instance)),
  );
  if (rows.length === 0) return null;
  return (
    <section className="space-y-3" data-testid="usage-accounts">
      <h3 className="text-sm font-semibold text-foreground">{gt("Accounts on this Mac")}</h3>
      {rows.map((s) => {
        const descriptor = providerById(s.provider);
        const credits = s.credits;
        return (
          <div
            key={`${s.provider}|${s.instance ?? ""}`}
            className="space-y-2 rounded-lg border border-border/60 px-3 py-3"
            data-testid={`usage-account-${s.provider}`}
          >
            <div className="flex flex-wrap items-center justify-between gap-2">
              <div className="flex min-w-0 items-center gap-2">
                <span
                  className="size-2.5 shrink-0 rounded-full"
                  style={{ background: descriptor?.color ?? "currentColor" }}
                />
                <span className="truncate text-sm font-medium text-foreground">
                  {accountName(s, descriptor?.name ?? s.provider)}
                </span>
                {s.plan ? (
                  <span className="rounded bg-muted px-1.5 py-0.5 text-2xs text-muted-foreground">
                    {s.plan}
                  </span>
                ) : null}
              </div>
              {s.unconfigured ? <AddAccountButton snapshot={s} /> : null}
            </div>
            {s.windows.length > 0 ? (
              <LimitWindows
                driver={s.provider as ServerProvider["driver"]}
                accentColor={descriptor?.color}
                windows={asWindows(s)}
                now={now}
              />
            ) : null}
            {credits?.balance !== undefined ? (
              <p className="text-xs text-muted-foreground tabular-nums">
                {gt("Balance {amount}", {
                  amount:
                    credits.currency === "USD"
                      ? `$${credits.balance.toFixed(2)}`
                      : `${credits.balance.toFixed(2)} ${credits.currency}`,
                })}
              </p>
            ) : null}
            {s.error ? <p className="text-xs text-muted-foreground">{s.error}</p> : null}
          </div>
        );
      })}
    </section>
  );
}
