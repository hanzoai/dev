import type { EnvironmentDiscovery } from "@hanzodev/client-runtime/discovery";

export interface DrawerMac {
  readonly id: string;
  readonly name: string;
  readonly online: boolean;
}

/**
 * The account's linked Macs for the drawer, online first. An endpoint that is missing, failing
 * or not yet asked yields no entries and no message: signing in is linking, so an empty list is
 * simply not drawn.
 */
export function drawerMacs(discovery: EnvironmentDiscovery | null): readonly DrawerMac[] {
  if (discovery === null || discovery.sources.status !== "ok") return [];
  const macs: DrawerMac[] = [];
  for (const environment of discovery.environments) {
    if (environment.type !== "linked" || environment.machine.kind !== "desktop") continue;
    macs.push({
      id: environment.id,
      name: environment.label,
      online: environment.machine.state === "online",
    });
  }
  return macs.toSorted((a, b) => Number(b.online) - Number(a.online));
}
