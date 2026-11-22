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
  return [...macs].sort((a, b) => Number(b.online) - Number(a.online));
}

export interface DrawerCloud {
  /** False until the cloud can start a session: the row then reads "Not available yet". */
  readonly available: boolean;
}

/** The Cloud environment, listed for every signed-in account; null before discovery answers. */
export function drawerCloud(discovery: EnvironmentDiscovery | null): DrawerCloud | null {
  if (discovery === null || !discovery.environments.some(({ type }) => type === "cloud")) {
    return null;
  }
  return { available: discovery.cloud.status === "ok" };
}

export type MacTap = "open" | "connect" | "settings";

/**
 * What tapping a linked Mac does: already connected opens its threads; known to the relay and not
 * yet connected connects it; unknown to the relay is a pairing job, which lives in Settings.
 */
export function resolveMacTap(input: {
  readonly macId: string;
  readonly connectedIds: ReadonlySet<string>;
  readonly relayIds: ReadonlySet<string>;
}): MacTap {
  if (input.connectedIds.has(input.macId)) return "open";
  return input.relayIds.has(input.macId) ? "connect" : "settings";
}
