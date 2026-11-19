import { useAtomValue } from "@effect/atom-react";
import {
  createManagedRelayQueryManager,
  createManagedRelayHanzoChatLinkRequest,
  deregisterManagedRelayEnvironment,
  ManagedRelay,
  managedRelaySessionAtom,
  readManagedRelaySnapshotState,
  unlinkManagedRelayHanzoChat,
} from "@hanzodev/client-runtime/relay";
import {
  createAtomCommandScheduler,
  createRuntimeCommand,
} from "@hanzodev/client-runtime/state/runtime";
import {
  DEFAULT_MANAGED_TUNNEL_LIMIT,
  HANZOCHAT_PLAN_TUNNEL_LIMITS,
  type RelayClientDeviceRecord,
  type RelayClientEnvironmentRecord,
  type RelayHanzoChatLinkResponse,
} from "@hanzodev/contracts/relay";
import type { EnvironmentId } from "@hanzodev/contracts";
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import { AsyncResult, Atom } from "effect/reactivity";

import { runtime } from "../lib/runtime";
import { appAtomRegistry } from "../rpc/atomRegistry";

const managedRelayAtomRuntime = Atom.runtime(
  Layer.effect(
    ManagedRelay.ManagedRelayClient,
    runtime.contextEffect.pipe(
      Effect.map((context) => Context.get(context, ManagedRelay.ManagedRelayClient)),
    ),
  ),
);

const managedRelayQueryManager = createManagedRelayQueryManager(managedRelayAtomRuntime);

const managedRelayMutationScheduler = createAtomCommandScheduler();

export const deregisterManagedRelayEnvironmentCommand = createRuntimeCommand(
  managedRelayAtomRuntime,
  {
    label: "web:managed-relay:deregister-environment",
    scheduler: managedRelayMutationScheduler,
    concurrency: {
      mode: "serial",
      key: (input: { readonly accountId: string; readonly environmentId: EnvironmentId }) =>
        input.accountId,
    },
    execute: (input, registry) => deregisterManagedRelayEnvironment(registry, input),
  },
);

const accountSerial = {
  mode: "serial",
  key: (input: { readonly accountId: string }) => input.accountId,
} as const;

export const createManagedRelayHanzoChatLinkRequestCommand = createRuntimeCommand(
  managedRelayAtomRuntime,
  {
    label: "web:managed-relay:create-hanzochat-link-request",
    scheduler: managedRelayMutationScheduler,
    concurrency: accountSerial,
    execute: (input: { readonly accountId: string }, registry) =>
      createManagedRelayHanzoChatLinkRequest(registry, input),
  },
);

export const unlinkManagedRelayHanzoChatCommand = createRuntimeCommand(managedRelayAtomRuntime, {
  label: "web:managed-relay:unlink-hanzochat",
  scheduler: managedRelayMutationScheduler,
  concurrency: accountSerial,
  execute: (input: { readonly accountId: string }, registry) =>
    unlinkManagedRelayHanzoChat(registry, input),
});

const EMPTY_HANZOCHAT_LINK_ATOM = Atom.make(
  AsyncResult.success<RelayHanzoChatLinkResponse>({ available: false, link: null }),
).pipe(Atom.keepAlive, Atom.withLabel("managed-relay:web:hanzochat-link:null"));

export function useManagedRelayHanzoChatLink() {
  const session = useAtomValue(managedRelaySessionAtom);
  const accountId = session?.accountId ?? null;
  const atom = accountId
    ? managedRelayQueryManager.hanzoChatLinkAtom(accountId)
    : EMPTY_HANZOCHAT_LINK_ATOM;
  const snapshot = readManagedRelaySnapshotState(useAtomValue(atom));
  const refresh = () => {
    if (accountId) {
      managedRelayQueryManager.refreshHanzoChatLink(appAtomRegistry, accountId);
    }
  };

  return { ...snapshot, accountId, refresh };
}

const EMPTY_ENVIRONMENTS_ATOM = Atom.make(
  AsyncResult.success<ReadonlyArray<RelayClientEnvironmentRecord>>([]),
).pipe(Atom.keepAlive, Atom.withLabel("managed-relay:web:environments:null"));

const EMPTY_DEVICES_ATOM = Atom.make(
  AsyncResult.success<ReadonlyArray<RelayClientDeviceRecord>>([]),
).pipe(Atom.keepAlive, Atom.withLabel("managed-relay:web:devices:null"));

export function useManagedRelayEnvironments() {
  const session = useAtomValue(managedRelaySessionAtom);
  const accountId = session?.accountId ?? null;
  const atom = accountId
    ? managedRelayQueryManager.environmentsAtom(accountId)
    : EMPTY_ENVIRONMENTS_ATOM;
  const result = useAtomValue(atom);
  const snapshot = readManagedRelaySnapshotState(result);
  const refresh = () => {
    if (accountId) {
      managedRelayQueryManager.refreshEnvironments(appAtomRegistry, accountId);
    }
  };

  return {
    ...snapshot,
    accountId,
    refresh,
  };
}

export function useManagedRelayDevices() {
  const session = useAtomValue(managedRelaySessionAtom);
  const accountId = session?.accountId ?? null;
  const atom = accountId ? managedRelayQueryManager.devicesAtom(accountId) : EMPTY_DEVICES_ATOM;
  const result = useAtomValue(atom);
  const snapshot = readManagedRelaySnapshotState(result);
  const refresh = () => {
    if (accountId) {
      managedRelayQueryManager.refreshDevices(appAtomRegistry, accountId);
    }
  };

  return {
    ...snapshot,
    accountId,
    refresh,
  };
}

/** How this account can raise its tunnel cap: link Hanzo Chat, or upgrade the linked plan. */
export type ManagedTunnelUpgrade = "link" | "pro" | "premier" | null;

// Mirrors the relay's cap: a manual override is exact, otherwise the Hanzo Chat plan raises the default.
function managedTunnelCap(hanzoChat: RelayHanzoChatLinkResponse): {
  readonly limit: number;
  readonly upgrade: ManagedTunnelUpgrade;
} {
  if (hanzoChat.maxTunnelsOverride !== undefined) {
    return { limit: hanzoChat.maxTunnelsOverride, upgrade: null };
  }
  if (!hanzoChat.available) return { limit: DEFAULT_MANAGED_TUNNEL_LIMIT, upgrade: null };
  switch (hanzoChat.link?.plan) {
    case "premier":
      return { limit: HANZOCHAT_PLAN_TUNNEL_LIMITS.premier, upgrade: null };
    case "pro":
      return { limit: HANZOCHAT_PLAN_TUNNEL_LIMITS.pro, upgrade: "premier" };
    case "free":
      return { limit: DEFAULT_MANAGED_TUNNEL_LIMIT, upgrade: "pro" };
    case undefined:
      return { limit: DEFAULT_MANAGED_TUNNEL_LIMIT, upgrade: "link" };
  }
}

/** Refetches the account's environments and Hanzo Chat plan after a tunnel changes. */
export function refreshManagedTunnelUsage() {
  const accountId = appAtomRegistry.get(managedRelaySessionAtom)?.accountId;
  if (!accountId) return;
  managedRelayQueryManager.refreshEnvironments(appAtomRegistry, accountId);
  managedRelayQueryManager.refreshHanzoChatLink(appAtomRegistry, accountId);
}

/** Managed tunnels on this account and its cap. Null fields are still loading or failed. */
export function useManagedTunnelUsage() {
  const environments = useManagedRelayEnvironments();
  const hanzoChat = useManagedRelayHanzoChatLink();
  const used =
    environments.accountId === null
      ? null
      : (environments.data?.filter(
          (environment) => environment.endpoint.providerKind === "cloudflare_tunnel",
        ).length ?? null);
  const cap = hanzoChat.accountId !== null && hanzoChat.data ? managedTunnelCap(hanzoChat.data) : null;
  return {
    used,
    limit: cap?.limit ?? null,
    upgrade: cap?.upgrade ?? null,
    refresh: refreshManagedTunnelUsage,
  };
}
