import { useAtomValue } from "@effect/atom-react";
import {
  RelayConnectionRegistration,
  RelayConnectionTarget,
} from "@hanzodev/client-runtime/connection";
import { useEffect, useRef } from "react";

import { environmentCatalog } from "../../connection/catalog";
import { relayEnvironmentDiscovery } from "../../state/relay";
import { useAtomCommand } from "../../state/use-atom-command";
import { useAuth } from "../cloud/hanzoMobileAuth";

/**
 * Automatically links and connects online desktop sessions discovered under the signed-in Hanzo
 * account (`/v1/sources`) so mobile can immediately remote-control them without manual pairing.
 */
export function AccountAutoLinkCoordinator() {
  const { isSignedIn, userId } = useAuth({ treatPendingAsSignedOut: false });
  const discovery = useAtomValue(relayEnvironmentDiscovery.stateValueAtom);
  const catalog = useAtomValue(environmentCatalog.catalogValueAtom);
  const registerEnvironment = useAtomCommand(environmentCatalog.register, {
    label: "account desktop auto-link",
    reportFailure: false,
  });
  const attemptedByAccountRef = useRef<{ accountId: string | null; ids: Set<string> }>({
    accountId: null,
    ids: new Set(),
  });

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- auto-connects signed-in account's online desktop sessions as /v1/sources discovers them
  useEffect(() => {
    if (!isSignedIn || !userId) {
      attemptedByAccountRef.current = { accountId: null, ids: new Set() };
      return;
    }
    if (attemptedByAccountRef.current.accountId !== userId) {
      attemptedByAccountRef.current = { accountId: userId, ids: new Set() };
    }
    if (!catalog.isReady || !discovery.listed) return;

    const attempted = attemptedByAccountRef.current.ids;
    for (const entry of discovery.environments.values()) {
      const envId = entry.environment.environmentId;
      const isOnline =
        entry.availability === "online" || entry.environment.state === "online";
      if (!isOnline) continue;
      if (catalog.entries.has(envId) || attempted.has(envId)) continue;
      attempted.add(envId);
      void registerEnvironment(
        new RelayConnectionRegistration({
          target: new RelayConnectionTarget({
            environmentId: envId,
            label: entry.environment.label,
          }),
        }),
      );
    }
  }, [isSignedIn, userId, catalog.isReady, catalog.entries, discovery.listed, discovery.environments, registerEnvironment]);

  return null;
}
