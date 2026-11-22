import { useAtomValue } from "@effect/atom-react";
import {
  RelayConnectionRegistration,
  RelayConnectionTarget,
} from "@hanzodev/client-runtime/connection";
import { managedRelaySessionAtom } from "@hanzodev/client-runtime/relay";
import { useEffect, useRef } from "react";
import { AppState } from "react-native";

import { environmentCatalog } from "../../connection/catalog";
import { relayEnvironmentDiscovery } from "../../state/relay";
import { useAtomCommand } from "../../state/use-atom-command";
import { useAuth } from "../cloud/hanzoMobileAuth";

/**
 * Keeps `/v1/sources` discovery active for the signed-in Hanzo account (at launch, sign-in, and
 * foreground return) and automatically links & connects every online desktop session (`Hanzo.app`,
 * `Hanzo Dev.app`, `hanzod`) under the same account without requiring manual pairing.
 */
export function AccountAutoLinkCoordinator() {
  const { isSignedIn, userId } = useAuth({ treatPendingAsSignedOut: false });
  const relaySession = useAtomValue(managedRelaySessionAtom);
  const discovery = useAtomValue(relayEnvironmentDiscovery.stateValueAtom);
  const catalog = useAtomValue(environmentCatalog.catalogValueAtom);
  const refreshRelayEnvironments = useAtomCommand(relayEnvironmentDiscovery.refresh, {
    label: "account desktop discovery refresh",
    reportFailure: false,
  });
  const registerEnvironment = useAtomCommand(environmentCatalog.register, {
    label: "account desktop auto-link",
    reportFailure: false,
  });
  const initialRefreshRef = useRef<string | null>(null);
  const attemptedByAccountRef = useRef<{ accountId: string | null; ids: Set<string> }>({
    accountId: null,
    ids: new Set(),
  });

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- ensures /v1/sources runs when already signed in at startup or when returning to foreground
  useEffect(() => {
    if (!isSignedIn || !userId || !relaySession) {
      initialRefreshRef.current = null;
      return;
    }
    if (!discovery.listed && !discovery.refreshing && initialRefreshRef.current !== userId) {
      initialRefreshRef.current = userId;
      void refreshRelayEnvironments();
    }
  }, [
    isSignedIn,
    userId,
    relaySession,
    discovery.listed,
    discovery.refreshing,
    refreshRelayEnvironments,
  ]);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- refreshes linked desktop machines whenever the mobile app returns to foreground
  useEffect(() => {
    if (!isSignedIn || !userId || !relaySession) return;
    const subscription = AppState.addEventListener("change", (nextState) => {
      if (nextState === "active") {
        void refreshRelayEnvironments();
      }
    });
    return () => {
      subscription.remove();
    };
  }, [isSignedIn, userId, relaySession, refreshRelayEnvironments]);

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
      const isOnline = entry.availability === "online" || entry.environment.state === "online";
      if (!isOnline) {
        // Allow re-attempting automatically if this machine comes back online later.
        attempted.delete(envId);
        continue;
      }
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
  }, [
    isSignedIn,
    userId,
    catalog.isReady,
    catalog.entries,
    discovery.listed,
    discovery.environments,
    registerEnvironment,
  ]);

  return null;
}
