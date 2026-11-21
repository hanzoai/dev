import {
  discoverEnvironments,
  type EnvironmentDiscovery,
} from "@hanzodev/client-runtime/discovery";
import { useEffect, useState } from "react";

import { hanzoMobileAuthStore, useAuth } from "../../cloud/hanzoMobileAuth";
import { drawerCloud, drawerMacs, type DrawerCloud, type DrawerMac } from "./drawer-macs.logic";

/** The signed-in account's linked Macs and Cloud. Quiet by design: any failure is simply no entries. */
export function useDrawerEnvironments(): {
  readonly macs: readonly DrawerMac[];
  readonly cloud: DrawerCloud | null;
} {
  const { isSignedIn, userId, orgId } = useAuth();
  const [discovery, setDiscovery] = useState<EnvironmentDiscovery | null>(null);
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- discovery runs once per signed-in account and org
  useEffect(() => {
    if (!isSignedIn) {
      setDiscovery(null);
      return;
    }
    let cancelled = false;
    void hanzoMobileAuthStore
      .getToken()
      .then((accessToken) => discoverEnvironments({ accessToken, orgId }, []))
      .then((result) => {
        if (!cancelled) setDiscovery(result);
      })
      .catch(() => undefined);
    return () => {
      cancelled = true;
    };
  }, [isSignedIn, userId, orgId]);
  return { macs: drawerMacs(discovery), cloud: drawerCloud(discovery) };
}
