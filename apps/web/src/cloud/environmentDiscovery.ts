// The only place the web app reads environment discovery (client-runtime/discovery).
import {
  discoverEnvironments,
  type EnvironmentDiscovery,
} from "@hanzodev/client-runtime/discovery";
import { useEffect, useState } from "react";

import { useAuth } from "./hanzoWebAuth";

/** Discovery for the signed-in account: linked machines and the cloud offer. Null until known. */
export function useEnvironmentDiscovery(): EnvironmentDiscovery | null {
  const { isSignedIn, getToken, orgId } = useAuth();
  const [discovery, setDiscovery] = useState<EnvironmentDiscovery | null>(null);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- one discovery request per signed-in account
  useEffect(() => {
    if (!isSignedIn) return;
    const controller = new AbortController();
    void getToken().then(async (accessToken) => {
      const result = await discoverEnvironments({ accessToken, orgId }, []);
      if (!controller.signal.aborted) setDiscovery(result);
    });
    return () => controller.abort();
  }, [isSignedIn, getToken, orgId]);

  return isSignedIn ? discovery : null;
}
