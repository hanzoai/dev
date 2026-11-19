import { type ReactNode, useEffect } from "react";

import { cloudAccountService, deactivateCloudRelayAccount } from "./cloudAccountService";
import { hanzoMobileAuthStore } from "./hanzoMobileAuth";
import { hasCloudPublicConfig } from "./publicConfig";

const cloudEnabled = hasCloudPublicConfig();

if (cloudEnabled) {
  void hanzoMobileAuthStore.hydrate();
  cloudAccountService.attach(hanzoMobileAuthStore);
} else {
  deactivateCloudRelayAccount();
}

export function CloudAuthProvider(props: { readonly children: ReactNode }) {
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- ensures SecureStore vault hydration runs when mounted in isolated test trees
  useEffect(() => {
    if (cloudEnabled) {
      void hanzoMobileAuthStore.hydrate();
    }
  }, []);

  return props.children;
}
