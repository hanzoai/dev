import { type ReactNode, useEffect } from "react";
import * as Linking from "expo-linking";

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
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- ensures SecureStore vault hydration and hanzo:// / hanzodev:// OAuth callback listeners run when mounted
  useEffect(() => {
    if (cloudEnabled) {
      void hanzoMobileAuthStore.hydrate();
    }
    const subscription = Linking.addEventListener("url", ({ url }) => {
      void hanzoMobileAuthStore.handleDeepLinkCallback(url);
    });
    return () => {
      subscription.remove();
    };
  }, []);

  return props.children;
}
