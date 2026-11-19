import {
  createCloudAccountService,
  ManagedRelay,
  setManagedRelaySession,
} from "@hanzodev/client-runtime/relay";
import {
  reportAtomCommandResult,
  runAtomCommand,
  settleAsyncResult,
  squashAtomCommandFailure,
} from "@hanzodev/client-runtime/state/runtime";
import * as Effect from "effect/Effect";

import { runtime } from "../../lib/runtime";
import { appAtomRegistry } from "../../state/atom-registry";
import {
  getComposerCloudAccountId,
  restoreCloudComposerDrafts,
} from "../../state/use-composer-drafts";
import {
  releaseAgentAwarenessRelayTokenProvider,
  setAgentAwarenessRelayTokenProvider,
  unregisterAgentAwarenessDeviceForCurrentUser,
} from "../agent-awareness/remoteRegistration";
import { removeCloudEnvironments } from "./cloud-drafts";
import { clearConnectOnboardingRequest, requestConnectOnboarding } from "./connectOnboarding";
import type { HanzoMobileAuthController } from "./hanzoMobileAuth";

export function deactivateCloudRelayAccount(): void {
  setAgentAwarenessRelayTokenProvider(null);
  setManagedRelaySession(appAtomRegistry, null);
}

export function activateCloudRelayAccount(
  accountId: string,
  tokenProvider: () => Promise<string | null>,
): void {
  setAgentAwarenessRelayTokenProvider(tokenProvider, accountId);
  setManagedRelaySession(appAtomRegistry, {
    accountId,
    readClerkToken: tokenProvider,
  });
}

function hanzoTokenReader(
  source: Pick<HanzoMobileAuthController, "getToken">,
  accountId?: string,
): () => Promise<string | null> {
  return async () => await source.getToken(accountId);
}

const cleanUpCloudAccount = async (
  source: HanzoMobileAuthController,
  accountId: string | null,
  wasActive: boolean,
) => {
  const removal = await runAtomCommand(appAtomRegistry, removeCloudEnvironments, accountId, {
    reportFailure: false,
    reportDefect: false,
  });
  if (removal._tag !== "Success") throw squashAtomCommandFailure(removal);
  const results = await Promise.all([
    settleAsyncResult(() =>
      runtime.runPromiseExit(
        ManagedRelay.ManagedRelayClient.pipe(Effect.flatMap((client) => client.resetTokenCache)),
      ),
    ),
    ...(wasActive
      ? [
          settleAsyncResult(() =>
            runtime.runPromiseExit(
              unregisterAgentAwarenessDeviceForCurrentUser(
                hanzoTokenReader(source, accountId ?? undefined),
              ),
            ),
          ),
        ]
      : []),
  ]);
  for (const result of results) {
    reportAtomCommandResult(result, { label: "cloud account cleanup" });
  }
};

export const cloudAccountService = createCloudAccountService<HanzoMobileAuthController>({
  deactivate: deactivateCloudRelayAccount,
  // Detaching is not a sign-out: the user is usually still signed in, so keep
  // lock-screen activities and the persisted registration for the next attach.
  detach: () => {
    releaseAgentAwarenessRelayTokenProvider();
    setManagedRelaySession(appAtomRegistry, null);
  },
  // A not-yet-presented onboarding request belongs to the account that left.
  onSignOutTransition: clearConnectOnboardingRequest,
  cleanUp: ({ source, accountId, wasActive }) => cleanUpCloudAccount(source, accountId, wasActive),
  prepare: async ({ source, accountId, isCurrent }) => {
    const storedAccount = await getComposerCloudAccountId();
    if (storedAccount !== null && storedAccount !== accountId) {
      await cleanUpCloudAccount(source, storedAccount, false);
    }
    if (!isCurrent()) return;
    await restoreCloudComposerDrafts(accountId);
  },
  activate: ({ source, accountId, isAccountTransition }) => {
    activateCloudRelayAccount(accountId, hanzoTokenReader(source, accountId));
    // Issued after cleanup so the sheet never lists the previous account's environments.
    if (isAccountTransition) requestConnectOnboarding(accountId);
  },
});
