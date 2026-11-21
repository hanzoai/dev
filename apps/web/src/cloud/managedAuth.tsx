import {
  type CloudAccountSource,
  createCloudAccountService,
  ManagedRelay,
  setManagedRelaySession,
} from "@hanzodev/client-runtime/relay";
import {
  reportAtomCommandResult,
  runAtomCommand,
  settleAsyncResult,
} from "@hanzodev/client-runtime/state/runtime";
import * as Effect from "effect/Effect";
import { useEffect, type ReactNode } from "react";

import { environmentCatalog } from "../connection/catalog";
import { runtime } from "../lib/runtime";
import { appAtomRegistry } from "../rpc/atomRegistry";

export function deactivateManagedRelayAuthentication(): void {
  setManagedRelaySession(appAtomRegistry, null);
}

export function activateManagedRelayAuthentication(
  accountId: string,
  readIamToken: () => Promise<string | null>,
): void {
  setManagedRelaySession(appAtomRegistry, {
    accountId,
    readIamToken,
  });
}

interface ManagedRelayIamSession extends CloudAccountSource {
  readonly session: {
    readonly id: string;
    readonly getToken: () => Promise<string | null>;
  } | null;
}

function iamTokenReader(session: ManagedRelayIamSession): () => Promise<string | null> {
  return async () =>
    session.session ? await session.session.getToken() : null;
}

const managedRelayAccountService = createCloudAccountService<ManagedRelayIamSession>({
  deactivate: deactivateManagedRelayAuthentication,
  detach: deactivateManagedRelayAuthentication,
  cleanUp: async () => {
    const results = await Promise.all([
      runAtomCommand(appAtomRegistry, environmentCatalog.removeRelayEnvironments, undefined, {
        reportFailure: false,
        reportDefect: false,
      }),
      settleAsyncResult(() =>
        runtime.runPromiseExit(
          ManagedRelay.ManagedRelayClient.pipe(Effect.flatMap((client) => client.resetTokenCache)),
        ),
      ),
    ]);
    for (const result of results) {
      reportAtomCommandResult(result, { label: "cloud account cleanup" });
    }
  },
  activate: ({ source, accountId }) =>
    activateManagedRelayAuthentication(accountId, iamTokenReader(source)),
});

export function ManagedRelayAuthProvider({
  session,
  children,
}: {
  readonly session: ManagedRelayIamSession;
  readonly children: ReactNode;
}) {
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- attaches the auth source to the managed relay account service for the provider's lifetime
  useEffect(() => managedRelayAccountService.attach(session), [session]);
  return children;
}
