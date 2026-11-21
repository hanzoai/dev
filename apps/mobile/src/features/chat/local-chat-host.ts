import { createLocalChatHost, type LocalChatHost } from "@hanzodev/client-runtime/cloud-chat";
import type { DiscoveryFetch } from "@hanzodev/client-runtime/discovery";
import { AuthOrchestrationOperateScope, type EnvironmentId } from "@hanzodev/contracts";
import * as Option from "effect/Option";
import { Atom } from "effect/reactivity";
import { fetch as expoFetch } from "expo/fetch";

import { appAtomRegistry } from "../../state/atom-registry";
import { environmentPresentations } from "../../state/presentation";
import { environmentSession, sessionHasScope } from "../../state/session";

// The linked Mac's token, when it presents as a plain bearer. A DPoP-bound connection (Hanzo
// Link relay) needs a signed proof per request, which a streaming fetch cannot yet carry.
const macHost = Atom.family((id: EnvironmentId) =>
  Atom.make((get): LocalChatHost | null => {
    const environment = get(environmentPresentations.presentationAtom(id));
    if (
      environment?.connection.phase !== "connected" ||
      environment.serverConfig?.environment.capabilities.localChat !== true ||
      !sessionHasScope(get(environmentSession.sessionStateAtom(id)), AuthOrchestrationOperateScope)
    )
      return null;
    const prepared = Option.getOrNull(get(environmentSession.preparedConnectionValueAtom(id)));
    if (prepared === null || prepared.httpAuthorization?._tag !== "Bearer") return null;
    return createLocalChatHost({
      baseUrl: prepared.httpBaseUrl,
      bearer: prepared.httpAuthorization.token,
      // expo's fetch streams the body; React Native's own buffers it.
      fetch: ((input, init) =>
        expoFetch(
          input,
          init as Parameters<typeof expoFetch>[1],
        ) as unknown as Promise<Response>) satisfies DiscoveryFetch,
    });
  }),
);

/** Every connected environment that can chat locally; the first is the linked Mac. */
export const linkedMacHostAtom = Atom.make((get): LocalChatHost | null => {
  for (const id of get(environmentPresentations.presentationsAtom).keys()) {
    const host = get(macHost(id));
    if (host !== null) return host;
  }
  return null;
}).pipe(Atom.withLabel("mobile:chat:linked-mac-host"));

/** The linked Mac right now, or null while it is offline. Read at send time, never cached. */
export const linkedMacHost = (): LocalChatHost | null => appAtomRegistry.get(linkedMacHostAtom);
