import {
  createCloudChatController,
  createLocalChatHost,
  isLocalChatSlug,
  listPersonas,
  readPersona,
  type CloudChatController,
  type CloudChatState,
  type LocalChatHost,
  type Persona,
  type PersonaRecord,
} from "@hanzodev/client-runtime/cloud-chat";
import { useCallback, useEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";

import type { DevCloud, DevEnvironment } from "./runtime";

/**
 * One cloud-chat controller per account (the one implementation, packages/client-runtime/cloudChat,
 * the phone uses too), so the cloud conversations in Recents are the same on web, desktop and phone.
 * The linked Mac is the controller's `local` host: its models come from `GET /v1/local-ai/models` and
 * a turn on one goes to `POST /v1/local-ai/chat` at that Mac, never to api.hanzo.ai.
 */
const controllers = new Map<
  string,
  { controller: CloudChatController; local: LocalChatHost | null; cloud: DevCloud }
>();

export function controllerFor(cloud: DevCloud) {
  let held = controllers.get(cloud.api);
  if (held === undefined) {
    const box: { controller: CloudChatController; local: LocalChatHost | null; cloud: DevCloud } = {
      controller: undefined as never,
      local: null,
      cloud,
    };
    box.controller = createCloudChatController({
      getAuth: async () => ({ accessToken: await box.cloud.token(), orgId: box.cloud.org ?? null }),
      options: { baseUrl: cloud.api },
      local: () => box.local,
    });
    held = box;
    controllers.set(cloud.api, held);
  }
  // The host may hand a new token source (a refreshed session); the one controller reads the latest.
  held.cloud = cloud;
  return held;
}

export function useHomeChat(cloud: DevCloud, machine: DevEnvironment | null) {
  const held = controllerFor(cloud);
  const controller = held.controller;
  const state: CloudChatState = useSyncExternalStore(
    controller.subscribe,
    controller.getState,
    controller.getState,
  );
  const runtime = machine?.runtime ?? null;
  useEffect(() => {
    held.local = runtime
      ? createLocalChatHost({
          baseUrl: runtime.zap,
          bearer: null,
          fetch: async (input, init) => {
            const token = await runtime.token();
            return fetch(input, {
              ...init,
              headers: { ...(init?.headers as Record<string, string>), Authorization: `Bearer ${token}` },
            });
          },
        })
      : null;
    void controller.loadLocalModels();
  }, [held, controller, runtime]);
  useEffect(() => {
    void controller.loadModels();
    void controller.loadThreads();
  }, [controller]);
  // Personas: the org's agents, chosen per conversation. Their records are read when first used.
  const [personas, setPersonas] = useState<ReadonlyArray<Persona>>([]);
  const [persona, setPersona] = useState<string | null>(null);
  const records = useRef(new Map<string, PersonaRecord | null>());
  useEffect(() => {
    let live = true;
    void (async () => {
      const auth = { accessToken: await cloud.token(), orgId: cloud.org ?? null };
      const roster = await listPersonas(auth, { baseUrl: cloud.api });
      if (live) setPersonas(roster);
    })().catch(() => {});
    return () => {
      live = false;
    };
  }, [cloud]);
  const personaRecord = useCallback(async (): Promise<PersonaRecord | null> => {
    if (persona === null) return null;
    if (!records.current.has(persona)) {
      const auth = { accessToken: await cloud.token(), orgId: cloud.org ?? null };
      records.current.set(persona, await readPersona(auth, persona, { baseUrl: cloud.api }));
    }
    return records.current.get(persona) ?? null;
  }, [cloud, persona]);
  return useMemo(
    () => ({
      state,
      controller,
      localSelected: isLocalChatSlug(state.model),
      personas,
      persona,
      selectPersona: setPersona,
      personaRecord,
    }),
    [state, controller, personas, persona, personaRecord],
  );
}

/** The account's cloud conversations, newest first (pinned first): the ones hanzo.app Chat lists and the phone shows. */
export function useCloudThreads(cloud: DevCloud) {
  const { controller } = controllerFor(cloud);
  const state = useSyncExternalStore(controller.subscribe, controller.getState, controller.getState);
  useEffect(() => {
    void controller.loadThreads();
  }, [controller]);
  return { controller, threads: state.threads, open: state.threadKey ?? state.threadId };
}
