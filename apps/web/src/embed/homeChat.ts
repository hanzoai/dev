import {
  createCloudChatController,
  createLocalChatHost,
  isLocalChatSlug,
  type CloudChatController,
  type CloudChatState,
  type LocalChatHost,
} from "@hanzodev/client-runtime/cloud-chat";
import { useEffect, useMemo, useSyncExternalStore } from "react";

import type { DevCloud, DevEnvironment } from "./runtime";

/**
 * One cloud-chat controller per account (the one implementation, packages/client-runtime/cloudChat,
 * the phone uses too), so the cloud conversations in Recents are the same on web, desktop and phone.
 * The linked Mac is the controller's `local` host: its models come from `GET /v1/local-ai/models` and
 * a turn on one goes to `POST /v1/local-ai/chat` at that Mac, never to api.hanzo.ai.
 */
const controllers = new Map<string, { controller: CloudChatController; local: LocalChatHost | null }>();

function controllerFor(cloud: DevCloud) {
  let held = controllers.get(cloud.api);
  if (held === undefined) {
    const box: { controller: CloudChatController; local: LocalChatHost | null } = {
      controller: undefined as never,
      local: null,
    };
    box.controller = createCloudChatController({
      getAuth: async () => ({ accessToken: await cloud.token(), orgId: cloud.org ?? null }),
      options: { baseUrl: cloud.api },
      local: () => box.local,
    });
    held = box;
    controllers.set(cloud.api, held);
  }
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
  return useMemo(
    () => ({ state, controller, localSelected: isLocalChatSlug(state.model) }),
    [state, controller],
  );
}
