import {
  createCloudChatController,
  runInCloud,
  type CloudChatController,
  type CloudChatModel,
  type CloudChatState,
  type RunInCloudResult,
} from "@hanzodev/client-runtime/cloud-chat";
import type { DiscoveryFetch } from "@hanzodev/client-runtime/discovery";
import { fetch as expoFetch } from "expo/fetch";
import { useEffect, useMemo, useSyncExternalStore } from "react";

import { hanzoMobileAuthStore, useAuth } from "../cloud/hanzoMobileAuth";
import { cloudThreadStore } from "./cloud-thread-store";

export type { CloudChatModel, CloudChatState };

export interface CloudChat {
  /** Whether a prompt can be answered without an environment: true while signed in. */
  readonly available: boolean;
  readonly models: readonly CloudChatModel[];
  readonly state: CloudChatState;
  readonly send: (prompt: string) => Promise<void>;
  readonly selectModel: (id: string) => void;
  readonly openThread: (key: string) => Promise<void>;
  readonly newChat: () => void;
  readonly runInCloud: () => Promise<RunInCloudResult>;
}

const getAuth = async () => ({
  accessToken: await hanzoMobileAuthStore.getToken(),
  orgId: hanzoMobileAuthStore.getSnapshot().activeOrgId,
  userId: hanzoMobileAuthStore.getSnapshot().activeAccount?.id ?? null,
});

// React Native's own fetch buffers the body; expo's streams it, so tokens arrive as they are written.
const streamingFetch: DiscoveryFetch = (input, init) =>
  expoFetch(input, init as Parameters<typeof expoFetch>[1]) as unknown as Promise<Response>;

let controller: CloudChatController | null = null;
const shared = (): CloudChatController => {
  if (controller === null) {
    controller = createCloudChatController({
      getAuth,
      store: cloudThreadStore,
      options: { fetch: streamingFetch },
    });
    void controller.loadModels();
  }
  return controller;
};

/** Chat that needs no environment: replies stream from api.hanzo.ai, threads sync with hanzo.app. */
export function useCloudChat(): CloudChat {
  const { isSignedIn, userId } = useAuth();
  const chat = shared();
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- threads are per account: reload the list when the signed-in account changes
  useEffect(() => {
    if (isSignedIn) void chat.loadThreads();
  }, [chat, isSignedIn, userId]);
  const state = useSyncExternalStore(chat.subscribe, chat.getState, chat.getState);
  return useMemo(
    () => ({
      available: isSignedIn,
      models: state.models,
      state,
      send: chat.send,
      selectModel: chat.selectModel,
      openThread: chat.openThread,
      newChat: chat.reset,
      runInCloud: async () => runInCloud(await getAuth()),
    }),
    [chat, isSignedIn, state],
  );
}
