import {
  type CloudChatAttachment,
  cloudChatServerConfig,
  createCloudChatController,
  type CloudChatController,
  type CloudChatModel,
  type CloudChatState,
} from "@hanzodev/client-runtime/cloud-chat";
import type { ServerConfig } from "@hanzodev/contracts";
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
  /** The catalog as the server config the thread composer and its model picker read. */
  readonly serverConfig: ServerConfig;
  readonly state: CloudChatState;
  readonly send: (prompt: string, attachments?: readonly CloudChatAttachment[]) => Promise<void>;
  readonly selectModel: (id: string, effort?: string | null) => void;
  readonly openThread: (key: string) => Promise<void>;
  readonly newChat: () => void;
  readonly renameThread: (key: string, title: string) => Promise<void>;
  readonly pinThread: (key: string, pinned: boolean) => Promise<void>;
  readonly deleteThread: (key: string) => Promise<void>;
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
      serverConfig: cloudChatServerConfig(state.models),
      state,
      send: chat.send,
      selectModel: chat.selectModel,
      openThread: chat.openThread,
      newChat: chat.reset,
      renameThread: chat.renameThread,
      pinThread: chat.pinThread,
      deleteThread: chat.deleteThread,
    }),
    [chat, isSignedIn, state],
  );
}

/** Starts a fresh conversation; the next message opens a new thread. */
export const startNewCloudChat = (): void => shared().reset();
