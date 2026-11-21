import {
  createCloudChatController,
  runInCloud,
  type CloudChatController,
  type CloudChatModel,
  type CloudChatState,
  type RunInCloudResult,
} from "@hanzodev/client-runtime/cloud-chat";
import { useMemo, useSyncExternalStore } from "react";

import { hanzoMobileAuthStore, useAuth } from "../cloud/hanzoMobileAuth";

export type { CloudChatModel, CloudChatState };

export interface CloudChat {
  /** Whether a prompt can be answered without an environment: true while signed in. */
  readonly available: boolean;
  readonly models: readonly CloudChatModel[];
  readonly state: CloudChatState;
  readonly send: (prompt: string) => Promise<void>;
  readonly selectModel: (id: string) => void;
  readonly runInCloud: () => Promise<RunInCloudResult>;
}

const getAuth = async () => ({
  accessToken: await hanzoMobileAuthStore.getToken(),
  orgId: hanzoMobileAuthStore.getSnapshot().activeOrgId,
});

let controller: CloudChatController | null = null;
const shared = (): CloudChatController => {
  if (controller === null) {
    controller = createCloudChatController({ getAuth });
    void controller.loadModels();
  }
  return controller;
};

/** Chat that needs no environment: replies stream from api.hanzo.ai, threads sync with hanzo.app. */
export function useCloudChat(): CloudChat {
  const { isSignedIn } = useAuth();
  const chat = shared();
  const state = useSyncExternalStore(chat.subscribe, chat.getState, chat.getState);
  return useMemo(
    () => ({
      available: isSignedIn,
      models: state.models,
      state,
      send: chat.send,
      selectModel: chat.selectModel,
      runInCloud: async () => runInCloud(await getAuth()),
    }),
    [chat, isSignedIn, state],
  );
}
