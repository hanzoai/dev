import {
  createCloudChatController,
  listChatThreads,
  runInCloud,
  type CloudChatController,
  type CloudChatState,
  type CloudChatThread,
} from "@hanzodev/client-runtime/cloud-chat";
import { useEffect, useState, useSyncExternalStore } from "react";

import { getHanzoWebAuthController, useAuth } from "./hanzoWebAuth";

const getAuth = async () => {
  const auth = getHanzoWebAuthController();
  return { accessToken: await auth.getToken(), orgId: auth.activeOrgId };
};

let controller: CloudChatController | null = null;
const shared = (): CloudChatController => {
  if (controller === null) {
    controller = createCloudChatController({ getAuth });
    void controller.loadModels();
  }
  return controller;
};

/** Plain chat with no environment: replies stream from api.hanzo.ai, threads sync with hanzo.app. */
export function useCloudChat() {
  const chat = shared();
  const state: CloudChatState = useSyncExternalStore(chat.subscribe, chat.getState, chat.getState);
  return {
    state,
    send: chat.send,
    open: chat.open,
    reset: chat.reset,
    selectModel: chat.selectModel,
    runInCloud: async () => runInCloud(await getAuth()),
  };
}

/** Saved threads, refreshed when a turn finishes or syncs. Empty while signed out or unavailable. */
export function useCloudThreads(): ReadonlyArray<CloudChatThread> {
  const { isSignedIn } = useAuth();
  const { sending, threadId } = useCloudChat().state;
  const [threads, setThreads] = useState<ReadonlyArray<CloudChatThread>>([]);
  const refresh = async () => {
    try {
      setThreads(await listChatThreads(await getAuth()));
    } catch {
      setThreads([]);
    }
  };
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- mirrors the server thread list after each turn
  useEffect(() => {
    // oxlint-disable-next-line react/set-state-in-effect -- State changes only after the thread list request settles.
    if (isSignedIn && !sending) void refresh();
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- A new or switched thread refreshes the list.
  }, [isSignedIn, sending, threadId, refresh]);
  return isSignedIn ? threads : [];
}
