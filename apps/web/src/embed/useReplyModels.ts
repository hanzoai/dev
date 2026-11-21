import { listChatModels, type CloudChatModel } from "@hanzodev/client-runtime/cloud-chat";
import { useEffect, useState } from "react";

import type { DevCloud } from "./runtime";

/** Every model that writes replies: the catalog's text-output models, decision models never listed. */
export function useReplyModels(cloud: DevCloud): {
  readonly models: ReadonlyArray<CloudChatModel>;
  readonly error: string | null;
} {
  const [state, setState] = useState<{ models: ReadonlyArray<CloudChatModel>; error: string | null }>({
    models: [],
    error: null,
  });
  useEffect(() => {
    let live = true;
    void (async () => {
      try {
        const models = await listChatModels(
          { accessToken: await cloud.token(), orgId: cloud.org ?? null },
          { baseUrl: cloud.api },
        );
        if (live) setState({ models, error: null });
      } catch (e) {
        if (live) setState({ models: [], error: e instanceof Error ? e.message : "unavailable" });
      }
    })();
    return () => {
      live = false;
    };
  }, [cloud]);
  return state;
}
