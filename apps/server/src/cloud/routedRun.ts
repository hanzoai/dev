import { CLOUD_CONNECT_SUBJECT } from "../auth/EnvironmentAuth.ts";
import type { OrchestrationV2Command, ThreadId } from "@hanzodev/contracts";

/**
 * A routed run is one started by another of the user's surfaces through Hanzo Link, whose
 * sessions prove a DPoP key under the Link subject. A paired device (pairing code, LAN) is
 * the owner's own and is not routed.
 */
export const isRoutedSession = (session: {
  readonly method: string;
  readonly subject: string;
}): boolean => session.method === "dpop-access-token" && session.subject === CLOUD_CONNECT_SUBJECT;

/** The provider instance a command would run on, or the thread whose instance it would use. */
export const commandProviderTarget = (
  command: OrchestrationV2Command,
):
  | { readonly _tag: "instance"; readonly instanceId: string }
  | { readonly _tag: "thread"; readonly threadId: ThreadId }
  | null => {
  switch (command.type) {
    case "thread.create":
    case "thread.model-selection.set":
    case "provider.switch":
    case "delegated_task.request":
      return { _tag: "instance", instanceId: command.modelSelection.instanceId };
    case "message.dispatch":
      return command.modelSelection === undefined
        ? { _tag: "thread", threadId: command.threadId }
        : { _tag: "instance", instanceId: command.modelSelection.instanceId };
    default:
      return null;
  }
};
