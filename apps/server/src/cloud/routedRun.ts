import type { OrchestrationV2Command, ThreadId } from "@hanzodev/contracts";

/**
 * A routed run is one started by another of the user's surfaces through Hanzo Connect, whose
 * sessions prove a DPoP key. This desktop's own client uses a cookie or a desktop bearer.
 */
export const isRoutedSession = (method: string): boolean => method === "dpop-access-token";

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
