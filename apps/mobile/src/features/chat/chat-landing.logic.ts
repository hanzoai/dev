import { LOCAL_CHAT_DRIVER } from "@hanzodev/client-runtime/cloud-chat";

import type { ModelOption } from "../../lib/modelOptions";

/**
 * Whether a signed-in user with this catalog still sees the environment gate on the first screen.
 * The phone lands in chat, so it never does.
 */
export function landsInChat(input: { readonly signedIn: boolean }): boolean {
  return input.signedIn;
}

/** The composer draft the landing keeps, so its text and attachments survive a reload. */
export const CHAT_LANDING_DRAFT_KEY = "chat-landing";

/**
 * The picker rows with the Local group's state applied: while the linked Mac is offline its rows
 * say so and cannot be picked, so a local model is never chosen and quietly answered by the cloud.
 */
export function markLocalOptions(
  options: ReadonlyArray<ModelOption>,
  macOnline: boolean,
  offlineLabel: string,
): ReadonlyArray<ModelOption> {
  if (macOnline) return options;
  return options.map((option) =>
    option.providerDriver === LOCAL_CHAT_DRIVER
      ? { ...option, subtitle: offlineLabel, isUnavailable: true }
      : option,
  );
}
