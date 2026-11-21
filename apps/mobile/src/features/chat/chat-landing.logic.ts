/**
 * Whether a signed-in user with this catalog still sees the environment gate on the first screen.
 * The phone lands in chat, so it never does.
 */
export function landsInChat(input: { readonly signedIn: boolean }): boolean {
  return input.signedIn;
}

/** The composer draft the landing keeps, so its text and attachments survive a reload. */
export const CHAT_LANDING_DRAFT_KEY = "chat-landing";
