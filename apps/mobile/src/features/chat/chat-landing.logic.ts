export type RunTarget = "mac" | "cloud";

export type SendOutcome =
  /** Nothing to send. */
  | { readonly kind: "none" }
  /** Answer in the cloud chat; no machine is involved. */
  | { readonly kind: "chat" }
  /** The user has a ready environment: start the task on it. */
  | { readonly kind: "machine" }
  /** No chat path and no environment: ask where the work should run. */
  | { readonly kind: "choose-target" };

/**
 * What sending from the landing composer does. A machine is needed only to build on one; plain
 * chat never needs an environment. `build` is the user's explicit ask to run on a machine.
 */
export function resolveSend(input: {
  readonly prompt: string;
  readonly build: boolean;
  readonly hasReadyEnvironment: boolean;
  readonly cloudChatAvailable: boolean;
}): SendOutcome {
  if (input.prompt.trim() === "") return { kind: "none" };
  if (input.build)
    return input.hasReadyEnvironment ? { kind: "machine" } : { kind: "choose-target" };
  if (input.cloudChatAvailable) return { kind: "chat" };
  return input.hasReadyEnvironment ? { kind: "machine" } : { kind: "choose-target" };
}

/** The targets the run prompt offers, in order. */
export const RUN_TARGETS: readonly RunTarget[] = ["mac", "cloud"];

/**
 * Whether a signed-in user with this catalog still sees the environment gate on the first screen.
 * The phone lands in chat, so it never does.
 */
export function landsInChat(input: { readonly signedIn: boolean }): boolean {
  return input.signedIn;
}

export interface SandboxNotice {
  readonly title: string;
  readonly detail: string;
  /** Whether the notice offers to open the sandbox's environment. */
  readonly opens: boolean;
}

/** What the Build chip's cloud sandbox answer says, in plain words. */
export function sandboxNotice(
  result:
    | { readonly kind: "started"; readonly sessionId: string }
    | { readonly kind: "unavailable" }
    | { readonly kind: "failed"; readonly message: string },
  text: {
    readonly ready: string;
    readonly readyDetail: string;
    readonly unavailable: string;
    readonly unavailableDetail: string;
    readonly failed: string;
  },
): SandboxNotice {
  if (result.kind === "started")
    return { title: text.ready, detail: text.readyDetail, opens: true };
  if (result.kind === "unavailable")
    return { title: text.unavailable, detail: text.unavailableDetail, opens: false };
  return { title: text.failed, detail: result.message, opens: false };
}

/** The composer draft the landing keeps, so its text and attachments survive a reload. */
export const CHAT_LANDING_DRAFT_KEY = "chat-landing";
