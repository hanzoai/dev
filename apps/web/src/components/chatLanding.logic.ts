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
  if (input.build) {
    return input.hasReadyEnvironment ? { kind: "machine" } : { kind: "choose-target" };
  }
  if (input.cloudChatAvailable) return { kind: "chat" };
  return input.hasReadyEnvironment ? { kind: "machine" } : { kind: "choose-target" };
}

/** The targets the run prompt offers, in order. */
export const RUN_TARGETS: readonly RunTarget[] = ["mac", "cloud"];

/** What a signed-out visitor to the hosted app sees: the sign-in and nothing else. */
export function hostedLanding(input: {
  readonly isLoaded: boolean;
  readonly isSignedIn: boolean;
}): "loading" | "sign-in" | "chat" {
  if (!input.isLoaded) return "loading";
  return input.isSignedIn ? "chat" : "sign-in";
}

/** A saved thread's row label: its title, else the fallback. */
export function threadRowLabel(
  thread: { readonly title: string },
  fallback: string,
): string {
  const title = thread.title.trim();
  return title === "" ? fallback : title;
}

/** What "Run in the cloud" says after the sandbox request. Null while nothing was asked. */
export function cloudRunNote(
  result: { readonly kind: "started" | "unavailable" | "failed" } | null,
): "started" | "unavailable" | "failed" | null {
  return result === null ? null : result.kind;
}

/** The model pill's label: the chosen model's name, falling back to its id. */
export function modelPillLabel(
  models: ReadonlyArray<{ readonly id: string; readonly label: string }>,
  id: string,
): string {
  return models.find((model) => model.id === id)?.label ?? id;
}
