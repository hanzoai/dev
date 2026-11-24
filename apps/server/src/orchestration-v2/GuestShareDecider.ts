import type {
  GuestMessageAttribution,
  GuestMessageId,
  OrchestrationMessageContext,
  OrchestrationV2AppThread,
  OrchestrationV2ServerCommand,
  ThreadGuestShare,
} from "@hanzodev/contracts";
import * as DateTime from "effect/DateTime";
import * as Result from "effect/Result";

import { decide } from "./DevWasm.ts";

export type GuestShareCommand = Extract<
  OrchestrationV2ServerCommand,
  {
    readonly type:
      | "thread.guest-share.open"
      | "thread.guest-share.redeem"
      | "thread.guest-share.close"
      | "thread.guest-message.submit"
      | "thread.guest-message.dismiss";
  }
>;

/** Failures a guest may be told about. Any other dispatch failure reaches them as "rejected". */
export const GUEST_FAILURE = {
  inactive: "This guest link is no longer active.",
  used: "This guest link was already used.",
  busy: "Wait for the owner to answer your earlier messages.",
  filesBusy: "Wait for the owner to answer your earlier messages before you attach more files.",
} as const;

/**
 * Stored files a share change leaves behind: those of waiting messages that were dismissed or
 * dropped with the share. A sent message keeps its files, which its thread message now holds.
 * Decided by `dev.wasm`.
 */
export function releasedGuestAttachmentIds(
  before: ThreadGuestShare | null,
  after: ThreadGuestShare | null,
): ReadonlyArray<string> {
  return decide("guestShareReleasedAttachments", { before, after }) as ReadonlyArray<string>;
}

type GuestShareCommandDecision =
  | { readonly ok: false; readonly error: string }
  | { readonly ok: true; readonly changed: false }
  | {
      readonly ok: true;
      readonly changed: true;
      readonly guestShare: ThreadGuestShare | null;
      readonly unsettle: boolean;
    };

/**
 * Applies a guest share command to the thread. The orchestrator runs commands one at a time, so
 * checking state here is what makes a link redeem once and a message send once. Decided by `dev.wasm`.
 */
export function decideGuestShareCommand(input: {
  readonly thread: OrchestrationV2AppThread;
  readonly command: GuestShareCommand;
  readonly now: DateTime.Utc;
}): Result.Result<OrchestrationV2AppThread, string> {
  const { thread, command, now } = input;
  const share = thread.guestShare ?? null;
  const parsedExpiresAt = share === null ? Number.NaN : Date.parse(share.expiresAt);
  const decision = decide("guestShareCommand", {
    share,
    archived: thread.archivedAt !== null,
    settled: thread.settledOverride === "settled",
    command,
    nowIso: DateTime.formatIso(now),
    nowMs: DateTime.toEpochMillis(now),
    expiresAtMs: Number.isFinite(parsedExpiresAt) ? parsedExpiresAt : null,
  }) as GuestShareCommandDecision;

  if (!decision.ok) return Result.fail(decision.error);
  if (!decision.changed) return Result.succeed(thread);
  return decision.unsettle
    ? Result.succeed({
        ...thread,
        guestShare: decision.guestShare,
        settledOverride: null,
        settledAt: null,
        unsettledAt: now,
        updatedAt: now,
      })
    : Result.succeed({
        ...thread,
        guestShare: decision.guestShare,
        updatedAt: now,
      });
}

type GuestMessageSendDecision =
  | { readonly ok: false; readonly error: string }
  | {
      readonly ok: true;
      readonly guestShare: ThreadGuestShare;
      readonly text: string;
      readonly guest: GuestMessageAttribution;
    };

/**
 * Marks a guest message sent. `text` is what the agent receives: it names the guest so the agent
 * does not mistake it for the owner's words, and so a guest cannot start a slash command.
 * `guest` lets clients show the guest's own text with their name beside it. Decided by `dev.wasm`.
 */
export function decideGuestMessageSend(input: {
  readonly thread: OrchestrationV2AppThread;
  readonly guestMessageId: GuestMessageId;
  readonly now: DateTime.Utc;
}): Result.Result<
  {
    readonly thread: OrchestrationV2AppThread;
    readonly text: string;
    readonly guest: GuestMessageAttribution;
  },
  string
> {
  const decision = decide("guestShareSend", {
    share: input.thread.guestShare ?? null,
    guestMessageId: input.guestMessageId,
  }) as GuestMessageSendDecision;
  if (!decision.ok) return Result.fail(decision.error);
  return Result.succeed({
    thread: { ...input.thread, guestShare: decision.guestShare, updatedAt: input.now },
    text: decision.text,
    guest: decision.guest,
  });
}

type GuestShareCancelDecision = {
  readonly changed: boolean;
  readonly guestShare: ThreadGuestShare | null;
};

/**
 * The owner cancelled a queued send, so the agent never got the message. It shows as dismissed,
 * which the guest can see, instead of sent with no reply ever coming. A share that changed since
 * the send is left alone. Decided by `dev.wasm`.
 */
export function guestShareAfterCancelledSend(
  thread: OrchestrationV2AppThread,
  guest: Pick<GuestMessageAttribution, "guestMessageId" | "shareId">,
  now: DateTime.Utc,
): OrchestrationV2AppThread {
  const decision = decide("guestShareAfterCancelledSend", {
    share: thread.guestShare ?? null,
    guestMessageId: guest.guestMessageId,
    shareId: guest.shareId,
  }) as GuestShareCancelDecision;
  if (!decision.changed) return thread;
  return {
    ...thread,
    guestShare: decision.guestShare,
    updatedAt: now,
  };
}

/** Guest attribution comes only from `decideGuestMessageSend`; a client cannot claim it. */
export function withoutGuestAttribution<
  C extends { readonly context?: OrchestrationMessageContext | undefined },
>(command: C): C {
  if (command.context?.guest === undefined) return command;
  const { guest: _guest, ...context } = command.context;
  return { ...command, context };
}
