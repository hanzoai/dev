import * as DateTime from "effect/DateTime";
import * as Result from "effect/Result";
import { modelSwitchSince } from "./modelSwitch.ts";
import {
  decideGuestMessageSend,
  decideGuestShareCommand,
  guestShareAfterCancelledSend,
  releasedGuestAttachmentIds,
} from "./GuestShareDecider.ts";
import {
  exceededDispatchModeLimit,
  narrowToCeiling,
  runtimeModeCeilingRefusal,
  threadsStartingWork,
} from "./DispatchModeLimit.ts";

/** One recorded call: `fn` applied to `input` gave `output` (`null` for undefined). */
export interface DecideCase {
  readonly source: "real" | "synthetic" | "exhaustive";
  readonly fn: string;
  readonly input: any;
  readonly output: unknown;
}

/** The pure decide functions by their corpus names, over JSON inputs. */
export const decideFns: Record<string, (input: any) => unknown> = {
  exceeded: ({ limit, modes }) => exceededDispatchModeLimit(limit, modes),
  narrow: ({ limit, ceiling }) => narrowToCeiling(limit ?? undefined, ceiling ?? undefined),
  threadsStartingWork: ({ events }) => [...threadsStartingWork(events)],
  modelSwitch: (input) =>
    modelSwitchSince(
      {
        runs: input.runs.map((r: any) => ({
          ordinal: r.ordinal,
          status: r.status,
          completedAt: r.completedAt === null ? null : DateTime.makeUnsafe(r.completedAt),
          contextHandoffId: r.handoff ? "h" : null,
          userMessageId: r.userMessageId,
          modelSelection: r.modelSelection,
        })),
        turnItems: input.items.map((i: any) => ({
          type: "user_message",
          messageId: i.messageId,
          inputIntent: i.inputIntent,
          status: i.status,
          text: i.text,
          attachments: Array.from({ length: i.attachments }),
        })),
      } as never,
      input.to,
      input.baseline,
    ),
  ceilingRefusal: ({ max, events, runtimeModes }) =>
    runtimeModeCeilingRefusal(max, events, new Map(runtimeModes)),
  guestShareReleasedAttachments: ({ before, after }) => releasedGuestAttachmentIds(before, after),
  guestShareCommand: (input) => {
    const now = DateTime.toUtc(DateTime.makeUnsafe(input.nowIso));
    const thread = {
      guestShare: input.share,
      archivedAt: input.archived ? now : null,
      settledOverride: input.settled ? "settled" : null,
    } as never;
    const res = decideGuestShareCommand({ thread, command: input.command, now });
    if (Result.isFailure(res)) return { ok: false, error: res.failure };
    if (res.success === thread) return { ok: true, changed: false };
    return {
      ok: true,
      changed: true,
      guestShare: res.success.guestShare ?? null,
      unsettle: input.settled && res.success.settledOverride === null,
    };
  },
  guestShareSend: ({ share, guestMessageId }) => {
    const now = DateTime.toUtc(DateTime.makeUnsafe("2026-11-20T12:00:00.000Z"));
    const res = decideGuestMessageSend({
      thread: { guestShare: share } as never,
      guestMessageId,
      now,
    });
    return Result.isFailure(res)
      ? { ok: false, error: res.failure }
      : {
          ok: true,
          guestShare: res.success.thread.guestShare ?? null,
          text: res.success.text,
          guest: res.success.guest,
        };
  },
  guestShareAfterCancelledSend: ({ share, guestMessageId, shareId }) => {
    const now = DateTime.toUtc(DateTime.makeUnsafe("2026-11-20T12:00:00.000Z"));
    const thread = { guestShare: share } as never;
    const next = guestShareAfterCancelledSend(thread, { guestMessageId, shareId }, now);
    return next === thread
      ? { changed: false, guestShare: share }
      : { changed: true, guestShare: next.guestShare ?? null };
  },
};
