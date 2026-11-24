import type { OrchestrationV2AppThread, OrchestrationV2Command } from "@hanzodev/contracts";
import * as DateTime from "effect/DateTime";
import * as Result from "effect/Result";

import { decide } from "./DevWasm.ts";

export type ThreadTriageCommand = Extract<
  OrchestrationV2Command,
  {
    readonly type:
      | "thread.pin"
      | "thread.unpin"
      | "thread.pin.reorder"
      | "thread.active.reorder"
      | "thread.tag.set"
      | "thread.unsnooze"
      | "thread.auto-settle.set"
      | "thread.mark-unread";
  }
>;

interface ThreadTriagePatch {
  readonly pinnedAt?: string | null;
  readonly pinOrderKey?: string | null;
  readonly activeOrderKey?: string | null;
  readonly settledOverride?: "active" | "settled" | null;
  readonly settledAt?: string | null;
  readonly unsettledAt?: string | null;
  readonly snoozedUntil?: string | null;
  readonly snoozedAt?: string | null;
  readonly lastSnoozeWakeAt?: string | null;
  readonly autoSettleDisabledAt?: string | null;
  readonly tags?: ReadonlyArray<string>;
  readonly lastVisitedAt?: string | null;
  readonly updatedAt?: string;
}

type ThreadTriageDecision =
  | { readonly ok: false; readonly error: string }
  | { readonly ok: true; readonly patch: ThreadTriagePatch };

const isoOrNull = (value: DateTime.Utc | null | undefined): string | null =>
  value == null ? null : DateTime.formatIso(value);

const dateOrNull = (value: string | null): DateTime.Utc | null =>
  value === null ? null : DateTime.makeUnsafe(value);

/**
 * Applies a pure thread organization / triage command (`pin`, `unpin`, `pin.reorder`,
 * `active.reorder`, `tag.set`, `unsnooze`, `auto-settle.set`, `mark-unread`) to the thread.
 * Decided by `dev.wasm`.
 */
export function decideThreadTriageCommand(input: {
  readonly thread: OrchestrationV2AppThread;
  readonly command: ThreadTriageCommand;
  readonly now: DateTime.Utc;
  readonly markUnreadVisitedAt?: DateTime.Utc | null;
}): Result.Result<OrchestrationV2AppThread, string> {
  const { thread, command, now } = input;
  const wasmCommand =
    command.type === "thread.mark-unread"
      ? {
          type: "thread.mark-unread" as const,
          markUnreadVisitedAt: isoOrNull(input.markUnreadVisitedAt),
        }
      : command.type === "thread.pin"
        ? {
            type: "thread.pin" as const,
            orderKey: command.orderKey ?? null,
          }
        : command.type === "thread.unpin"
          ? { type: "thread.unpin" as const }
          : command.type === "thread.pin.reorder"
            ? { type: "thread.pin.reorder" as const, orderKey: command.orderKey }
            : command.type === "thread.active.reorder"
              ? { type: "thread.active.reorder" as const, orderKey: command.orderKey }
              : command.type === "thread.tag.set"
                ? { type: "thread.tag.set" as const, tag: command.tag, tagged: command.tagged }
                : command.type === "thread.unsnooze"
                  ? { type: "thread.unsnooze" as const }
                  : { type: "thread.auto-settle.set" as const, enabled: command.enabled };

  const decision = decide("threadTriageCommand", {
    state: {
      threadId: thread.id,
      archived: thread.archivedAt !== null,
      pinnedAt: isoOrNull(thread.pinnedAt),
      pinOrderKey: thread.pinOrderKey ?? null,
      activeOrderKey: thread.activeOrderKey ?? null,
      settledOverride: thread.settledOverride ?? null,
      settledAt: isoOrNull(thread.settledAt),
      unsettledAt: isoOrNull(thread.unsettledAt),
      snoozedUntil: isoOrNull(thread.snoozedUntil),
      snoozedAt: isoOrNull(thread.snoozedAt),
      lastSnoozeWakeAt: isoOrNull(thread.lastSnoozeWakeAt),
      autoSettleDisabledAt: isoOrNull(thread.autoSettleDisabledAt),
      tags: thread.tags ?? [],
      lastVisitedAt: isoOrNull(thread.lastVisitedAt),
      updatedAt: DateTime.formatIso(thread.updatedAt),
    },
    command: wasmCommand,
    nowIso: DateTime.formatIso(now),
    nowMs: DateTime.toEpochMillis(now),
    snoozedUntilMs:
      thread.snoozedUntil == null ? null : DateTime.toEpochMillis(thread.snoozedUntil),
  }) as ThreadTriageDecision;

  if (!decision.ok) return Result.fail(decision.error);
  const { patch } = decision;
  return Result.succeed({
    ...thread,
    ...(patch.pinnedAt !== undefined ? { pinnedAt: dateOrNull(patch.pinnedAt) } : {}),
    ...(patch.pinOrderKey !== undefined ? { pinOrderKey: patch.pinOrderKey } : {}),
    ...(patch.activeOrderKey !== undefined ? { activeOrderKey: patch.activeOrderKey } : {}),
    ...(patch.settledOverride !== undefined ? { settledOverride: patch.settledOverride } : {}),
    ...(patch.settledAt !== undefined ? { settledAt: dateOrNull(patch.settledAt) } : {}),
    ...(patch.unsettledAt !== undefined ? { unsettledAt: dateOrNull(patch.unsettledAt) } : {}),
    ...(patch.snoozedUntil !== undefined ? { snoozedUntil: dateOrNull(patch.snoozedUntil) } : {}),
    ...(patch.snoozedAt !== undefined ? { snoozedAt: dateOrNull(patch.snoozedAt) } : {}),
    ...(patch.lastSnoozeWakeAt !== undefined
      ? { lastSnoozeWakeAt: dateOrNull(patch.lastSnoozeWakeAt) }
      : {}),
    ...(patch.autoSettleDisabledAt !== undefined
      ? { autoSettleDisabledAt: dateOrNull(patch.autoSettleDisabledAt) }
      : {}),
    ...(patch.tags !== undefined ? { tags: patch.tags as NonNullable<typeof thread.tags> } : {}),
    ...(patch.lastVisitedAt !== undefined
      ? { lastVisitedAt: dateOrNull(patch.lastVisitedAt) }
      : {}),
    ...(patch.updatedAt !== undefined ? { updatedAt: DateTime.makeUnsafe(patch.updatedAt) } : {}),
  });
}
