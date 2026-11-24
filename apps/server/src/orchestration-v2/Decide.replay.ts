import * as DateTime from "effect/DateTime";
import * as Effect from "effect/Effect";
import * as Exit from "effect/Exit";
import * as Result from "effect/Result";
import { modelSwitchSince } from "./modelSwitch.ts";
import {
  canForkBusySourceNatively,
  CommandPolicyV2,
  type CommandPolicyV2Error,
  goalCommandStopsActiveRun,
  isNativeMaintenanceCommand,
  layer as commandPolicyLayer,
  providerSwitchWaitsForTurn,
  resolveMessageDispatchIntent,
} from "./CommandPolicy.ts";
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

const commandPolicy = Effect.runSync(Effect.provide(CommandPolicyV2, commandPolicyLayer));

const runPolicyCheck = <A>(eff: Effect.Effect<A, CommandPolicyV2Error>) => {
  const exit = Effect.runSyncExit(eff);
  if (Exit.isSuccess(exit)) return { ok: true, value: exit.value ?? null };
  const err = exit.cause.reasons[0]?._tag === "Fail" ? exit.cause.reasons[0].error : undefined;
  return { ok: false, capability: err?.capability, detail: err?.detail };
};

const policyIds = {
  commandId: "c1" as never,
  threadId: "t1" as never,
  providerInstanceId: "p1" as never,
};

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
  commandPolicyNativeMaintenance: ({ text, attachments }) =>
    isNativeMaintenanceCommand({ text, attachments: Array.from({ length: attachments }) as never }),
  commandPolicyGoalStopsActiveRun: ({ commandType, goalStatus }) =>
    goalCommandStopsActiveRun({ type: commandType } as never, goalStatus),
  commandPolicyCanForkBusySourceNatively: ({
    canForkWhileSourceRunning,
    exposesNativeTurnId,
    hasNativeTurnBoundary,
  }) =>
    canForkBusySourceNatively(
      {
        threads: { canForkWhileSourceRunning },
        turns: { exposesNativeTurnId },
      } as never,
      hasNativeTurnBoundary,
    ),
  commandPolicyProviderSwitchWaitsForTurn: (turns) => providerSwitchWaitsForTurn(turns),
  commandPolicySteeringExecution: ({
    forceRestart,
    supportsActiveSteering,
    supportsInterrupt,
    supportsSteeringByInterruptRestart,
  }) =>
    runPolicyCheck(
      commandPolicy.decideSteeringExecution({
        ...policyIds,
        forceRestart,
        capabilities: {
          turns: { supportsActiveSteering, supportsInterrupt, supportsSteeringByInterruptRestart },
        } as never,
      }),
    ),
  commandPolicyEnsureQueuedMessages: ({ supportsQueuedMessages }) =>
    runPolicyCheck(
      commandPolicy.ensureQueuedMessages({
        ...policyIds,
        capabilities: { turns: { supportsQueuedMessages } } as never,
      }),
    ),
  commandPolicyEnsureInterrupt: ({ supportsInterrupt }) =>
    runPolicyCheck(
      commandPolicy.ensureInterrupt({
        ...policyIds,
        capabilities: { turns: { supportsInterrupt } } as never,
      }),
    ),
  commandPolicyEnsureNativeFork: ({
    canForkThread,
    canForkFromTurn,
    fromSpecificTurn,
    nativeThreadIds,
  }) =>
    runPolicyCheck(
      commandPolicy.ensureNativeFork({
        ...policyIds,
        fromSpecificTurn,
        capabilities: {
          threads: { canForkThread, canForkFromTurn },
          identity: { nativeThreadIds },
        } as never,
      }),
    ),
  commandPolicyEnsureRollback: ({
    canRollbackThread,
    providerCanRollbackConversation,
    canConsumeHandoffSummaries,
    providerRollbackReturnsSnapshot,
  }) =>
    runPolicyCheck(
      commandPolicy.ensureRollback({
        ...policyIds,
        capabilities: {
          threads: { canRollbackThread },
          checkpointing: { providerCanRollbackConversation, providerRollbackReturnsSnapshot },
          context: { canConsumeHandoffSummaries },
        } as never,
      }),
    ),
  commandPolicyEnsureContextHandoff: ({
    strategy,
    canConsumeHandoffSummaries,
    acceptsSyntheticUserContext,
    supportsDeltaHandoff,
    supportsFullThreadHandoff,
  }) =>
    runPolicyCheck(
      commandPolicy.ensureContextHandoff({
        ...policyIds,
        strategy,
        capabilities: {
          context: {
            canConsumeHandoffSummaries,
            acceptsSyntheticUserContext,
            supportsDeltaHandoff,
            supportsFullThreadHandoff,
          },
        } as never,
      }),
    ),
  commandPolicyForkExecution: (input) =>
    runPolicyCheck(
      commandPolicy.decideForkExecution({
        ...policyIds,
        sameProvider: input.sameProvider,
        hasStrongNativeSource: input.hasStrongNativeSource,
        hasNativeTurnBoundary: input.hasNativeTurnBoundary,
        sourceRunStatus: input.sourceRunStatus,
        fromSpecificTurn: input.fromSpecificTurn,
        sourceBusy: input.sourceBusy,
        capabilities: {
          threads: {
            canForkThread: input.canForkThread,
            canForkFromTurn: input.canForkFromTurn,
            canForkWhileSourceRunning: input.canForkWhileSourceRunning,
          },
          turns: { exposesNativeTurnId: input.exposesNativeTurnId },
          identity: { nativeThreadIds: input.nativeThreadIds },
          context: {
            canConsumeHandoffSummaries: input.canConsumeHandoffSummaries,
            acceptsSyntheticUserContext: input.acceptsSyntheticUserContext,
            supportsFullThreadHandoff: input.supportsFullThreadHandoff,
          },
        } as never,
      }),
    ),
  commandPolicyMessageDispatchIntent: (input) =>
    resolveMessageDispatchIntent(
      {
        runs: input.runs.map((r: any, idx: number) => ({
          id: r.id,
          status: r.status,
          userMessageId: r.userMessageId,
          providerThreadId: `pt-${idx}`,
          activeAttemptId: `att-${idx}`,
        })),
        messages: input.messages.map((m: any) => ({
          id: m.id,
          text: m.text,
          attachments: Array.from({ length: m.attachments }),
        })),
        providerTurns: input.runs.flatMap((r: any, idx: number) =>
          r.hasRunningProviderTurn ? [{ runAttemptId: `att-${idx}`, status: "running" }] : [],
        ),
        providerThreads: input.runs.map((_: any, idx: number) => ({
          id: `pt-${idx}`,
          providerSessionId: `ps-${idx}`,
        })),
        providerSessions: input.runs.flatMap((r: any, idx: number) =>
          r.turns ? [{ id: `ps-${idx}`, capabilities: { turns: r.turns } }] : [],
        ),
      } as never,
      input.requestedMode,
      input.deliveryIntent,
    ),
};
