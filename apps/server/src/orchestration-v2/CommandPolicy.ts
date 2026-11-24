import {
  type ChatAttachment,
  CommandId,
  type OrchestrationV2Command,
  OrchestrationV2ProviderCapabilities,
  type OrchestrationV2GoalCommand,
  type OrchestrationV2ProviderGoalStatus,
  type OrchestrationV2Run,
  OrchestrationV2ThreadProjection,
  ProviderInstanceId,
  ThreadId,
} from "@hanzodev/contracts";
import { latestExecutedRun } from "@hanzodev/shared/orchestrationV2ThreadError";
import * as Context from "effect/Context";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Schema from "effect/Schema";

import { decide } from "./DevWasm.ts";

export const SteeringExecutionPolicyV2 = Schema.Literals(["active_steering", "interrupt_restart"]);
export type SteeringExecutionPolicyV2 = typeof SteeringExecutionPolicyV2.Type;

export const ForkExecutionPolicyV2 = Schema.Literals(["native_fork", "portable_context"]);
export type ForkExecutionPolicyV2 = typeof ForkExecutionPolicyV2.Type;

export const CommandPolicyCapability = Schema.Literals([
  "queued_messages",
  "active_steering",
  "interrupt",
  "interrupt_restart_steering",
  "native_fork",
  "fork_from_turn",
  "rollback",
  "rollback_snapshot",
  "context_handoff",
  "strong_terminal_status",
]);
export type CommandPolicyCapability = typeof CommandPolicyCapability.Type;

export class CommandPolicyCapabilityUnsupportedError extends Schema.TaggedError<CommandPolicyCapabilityUnsupportedError>()(
  "CommandPolicyCapabilityUnsupportedError",
  {
    commandId: CommandId,
    threadId: ThreadId,
    providerInstanceId: ProviderInstanceId,
    capability: CommandPolicyCapability,
    detail: Schema.String,
  },
) {
  override get message(): string {
    return `${this.providerInstanceId} cannot satisfy ${this.capability} for command ${this.commandId}: ${this.detail}`;
  }
}

export const CommandPolicyV2Error = Schema.Union([CommandPolicyCapabilityUnsupportedError]);
export type CommandPolicyV2Error = typeof CommandPolicyV2Error.Type;

type MessageDispatchMode = Extract<
  OrchestrationV2Command,
  { readonly type: "message.dispatch" }
>["dispatchMode"];

export function isNativeMaintenanceCommand(message: {
  readonly text: string;
  readonly attachments: ReadonlyArray<ChatAttachment>;
  readonly context?: import("@hanzodev/contracts").OrchestrationMessageContext | undefined;
}): boolean {
  return decide("commandPolicyNativeMaintenance", {
    text: message.text,
    attachments: message.attachments.length,
  }) as boolean;
}

/**
 * A goal command sent into active work stops that work when it starts or
 * replaces goal work, or ends a goal that is driving the run. Decided by `dev.wasm`.
 */
export function goalCommandStopsActiveRun(
  command: OrchestrationV2GoalCommand,
  goalStatus: OrchestrationV2ProviderGoalStatus | null,
): boolean {
  return decide("commandPolicyGoalStopsActiveRun", {
    commandType: command.type,
    goalStatus,
  }) as boolean;
}

export function resolveMessageDispatchIntent(
  projection: OrchestrationV2ThreadProjection,
  requestedMode: MessageDispatchMode,
  deliveryIntent?: "auto" | "steer" | "restart",
): MessageDispatchMode {
  return decide("commandPolicyMessageDispatchIntent", {
    runs: projection.runs.map((run) => ({
      id: run.id,
      status: run.status,
      userMessageId: run.userMessageId,
      hasRunningProviderTurn: projection.providerTurns.some(
        (turn) => turn.runAttemptId === run.activeAttemptId && turn.status === "running",
      ),
      turns: providerTurnCapabilities(projection, run) ?? null,
    })),
    messages: projection.messages.map((message) => ({
      id: message.id,
      text: message.text,
      attachments: message.attachments.length,
    })),
    requestedMode,
    ...(deliveryIntent === undefined ? {} : { deliveryIntent }),
  }) as MessageDispatchMode;
}

/** A run that still owns the thread's provider turn. */
export function isActiveRun(run: OrchestrationV2Run): boolean {
  return (
    run.status === "preparing" ||
    run.status === "starting" ||
    run.status === "running" ||
    run.status === "waiting"
  );
}

/**
 * Providers that key forks on native turn ids can only fork a busy source from a turn with one;
 * a synthetic or missing boundary would fork the live conversation, including the running turn.
 * Decided by `dev.wasm`.
 */
export function canForkBusySourceNatively(
  capabilities: OrchestrationV2ProviderCapabilities,
  hasNativeTurnBoundary: boolean,
): boolean {
  return decide("commandPolicyCanForkBusySourceNatively", {
    canForkWhileSourceRunning: capabilities.threads.canForkWhileSourceRunning === true,
    exposesNativeTurnId: capabilities.turns.exposesNativeTurnId,
    hasNativeTurnBoundary,
  }) as boolean;
}

/** The run a follow-up to a finished child binds to: the latest active one, else the last that ran. */
export function followUpOwnerRun(
  runs: ReadonlyArray<OrchestrationV2Run>,
): OrchestrationV2Run | null {
  return (
    runs.filter(isActiveRun).toSorted((a, b) => b.ordinal - a.ordinal)[0] ?? latestExecutedRun(runs)
  );
}

/** A provider that cannot restart its turn for new input runs a provider switch after the turn. Decided by `dev.wasm`. */
export function providerSwitchWaitsForTurn(
  turns: OrchestrationV2ProviderCapabilities["turns"],
): boolean {
  return decide("commandPolicyProviderSwitchWaitsForTurn", {
    supportsQueuedMessages: turns.supportsQueuedMessages,
    supportsInterrupt: turns.supportsInterrupt,
    supportsSteeringByInterruptRestart: turns.supportsSteeringByInterruptRestart,
  }) as boolean;
}

export function providerTurnCapabilities(
  projection: OrchestrationV2ThreadProjection,
  run: OrchestrationV2Run,
) {
  const providerThread = projection.providerThreads.find(
    (candidate) => candidate.id === run.providerThreadId,
  );
  const providerSession =
    providerThread?.providerSessionId == null
      ? undefined
      : projection.providerSessions.find(
          (candidate) => candidate.id === providerThread.providerSessionId,
        );
  return providerSession?.capabilities.turns;
}

interface CapabilityCheckInput {
  readonly commandId: CommandId;
  readonly threadId: ThreadId;
  readonly providerInstanceId: ProviderInstanceId;
  readonly capabilities: OrchestrationV2ProviderCapabilities;
}

export interface CommandPolicyV2Shape {
  readonly ensureQueuedMessages: (
    input: CapabilityCheckInput,
  ) => Effect.Effect<void, CommandPolicyV2Error>;
  readonly decideSteeringExecution: (
    input: CapabilityCheckInput & {
      readonly forceRestart?: boolean;
    },
  ) => Effect.Effect<SteeringExecutionPolicyV2, CommandPolicyV2Error>;
  readonly ensureInterrupt: (
    input: CapabilityCheckInput,
  ) => Effect.Effect<void, CommandPolicyV2Error>;
  readonly ensureNativeFork: (
    input: CapabilityCheckInput & {
      readonly fromSpecificTurn: boolean;
    },
  ) => Effect.Effect<void, CommandPolicyV2Error>;
  readonly decideForkExecution: (
    input: CapabilityCheckInput & {
      readonly sameProvider: boolean;
      readonly hasStrongNativeSource: boolean;
      readonly hasNativeTurnBoundary: boolean;
      readonly sourceRunStatus: OrchestrationV2Run["status"];
      readonly fromSpecificTurn: boolean;
      /** Later source runs are still active. */
      readonly sourceBusy: boolean;
    },
  ) => Effect.Effect<ForkExecutionPolicyV2, CommandPolicyV2Error>;
  readonly ensureRollback: (
    input: CapabilityCheckInput,
  ) => Effect.Effect<void, CommandPolicyV2Error>;
  readonly ensureContextHandoff: (
    input: CapabilityCheckInput & {
      readonly strategy: "fork_delta_context" | "delta_context" | "full_thread_summary";
    },
  ) => Effect.Effect<void, CommandPolicyV2Error>;
}

export class CommandPolicyV2 extends Context.Service<CommandPolicyV2, CommandPolicyV2Shape>()(
  "hanzo/orchestration-v2/CommandPolicy/CommandPolicyV2",
) {}

function unsupported(
  input: CapabilityCheckInput,
  capability: CommandPolicyCapability,
  detail: string,
) {
  return new CommandPolicyCapabilityUnsupportedError({
    commandId: input.commandId,
    threadId: input.threadId,
    providerInstanceId: input.providerInstanceId,
    capability,
    detail,
  });
}

type PolicyDecision<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly capability: CommandPolicyCapability; readonly detail: string };

const toEffect = <T>(
  input: CapabilityCheckInput,
  decision: PolicyDecision<T>,
): Effect.Effect<T, CommandPolicyV2Error> =>
  decision.ok
    ? Effect.succeed(decision.value)
    : Effect.fail(unsupported(input, decision.capability, decision.detail));

const toVoidEffect = (
  input: CapabilityCheckInput,
  decision: PolicyDecision<null>,
): Effect.Effect<void, CommandPolicyV2Error> =>
  decision.ok ? Effect.void : Effect.fail(unsupported(input, decision.capability, decision.detail));

const ensureQueuedMessages: CommandPolicyV2Shape["ensureQueuedMessages"] = (input) =>
  toVoidEffect(
    input,
    decide("commandPolicyEnsureQueuedMessages", {
      supportsQueuedMessages: input.capabilities.turns.supportsQueuedMessages,
    }) as PolicyDecision<null>,
  );

const decideSteeringExecution: CommandPolicyV2Shape["decideSteeringExecution"] = (input) =>
  toEffect(
    input,
    decide("commandPolicySteeringExecution", {
      forceRestart: input.forceRestart === true,
      supportsActiveSteering: input.capabilities.turns.supportsActiveSteering,
      supportsInterrupt: input.capabilities.turns.supportsInterrupt,
      supportsSteeringByInterruptRestart:
        input.capabilities.turns.supportsSteeringByInterruptRestart,
    }) as PolicyDecision<SteeringExecutionPolicyV2>,
  );

const ensureInterrupt: CommandPolicyV2Shape["ensureInterrupt"] = (input) =>
  toVoidEffect(
    input,
    decide("commandPolicyEnsureInterrupt", {
      supportsInterrupt: input.capabilities.turns.supportsInterrupt,
    }) as PolicyDecision<null>,
  );

const ensureNativeFork: CommandPolicyV2Shape["ensureNativeFork"] = (input) =>
  toVoidEffect(
    input,
    decide("commandPolicyEnsureNativeFork", {
      canForkThread: input.capabilities.threads.canForkThread,
      canForkFromTurn: input.capabilities.threads.canForkFromTurn,
      fromSpecificTurn: input.fromSpecificTurn,
      nativeThreadIds: input.capabilities.identity.nativeThreadIds,
    }) as PolicyDecision<null>,
  );

const ensureRollback: CommandPolicyV2Shape["ensureRollback"] = (input) =>
  toVoidEffect(
    input,
    decide("commandPolicyEnsureRollback", {
      canRollbackThread: input.capabilities.threads.canRollbackThread,
      providerCanRollbackConversation:
        input.capabilities.checkpointing.providerCanRollbackConversation,
      canConsumeHandoffSummaries: input.capabilities.context.canConsumeHandoffSummaries,
      providerRollbackReturnsSnapshot:
        input.capabilities.checkpointing.providerRollbackReturnsSnapshot,
    }) as PolicyDecision<null>,
  );

const ensureContextHandoff: CommandPolicyV2Shape["ensureContextHandoff"] = (input) =>
  toVoidEffect(
    input,
    decide("commandPolicyEnsureContextHandoff", {
      strategy: input.strategy,
      canConsumeHandoffSummaries: input.capabilities.context.canConsumeHandoffSummaries,
      acceptsSyntheticUserContext: input.capabilities.context.acceptsSyntheticUserContext,
      supportsDeltaHandoff: input.capabilities.context.supportsDeltaHandoff,
      supportsFullThreadHandoff: input.capabilities.context.supportsFullThreadHandoff,
    }) as PolicyDecision<null>,
  );

const decideForkExecution: CommandPolicyV2Shape["decideForkExecution"] = (input) =>
  toEffect(
    input,
    decide("commandPolicyForkExecution", {
      sameProvider: input.sameProvider,
      hasStrongNativeSource: input.hasStrongNativeSource,
      hasNativeTurnBoundary: input.hasNativeTurnBoundary,
      sourceRunStatus: input.sourceRunStatus,
      fromSpecificTurn: input.fromSpecificTurn,
      sourceBusy: input.sourceBusy,
      canForkThread: input.capabilities.threads.canForkThread,
      canForkFromTurn: input.capabilities.threads.canForkFromTurn,
      canForkWhileSourceRunning: input.capabilities.threads.canForkWhileSourceRunning === true,
      exposesNativeTurnId: input.capabilities.turns.exposesNativeTurnId,
      nativeThreadIds: input.capabilities.identity.nativeThreadIds,
      canConsumeHandoffSummaries: input.capabilities.context.canConsumeHandoffSummaries,
      acceptsSyntheticUserContext: input.capabilities.context.acceptsSyntheticUserContext,
      supportsFullThreadHandoff: input.capabilities.context.supportsFullThreadHandoff,
    }) as PolicyDecision<ForkExecutionPolicyV2>,
  );

export const layer: Layer.Layer<CommandPolicyV2> = Layer.succeed(CommandPolicyV2, {
  ensureQueuedMessages,
  decideSteeringExecution,
  ensureInterrupt,
  ensureNativeFork,
  decideForkExecution,
  ensureRollback,
  ensureContextHandoff,
});
