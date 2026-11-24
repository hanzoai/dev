import { CLOUD_CONNECT_SUBJECT } from "../auth/EnvironmentAuth.ts";
import type {
  OrchestrationV2Command,
  OrchestrationV2DomainEvent,
  OrchestrationV2RunStatus,
  OrchestrationV2StoredEvent,
  RunId,
  ThreadId,
} from "@hanzodev/contracts";
import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";

export class RoutedRunBusyError extends Schema.TaggedError<RoutedRunBusyError>()(
  "RoutedRunBusyError",
  {
    instanceId: Schema.String,
    message: Schema.String,
  },
) {}

/**
 * A routed run is one started by another of the user's surfaces through Hanzo Link, whose
 * sessions prove a DPoP key under the Link subject. A paired device (pairing code, LAN) is
 * the owner's own and is not routed.
 */
export const isRoutedSession = (session: {
  readonly method: string;
  readonly subject: string;
}): boolean => session.method === "dpop-access-token" && session.subject === CLOUD_CONNECT_SUBJECT;

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

/** Commands that start an execution run on a provider instance (vs thread setup/config). */
export const isRunStartingCommand = (command: OrchestrationV2Command): boolean =>
  command.type === "message.dispatch" || command.type === "delegated_task.request";

export const isActiveRunStatus = (status: OrchestrationV2RunStatus): boolean =>
  status === "preparing" ||
  status === "queued" ||
  status === "starting" ||
  status === "running" ||
  status === "waiting";

type LeaseEntry =
  | { readonly kind: "starting"; readonly commandId: string }
  | { readonly kind: "running"; readonly threadId: ThreadId; readonly runId: RunId };

const MAX_RECENTLY_ENDED_RUNS = 256;

export interface RoutedRunLeaseRegistry {
  readonly tryAcquire: (
    instanceId: string,
    commandId: string,
    isRunStillActive?: (threadId: ThreadId, runId: RunId) => Effect.Effect<boolean>,
  ) => Effect.Effect<void, RoutedRunBusyError>;
  readonly bindEvents: (
    instanceId: string,
    commandId: string,
    storedEvents: ReadonlyArray<OrchestrationV2StoredEvent>,
  ) => void;
  readonly releaseIfStarting: (instanceId: string, commandId: string) => void;
  readonly onDomainEvent: (event: OrchestrationV2DomainEvent) => void;
  readonly activeLeaseFor: (instanceId: string) => LeaseEntry | undefined;
}

/**
 * Enforces at most one active routed run per provider login instance on the desktop.
 * Leases are acquired before dispatching a routed run-starting command, bound to the
 * created run id from the dispatch events, and released when the run reaches a terminal
 * status (via Orchestrator domain events, with a projection fallback on acquire).
 */
export const createRoutedRunLeaseRegistry = (): RoutedRunLeaseRegistry => {
  const activeByInstance = new Map<string, LeaseEntry>();
  const recentlyEndedRuns = new Set<RunId>();

  const rememberEndedRun = (runId: RunId) => {
    recentlyEndedRuns.add(runId);
    if (recentlyEndedRuns.size > MAX_RECENTLY_ENDED_RUNS) {
      const oldest = recentlyEndedRuns.values().next().value;
      if (oldest !== undefined) recentlyEndedRuns.delete(oldest);
    }
  };

  const busyError = (instanceId: string) =>
    new RoutedRunBusyError({
      instanceId,
      message: "Another routed run is already active on this provider instance.",
    });

  const tryAcquire: RoutedRunLeaseRegistry["tryAcquire"] = Effect.fn("routedRun.lease.tryAcquire")(
    function* (instanceId, commandId, isRunStillActive) {
      const existing = activeByInstance.get(instanceId);
      if (existing !== undefined) {
        if (existing.kind === "starting") {
          if (existing.commandId === commandId) return;
          return yield* busyError(instanceId);
        }
        if (recentlyEndedRuns.has(existing.runId)) {
          activeByInstance.delete(instanceId);
        } else {
          const stillActive =
            isRunStillActive === undefined
              ? true
              : yield* isRunStillActive(existing.threadId, existing.runId).pipe(
                  Effect.orElseSucceed(() => true),
                );
          if (stillActive) {
            return yield* busyError(instanceId);
          }
          // Only clear if another fiber hasn't replaced the entry while we checked.
          if (activeByInstance.get(instanceId) === existing) {
            activeByInstance.delete(instanceId);
          }
        }
      }

      const current = activeByInstance.get(instanceId);
      if (current !== undefined) {
        if (current.kind === "starting" && current.commandId === commandId) return;
        return yield* busyError(instanceId);
      }

      activeByInstance.set(instanceId, { kind: "starting", commandId });
    },
  );

  const bindEvents: RoutedRunLeaseRegistry["bindEvents"] = (
    instanceId,
    commandId,
    storedEvents,
  ) => {
    const entry = activeByInstance.get(instanceId);
    if (entry?.kind !== "starting" || entry.commandId !== commandId) return;

    let latestRun:
      | { readonly threadId: ThreadId; readonly runId: RunId; readonly active: boolean }
      | undefined;
    for (const stored of storedEvents) {
      const event = stored.event;
      if (event.type === "run.created" || event.type === "run.updated") {
        latestRun = {
          threadId: event.payload.threadId,
          runId: event.payload.id,
          active: isActiveRunStatus(event.payload.status),
        };
      }
    }

    if (latestRun === undefined || !latestRun.active || recentlyEndedRuns.has(latestRun.runId)) {
      activeByInstance.delete(instanceId);
      return;
    }

    activeByInstance.set(instanceId, {
      kind: "running",
      threadId: latestRun.threadId,
      runId: latestRun.runId,
    });
  };

  const releaseIfStarting: RoutedRunLeaseRegistry["releaseIfStarting"] = (
    instanceId,
    commandId,
  ) => {
    const entry = activeByInstance.get(instanceId);
    if (entry?.kind === "starting" && entry.commandId === commandId) {
      activeByInstance.delete(instanceId);
    }
  };

  const onDomainEvent: RoutedRunLeaseRegistry["onDomainEvent"] = (event) => {
    if (event.type === "run.updated" && !isActiveRunStatus(event.payload.status)) {
      rememberEndedRun(event.payload.id);
      for (const [instanceId, entry] of activeByInstance) {
        if (entry.kind === "running" && entry.runId === event.payload.id) {
          activeByInstance.delete(instanceId);
        }
      }
      return;
    }
    if (event.type === "thread.deleted") {
      for (const [instanceId, entry] of activeByInstance) {
        if (entry.kind === "running" && entry.threadId === event.threadId) {
          activeByInstance.delete(instanceId);
        }
      }
    }
  };

  return {
    tryAcquire,
    bindEvents,
    releaseIfStarting,
    onDomainEvent,
    activeLeaseFor: (instanceId) => activeByInstance.get(instanceId),
  };
};
