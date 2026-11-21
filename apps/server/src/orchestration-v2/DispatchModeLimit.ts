import {
  type OrchestrationV2DomainEvent,
  type ProviderInteractionMode,
  type RuntimeMode,
  type ThreadId,
} from "@hanzodev/contracts";
import * as Context from "effect/Context";
import { decide } from "./DevWasm.ts";
import type * as Ref from "effect/Ref";

export interface DispatchModes {
  readonly runtimeMode: RuntimeMode;
  readonly interactionMode: ProviderInteractionMode;
}

export interface DispatchModeRefusal extends DispatchModes {
  readonly threadId: ThreadId;
  readonly mode: "runtime" | "interaction";
}

/** Recheck target modes under the command lock; the user can raise them after preflight. */
export interface DispatchModeLimitValue extends DispatchModes {
  readonly refused?: Ref.Ref<DispatchModeRefusal | undefined>;
}

export const DispatchModeLimit = Context.Reference<DispatchModeLimitValue | undefined>(
  "hanzo/orchestration-v2/DispatchModeLimit",
  { defaultValue: () => undefined },
);

/** Which of the two modes a dispatch asks for is above the limit; decided by `dev.wasm`. */
export const exceededDispatchModeLimit = (
  limit: DispatchModes,
  modes: DispatchModes,
): "runtime" | "interaction" | undefined =>
  (decide("exceeded", { limit, modes }) as "runtime" | "interaction" | null) ?? undefined;

/** The administrator's `limits.maxRuntimeMode`, which applies to every caller, the user included. */
export interface RuntimeModeCeiling {
  readonly organization: string;
  readonly maxRuntimeMode: RuntimeMode;
}

/** A caller's limit, lowered to the ceiling when the caller may otherwise go above it. */
export const narrowToCeiling = <Limit extends DispatchModes>(
  limit: Limit | undefined,
  ceiling: RuntimeModeCeiling | undefined,
): Limit | undefined =>
  limit === undefined || ceiling === undefined ? limit : narrowed(limit, ceiling);

const narrowed = <Limit extends DispatchModes>(
  limit: Limit,
  ceiling: RuntimeModeCeiling,
): Limit => {
  const { runtimeMode } = decide("narrow", { limit, ceiling }) as DispatchModes;
  return runtimeMode === limit.runtimeMode ? limit : { ...limit, runtimeMode };
};

export interface RuntimeModeCeilingRefusal {
  readonly threadId: ThreadId;
  readonly runtimeMode: RuntimeMode;
  /** `mode`: a thread would be created in or switched to the mode. `turn`: a turn would run in it. */
  readonly refused: "mode" | "turn";
}

/**
 * What a planned command would do above the ceiling, judged by its events so every command that
 * creates a thread, switches its mode, or starts or steers a turn is covered. `runtimeModes` holds
 * the committed mode of each thread a planned run belongs to. Lowering a mode, and commands that
 * start no work, stay open on a thread above the ceiling: they are how its user gets back under.
 */
export const runtimeModeCeilingRefusal = (
  maxRuntimeMode: RuntimeMode,
  events: ReadonlyArray<OrchestrationV2DomainEvent>,
  runtimeModes: ReadonlyMap<ThreadId, RuntimeMode>,
): RuntimeModeCeilingRefusal | undefined =>
  (decide("ceilingRefusal", {
    max: maxRuntimeMode,
    events: ceilingEvents(events),
    runtimeModes: [...runtimeModes],
  }) as RuntimeModeCeilingRefusal | null) ?? undefined;

/** The fields of an event the ceiling check reads; the engine is not sent the rest. */
const ceilingEvents = (events: ReadonlyArray<OrchestrationV2DomainEvent>) =>
  events.map((event) => {
    const payload =
      "payload" in event ? (event.payload as { runtimeMode?: RuntimeMode }) : undefined;
    return payload?.runtimeMode === undefined
      ? { type: event.type, threadId: event.threadId }
      : {
          type: event.type,
          threadId: event.threadId,
          payload: { runtimeMode: payload.runtimeMode },
        };
  });

/** Threads whose committed mode `runtimeModeCeilingRefusal` needs: those a planned run belongs to. */
export const threadsStartingWork = (
  events: ReadonlyArray<OrchestrationV2DomainEvent>,
): ReadonlySet<ThreadId> =>
  new Set(decide("threadsStartingWork", { events: ceilingEvents(events) }) as ThreadId[]);
