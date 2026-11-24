import type { OrchestrationV2Run, RunId } from "@hanzodev/contracts";
import * as Result from "effect/Result";

import { decide } from "./DevWasm.ts";

export interface QueuedRunReorderEntry {
  readonly id: RunId;
  readonly queuePosition: number | null;
  readonly isAutomaticCompletion: boolean;
}

export interface QueuePositionUpdate {
  readonly runId: RunId;
  readonly queuePosition: number;
}

type ReorderDecision =
  | { readonly ok: false; readonly error: string }
  | { readonly ok: true; readonly updates: ReadonlyArray<QueuePositionUpdate> };

type ValidationDecision = { readonly ok: false; readonly error: string } | { readonly ok: true };

/**
 * Computes queue position updates for `queued-run.reorder`, preserving automatic completion
 * deliveries ahead of user-queued messages. Decided by `dev.wasm`.
 */
export function decideQueuedRunReorder(input: {
  readonly queuedRuns: ReadonlyArray<QueuedRunReorderEntry>;
  readonly runId: RunId;
  readonly beforeRunId: RunId | null;
}): Result.Result<ReadonlyArray<QueuePositionUpdate>, string> {
  const decision = decide("queuedRunReorder", input) as ReorderDecision;
  if (!decision.ok) return Result.fail(decision.error);
  return Result.succeed(decision.updates);
}

/**
 * Validates that a run is eligible for `queued-run.cancel`. Decided by `dev.wasm`.
 */
export function decideQueuedRunCancel(input: {
  readonly runId: RunId;
  readonly run: Pick<OrchestrationV2Run, "id" | "status"> | null;
}): Result.Result<void, string> {
  const decision = decide("queuedRunCancel", input) as ValidationDecision;
  if (!decision.ok) return Result.fail(decision.error);
  return Result.succeed(undefined);
}

/**
 * Validates that a queued run's user message is eligible for `queued-run.edit`.
 * Decided by `dev.wasm`.
 */
export function decideQueuedRunEdit(input: {
  readonly runId: RunId;
  readonly text: string;
  readonly run: Pick<OrchestrationV2Run, "id" | "status"> | null;
  readonly hasMessage: boolean;
  readonly isAutomaticCompletion: boolean;
  readonly isGuest: boolean;
}): Result.Result<void, string> {
  const decision = decide("queuedRunEdit", input) as ValidationDecision;
  if (!decision.ok) return Result.fail(decision.error);
  return Result.succeed(undefined);
}
