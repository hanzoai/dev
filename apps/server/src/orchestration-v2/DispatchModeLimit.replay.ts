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

/** The pure functions of DispatchModeLimit by their corpus names, over JSON inputs. */
export const dispatchModeLimitFns: Record<string, (input: any) => unknown> = {
  exceeded: ({ limit, modes }) => exceededDispatchModeLimit(limit, modes),
  narrow: ({ limit, ceiling }) => narrowToCeiling(limit ?? undefined, ceiling ?? undefined),
  threadsStartingWork: ({ events }) => [...threadsStartingWork(events)],
  ceilingRefusal: ({ max, events, runtimeModes }) =>
    runtimeModeCeilingRefusal(max, events, new Map(runtimeModes)),
};
