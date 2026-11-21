import * as DateTime from "effect/DateTime";
import { modelSwitchSince } from "./modelSwitch.ts";
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
};
