import { RunId } from "@hanzodev/contracts";
import * as Result from "effect/Result";
import { describe, expect, it } from "vite-plus/test";

import {
  decideQueuedRunCancel,
  decideQueuedRunEdit,
  decideQueuedRunReorder,
} from "./QueuedRunDecider.ts";

describe("QueuedRunDecider", () => {
  describe("decideQueuedRunReorder", () => {
    it("reorders user-queued runs while keeping automatic completion deliveries ahead", () => {
      const auto = RunId.make("run-auto");
      const first = RunId.make("run-1");
      const second = RunId.make("run-2");
      const third = RunId.make("run-3");

      const result = decideQueuedRunReorder({
        queuedRuns: [
          { id: auto, queuePosition: 1, isAutomaticCompletion: true },
          { id: first, queuePosition: 2, isAutomaticCompletion: false },
          { id: second, queuePosition: 3, isAutomaticCompletion: false },
          { id: third, queuePosition: 4, isAutomaticCompletion: false },
        ],
        runId: third,
        beforeRunId: first,
      });

      expect(Result.isSuccess(result)).toBe(true);
      if (Result.isSuccess(result)) {
        expect(result.success).toEqual([
          { runId: third, queuePosition: 2 },
          { runId: first, queuePosition: 3 },
          { runId: second, queuePosition: 4 },
        ]);
      }
    });

    it("returns an empty update list when moving a run to its existing position", () => {
      const first = RunId.make("run-1");
      const second = RunId.make("run-2");

      const result = decideQueuedRunReorder({
        queuedRuns: [
          { id: first, queuePosition: 1, isAutomaticCompletion: false },
          { id: second, queuePosition: 2, isAutomaticCompletion: false },
        ],
        runId: second,
        beforeRunId: null,
      });

      expect(Result.isSuccess(result)).toBe(true);
      if (Result.isSuccess(result)) {
        expect(result.success).toEqual([]);
      }
    });

    it("rejects unknown runs, automatic deliveries, and reordering ahead of automatic deliveries", () => {
      const auto = RunId.make("run-auto");
      const user = RunId.make("run-user");
      const missing = RunId.make("run-missing");

      const missingRes = decideQueuedRunReorder({
        queuedRuns: [{ id: user, queuePosition: 1, isAutomaticCompletion: false }],
        runId: missing,
        beforeRunId: null,
      });
      expect(Result.isFailure(missingRes) ? missingRes.failure : null).toBe(
        `Run ${missing} is not queued.`,
      );

      const autoRes = decideQueuedRunReorder({
        queuedRuns: [
          { id: auto, queuePosition: 1, isAutomaticCompletion: true },
          { id: user, queuePosition: 2, isAutomaticCompletion: false },
        ],
        runId: auto,
        beforeRunId: null,
      });
      expect(Result.isFailure(autoRes) ? autoRes.failure : null).toBe(
        "Automatic completion deliveries cannot be reordered.",
      );

      const aheadRes = decideQueuedRunReorder({
        queuedRuns: [
          { id: auto, queuePosition: 1, isAutomaticCompletion: true },
          { id: user, queuePosition: 2, isAutomaticCompletion: false },
        ],
        runId: user,
        beforeRunId: auto,
      });
      expect(Result.isFailure(aheadRes) ? aheadRes.failure : null).toBe(
        "Queued messages cannot be reordered ahead of automatic completion delivery.",
      );

      const badTargetRes = decideQueuedRunReorder({
        queuedRuns: [{ id: user, queuePosition: 1, isAutomaticCompletion: false }],
        runId: user,
        beforeRunId: missing,
      });
      expect(Result.isFailure(badTargetRes) ? badTargetRes.failure : null).toBe(
        `Queue target ${missing} is not queued.`,
      );
    });
  });

  describe("decideQueuedRunCancel", () => {
    it("accepts queued runs and rejects missing or non-queued runs", () => {
      const id = RunId.make("run-1");
      expect(
        Result.isSuccess(decideQueuedRunCancel({ runId: id, run: { id, status: "queued" } })),
      ).toBe(true);
      const missing = decideQueuedRunCancel({ runId: id, run: null });
      expect(Result.isFailure(missing) ? missing.failure : null).toBe(`Run ${id} is not queued.`);
      const running = decideQueuedRunCancel({ runId: id, run: { id, status: "running" } });
      expect(Result.isFailure(running) ? running.failure : null).toBe(`Run ${id} is not queued.`);
    });
  });

  describe("decideQueuedRunEdit", () => {
    it("enforces empty text, queued status, message presence, automatic delivery, and guest checks", () => {
      const id = RunId.make("run-1");
      const base = {
        runId: id,
        text: "Updated prompt",
        run: { id, status: "queued" as const },
        hasMessage: true,
        isAutomaticCompletion: false,
        isGuest: false,
      };

      expect(Result.isSuccess(decideQueuedRunEdit(base))).toBe(true);

      const empty = decideQueuedRunEdit({ ...base, text: "   \n\t " });
      expect(Result.isFailure(empty) ? empty.failure : null).toBe(
        `Queued run ${id} cannot be edited to an empty message.`,
      );

      const notQueued = decideQueuedRunEdit({ ...base, run: { id, status: "completed" } });
      expect(Result.isFailure(notQueued) ? notQueued.failure : null).toBe(
        `Run ${id} is not queued.`,
      );

      const noMessage = decideQueuedRunEdit({ ...base, hasMessage: false });
      expect(Result.isFailure(noMessage) ? noMessage.failure : null).toBe(
        `Queued run ${id} has no user message.`,
      );

      const auto = decideQueuedRunEdit({ ...base, isAutomaticCompletion: true });
      expect(Result.isFailure(auto) ? auto.failure : null).toBe(
        "Automatic completion deliveries cannot be edited.",
      );

      const guest = decideQueuedRunEdit({ ...base, isGuest: true });
      expect(Result.isFailure(guest) ? guest.failure : null).toBe(
        "A guest's message cannot be edited. Cancel it instead.",
      );
    });
  });
});
