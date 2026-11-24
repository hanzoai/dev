import {
  CommandId,
  ProjectId,
  ProviderInstanceId,
  ThreadId,
  type OrchestrationV2AppThread,
} from "@hanzodev/contracts";
import * as DateTime from "effect/DateTime";
import * as Result from "effect/Result";
import { describe, expect, it } from "vite-plus/test";

import { decideThreadTriageCommand } from "./ThreadTriageDecider.ts";

const T0 = DateTime.makeUnsafe("2026-01-01T00:00:00.000Z");
const T1 = DateTime.makeUnsafe("2026-01-01T01:00:00.000Z");
const T2 = DateTime.makeUnsafe("2026-01-01T02:00:00.000Z");

const THREAD_ID = ThreadId.make("thread-triage-1");

const baseThread = (
  overrides: Partial<OrchestrationV2AppThread> = {},
): OrchestrationV2AppThread => ({
  createdBy: "user",
  creationSource: "web",
  id: THREAD_ID,
  projectId: ProjectId.make("project-1"),
  title: "Triage test",
  modelSelection: {
    instanceId: ProviderInstanceId.make("codex"),
    model: "gpt-5.4",
  },
  providerInstanceId: ProviderInstanceId.make("codex"),
  runtimeMode: "approval-required",
  interactionMode: "default",
  branch: null,
  worktreePath: null,
  branchPullRequest: null,
  activeOrderKey: null,
  activeProviderThreadId: null,
  lineage: { parentThreadId: null, relationshipToParent: null, rootThreadId: THREAD_ID },
  forkedFrom: null,
  settledOverride: null,
  settledAt: null,
  lastVisitedAt: null,
  archivedAt: null,
  deletedAt: null,
  createdAt: T0,
  updatedAt: T0,
  ...overrides,
});

describe("ThreadTriageDecider", () => {
  it("pins a settled and snoozed thread, promoting it to active and clearing snooze", () => {
    const thread = baseThread({
      settledOverride: "settled",
      settledAt: T0,
      snoozedUntil: T2,
      snoozedAt: T0,
    });
    const res = decideThreadTriageCommand({
      thread,
      command: {
        type: "thread.pin",
        commandId: CommandId.make("cmd-1"),
        threadId: thread.id,
        orderKey: "a0",
      },
      now: T1,
    });
    expect(Result.isSuccess(res)).toBe(true);
    if (Result.isSuccess(res)) {
      expect(res.success.pinnedAt).toEqual(T1);
      expect(res.success.pinOrderKey).toBe("a0");
      expect(res.success.settledOverride).toBe("active");
      expect(res.success.settledAt).toBeNull();
      expect(res.success.unsettledAt).toEqual(T1);
      expect(res.success.snoozedUntil).toBeNull();
      expect(res.success.lastSnoozeWakeAt).toEqual(T1);
      expect(res.success.updatedAt).toEqual(T1);
    }
  });

  it("rejects pin.reorder on unpinned threads and active.reorder on pinned or settled threads", () => {
    const unpinned = baseThread();
    const pinReorderErr = decideThreadTriageCommand({
      thread: unpinned,
      command: {
        type: "thread.pin.reorder",
        commandId: CommandId.make("cmd-2"),
        threadId: unpinned.id,
        orderKey: "b0",
      },
      now: T1,
    });
    expect(Result.isFailure(pinReorderErr) ? pinReorderErr.failure : null).toBe(
      `Thread ${unpinned.id} is not pinned and cannot be reordered.`,
    );

    const pinned = baseThread({ pinnedAt: T0 });
    const activeReorderErr = decideThreadTriageCommand({
      thread: pinned,
      command: {
        type: "thread.active.reorder",
        commandId: CommandId.make("cmd-3"),
        threadId: pinned.id,
        orderKey: "b0",
      },
      now: T1,
    });
    expect(Result.isFailure(activeReorderErr) ? activeReorderErr.failure : null).toBe(
      `Thread ${pinned.id} is not active and cannot be reordered.`,
    );
  });

  it("adds and removes tags without duplicating existing tags", () => {
    const thread = baseThread({ tags: ["bug"] });
    const dup = decideThreadTriageCommand({
      thread,
      command: {
        type: "thread.tag.set",
        commandId: CommandId.make("cmd-4"),
        threadId: thread.id,
        tag: "bug",
        tagged: true,
      },
      now: T1,
    });
    expect(Result.isSuccess(dup) ? dup.success.tags : null).toEqual(["bug"]);

    const removed = decideThreadTriageCommand({
      thread,
      command: {
        type: "thread.tag.set",
        commandId: CommandId.make("cmd-5"),
        threadId: thread.id,
        tag: "bug",
        tagged: false,
      },
      now: T1,
    });
    expect(Result.isSuccess(removed) ? removed.success.tags : null).toEqual([]);
  });

  it("retires expired snooze when marking unread while preserving unexpired snooze", () => {
    const expired = baseThread({ snoozedUntil: T0, snoozedAt: T0 });
    const expiredRes = decideThreadTriageCommand({
      thread: expired,
      command: {
        type: "thread.mark-unread",
        commandId: CommandId.make("cmd-6"),
        threadId: expired.id,
      },
      now: T1,
      markUnreadVisitedAt: T0,
    });
    expect(Result.isSuccess(expiredRes)).toBe(true);
    if (Result.isSuccess(expiredRes)) {
      expect(expiredRes.success.lastVisitedAt).toEqual(T0);
      expect(expiredRes.success.snoozedUntil).toBeNull();
      expect(expiredRes.success.lastSnoozeWakeAt).toEqual(T0);
    }

    const active = baseThread({ snoozedUntil: T2, snoozedAt: T0 });
    const activeRes = decideThreadTriageCommand({
      thread: active,
      command: {
        type: "thread.mark-unread",
        commandId: CommandId.make("cmd-7"),
        threadId: active.id,
      },
      now: T1,
      markUnreadVisitedAt: T0,
    });
    expect(Result.isSuccess(activeRes)).toBe(true);
    if (Result.isSuccess(activeRes)) {
      expect(activeRes.success.lastVisitedAt).toEqual(T0);
      expect(activeRes.success.snoozedUntil).toEqual(T2);
    }
  });
});
