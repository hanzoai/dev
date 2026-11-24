import { EnvironmentId, ThreadId } from "@hanzodev/contracts";
import { beforeEach, describe, expect, it } from "vite-plus/test";

import type { DraftId } from "./composerDraftStore";
import { useMemorySwitchStore } from "./memorySwitchStore";

describe("useMemorySwitchStore", () => {
  beforeEach(() => {
    useMemorySwitchStore.setState({ enabledByKey: {} });
  });

  it("defaults off and persists toggles per thread", () => {
    const threadA = {
      environmentId: EnvironmentId.make("env-1"),
      threadId: ThreadId.make("thread-a"),
    };
    const threadB = {
      environmentId: EnvironmentId.make("env-1"),
      threadId: ThreadId.make("thread-b"),
    };

    expect(useMemorySwitchStore.getState().isMemoryEnabled(threadA)).toBe(false);
    expect(useMemorySwitchStore.getState().toggleMemoryEnabled(threadA)).toBe(true);
    expect(useMemorySwitchStore.getState().isMemoryEnabled(threadA)).toBe(true);
    expect(useMemorySwitchStore.getState().isMemoryEnabled(threadB)).toBe(false);

    expect(useMemorySwitchStore.getState().toggleMemoryEnabled(threadA)).toBe(false);
    expect(useMemorySwitchStore.getState().isMemoryEnabled(threadA)).toBe(false);
  });

  it("transfers an enabled draft setting to the created thread", () => {
    const draftId = "draft-1" as DraftId;
    const threadRef = {
      environmentId: EnvironmentId.make("env-1"),
      threadId: ThreadId.make("thread-created"),
    };

    useMemorySwitchStore.getState().setMemoryEnabled(draftId, true);
    useMemorySwitchStore.getState().transferMemorySetting(draftId, threadRef);

    expect(useMemorySwitchStore.getState().isMemoryEnabled(draftId)).toBe(false);
    expect(useMemorySwitchStore.getState().isMemoryEnabled(threadRef)).toBe(true);
  });
});
