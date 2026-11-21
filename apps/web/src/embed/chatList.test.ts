import type { ThreadSummary } from "@hanzodev/client-runtime/cloud-chat";
import { describe, expect, it } from "vite-plus/test";

import { orderThreads, refOfThread, threadFor } from "./chatList";

const t = (key: string, over: Partial<ThreadSummary> = {}): ThreadSummary => ({
  key,
  remoteId: null,
  title: key,
  updatedAt: 0,
  local: true,
  pinned: false,
  renamed: false,
  ...over,
});

describe("cloud chat list", () => {
  it("lists pinned first, then newest", () => {
    const order = orderThreads([t("old", { updatedAt: 1 }), t("new", { updatedAt: 9 }), t("pin", { pinned: true })]);
    expect(order.map((x) => x.key)).toEqual(["pin", "new", "old"]);
  });
  it("addresses a thread by its remote id, else its device key, and finds it by either", () => {
    const remote = t("k1", { remoteId: "c-1" });
    const local = t("k2");
    expect(refOfThread(remote)).toBe("c-1");
    expect(refOfThread(local)).toBe("k2");
    expect(threadFor([remote, local], "c-1")).toBe(remote);
    expect(threadFor([remote, local], "k2")).toBe(local);
    expect(threadFor([remote, local], "nope")).toBeNull();
    expect(threadFor([remote], null)).toBeNull();
  });
});
