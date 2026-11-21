import { describe, expect, it } from "vite-plus/test";

import { refOf } from "./navigation";

describe("refOf", () => {
  it("names a thread and a draft by their path, and nothing else", () => {
    expect(refOf("/env-1/thread-9")).toBe("env-1/thread-9");
    expect(refOf("/draft/abc")).toBe("draft/abc");
    expect(refOf("/settings/general")).toBeNull();
    expect(refOf("/")).toBeNull();
  });
});
