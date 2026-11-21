import { describe, expect, it } from "vite-plus/test";

import { hostedLanding, resolveSend } from "./chatLanding.logic";

describe("resolveSend", () => {
  const base = {
    prompt: "hi",
    build: false,
    hasReadyEnvironment: false,
    cloudChatAvailable: false,
  };

  it("sends nothing for a blank prompt", () => {
    expect(resolveSend({ ...base, prompt: "  " })).toEqual({ kind: "none" });
  });

  it("chats in the cloud without any environment", () => {
    expect(resolveSend({ ...base, cloudChatAvailable: true })).toEqual({ kind: "chat" });
  });

  it("asks where to run only when building needs a machine", () => {
    expect(resolveSend({ ...base, build: true, cloudChatAvailable: true })).toEqual({
      kind: "choose-target",
    });
    expect(
      resolveSend({ ...base, build: true, hasReadyEnvironment: true, cloudChatAvailable: true }),
    ).toEqual({ kind: "machine" });
  });

  it("falls back to a machine, then to the question, when chat is unavailable", () => {
    expect(resolveSend({ ...base, hasReadyEnvironment: true })).toEqual({ kind: "machine" });
    expect(resolveSend(base)).toEqual({ kind: "choose-target" });
  });
});

describe("hostedLanding", () => {
  it("lands signed-in users in chat and signed-out users on sign-in", () => {
    expect(hostedLanding({ isLoaded: false, isSignedIn: false })).toBe("loading");
    expect(hostedLanding({ isLoaded: true, isSignedIn: false })).toBe("sign-in");
    expect(hostedLanding({ isLoaded: true, isSignedIn: true })).toBe("chat");
  });
});

describe("landing labels", () => {
  it("labels threads and models", async () => {
    const { threadRowLabel, modelPillLabel, cloudRunNote } = await import("./chatLanding.logic");
    expect(threadRowLabel({ title: " " }, "New chat")).toBe("New chat");
    expect(threadRowLabel({ title: "Plan" }, "New chat")).toBe("Plan");
    expect(modelPillLabel([{ id: "a", label: "Alpha" }], "a")).toBe("Alpha");
    expect(modelPillLabel([], "b")).toBe("b");
    expect(cloudRunNote(null)).toBeNull();
    expect(cloudRunNote({ kind: "unavailable" })).toBe("unavailable");
  });
});
