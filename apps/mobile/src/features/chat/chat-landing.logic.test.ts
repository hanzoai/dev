import { describe, expect, it } from "vite-plus/test";

import { RUN_TARGETS, landsInChat, resolveSend, sandboxNotice } from "./chat-landing.logic";

const base = { prompt: "hi", build: false, hasReadyEnvironment: false, cloudChatAvailable: false };

describe("sending from the chat landing", () => {
  it("does nothing for an empty prompt", () => {
    expect(resolveSend({ ...base, prompt: "  " })).toEqual({ kind: "none" });
  });

  it("chats in the cloud with no environment when a chat path exists", () => {
    expect(resolveSend({ ...base, cloudChatAvailable: true })).toEqual({ kind: "chat" });
  });

  it("asks where to run only when work needs a machine and none is connected", () => {
    expect(resolveSend({ ...base, build: true })).toEqual({ kind: "choose-target" });
    expect(resolveSend({ ...base, build: true, cloudChatAvailable: true })).toEqual({
      kind: "choose-target",
    });
    expect(resolveSend({ ...base, build: true, hasReadyEnvironment: true })).toEqual({
      kind: "machine",
    });
  });

  it("without a chat path, an environment runs the task and no environment asks", () => {
    expect(resolveSend({ ...base, hasReadyEnvironment: true })).toEqual({ kind: "machine" });
    expect(resolveSend(base)).toEqual({ kind: "choose-target" });
  });

  it("offers the Mac then the cloud", () => {
    expect(RUN_TARGETS).toEqual(["mac", "cloud"]);
  });

  it("lands a signed-in user in chat, never on an environment gate", () => {
    expect(landsInChat({ signedIn: true })).toBe(true);
  });
});

describe("cloud sandbox notice", () => {
  const text = { ready: "R", readyDetail: "RD", unavailable: "U", unavailableDetail: "UD", failed: "F" };
  it("offers the sandbox when the endpoint is live", () => {
    expect(sandboxNotice({ kind: "started", sessionId: "s" }, text)).toEqual({ title: "R", detail: "RD", opens: true });
  });
  it("says plainly when it is not available", () => {
    expect(sandboxNotice({ kind: "unavailable" }, text)).toEqual({ title: "U", detail: "UD", opens: false });
  });
  it("reports a failure with its message", () => {
    expect(sandboxNotice({ kind: "failed", message: "HTTP 500" }, text).detail).toBe("HTTP 500");
  });
});
