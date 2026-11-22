import { describe, expect, it } from "vite-plus/test";

import {
  consumePendingWidgetQuickAction,
  dispatchWidgetQuickAction,
  parseAskWidgetPath,
} from "./widgetQuickAction";

describe("widgetQuickAction", () => {
  it("parses ask deep links with actions and prompts", () => {
    expect(parseAskWidgetPath("ask")).toEqual({ action: "chat" });
    expect(parseAskWidgetPath("/ask?action=camera")).toEqual({ action: "camera" });
    expect(parseAskWidgetPath("ask?action=voice")).toEqual({ action: "voice" });
    expect(parseAskWidgetPath("ask?action=attach")).toEqual({ action: "attach" });
    expect(parseAskWidgetPath("ask?action=clipboard")).toEqual({ action: "clipboard" });
    expect(parseAskWidgetPath("ask?prompt=Review%20this%20diff")).toEqual({
      action: "chat",
      prompt: "Review this diff",
    });
    expect(parseAskWidgetPath("ask?action=unknown")).toEqual({ action: "chat" });
    expect(parseAskWidgetPath("settings/usage")).toBeNull();
  });

  it("parses chat widget deep links for new chat, voice, thread open, and prompts", () => {
    expect(parseAskWidgetPath("chat")).toEqual({ action: "chat" });
    expect(parseAskWidgetPath("chat/new")).toEqual({ action: "new" });
    expect(parseAskWidgetPath("/chat/voice")).toEqual({ action: "voice" });
    expect(parseAskWidgetPath("chat/thread/t-abc123")).toEqual({
      action: "chat",
      threadId: "t-abc123",
    });
    expect(parseAskWidgetPath("chat?prompt=Summarize%20today&send=1")).toEqual({
      action: "chat",
      prompt: "Summarize today",
      autoSend: true,
    });
    expect(parseAskWidgetPath("chat/settings")).toBeNull();
  });

  it("queues and consumes pending widget quick actions once", () => {
    dispatchWidgetQuickAction({ action: "camera" });
    expect(consumePendingWidgetQuickAction()).toMatchObject({ action: "camera" });
    expect(consumePendingWidgetQuickAction()).toBeNull();
  });
});
