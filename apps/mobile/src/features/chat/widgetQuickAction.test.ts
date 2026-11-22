import { describe, expect, it } from "vitest";

import {
  consumePendingWidgetQuickAction,
  dispatchWidgetQuickAction,
  parseAskWidgetPath,
} from "./widgetQuickAction";

describe("widgetQuickAction", () => {
  it("parses ask deep links with actions and prompts", () => {
    expect(parseAskWidgetPath("ask")).toEqual({ action: "chat", prompt: undefined });
    expect(parseAskWidgetPath("/ask?action=camera")).toEqual({ action: "camera", prompt: undefined });
    expect(parseAskWidgetPath("ask?action=voice")).toEqual({ action: "voice", prompt: undefined });
    expect(parseAskWidgetPath("ask?action=attach")).toEqual({ action: "attach", prompt: undefined });
    expect(parseAskWidgetPath("ask?action=clipboard")).toEqual({ action: "clipboard", prompt: undefined });
    expect(parseAskWidgetPath("ask?prompt=Review%20this%20diff")).toEqual({
      action: "chat",
      prompt: "Review this diff",
    });
    expect(parseAskWidgetPath("ask?action=unknown")).toEqual({ action: "chat", prompt: undefined });
    expect(parseAskWidgetPath("settings/usage")).toBeNull();
  });

  it("queues and consumes pending widget quick actions once", () => {
    dispatchWidgetQuickAction({ action: "camera" });
    expect(consumePendingWidgetQuickAction()).toMatchObject({ action: "camera" });
    expect(consumePendingWidgetQuickAction()).toBeNull();
  });
});
