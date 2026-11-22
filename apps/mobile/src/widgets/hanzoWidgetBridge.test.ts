import { describe, expect, it } from "vite-plus/test";

import { buildWidgetConnectionStatus } from "./hanzoWidgetSnapshot";

describe("hanzoWidgetSnapshot", () => {
  it("formats connection status accurately across signed-out, cloud-only, and linked Mac states", () => {
    expect(
      buildWidgetConnectionStatus({
        signedIn: false,
        onlineMachinesCount: 0,
        totalMachinesCount: 0,
      }),
    ).toBe("Sign in to connect Hanzo Cloud");

    expect(
      buildWidgetConnectionStatus({
        signedIn: true,
        onlineMachinesCount: 0,
        totalMachinesCount: 0,
      }),
    ).toBe("Hanzo Cloud · Ready");

    expect(
      buildWidgetConnectionStatus({
        signedIn: true,
        onlineMachinesCount: 1,
        totalMachinesCount: 2,
      }),
    ).toBe("Hanzo Cloud · 1 Mac online");

    expect(
      buildWidgetConnectionStatus({
        signedIn: true,
        onlineMachinesCount: 2,
        totalMachinesCount: 2,
      }),
    ).toBe("Hanzo Cloud · 2 Macs online");

    expect(
      buildWidgetConnectionStatus({
        signedIn: true,
        onlineMachinesCount: 0,
        totalMachinesCount: 3,
      }),
    ).toBe("Hanzo Cloud · 3 linked");
  });
});
