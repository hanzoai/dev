import { assert, describe, it } from "@effect/vitest";

import {
  HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS,
  HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS_WITH_TOOL_SEARCH,
  HANZO_DEV_ORCHESTRATION_INSTRUCTIONS,
  hanzoAcpPromptWithInstructions,
  hanzoOrchestrationPrompt,
  hanzoOrchestrationSystemPrompt,
} from "./orchestrationInstructions.ts";

describe("Hanzo device instructions", () => {
  it("route device work through device_open and give Claude the line that loads the tools", () => {
    for (const intent of ["`device_list`", "`device_open`", "`agent-device`", "ssh", "`hostId`"]) {
      assert.include(HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS, intent);
    }
    assert.notInclude(HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS, "ToolSearch");
    const loadLine =
      " Load them with ToolSearch `select:mcp__hanzo-dev__device_list,mcp__hanzo-dev__device_open,mcp__hanzo-dev__device_screenshot,mcp__hanzo-dev__device_close`.";
    assert.include(HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS_WITH_TOOL_SEARCH, loadLine);
    assert.equal(
      HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS_WITH_TOOL_SEARCH.replace(loadLine, ""),
      HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS,
    );
  });

  it("open the app before the returned command's snapshots, screenshots and input", () => {
    assert.include(
      HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS,
      "its snapshots, screenshots and input need `open <app>` first,",
    );
  });

  it("keep the failure rule from taking over or closing a session another thread holds", () => {
    // agent-device's DEVICE_IN_USE advice is to rerun with the owner's --session or close it first.
    assert.include(
      HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS,
      "follow its advice once, but never reuse or close another session; if it still fails,",
    );
  });

  it("reach OpenCode and Cursor only when the session holds device access", () => {
    assert.include(hanzoOrchestrationSystemPrompt(true, true), HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS);
    assert.notInclude(hanzoOrchestrationSystemPrompt(true, false), "## Hanzo Dev devices");
    assert.isUndefined(hanzoOrchestrationSystemPrompt(false, true));

    const firstRun = (hasDeviceTools: boolean) =>
      hanzoOrchestrationPrompt({
        prompt: "Check the login screen.",
        hasHanzoMcp: true,
        hasDeviceTools,
        orchestrationDelivered: false,
        deviceInstructionsDelivered: false,
      });
    assert.include(firstRun(true), HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS);
    assert.notInclude(firstRun(false), "## Hanzo Dev devices");

    const laterRun = (deviceInstructionsDelivered: boolean) =>
      hanzoOrchestrationPrompt({
        prompt: "Check the login screen.",
        hasHanzoMcp: true,
        hasDeviceTools: true,
        orchestrationDelivered: true,
        deviceInstructionsDelivered,
      });
    assert.include(laterRun(false), HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS);
    assert.notInclude(laterRun(false), HANZO_DEV_ORCHESTRATION_INSTRUCTIONS.trim());
    assert.equal(laterRun(true), "Check the login screen.");
  });

  it("reach ACP agents again when device access changes mid-thread", () => {
    const state = { interactionMode: "default", hasHanzoMcp: true, hasDeviceTools: false } as const;
    const withDevices = hanzoAcpPromptWithInstructions({
      prompt: "Check the login screen.",
      state: { ...state, hasDeviceTools: true },
      previousState: state,
    });
    assert.include(withDevices, HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS);
    assert.equal(
      hanzoAcpPromptWithInstructions({ prompt: "Again.", state, previousState: state }),
      "Again.",
    );
    assert.notInclude(hanzoAcpPromptWithInstructions({ prompt: "Hi.", state }), "## Hanzo Dev devices");
  });
});
