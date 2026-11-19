import { describe, expect, it } from "vite-plus/test";

import {
  HANZO_MCP_TOOL_NAMES,
  resolveHanzoMcpSummaryAction,
  resolveHanzoMcpToolDefinition,
  resolveHanzoMcpToolPresentation,
} from "./hanzoMcpToolPresentation.ts";

describe("resolveHanzoMcpToolPresentation", () => {
  it("recognizes every Hanzo tool across provider prefixes and completion suffixes", () => {
    for (const tool of HANZO_MCP_TOOL_NAMES) {
      const presentation = resolveHanzoMcpToolPresentation(tool);
      for (const prefix of [
        "mcp__hanzo-dev__",
        "mcp__hanzo_dev__",
        "mcp__hanzodev__",
        "Hanzo-dev.",
        "hanzodev.",
        "hanzo-dev-direct.",
        "mcp__hanzo_dev_direct__",
        "hanzo_dev/",
        "hanzodev:",
        "mcp_hanzo-dev_",
        "Hanzo Dev ",
        "hanzo-dev · ",
      ]) {
        expect(resolveHanzoMcpToolPresentation(`${prefix}${tool} completed`), tool).toEqual(
          presentation,
        );
      }
      expect(resolveHanzoMcpToolPresentation(`mcp__another-server__${tool}`), tool).toBeNull();
    }
  });

  it("matches OpenCode 2's per-thread server names, whose thread ids hold underscores", () => {
    expect(
      resolveHanzoMcpToolPresentation("hanzo-dev-thread_opencode2-adapter_delegate_task")
        ?.displayName,
    ).toBe("Delegate a child task");
    expect(
      resolveHanzoMcpToolPresentation("hanzo-dev-thread_opencode2-adapter_not_a_tool"),
    ).toBeNull();
  });
});

describe("resolveHanzoMcpToolDefinition", () => {
  it("labels an action tool call as the action it ran", () => {
    expect(
      resolveHanzoMcpToolDefinition("mcp__hanzo-dev__preview_act", { action: "type" })?.labels,
    ).toEqual(resolveHanzoMcpToolDefinition("preview_type")?.labels);
    expect(
      resolveHanzoMcpToolDefinition("hanzo_thread_transfer", { action: "fork" })?.displayName,
    ).toBe(resolveHanzoMcpToolDefinition("hanzo_thread_fork")?.displayName);
    expect(resolveHanzoMcpToolDefinition("preview_act", { action: "unknown" })?.labels[2]).toBe(
      "Used",
    );
  });
});

describe("resolveHanzoMcpSummaryAction", () => {
  it("summarizes an action tool by the action each call ran", () => {
    expect(resolveHanzoMcpSummaryAction("mcp__hanzo-dev__hanzo_queue", { action: "list" })).toBe(
      "queue-list",
    );
    expect(resolveHanzoMcpSummaryAction("hanzo_queue", { action: "edit" })).toBe("queue-edit");
    expect(resolveHanzoMcpSummaryAction("hanzo_thread_transfer", { action: "status" })).toBe(
      "thread-transfers",
    );
    expect(resolveHanzoMcpSummaryAction("hanzo_thread_configure", {})).toBe("thread-configuration");
    expect(resolveHanzoMcpSummaryAction("schedule_task", { scheduledTaskId: "task-1" })).toBe(
      "schedule-update",
    );
    expect(resolveHanzoMcpSummaryAction("delegate_task", { task: "x" })).toBe("delegate");
  });
});
