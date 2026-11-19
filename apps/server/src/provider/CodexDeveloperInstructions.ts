import type { V2TurnStartParams__AdditionalContextEntry } from "effect-codex-app-server/schema";
import { buildRuntimeInstructions } from "@hanzodev/provider-core/server/runtimeInstructions";

import {
  HANZO_DEV_BROWSER_TOOL_INSTRUCTIONS,
  HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS,
  HANZO_DEV_ORCHESTRATION_SECTIONS,
} from "@hanzodev/provider-core/server/orchestrationInstructions";

/** Codex silently truncates additionalContext values over 1,000 tokens, counted as UTF-8 bytes / 4. */
export const CODEX_ADDITIONAL_CONTEXT_VALUE_MAX_BYTES = 4_000;

export interface HanzoDevToolAvailability {
  readonly browser: boolean;
  readonly device: boolean;
}

const normalizeAvailability = (
  availability: boolean | HanzoDevToolAvailability,
): HanzoDevToolAvailability =>
  typeof availability === "boolean" ? { browser: availability, device: false } : availability;

/** Instructions for unavailable tools would steer the model away from its remaining automation. */
const toolInstructions = (availability: boolean | HanzoDevToolAvailability): string => {
  const tools = normalizeAvailability(availability);
  return [
    tools.browser ? HANZO_DEV_BROWSER_TOOL_INSTRUCTIONS : "",
    tools.device ? HANZO_DEV_DEVICE_TOOL_INSTRUCTIONS : "",
  ]
    .filter(Boolean)
    .join("\n\n");
};

export interface CodexRuntimeInfo {
  readonly model: string;
  readonly modelName?: string | undefined;
  readonly reasoningEffort?: string | undefined;
}

/** Keep this outside collaboration mode; model-catalog mode text overrides client developer_instructions. */
export function buildCodexAdditionalContext(
  runtime: CodexRuntimeInfo,
  toolsAvailable: boolean | HanzoDevToolAvailability = true,
): Record<string, V2TurnStartParams__AdditionalContextEntry> {
  const tools = toolInstructions(toolsAvailable);
  // Split values to fit per-entry caps; number keys because Codex emits them in key order.
  return {
    ...Object.fromEntries(
      HANZO_DEV_ORCHESTRATION_SECTIONS.map((section, index) => [
        `hanzo_dev_orchestration_${String(index + 1).padStart(2, "0")}`,
        { kind: "application" as const, value: section },
      ]),
    ),
    hanzo_dev_runtime: {
      kind: "application",
      value: buildRuntimeInstructions({ harness: "Codex", ...runtime }),
    },
    ...(tools ? { hanzo_dev_tools: { kind: "application", value: tools } } : {}),
  };
}
