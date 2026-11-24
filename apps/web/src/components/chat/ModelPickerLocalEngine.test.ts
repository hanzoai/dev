import type { ProviderInstanceId } from "@hanzodev/contracts";
import { describe, expect, it } from "vite-plus/test";

import type { ProviderInstanceEntry } from "../../providerInstances";
import { localEngineHealthStatus } from "./ModelPickerSidebar";

const makeEntry = (
  instanceId: string,
  status: ProviderInstanceEntry["status"],
  isAvailable = true,
): ProviderInstanceEntry =>
  ({
    instanceId: instanceId as ProviderInstanceId,
    driverKind: "hanzo",
    displayName: "Ollama",
    enabled: true,
    isAvailable,
    status,
    snapshot: { status },
  }) as unknown as ProviderInstanceEntry;

describe("localEngineHealthStatus", () => {
  it("returns null for non-local provider instances", () => {
    expect(localEngineHealthStatus(makeEntry("claude", "ready"))).toBeNull();
    expect(localEngineHealthStatus(makeEntry("codex", "error"))).toBeNull();
  });

  it("reports ready, error, and warning health states for local engines", () => {
    expect(localEngineHealthStatus(makeEntry("local_127_0_0_1_11434", "ready", true))).toBe(
      "ready",
    );
    expect(localEngineHealthStatus(makeEntry("local_127_0_0_1_11434", "error", true))).toBe(
      "error",
    );
    expect(localEngineHealthStatus(makeEntry("local_127_0_0_1_11434", "ready", false))).toBe(
      "error",
    );
    expect(localEngineHealthStatus(makeEntry("local_127_0_0_1_11434", "warning", true))).toBe(
      "warning",
    );
  });
});
