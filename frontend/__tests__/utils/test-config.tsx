import { vi } from "vitest";
import Hanzo from "#/api/hanzo";

export const setupTestConfig = () => {
  const getConfigSpy = vi.spyOn(Hanzo, "getConfig");
  getConfigSpy.mockResolvedValue({
    APP_MODE: "oss",
    GITHUB_CLIENT_ID: "test-id",
    POSTHOG_CLIENT_KEY: "test-key",
  });
};

export const setupSaasTestConfig = () => {
  const getConfigSpy = vi.spyOn(Hanzo, "getConfig");
  getConfigSpy.mockResolvedValue({
    APP_MODE: "saas",
    GITHUB_CLIENT_ID: "test-id",
    POSTHOG_CLIENT_KEY: "test-key",
  });
};
