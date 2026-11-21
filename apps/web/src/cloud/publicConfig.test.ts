import { afterEach, describe, expect, it, vi } from "vite-plus/test";

import { hasCloudPublicConfig } from "./publicConfig.ts";

afterEach(() => {
  vi.unstubAllEnvs();
});

describe("hasCloudPublicConfig", () => {
  it("rejects an insecure relay URL", () => {
    vi.stubEnv("VITE_HANZO_DEV_RELAY_URL", "http://relay.example.test");

    expect(hasCloudPublicConfig()).toBe(false);
  });

  it("is on with a secure relay URL and the default Hanzo IAM", () => {
    vi.stubEnv("VITE_HANZO_DEV_RELAY_URL", "https://relay.example.test");

    expect(hasCloudPublicConfig()).toBe(true);
  });
});
