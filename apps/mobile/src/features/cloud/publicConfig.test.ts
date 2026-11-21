import { describe, expect, it, vi } from "vite-plus/test";

import { hasCloudPublicConfig, resolveCloudPublicConfig } from "./publicConfig";

vi.mock("expo-constants", () => ({
  default: {
    expoConfig: {
      extra: {},
    },
  },
}));

describe("resolveCloudPublicConfig", () => {
  it("defaults to Hanzo IAM (hanzo.ai) and no relay out-of-the-box", () => {
    expect(resolveCloudPublicConfig({})).toEqual({
      iam: {
        issuer: "https://hanzo.ai",
        clientId: "hanzo-app",
      },
      relay: {
        url: null,
      },
      observability: {
        tracesUrl: null,
        tracesDataset: null,
        tracesToken: null,
      },
    });
    expect(hasCloudPublicConfig({})).toBe(true);
  });

  it("rejects an insecure relay URL", () => {
    expect(
      resolveCloudPublicConfig({
        iam: { issuer: "https://lux.id", clientId: "lux-app" },
        relay: { url: "http://relay.example.test" },
      }),
    ).toEqual({
      iam: {
        issuer: "https://lux.id",
        clientId: "lux-app",
      },
      relay: {
        url: null,
      },
      observability: {
        tracesUrl: null,
        tracesDataset: null,
        tracesToken: null,
      },
    });
    expect(
      hasCloudPublicConfig({
        relay: { url: "http://relay.example.test" },
      }),
    ).toBe(true);
  });
});
