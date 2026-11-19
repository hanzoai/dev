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
  it("defaults to Hanzo IAM (hanzo.id) and Hanzo Relay out-of-the-box", () => {
    expect(resolveCloudPublicConfig({})).toEqual({
      iam: {
        issuer: "https://hanzo.id",
        clientId: "hanzo-app",
      },
      clerk: {
        publishableKey: null,
        jwtTemplate: null,
      },
      relay: {
        url: "https://relay.dev.hanzo.ai",
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
      clerk: {
        publishableKey: null,
        jwtTemplate: null,
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
    ).toBe(false);
  });
});
