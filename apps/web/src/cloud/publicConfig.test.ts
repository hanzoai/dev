import { describe, expect, it } from "vite-plus/test";

import { hasCloudPublicConfig } from "./publicConfig.ts";

describe("hasCloudPublicConfig", () => {
  it("is on with the default Hanzo IAM and no relay, so the account shows", () => {
    expect(hasCloudPublicConfig()).toBe(true);
  });
});
