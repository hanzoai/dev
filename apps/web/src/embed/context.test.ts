import { describe, expect, it } from "vite-plus/test";

import { isEmbedded, markEmbedded } from "./context";

describe("embedded", () => {
  it("is off until a host mounts the core, then stays on for the page", () => {
    expect(isEmbedded()).toBe(false);
    markEmbedded();
    expect(isEmbedded()).toBe(true);
  });
});
