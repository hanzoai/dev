import { describe, expect, it } from "vite-plus/test";

import { landsInChat } from "./chat-landing.logic";

describe("chat landing", () => {
  it("lands a signed-in user in chat, never on an environment gate", () => {
    expect(landsInChat({ signedIn: true })).toBe(true);
  });
});
