import { describe, expect, it } from "vitest";

import {
  cloudChatDriver,
  cloudChatEffort,
  cloudChatProviders,
  cloudChatSelection,
  createCloudChatController,
} from "./index.ts";

describe("cloud chat catalog", () => {
  it("groups catalog models by provider and gives Hanzo's the effort panel", () => {
    const providers = cloudChatProviders([
      { id: "enso-flash", label: "Enso Flash" },
      { id: "claude-sonnet", label: "Claude Sonnet" },
      { id: "gpt-5", label: "GPT 5" },
      { id: "kai-1", label: "Kai", outputs: ["decision"] } as never,
    ]);
    expect(providers.map((p) => p.driver)).toEqual(["hanzo", "claudeAgent", "codex"]);
    expect(providers[0]?.models.map((m) => m.slug)).toEqual(["enso-flash"]);
    expect(providers[0]?.models[0]?.capabilities?.optionDescriptors?.[0]?.id).toBe("reasoningEffort");
    expect(providers[1]?.models[0]?.capabilities).toBeNull();
  });

  it("round-trips effort through the selection", () => {
    expect(cloudChatDriver("claude-opus")).toBe("claudeAgent");
    const picked = cloudChatSelection("enso", "high");
    expect(cloudChatEffort(picked)).toBe("high");
    expect(cloudChatSelection("claude-opus", "high").options).toBeUndefined();
  });

  it("sends the chosen effort with the reply request", async () => {
    let body: unknown;
    const controller = createCloudChatController({
      getAuth: async () => ({ accessToken: "t" }),
      options: {
        fetch: async (url, init) => {
          if (String(url).endsWith("/v1/chat/completions")) body = JSON.parse(String(init?.body));
          return new Response("data: [DONE]\n\n", { status: 200 });
        },
      },
    });
    controller.selectModel("enso", "high");
    await controller.send("hi");
    expect(body).toMatchObject({ model: "enso", reasoning_effort: "high" });
  });
});
