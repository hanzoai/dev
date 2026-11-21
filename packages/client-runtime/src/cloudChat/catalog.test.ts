import { describe, expect, it } from "vitest";

import {
  cloudChatAcceptsInput,
  cloudChatDriver,
  parseChatModels,
  toWireMessage,
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
    ]);
    expect(providers.map((p) => p.driver)).toEqual(["hanzo", "claudeAgent", "codex"]);
    expect(providers[0]?.models.map((m) => m.slug)).toEqual(["enso-flash"]);
    expect(providers[0]?.models[0]?.capabilities?.optionDescriptors?.[0]?.id).toBe(
      "reasoningEffort",
    );
    expect(providers[1]?.models[0]?.capabilities).toBeNull();
  });

  it("round-trips effort through the selection", () => {
    expect(cloudChatDriver({ id: "claude-opus", label: "" })).toBe("claudeAgent");
    const picked = cloudChatSelection([{ id: "enso", label: "Enso" }], "enso", "high");
    expect(cloudChatEffort(picked)).toBe("high");
    expect(cloudChatSelection([], "claude-opus", "high").options).toBeUndefined();
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

  it("groups by the catalog's owner, falling back to the id only when it names none", () => {
    const providers = cloudChatProviders([
      { id: "claude-x", label: "Claude X", provider: "hanzo" },
      { id: "gemini-3", label: "Gemini 3", provider: "Google" },
      { id: "gpt-5", label: "GPT 5", provider: "openai" },
      { id: "grok-4", label: "Grok 4" },
    ]);
    expect(providers.map((p) => [p.driver, p.models.map((m) => m.slug)])).toEqual([
      ["hanzo", ["claude-x"]],
      ["google", ["gemini-3"]],
      ["codex", ["gpt-5"]],
      ["grok", ["grok-4"]],
    ]);
    expect(providers[1]?.displayName).toBe("Google");
    expect(providers[0]?.displayName).toBeUndefined();
  });

  it("reads owner and input modalities from the catalog", () => {
    const [vision, plain, unknown] = parseChatModels({
      data: [
        { id: "a", owned_by: "anthropic", inputs: ["text", "image"] },
        { id: "b", provider: "xai", architecture: { input_modalities: ["text"] } },
        { id: "c" },
      ],
    });
    expect(vision).toMatchObject({ provider: "anthropic", inputs: ["text", "image"] });
    expect(plain).toMatchObject({ provider: "xai", inputs: ["text"] });
    expect(cloudChatAcceptsInput(vision, "image")).toBe(true);
    expect(cloudChatAcceptsInput(plain, "image")).toBe(false);
    expect(cloudChatAcceptsInput(unknown, "image")).toBe(true);
    expect(cloudChatAcceptsInput(vision, "file")).toBe(false);
  });

  it("serialises attachments as content parts", () => {
    expect(toWireMessage({ role: "user", content: "hi" })).toEqual({ role: "user", content: "hi" });
    expect(
      toWireMessage({
        role: "user",
        content: "look",
        attachments: [
          { kind: "image", dataUrl: "data:image/png;base64,AAA" },
          { kind: "text", name: "a.txt", text: "body" },
          { kind: "file", name: "b.pdf", dataUrl: "data:application/pdf;base64,BBB" },
        ],
      }),
    ).toEqual({
      role: "user",
      content: [
        { type: "text", text: "look" },
        { type: "image_url", image_url: { url: "data:image/png;base64,AAA" } },
        { type: "text", text: "a.txt:\nbody" },
        { type: "file", file: { filename: "b.pdf", file_data: "data:application/pdf;base64,BBB" } },
      ],
    });
  });

  it("sends attachments in the request body, and only text to the thread store", async () => {
    const bodies: Array<{ url: string; body: any }> = [];
    const controller = createCloudChatController({
      getAuth: async () => ({ accessToken: "t" }),
      options: {
        fetch: async (url, init) => {
          bodies.push({ url: String(url), body: JSON.parse(String(init?.body)) });
          return new Response("data: [DONE]\n\n", { status: 200 });
        },
      },
    });
    await controller.send("look", [{ kind: "image", dataUrl: "data:image/png;base64,AAA" }]);
    await controller.send("again");
    const chats = bodies.filter((entry) => entry.url.endsWith("/v1/chat/completions"));
    expect(chats[0]?.body.messages[0].content).toEqual([
      { type: "text", text: "look" },
      { type: "image_url", image_url: { url: "data:image/png;base64,AAA" } },
    ]);
    // The image stays in the conversation's later requests.
    expect(chats[1]?.body.messages[0].content).toHaveLength(2);
    expect(chats[1]?.body.messages.at(-1)).toEqual({ role: "user", content: "again" });
    const recorded = bodies.find((entry) => entry.url.includes("/conversations"));
    expect(recorded?.body.messages[0]).toEqual({ role: "user", content: "look" });
  });
});
