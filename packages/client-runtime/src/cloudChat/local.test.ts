import { describe, expect, it } from "vite-plus/test";

import {
  cloudChatProviders,
  cloudChatSelection,
  createCloudChatController,
  createLocalChatHost,
  isLocalChatSlug,
  localChatSlug,
  parseLocalChatModels,
  type LocalChatHost,
} from "./index.ts";

const sse = (...chunks: string[]) =>
  new Response(
    chunks
      .map((c) => `data: ${JSON.stringify({ choices: [{ delta: { content: c } }] })}\n\n`)
      .join("") + "data: [DONE]\n\n",
  );

const hostOf = (replies: string[]): LocalChatHost & { calls: string[] } => {
  const calls: string[] = [];
  return {
    calls,
    models: async () => [{ id: "zen4", engine: "Hanzo Engine" }],
    stream: async ({ model, onDelta }) => {
      calls.push(model);
      for (const r of replies) onDelta(r);
      return replies.join("");
    },
  };
};

describe("local chat catalog", () => {
  it("adds a Local group after the cloud providers", () => {
    const providers = cloudChatProviders(
      [{ id: "enso-flash", label: "Enso Flash" }],
      [{ id: "zen4", engine: "Hanzo Engine" }, { id: "llama3" }],
    );
    expect(providers.map((p) => p.displayName ?? p.driver)).toEqual(["hanzo", "Local"]);
    expect(providers[1]?.models.map((m) => m.slug)).toEqual(["local:zen4", "local:llama3"]);
    expect(providers[1]?.models[0]?.subProvider).toBe("Hanzo Engine");
  });

  it("keeps one placeholder row when the Mac has listed nothing, and no group without a host", () => {
    const local = cloudChatProviders([], [])[0];
    expect(local?.models.map((m) => m.slug)).toEqual(["local:"]);
    expect(cloudChatProviders([{ id: "enso", label: "Enso" }]).map((p) => p.driver)).toEqual([
      "hanzo",
    ]);
  });

  it("selects local models on the local instance", () => {
    expect(cloudChatSelection([], localChatSlug("zen4"), "high")).toEqual({
      instanceId: "local",
      model: "local:zen4",
    });
    expect(isLocalChatSlug("enso")).toBe(false);
  });

  it("parses the Mac's model list", () => {
    expect(
      parseLocalChatModels({ data: [{ id: "a", engine: "Ollama" }, { id: "a" }, { id: "" }, 3] }),
    ).toEqual([{ id: "a", engine: "Ollama" }]);
    expect(parseLocalChatModels(null)).toEqual([]);
  });
});

describe("local chat routing", () => {
  const make = (local: () => LocalChatHost | null) => {
    const cloudCalls: string[] = [];
    const controller = createCloudChatController({
      getAuth: async () => ({ accessToken: "t" }),
      local,
      options: {
        fetch: async (url) => {
          cloudCalls.push(String(url));
          return sse("cloud");
        },
      },
    });
    return { controller, cloudCalls };
  };

  it("sends a local model to the Mac and never to api.hanzo.ai", async () => {
    const host = hostOf(["he", "llo"]);
    const { controller, cloudCalls } = make(() => host);
    await controller.loadLocalModels();
    expect(controller.getState().macOnline).toBe(true);
    expect(controller.getState().localModels).toEqual([{ id: "zen4", engine: "Hanzo Engine" }]);
    controller.selectModel(localChatSlug("zen4"));
    await controller.send("hi");
    expect(host.calls).toEqual(["zen4"]);
    expect(controller.getState().messages.at(-1)).toEqual({ role: "assistant", content: "hello" });
    expect(cloudCalls).toEqual([]);
  });

  it("with the Mac offline refuses the choice and does not fall back to cloud", async () => {
    let host: LocalChatHost | null = hostOf(["x"]);
    const { controller, cloudCalls } = make(() => host);
    controller.selectModel(localChatSlug("zen4"));
    host = null;
    await controller.loadLocalModels();
    expect(controller.getState().macOnline).toBe(false);
    await controller.send("hi");
    expect(controller.getState().error).toBe("offline");
    expect(controller.getState().messages).toEqual([{ role: "user", content: "hi" }]);
    expect(cloudCalls).toEqual([]);
    const fresh = make(() => null);
    fresh.controller.selectModel(localChatSlug("zen4"));
    expect(fresh.controller.getState().model).toBe("enso-flash");
  });

  it("keeps cloud as the default and cloud models on api.hanzo.ai", async () => {
    const { controller, cloudCalls } = make(() => hostOf([]));
    expect(controller.getState().model).toBe("enso-flash");
    await controller.send("hi");
    expect(cloudCalls[0]).toContain("https://api.hanzo.ai/v1/chat/completions");
  });
});

describe("local chat host", () => {
  it("talks to the Mac's routes with its bearer and reports an unreachable Mac as offline", async () => {
    const seen: Array<{ url: string; auth: string | null }> = [];
    const host = createLocalChatHost({
      baseUrl: "https://mac.example/",
      bearer: "tok",
      fetch: async (url, init) => {
        seen.push({ url: String(url), auth: new Headers(init?.headers).get("authorization") });
        return String(url).endsWith("models")
          ? Response.json({ data: [{ id: "zen4" }] })
          : sse("ok");
      },
    });
    expect(await host.models()).toEqual([{ id: "zen4" }]);
    expect(await host.stream({ model: "zen4", messages: [], onDelta: () => undefined })).toBe("ok");
    expect(seen).toEqual([
      { url: "https://mac.example/v1/local-ai/models", auth: "Bearer tok" },
      { url: "https://mac.example/v1/local-ai/chat", auth: "Bearer tok" },
    ]);
    const down = createLocalChatHost({
      baseUrl: "https://mac.example",
      bearer: null,
      fetch: async () => {
        throw new Error("down");
      },
    });
    await expect(down.models()).rejects.toMatchObject({ kind: "offline" });
  });
});
