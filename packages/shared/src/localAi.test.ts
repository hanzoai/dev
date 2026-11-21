import { describe, expect, it, vi } from "vite-plus/test";
import { tokenizeCliArgs } from "./cliArgs.ts";
import {
  BUILT_IN_LOCAL_ENGINES,
  HANZOD_ENGINE_BASE_URL,
  LOCAL_PRIVATE_GROUP,
  parseEngineCatalog,
  localEngineAction,
  localEngineInstanceId,
  localEngineNativeUrl,
  localEngineProviderInstance,
  normalizeLocalModels,
  parseLocalEngines,
  probeHanzodValidatorStatus,
  probeLocalEngine,
} from "./localAi.ts";

const json = (body: unknown, status = 200) => new Response(JSON.stringify(body), { status });
const asFetch = (impl: (url: string) => Promise<Response>) =>
  vi.fn(async (input: string | URL | Request) =>
    impl(typeof input === "string" ? input : input instanceof URL ? input.href : input.url),
  ) as unknown as typeof globalThis.fetch;

describe("local engine registry", () => {
  it("ships Hanzo Engine (the hanzod route), Ollama, and LM Studio", () => {
    expect(BUILT_IN_LOCAL_ENGINES.map((e) => [e.name, e.kind, e.baseUrl])).toEqual([
      ["Hanzo Engine", "hanzo-engine", "http://127.0.0.1:3690/v1/engine"],
      ["Ollama", "ollama", "http://127.0.0.1:11434/v1"],
      ["LM Studio", "lm-studio", "http://127.0.0.1:1234/v1"],
    ]);
  });

  it("parses stored custom engines, dropping invalid and duplicate rows", () => {
    const raw = JSON.stringify([
      { name: " vLLM ", kind: "openai", baseUrl: "http://10.0.0.5:8000/v1/" },
      { name: "dup", kind: "openai", baseUrl: "http://10.0.0.5:8000/v1" },
      { name: "no scheme", kind: "openai", baseUrl: "10.0.0.6:8000" },
      { name: "", kind: "openai", baseUrl: "http://10.0.0.7/v1" },
      { name: "odd kind", kind: "mystery", baseUrl: "http://10.0.0.8/v1" },
      "garbage",
      null,
    ]);
    expect(parseLocalEngines(raw)).toEqual([
      { name: "vLLM", kind: "openai", baseUrl: "http://10.0.0.5:8000/v1" },
      { name: "odd kind", kind: "openai", baseUrl: "http://10.0.0.8/v1" },
    ]);
    expect(parseLocalEngines("not json")).toEqual([]);
    expect(parseLocalEngines('{"a":1}')).toEqual([]);
    expect(parseLocalEngines(null)).toEqual([]);
  });

  it("offers pull for Ollama, load for Hanzo Engine, and nothing for other endpoints", () => {
    expect(localEngineAction("ollama")).toBe("pull");
    expect(localEngineAction("hanzo-engine")).toBeNull();
    expect(localEngineAction("lm-studio")).toBeNull();
    expect(localEngineAction("openai")).toBeNull();
    expect(localEngineNativeUrl({ baseUrl: "http://127.0.0.1:11434/v1" }, "/api/pull")).toBe(
      "http://127.0.0.1:11434/api/pull",
    );
    expect(localEngineNativeUrl({ baseUrl: HANZOD_ENGINE_BASE_URL }, "/v1/engine/load")).toBe(
      "http://127.0.0.1:3690/v1/engine/load",
    );
  });
});

describe("/v1/models normalization", () => {
  it("keeps every served model, whatever the architecture", () => {
    const models = normalizeLocalModels(
      {
        data: [
          { id: "qwen2.5:0.5b" },
          { id: "llama3.2:3b" },
          { id: "  " },
          { id: "llama3.2:3b" },
          {},
        ],
      },
      "ollama",
    );
    expect(models).toEqual([
      { id: "qwen2.5:0.5b", zen: false },
      { id: "llama3.2:3b", zen: false },
    ]);
  });

  it("labels Zen models only on Hanzo Engine and lists them first", () => {
    const body = {
      data: [{ id: "mistral-7b" }, { id: "zen6-coder" }, { id: "gemma-3" }, { id: "zen6" }],
    };
    expect(normalizeLocalModels(body, "hanzo-engine")).toEqual([
      { id: "zen6-coder", zen: true },
      { id: "zen6", zen: true },
      { id: "mistral-7b", zen: false },
      { id: "gemma-3", zen: false },
    ]);
    expect(normalizeLocalModels(body, "openai").some((m) => m.zen)).toBe(false);
  });

  it("accepts Ollama native tags, bare arrays, and rejects junk", () => {
    expect(
      normalizeLocalModels(
        { models: [{ name: "phi4:latest" }, { model: "gemma3:1b" }] },
        "ollama",
      ).map((m) => m.id),
    ).toEqual(["phi4:latest", "gemma3:1b"]);
    expect(normalizeLocalModels(["a", { id: "b" }], "openai").map((m) => m.id)).toEqual(["a", "b"]);
    expect(normalizeLocalModels(null, "openai")).toEqual([]);
    expect(normalizeLocalModels({ data: "nope" }, "openai")).toEqual([]);
  });
});

describe("engine probes", () => {
  it("reads /models from the engine and reports health", async () => {
    const fetchImpl = asFetch(async (url) =>
      url === "http://127.0.0.1:1234/v1/models"
        ? json({ data: [{ id: "qwen3-8b" }] })
        : json({}, 404),
    );
    const status = await probeLocalEngine(BUILT_IN_LOCAL_ENGINES[2]!, { fetchImpl });
    expect(status.online).toBe(true);
    expect(status.models).toEqual([{ id: "qwen3-8b", zen: false }]);
  });

  it("is offline when hanzod does not answer; there is no second port", async () => {
    const seen: string[] = [];
    const fetchImpl = asFetch(async (url) => {
      seen.push(url);
      throw new Error("ECONNREFUSED");
    });
    const status = await probeLocalEngine(BUILT_IN_LOCAL_ENGINES[0]!, { fetchImpl });
    expect(status.online).toBe(false);
    expect(seen).toEqual([`${HANZOD_ENGINE_BASE_URL}/models`]);
  });

  it("prefers the hanzod route when it answers", async () => {
    const fetchImpl = asFetch(async (url) =>
      json({ data: [{ id: url.includes("3690") ? "via-hanzod" : "x" }] }),
    );
    const status = await probeLocalEngine(BUILT_IN_LOCAL_ENGINES[0]!, { fetchImpl });
    expect(status.baseUrl).toBe(HANZOD_ENGINE_BASE_URL);
    expect(status.models[0]?.id).toBe("via-hanzod");
  });

  it("reports offline on refusal, non-200, and a non-JSON body without throwing", async () => {
    const down = await probeLocalEngine(BUILT_IN_LOCAL_ENGINES[1]!, {
      fetchImpl: asFetch(async () => {
        throw new Error("ECONNREFUSED");
      }),
    });
    expect(down).toMatchObject({ online: false, latencyMs: null, models: [] });
    const bad = await probeLocalEngine(BUILT_IN_LOCAL_ENGINES[1]!, {
      fetchImpl: asFetch(async () => json({}, 500)),
    });
    expect(bad.online).toBe(false);
    const html = await probeLocalEngine(BUILT_IN_LOCAL_ENGINES[1]!, {
      fetchImpl: asFetch(async () => new Response("<html>", { status: 200 })),
    });
    expect(html).toMatchObject({ online: true, models: [] });
  });
});

describe("model picker entry", () => {
  it("points a Hanzo agent instance at the engine and lists its models", () => {
    const engine = BUILT_IN_LOCAL_ENGINES[1]!;
    const entry = localEngineProviderInstance({
      engine,
      online: true,
      baseUrl: engine.baseUrl,
      latencyMs: 4,
      models: normalizeLocalModels({ data: [{ id: "llama3.2:3b" }] }, "ollama"),
    });
    const id = localEngineInstanceId(engine);
    expect(id).toBe("local_127_0_0_1_11434");
    expect(entry.displayName).toBe("Ollama");
    expect(entry.config.customModels).toEqual([{ slug: "llama3.2:3b" }]);
    expect(entry.config.launchArgs).toContain(
      `model_providers.${id}.base_url="http://127.0.0.1:11434/v1"`,
    );
    expect(entry.config.launchArgs).toContain(`-c model_provider="${id}"`);
    expect(tokenizeCliArgs(entry.config.launchArgs)).toEqual([
      "-c",
      `model_provider=${id}`,
      "-c",
      `model_providers.${id}.name=Ollama`,
      "-c",
      `model_providers.${id}.base_url=http://127.0.0.1:11434/v1`,
      "-c",
      `model_providers.${id}.wire_api=chat`,
    ]);
  });
});

describe("hanzod validator", () => {
  it("probes the hanzod validator (:9630 / :9631)", async () => {
    const fetchMock = vi.fn(async (url: string | URL | Request) => {
      const href = typeof url === "string" ? url : url instanceof URL ? url.toString() : url.url;
      if (href.endsWith("/health")) {
        return new Response(JSON.stringify({ ok: true }), { status: 200 });
      }
      if (href.endsWith("/v1/models")) {
        return new Response(
          JSON.stringify({
            data: [{ id: "qwen2.5-coder" }, { id: "Qwen/Qwen3-8B" }],
          }),
          { status: 200 },
        );
      }
      if (href.endsWith("/v1/engine/status")) {
        return new Response(
          JSON.stringify({
            loaded_model: "Qwen/Qwen3-8B",
            device: "metal",
            memory_usage: 5120,
          }),
          { status: 200 },
        );
      }
      if (href.endsWith("/v1/health")) {
        return new Response(JSON.stringify({ healthy: true }), { status: 200 });
      }
      if (href.endsWith("/ext/bc/C/rpc")) {
        const init = (
          fetchMock.mock.calls.at(-1) as unknown as [unknown, RequestInit | undefined]
        )?.[1];
        const body = JSON.parse(String(init?.body ?? "{}")) as { method?: string };
        if (body.method === "eth_blockNumber") {
          return new Response(JSON.stringify({ jsonrpc: "2.0", id: 1, result: "0x10858c" }), {
            status: 200,
          });
        }
        if (body.method === "eth_chainId") {
          return new Response(JSON.stringify({ jsonrpc: "2.0", id: 2, result: "0x9063" }), {
            status: 200,
          });
        }
      }
      return new Response(null, { status: 404 });
    });

    const hanzod = await probeHanzodValidatorStatus({
      fetchImpl: fetchMock as unknown as typeof globalThis.fetch,
    });

    expect(hanzod.online).toBe(true);
    expect(hanzod.healthy).toBe(true);
    expect(hanzod.blockHeight).toBe(0x10858c);
    expect(hanzod.chainId).toBe(36963);
    expect(hanzod.networkLabel).toBe("Hanzo Mainnet (36963)");
    expect(hanzod.stakingEndpoint).toBe("127.0.0.1:9631");
  });
});

describe("the engine catalog", () => {
  it("parses hanzod's catalog", () => {
    const catalog = parseEngineCatalog({
      memory_bytes: 17179869184,
      recommended: "zen-eco-3b-agent-q8",
      selected: null,
      models: [
        {
          id: "zen-eco-3b-agent",
          name: "Zen Eco 3B Agent",
          bytes: 1929902464,
          memory_gb: 8,
          installed: false,
          selected: false,
          pull: { state: "pulling", done: 10, total: 100, error: null },
        },
      ],
    });
    expect(catalog?.recommended).toBe("zen-eco-3b-agent-q8");
    expect(catalog?.models[0]).toMatchObject({
      id: "zen-eco-3b-agent",
      installed: false,
      pull: { state: "pulling", done: 10, total: 100 },
    });
    expect(parseEngineCatalog({ error: "x" })).toBeNull();
  });

  it("puts Hanzo Engine models under Local · Private", () => {
    const status = {
      engine: BUILT_IN_LOCAL_ENGINES[0]!,
      online: true,
      baseUrl: HANZOD_ENGINE_BASE_URL,
      latencyMs: 1,
      models: [{ id: "zen-eco-3b-agent", zen: true }],
    };
    expect(localEngineProviderInstance(status).displayName).toBe(LOCAL_PRIVATE_GROUP);
  });
});
