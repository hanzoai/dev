import { describe, expect, it, vi } from "vite-plus/test";
import {
  filterAllowedLocalAiModels,
  isAllowedLocalAiModel,
  probeHanzoEngineStatus,
  probeHanzodValidatorStatus,
  probeLocalAiStack,
  probeOllamaStatus,
  resolveOllamaPullTarget,
  ZEN_LOCAL_AI_CATALOG,
} from "./localAi.ts";

describe("localAi Zen / Qwen3+ catalog and runtime probes", () => {
  it("enforces qwen3+ only and rejects every qwen2 variant", () => {
    expect(ZEN_LOCAL_AI_CATALOG.length).toBeGreaterThanOrEqual(6);
    for (const model of ZEN_LOCAL_AI_CATALOG) {
      expect(isAllowedLocalAiModel(model.id)).toBe(true);
      expect(isAllowedLocalAiModel(model.ollamaTag)).toBe(true);
      expect(isAllowedLocalAiModel(model.hfRepo)).toBe(true);
      expect(model.ollamaTag.toLowerCase()).not.toContain("qwen2");
    }

    expect(isAllowedLocalAiModel("qwen3:8b")).toBe(true);
    expect(isAllowedLocalAiModel("qwen3-coder:30b")).toBe(true);
    expect(isAllowedLocalAiModel("zen4-mini")).toBe(true);
    expect(isAllowedLocalAiModel("qwen2:7b")).toBe(false);
    expect(isAllowedLocalAiModel("Qwen2.5-Coder:14b")).toBe(false);
    expect(isAllowedLocalAiModel("hf.co/Qwen/Qwen2.5-7B-Instruct")).toBe(false);
    expect(() => resolveOllamaPullTarget("qwen2.5:7b")).toThrow(/qwen2/i);
    expect(resolveOllamaPullTarget("zen4-mini")).toBe("qwen3:4b");
    expect(resolveOllamaPullTarget("zen4-coder:30b")).toBe("qwen3-coder:30b");

    expect(
      filterAllowedLocalAiModels([
        { id: "qwen2.5:7b" },
        { id: "qwen3:8b" },
        { id: "zen4:14b" },
      ]),
    ).toEqual([{ id: "qwen3:8b" }, { id: "zen4:14b" }]);
  });

  it("probes Ollama (:11434) and strips any installed qwen2 tags while mapping Zen aliases", async () => {
    const fetchMock = vi.fn(async () =>
      new Response(
        JSON.stringify({
          models: [
            { name: "qwen2.5:7b", size: 4_000_000_000 },
            { name: "qwen3:4b", size: 2_500_000_000 },
          ],
        }),
        { status: 200 },
      ),
    );
    const status = await probeOllamaStatus({
      fetchImpl: fetchMock as unknown as typeof globalThis.fetch,
    });
    expect(status.online).toBe(true);
    const ids = status.models.map((m) => m.id);
    expect(ids).toContain("qwen3:4b");
    expect(ids).toContain("zen4-mini");
    expect(ids.some((id) => /qwen2/i.test(id))).toBe(false);
  });

  it("probes Embedded Hanzo Engine (:36900) and hanzod validator (:9630 / :9631)", async () => {
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
        const init = (fetchMock.mock.calls.at(-1) as unknown as [unknown, RequestInit | undefined])?.[1];
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

    const [engine, hanzod, stack] = await Promise.all([
      probeHanzoEngineStatus({ fetchImpl: fetchMock as unknown as typeof globalThis.fetch }),
      probeHanzodValidatorStatus({ fetchImpl: fetchMock as unknown as typeof globalThis.fetch }),
      probeLocalAiStack({ fetchImpl: fetchMock as unknown as typeof globalThis.fetch }),
    ]);

    expect(engine.online).toBe(true);
    expect(engine.loadedModel).toBe("Qwen/Qwen3-8B");
    expect(engine.device).toBe("metal");
    expect(engine.models.map((m) => m.id)).toEqual(["Qwen/Qwen3-8B"]);

    expect(hanzod.online).toBe(true);
    expect(hanzod.healthy).toBe(true);
    expect(hanzod.blockHeight).toBe(0x10858c);
    expect(hanzod.chainId).toBe(36963);
    expect(hanzod.networkLabel).toBe("Hanzo Mainnet (36963)");
    expect(hanzod.stakingEndpoint).toBe("127.0.0.1:9631");

    expect(stack.installedModels.some((m) => /qwen2/i.test(m.id))).toBe(false);
  });
});
