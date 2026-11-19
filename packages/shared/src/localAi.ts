// @effect-diagnostics globalDate:off -- Latency probes measure wall-clock HTTP round-trips outside an Effect runtime.

export const LOCAL_OLLAMA_BASE_URL = "http://127.0.0.1:11434";
export const LOCAL_HANZO_ENGINE_BASE_URL = "http://127.0.0.1:36900";
export const LOCAL_HANZOD_RPC_BASE_URL = "http://127.0.0.1:9630";
export const LOCAL_HANZOD_STAKING_PORT = 9631;

export interface ZenCatalogModel {
  readonly id: string;
  readonly ollamaTag: string;
  readonly hfRepo: string;
  readonly title: string;
  readonly sizeLabel: string;
  readonly description: string;
  readonly family: "zen";
  readonly recommended?: boolean;
  readonly vision?: boolean;
  readonly thinking?: boolean;
  readonly coding?: boolean;
  readonly tools: boolean;
}

export interface InstalledLocalAiModel {
  readonly id: string;
  readonly source: "ollama" | "hanzo-engine" | "custom";
  readonly sizeBytes?: number;
}

export interface OllamaRuntimeStatus {
  readonly online: boolean;
  readonly baseUrl: string;
  readonly latencyMs: number | null;
  readonly models: ReadonlyArray<InstalledLocalAiModel>;
}

export interface HanzoEngineRuntimeStatus {
  readonly online: boolean;
  readonly baseUrl: string;
  readonly latencyMs: number | null;
  readonly loadedModel: string | null;
  readonly device: string | null;
  readonly memoryUsedMb: number | null;
  readonly models: ReadonlyArray<InstalledLocalAiModel>;
}

export interface HanzodValidatorStatus {
  readonly online: boolean;
  readonly healthy: boolean | null;
  readonly rpcBaseUrl: string;
  readonly stakingEndpoint: string;
  readonly chainId: number | null;
  readonly networkLabel: string | null;
  readonly blockHeight: number | null;
  readonly latencyMs: number | null;
}

export interface DiscoveredLanEngine {
  readonly name: string;
  readonly host: string;
  readonly port: number;
  readonly baseUrl: string;
  readonly kind: "hanzo-engine" | "ollama";
  readonly models: ReadonlyArray<string>;
  readonly latencyMs: number | null;
}

export interface LocalAiStackSnapshot {
  readonly ollama: OllamaRuntimeStatus;
  readonly engine: HanzoEngineRuntimeStatus;
  readonly hanzod: HanzodValidatorStatus;
  readonly installedModels: ReadonlyArray<InstalledLocalAiModel>;
}

/**
 * Strict evergreen Zen / Qwen3+ model policy guard: rejects any legacy `qwen2`, `zen4`, or `zen5`
 * weight or alias across catalog entries, Ollama tags, Engine models, and LAN discovery results.
 */
export function isAllowedLocalAiModel(modelId: string | null | undefined): boolean {
  const trimmed = modelId?.trim() ?? "";
  if (!trimmed) return false;
  return !/qwen2|zen[45]/iu.test(trimmed);
}

export function filterAllowedLocalAiModels<T extends { readonly id: string }>(
  models: ReadonlyArray<T>,
): T[] {
  return models.filter((entry) => isAllowedLocalAiModel(entry.id));
}

export const ZEN_LOCAL_AI_CATALOG: ReadonlyArray<ZenCatalogModel> = [
  {
    id: "zen6-flash:0.6b",
    ollamaTag: "qwen3:0.6b",
    hfRepo: "Qwen/Qwen3-0.6B",
    title: "Zen 6 Flash Nano (Qwen3 0.6B)",
    sizeLabel: "0.6 GB",
    description: "Ultra-fast on-device Qwen3 0.6B foundation model for instant offline responses.",
    family: "zen",
    tools: true,
  },
  {
    id: "zen6-flash",
    ollamaTag: "qwen3:4b",
    hfRepo: "Qwen/Qwen3-4B",
    title: "Zen 6 Flash (Qwen3 4B)",
    sizeLabel: "2.5 GB",
    description: "Compact agentic Qwen3 4B model tuned for tool use, fast edits, and laptop batteries.",
    family: "zen",
    thinking: true,
    tools: true,
  },
  {
    id: "zen6",
    ollamaTag: "qwen3:8b",
    hfRepo: "Qwen/Qwen3-8B",
    title: "Zen 6 (Qwen3 8B)",
    sizeLabel: "5.0 GB",
    description: "Flagship Zen 6 built on Qwen3 8B weights with strong reasoning and tool calling.",
    family: "zen",
    recommended: true,
    thinking: true,
    tools: true,
  },
  {
    id: "zen6:14b",
    ollamaTag: "qwen3:14b",
    hfRepo: "Qwen/Qwen3-14B",
    title: "Zen 6 14B (Qwen3 14B)",
    sizeLabel: "9.0 GB",
    description: "High-accuracy local Qwen3 14B reasoning model for multi-file refactors and reviews.",
    family: "zen",
    thinking: true,
    tools: true,
  },
  {
    id: "zen6-max",
    ollamaTag: "qwen3:30b-a3b",
    hfRepo: "Qwen/Qwen3-30B-A3B",
    title: "Zen 6 Max (Qwen3 30B-A3B MoE)",
    sizeLabel: "18 GB",
    description: "Sparse Mixture-of-Experts (3B active) delivering frontier reasoning at 4B token speed.",
    family: "zen",
    thinking: true,
    tools: true,
  },
  {
    id: "zen6-coder",
    ollamaTag: "qwen3-coder:30b",
    hfRepo: "Qwen/Qwen3-Coder-30B-A3B-Instruct",
    title: "Zen 6 Coder (Qwen3 Coder 30B)",
    sizeLabel: "19 GB",
    description: "Specialized agentic coding MoE model tuned for repository navigation and patch synthesis.",
    family: "zen",
    coding: true,
    thinking: true,
    tools: true,
  },
  {
    id: "zen-vl",
    ollamaTag: "qwen3-vl:8b",
    hfRepo: "Qwen/Qwen3-VL-8B-Instruct",
    title: "Zen VL (Qwen3-VL 8B)",
    sizeLabel: "5.8 GB",
    description: "Multimodal Qwen3-VL model for UI screenshot inspection, diagrams, and visual debugging.",
    family: "zen",
    vision: true,
    thinking: true,
    tools: true,
  },
] as const;

const OLLAMA_PULL_ALIASES: Readonly<Record<string, string>> = {
  "zen6-flash:0.6b": "qwen3:0.6b",
  "zen-nano": "qwen3:0.6b",
  "zen6-flash": "qwen3:4b",
  "zen-mini": "qwen3:4b",
  zen6: "qwen3:8b",
  zen: "qwen3:8b",
  "zen6:latest": "qwen3:8b",
  "zen6:8b": "qwen3:8b",
  "zen6:14b": "qwen3:14b",
  "zen6-max": "qwen3:30b-a3b",
  "zen-max": "qwen3:30b-a3b",
  "zen6-coder": "qwen3-coder:30b",
  "zen-coder": "qwen3-coder:30b",
  "zen6-coder:30b": "qwen3-coder:30b",
  "zen-vl": "qwen3-vl:8b",
  "zen-vl:8b": "qwen3-vl:8b",
};

export function resolveOllamaPullTarget(modelTag: string): string {
  const trimmed = modelTag.trim();
  if (!isAllowedLocalAiModel(trimmed)) {
    throw new Error("Legacy qwen2 models are not supported; select a Zen or Qwen3+ model.");
  }
  return OLLAMA_PULL_ALIASES[trimmed] ?? trimmed;
}

export function formatChainNetworkLabel(chainId: number | null): string | null {
  if (chainId === null) return null;
  switch (chainId) {
    case 36963:
      return "Hanzo Mainnet (36963)";
    case 36962:
      return "Hanzo Testnet (36962)";
    case 96369:
      return "Lux Mainnet (96369)";
    case 96368:
      return "Lux Testnet (96368)";
    case 200200:
      return "Zoo Mainnet (200200)";
    case 200201:
      return "Zoo Testnet (200201)";
    default:
      return `Chain ${chainId}`;
  }
}

function parseHexQuantity(raw: unknown): number | null {
  if (typeof raw === "number" && Number.isFinite(raw) && raw >= 0) {
    return Math.floor(raw);
  }
  if (typeof raw !== "string") return null;
  const trimmed = raw.trim();
  if (!/^0x[0-9a-f]+$/iu.test(trimmed)) return null;
  const parsed = Number.parseInt(trimmed, 16);
  return Number.isSafeInteger(parsed) ? parsed : null;
}

function normalizeBaseUrl(raw: string | undefined, fallback: string): string {
  const trimmed = raw?.trim().replace(/\/+$/u, "");
  return trimmed || fallback;
}

export async function probeOllamaStatus(options?: {
  readonly baseUrl?: string;
  readonly timeoutMs?: number;
  readonly fetchImpl?: typeof globalThis.fetch;
}): Promise<OllamaRuntimeStatus> {
  const fetchImpl = options?.fetchImpl ?? globalThis.fetch;
  const baseUrl = normalizeBaseUrl(options?.baseUrl, LOCAL_OLLAMA_BASE_URL);
  const timeoutMs = options?.timeoutMs ?? 1_500;
  const startedAt = Date.now();

  try {
    const response = await fetchImpl(`${baseUrl}/api/tags`, {
      method: "GET",
      signal: AbortSignal.timeout(timeoutMs),
    });
    if (!response.ok) {
      return { online: false, baseUrl, latencyMs: null, models: [] };
    }
    const latencyMs = Math.max(0, Date.now() - startedAt);
    const body = (await response.json()) as {
      readonly models?: ReadonlyArray<{
        readonly name?: string;
        readonly model?: string;
        readonly size?: number;
      }>;
    };
    const seen = new Set<string>();
    const models: InstalledLocalAiModel[] = [];
    for (const row of Array.isArray(body?.models) ? body.models : []) {
      const id = String(row?.name ?? row?.model ?? "").trim();
      if (!isAllowedLocalAiModel(id) || seen.has(id)) continue;
      seen.add(id);
      models.push({
        id,
        source: "ollama",
        ...(typeof row?.size === "number" ? { sizeBytes: row.size } : {}),
      });
      for (const [alias, target] of Object.entries(OLLAMA_PULL_ALIASES)) {
        if (target === id && !seen.has(alias)) {
          seen.add(alias);
          models.push({
            id: alias,
            source: "ollama",
            ...(typeof row?.size === "number" ? { sizeBytes: row.size } : {}),
          });
        }
      }
    }
    return { online: true, baseUrl, latencyMs, models };
  } catch {
    return { online: false, baseUrl, latencyMs: null, models: [] };
  }
}

export async function probeHanzoEngineStatus(options?: {
  readonly baseUrl?: string;
  readonly timeoutMs?: number;
  readonly fetchImpl?: typeof globalThis.fetch;
}): Promise<HanzoEngineRuntimeStatus> {
  const fetchImpl = options?.fetchImpl ?? globalThis.fetch;
  const baseUrl = normalizeBaseUrl(options?.baseUrl, LOCAL_HANZO_ENGINE_BASE_URL);
  const timeoutMs = options?.timeoutMs ?? 1_500;
  const startedAt = Date.now();

  try {
    const [healthRes, modelsRes, statusRes] = await Promise.all([
      fetchImpl(`${baseUrl}/health`, {
        method: "GET",
        signal: AbortSignal.timeout(timeoutMs),
      }).catch(() => null),
      fetchImpl(`${baseUrl}/v1/models`, {
        method: "GET",
        signal: AbortSignal.timeout(timeoutMs),
      }).catch(() => null),
      fetchImpl(`${baseUrl}/v1/engine/status`, {
        method: "GET",
        signal: AbortSignal.timeout(timeoutMs),
      }).catch(() => null),
    ]);

    const anyOk = Boolean(healthRes?.ok || modelsRes?.ok || statusRes?.ok);
    if (!anyOk) {
      return {
        online: false,
        baseUrl,
        latencyMs: null,
        loadedModel: null,
        device: null,
        memoryUsedMb: null,
        models: [],
      };
    }

    const latencyMs = Math.max(0, Date.now() - startedAt);
    const modelsJson =
      modelsRes && modelsRes.ok
        ? ((await modelsRes.json().catch(() => null)) as {
            readonly data?: ReadonlyArray<{ readonly id?: string }>;
          } | null)
        : null;
    const statusJson =
      statusRes && statusRes.ok
        ? ((await statusRes.json().catch(() => null)) as {
            readonly loaded_model?: string | null;
            readonly device?: string | null;
            readonly memory_usage?: number | null;
          } | null)
        : null;

    const seen = new Set<string>();
    const models: InstalledLocalAiModel[] = [];
    for (const row of Array.isArray(modelsJson?.data) ? modelsJson.data : []) {
      const id = String(row?.id ?? "").trim();
      if (!isAllowedLocalAiModel(id) || seen.has(id)) continue;
      seen.add(id);
      models.push({ id, source: "hanzo-engine" });
    }

    const rawLoaded = statusJson?.loaded_model ?? models[0]?.id ?? null;
    const loadedModel = isAllowedLocalAiModel(rawLoaded) ? rawLoaded : null;
    if (loadedModel && !seen.has(loadedModel)) {
      models.unshift({ id: loadedModel, source: "hanzo-engine" });
    }

    return {
      online: true,
      baseUrl,
      latencyMs,
      loadedModel,
      device: typeof statusJson?.device === "string" ? statusJson.device : null,
      memoryUsedMb:
        typeof statusJson?.memory_usage === "number" && Number.isFinite(statusJson.memory_usage)
          ? statusJson.memory_usage
          : null,
      models,
    };
  } catch {
    return {
      online: false,
      baseUrl,
      latencyMs: null,
      loadedModel: null,
      device: null,
      memoryUsedMb: null,
      models: [],
    };
  }
}

export async function probeHanzodValidatorStatus(options?: {
  readonly baseUrl?: string;
  readonly stakingPort?: number;
  readonly timeoutMs?: number;
  readonly fetchImpl?: typeof globalThis.fetch;
}): Promise<HanzodValidatorStatus> {
  const fetchImpl = options?.fetchImpl ?? globalThis.fetch;
  const rpcBaseUrl = normalizeBaseUrl(options?.baseUrl, LOCAL_HANZOD_RPC_BASE_URL);
  const stakingPort = options?.stakingPort ?? LOCAL_HANZOD_STAKING_PORT;
  const timeoutMs = options?.timeoutMs ?? 1_500;
  const stakingHost = (() => {
    try {
      return new URL(rpcBaseUrl).hostname;
    } catch {
      return "127.0.0.1";
    }
  })();
  const stakingEndpoint = `${stakingHost}:${stakingPort}`;
  const startedAt = Date.now();

  let healthy: boolean | null = null;
  let healthReachable = false;

  for (const healthPath of ["/v1/health", "/ext/health"]) {
    try {
      const res = await fetchImpl(`${rpcBaseUrl}${healthPath}`, {
        method: "GET",
        signal: AbortSignal.timeout(timeoutMs),
      });
      if (!res.ok) continue;
      healthReachable = true;
      const body = (await res.json().catch(() => null)) as { readonly healthy?: boolean } | null;
      healthy = typeof body?.healthy === "boolean" ? body.healthy : true;
      break;
    } catch {
      // Try fallback health path or EVM RPC endpoint.
    }
  }

  let blockHeight: number | null = null;
  let chainId: number | null = null;
  let rpcReachable = false;

  for (const rpcPath of ["/ext/bc/C/rpc", "/v1/bc/C/rpc", "/v1/chain/hanzo/rpc"]) {
    try {
      const rpcUrl = `${rpcBaseUrl}${rpcPath}`;
      const blockRes = await fetchImpl(rpcUrl, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          jsonrpc: "2.0",
          id: 1,
          method: "eth_blockNumber",
          params: [],
        }),
        signal: AbortSignal.timeout(timeoutMs),
      });
      if (!blockRes.ok) continue;
      const blockJson = (await blockRes.json().catch(() => null)) as {
        readonly result?: unknown;
      } | null;
      const parsedBlock = parseHexQuantity(blockJson?.result);
      if (parsedBlock === null) continue;
      rpcReachable = true;
      blockHeight = parsedBlock;

      const chainRes = await fetchImpl(rpcUrl, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          jsonrpc: "2.0",
          id: 2,
          method: "eth_chainId",
          params: [],
        }),
        signal: AbortSignal.timeout(timeoutMs),
      }).catch(() => null);
      if (chainRes?.ok) {
        const chainJson = (await chainRes.json().catch(() => null)) as {
          readonly result?: unknown;
        } | null;
        chainId = parseHexQuantity(chainJson?.result);
      }
      break;
    } catch {
      // Try next RPC path.
    }
  }

  const online = healthReachable || rpcReachable;
  return {
    online,
    healthy: online ? (healthy ?? true) : null,
    rpcBaseUrl,
    stakingEndpoint,
    chainId,
    networkLabel: formatChainNetworkLabel(chainId),
    blockHeight,
    latencyMs: online ? Math.max(0, Date.now() - startedAt) : null,
  };
}

export async function probeLocalAiStack(options?: {
  readonly ollamaBaseUrl?: string;
  readonly engineBaseUrl?: string;
  readonly hanzodBaseUrl?: string;
  readonly timeoutMs?: number;
  readonly fetchImpl?: typeof globalThis.fetch;
}): Promise<LocalAiStackSnapshot> {
  const [ollama, engine, hanzod] = await Promise.all([
    probeOllamaStatus({
      ...(options?.ollamaBaseUrl ? { baseUrl: options.ollamaBaseUrl } : {}),
      ...(options?.timeoutMs ? { timeoutMs: options.timeoutMs } : {}),
      ...(options?.fetchImpl ? { fetchImpl: options.fetchImpl } : {}),
    }),
    probeHanzoEngineStatus({
      ...(options?.engineBaseUrl ? { baseUrl: options.engineBaseUrl } : {}),
      ...(options?.timeoutMs ? { timeoutMs: options.timeoutMs } : {}),
      ...(options?.fetchImpl ? { fetchImpl: options.fetchImpl } : {}),
    }),
    probeHanzodValidatorStatus({
      ...(options?.hanzodBaseUrl ? { baseUrl: options.hanzodBaseUrl } : {}),
      ...(options?.timeoutMs ? { timeoutMs: options.timeoutMs } : {}),
      ...(options?.fetchImpl ? { fetchImpl: options.fetchImpl } : {}),
    }),
  ]);

  const seen = new Set<string>();
  const installedModels: InstalledLocalAiModel[] = [];
  for (const item of [...engine.models, ...ollama.models]) {
    if (!isAllowedLocalAiModel(item.id) || seen.has(item.id)) continue;
    seen.add(item.id);
    installedModels.push(item);
  }

  return {
    ollama,
    engine,
    hanzod,
    installedModels,
  };
}
