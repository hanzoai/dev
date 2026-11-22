// @effect-diagnostics globalDate:off -- Latency probes measure wall-clock HTTP round-trips outside an Effect runtime.

/** One pluggable local inference endpoint. `baseUrl` is the OpenAI-compatible API root (`<baseUrl>/models`, `<baseUrl>/chat/completions`). */
export type LocalEngineKind = "hanzo-engine" | "ollama" | "lm-studio" | "openai";

export interface LocalEngine {
  readonly name: string;
  readonly kind: LocalEngineKind;
  readonly baseUrl: string;
}

export interface LocalModel {
  readonly id: string;
  /** Zen-family model served by Hanzo Engine. */
  readonly zen: boolean;
}

export interface LocalEngineStatus {
  readonly engine: LocalEngine;
  readonly online: boolean;
  /** The base URL that answered, which may be the fallback. */
  readonly baseUrl: string;
  readonly latencyMs: number | null;
  readonly models: ReadonlyArray<LocalModel>;
}

export type LocalEngineAction = "pull" | null;

export const HANZOD_ENGINE_BASE_URL = "http://127.0.0.1:3690/v1/engine";
export const HANZOD_ENGINE_CATALOG_URL = `${HANZOD_ENGINE_BASE_URL}/catalog`;

/** The picker group every Hanzo Engine model sits under: weights on this machine, nothing sent out. */
export const LOCAL_PRIVATE_GROUP = "Local · Private";
export const LOCAL_HANZOD_RPC_BASE_URL = "http://127.0.0.1:9630";
const LOCAL_HANZOD_STAKING_PORT = 9631;

export const BUILT_IN_LOCAL_ENGINES: ReadonlyArray<LocalEngine> = [
  { name: "Hanzo Engine", kind: "hanzo-engine", baseUrl: HANZOD_ENGINE_BASE_URL },
  { name: "Ollama", kind: "ollama", baseUrl: "http://127.0.0.1:11434/v1" },
  { name: "LM Studio", kind: "lm-studio", baseUrl: "http://127.0.0.1:1234/v1" },
];

const ENGINE_KINDS: ReadonlySet<string> = new Set([
  "hanzo-engine",
  "ollama",
  "lm-studio",
  "openai",
]);

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

export function trimBaseUrl(raw: string): string {
  return raw.trim().replace(/\/+$/u, "");
}

/** Parses the persisted custom-engine list. Invalid rows are dropped, never thrown on. */
export function parseLocalEngines(raw: string | null | undefined): LocalEngine[] {
  if (!raw) return [];
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch {
    return [];
  }
  if (!Array.isArray(parsed)) return [];
  const seen = new Set<string>();
  const engines: LocalEngine[] = [];
  for (const row of parsed) {
    if (typeof row !== "object" || row === null) continue;
    const { name, kind, baseUrl } = row as Record<string, unknown>;
    if (typeof name !== "string" || typeof baseUrl !== "string") continue;
    const url = trimBaseUrl(baseUrl);
    const label = name.trim();
    if (!label || !/^https?:\/\/[^\s/]+/iu.test(url) || seen.has(url)) continue;
    const engineKind = typeof kind === "string" && ENGINE_KINDS.has(kind) ? kind : "openai";
    seen.add(url);
    engines.push({ name: label, kind: engineKind as LocalEngineKind, baseUrl: url });
  }
  return engines;
}

export function isZenModelId(id: string): boolean {
  return /^zen/iu.test(id.trim());
}

/**
 * Normalizes an engine's model listing. Accepts the OpenAI `{data:[{id}]}` shape, Ollama's native
 * `{models:[{name}]}`, and a bare array. Every served model is kept; Hanzo Engine's Zen family sorts first.
 */
export function normalizeLocalModels(body: unknown, kind: LocalEngineKind): LocalModel[] {
  const rows: unknown = Array.isArray(body)
    ? body
    : ((body as { data?: unknown; models?: unknown } | null)?.data ??
      (body as { models?: unknown } | null)?.models);
  if (!Array.isArray(rows)) return [];
  const seen = new Set<string>();
  const models: LocalModel[] = [];
  for (const row of rows) {
    const record = typeof row === "string" ? { id: row } : (row as Record<string, unknown> | null);
    const raw = record?.id ?? record?.name ?? record?.model;
    const id = typeof raw === "string" ? raw.trim() : "";
    if (!id || seen.has(id)) continue;
    seen.add(id);
    models.push({ id, zen: kind === "hanzo-engine" && isZenModelId(id) });
  }
  return [...models].sort((a, b) => Number(b.zen) - Number(a.zen));
}

export type EnginePullState = "idle" | "pulling" | "done" | "failed";

/** One Zen model hanzod can serve, as `/v1/engine/catalog` lists it. */
export interface EngineCatalogModel {
  readonly id: string;
  readonly name: string;
  readonly bytes: number;
  readonly memoryGb: number;
  readonly installed: boolean;
  readonly selected: boolean;
  readonly pull: {
    readonly state: EnginePullState;
    readonly done: number;
    readonly total: number;
    readonly error: string | null;
  };
}

export interface EngineCatalog {
  readonly memoryBytes: number;
  /** The model this machine's memory suits best. */
  readonly recommended: string;
  readonly models: ReadonlyArray<EngineCatalogModel>;
}

/** Parses the catalog body; null when it is not one. */
export function parseEngineCatalog(body: unknown): EngineCatalog | null {
  const root = body as Record<string, unknown> | null;
  if (typeof root !== "object" || root === null || !Array.isArray(root.models)) return null;
  const models: EngineCatalogModel[] = [];
  for (const row of root.models as ReadonlyArray<Record<string, unknown> | null>) {
    if (typeof row?.id !== "string" || typeof row.name !== "string") return null;
    const pull = (row.pull ?? {}) as Record<string, unknown>;
    const state = ["pulling", "done", "failed"].includes(String(pull.state))
      ? (pull.state as EnginePullState)
      : "idle";
    models.push({
      id: row.id,
      name: row.name,
      bytes: Number(row.bytes) || 0,
      memoryGb: Number(row.memory_gb) || 0,
      installed: row.installed === true,
      selected: row.selected === true,
      pull: {
        state,
        done: Number(pull.done) || 0,
        total: Number(pull.total) || 0,
        error: typeof pull.error === "string" ? pull.error : null,
      },
    });
  }
  return {
    memoryBytes: Number(root.memory_bytes) || 0,
    recommended: typeof root.recommended === "string" ? root.recommended : "",
    models,
  };
}

export async function fetchEngineCatalog(
  fetchImpl: typeof globalThis.fetch = globalThis.fetch,
): Promise<EngineCatalog | null> {
  try {
    const response = await fetchImpl(HANZOD_ENGINE_CATALOG_URL, {
      signal: AbortSignal.timeout(3_000),
    });
    return response.ok ? parseEngineCatalog(await response.json().catch(() => null)) : null;
  } catch {
    return null;
  }
}

/** `pull` downloads a model into the shared cache; `select` serves it. Resolves with an error message, or null. */
export async function engineCatalogAction(
  action: "pull" | "select",
  id: string,
  fetchImpl: typeof globalThis.fetch = globalThis.fetch,
): Promise<string | null> {
  try {
    const response = await fetchImpl(`${HANZOD_ENGINE_CATALOG_URL}/${action}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ id }),
    });
    if (response.ok) return null;
    const body = (await response.json().catch(() => null)) as { error?: unknown } | null;
    return typeof body?.error === "string" ? body.error : `HTTP ${response.status}`;
  } catch (error) {
    return error instanceof Error ? error.message : String(error);
  }
}

export function localEngineAction(kind: LocalEngineKind): LocalEngineAction {
  switch (kind) {
    case "ollama":
      return "pull";
    default:
      return null;
  }
}

/** Native (non-OpenAI) endpoints live at the server origin, not under the `/v1` root. */
export function localEngineNativeUrl(engine: Pick<LocalEngine, "baseUrl">, path: string): string {
  return `${new URL(engine.baseUrl).origin}${path}`;
}

export async function probeLocalEngine(
  engine: LocalEngine,
  options?: { readonly timeoutMs?: number; readonly fetchImpl?: typeof globalThis.fetch },
): Promise<LocalEngineStatus> {
  const fetchImpl = options?.fetchImpl ?? globalThis.fetch;
  // Hanzo Engine starts on its first request and compiles its Metal shaders: give it a cold start.
  const timeoutMs = options?.timeoutMs ?? (engine.kind === "hanzo-engine" ? 45_000 : 1_500);
  const baseUrl = engine.baseUrl;
  const startedAt = Date.now();
  try {
    const response = await fetchImpl(`${baseUrl}/models`, {
      method: "GET",
      signal: AbortSignal.timeout(timeoutMs),
    });
    if (response.ok) {
      const latencyMs = Math.max(0, Date.now() - startedAt);
      const body = await response.json().catch(() => null);
      return {
        engine,
        online: true,
        baseUrl,
        latencyMs,
        models: normalizeLocalModels(body, engine.kind),
      };
    }
  } catch {
    // Unreachable or timed out: report offline.
  }
  return { engine, online: false, baseUrl: engine.baseUrl, latencyMs: null, models: [] };
}

/** One bounded round of parallel probes. Callers invoke it on open and on explicit refresh, never on a timer. */
export function probeLocalEngines(
  engines: ReadonlyArray<LocalEngine>,
  options?: { readonly timeoutMs?: number; readonly fetchImpl?: typeof globalThis.fetch },
): Promise<LocalEngineStatus[]> {
  return Promise.all(engines.map((engine) => probeLocalEngine(engine, options)));
}

/**
 * The model-picker entry for an engine: a Hanzo agent instance whose model provider is the engine endpoint.
 * Models come from the engine's own listing.
 */
export function localEngineInstanceId(engine: Pick<LocalEngine, "baseUrl">): string {
  return `local_${new URL(engine.baseUrl).host.replace(/[^a-z0-9]+/giu, "_")}`;
}

/** Whether a provider instance is a local engine added through Local AI. */
export const isLocalEngineInstanceId = (instanceId: string): boolean =>
  instanceId.startsWith("local_");

export function localEngineProviderInstance(status: LocalEngineStatus): {
  readonly displayName: string;
  readonly enabled: true;
  readonly config: {
    readonly enabled: true;
    readonly launchArgs: string;
    readonly customModels: ReadonlyArray<{ readonly slug: string; readonly name?: string }>;
  };
} {
  const id = localEngineInstanceId(status.engine);
  const provider = `model_providers.${id}`;
  const displayName =
    status.engine.kind === "hanzo-engine" ? LOCAL_PRIVATE_GROUP : status.engine.name;
  const launchArgs = [
    `-c model_provider="${id}"`,
    `-c ${provider}.name="${status.engine.name.replace(/["\\]/gu, "")}"`,
    `-c ${provider}.base_url="${status.baseUrl}"`,
    `-c ${provider}.wire_api="chat"`,
  ].join(" ");
  return {
    displayName,
    enabled: true,
    config: {
      enabled: true,
      launchArgs,
      customModels: status.models.map((model) =>
        model.zen ? { slug: model.id, name: `${model.id} (Zen)` } : { slug: model.id },
      ),
    },
  };
}

function formatChainNetworkLabel(chainId: number | null): string | null {
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

export async function probeHanzodValidatorStatus(options?: {
  readonly baseUrl?: string;
  readonly stakingPort?: number;
  readonly timeoutMs?: number;
  readonly fetchImpl?: typeof globalThis.fetch;
}): Promise<HanzodValidatorStatus> {
  const fetchImpl = options?.fetchImpl ?? globalThis.fetch;
  const rpcBaseUrl = trimBaseUrl(options?.baseUrl ?? "") || LOCAL_HANZOD_RPC_BASE_URL;
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
