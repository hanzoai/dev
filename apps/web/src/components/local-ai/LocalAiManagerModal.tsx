import type { DesktopDiscoveredLanEngine } from "@hanzodev/contracts";
import {
  isAllowedLocalAiModel,
  LOCAL_HANZOD_RPC_BASE_URL,
  LOCAL_HANZO_ENGINE_BASE_URL,
  LOCAL_OLLAMA_BASE_URL,
  probeLocalAiStack,
  resolveOllamaPullTarget,
  ZEN_LOCAL_AI_CATALOG,
  type LocalAiStackSnapshot,
} from "@hanzodev/shared/localAi";
import { T, useGT } from "gt-react";
import {
  CheckIcon,
  CpuIcon,
  DownloadIcon,
  NetworkIcon,
  RefreshCwIcon,
  ServerIcon,
  Trash2Icon,
  XIcon,
} from "lucide-react";
import { useEffect, useState } from "react";

import {
  closeLocalAiManager,
  openLocalAiManager,
  readCustomHanzodUrl,
  readCustomLocalEngineUrl,
  readSelectedLocalAiModel,
  setLocalAiManagerTab,
  useLocalAiModalState,
  writeCustomHanzodUrl,
  writeCustomLocalEngineUrl,
  writeSelectedLocalAiModel,
} from "../../localAi/localAiState";
import { Button } from "../ui/button";
import { SettingsRow, SettingsSection } from "../settings/settingsLayout";
import { useSearchableSetting } from "../settings/settingsSearch";

export function LocalAiSettingsSection() {
  const gt = useGT();
  const searchableSetting = useSearchableSetting();
  const [snapshot, setSnapshot] = useState<LocalAiStackSnapshot | null>(null);

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- probes local Ollama, Hanzo Engine, and hanzod health when Settings mounts
  useEffect(() => {
    let cancelled = false;
    void probeLocalAiStack({
      engineBaseUrl: readCustomLocalEngineUrl() || LOCAL_HANZO_ENGINE_BASE_URL,
      hanzodBaseUrl: readCustomHanzodUrl() || LOCAL_HANZOD_RPC_BASE_URL,
    }).then((next) => {
      if (!cancelled) setSnapshot(next);
    });
    return () => {
      cancelled = true;
    };
  }, []);

  const ollamaOnline = snapshot?.ollama.online ?? false;
  const engineOnline = snapshot?.engine.online ?? false;
  const hanzodOnline = snapshot?.hanzod.online ?? false;
  const installedCount = snapshot?.installedModels.length ?? 0;

  return (
    <SettingsSection id="local-ai" title={gt("Local AI, Node & Engine")}>
      <SettingsRow
        {...searchableSetting("local-ai-models")}
        devicePreference
        title={gt("Zen local models")}
        description={gt(
          "Manage local Zen weights on Ollama (:11434) and Embedded Hanzo Engine (:36900).",
        )}
        status={
          snapshot === null
            ? gt("Checking local runtimes...")
            : gt("Ollama: {ollama} · Engine: {engine} · {count} models ready", {
                ollama: ollamaOnline ? gt("online") : gt("offline"),
                engine: engineOnline ? gt("online") : gt("standby"),
                count: installedCount,
              })
        }
        control={
          <Button variant="outline" size="sm" onClick={() => openLocalAiManager("models")}>
            <T>Manage models</T>
          </Button>
        }
      />
      <SettingsRow
        {...searchableSetting("local-ai-validator")}
        devicePreference
        title={gt("Hanzo validator node (hanzod)")}
        description={gt(
          "Monitor local hanzod RPC (:9630), EVM block height, and P2P staking mesh (:9631).",
        )}
        status={
          snapshot === null
            ? gt("Checking local node...")
            : hanzodOnline
              ? gt("Online · {network} · Block #{block}", {
                  network: snapshot.hanzod.networkLabel ?? "Hanzo Chain",
                  block: snapshot.hanzod.blockHeight ?? 0,
                })
              : gt("Offline on 127.0.0.1:9630")
        }
        control={
          <Button variant="outline" size="sm" onClick={() => openLocalAiManager("node")}>
            <T>Node status</T>
          </Button>
        }
      />
      <SettingsRow
        {...searchableSetting("local-ai-engine")}
        devicePreference
        title={gt("Embedded engine & LAN discovery")}
        description={gt(
          "Load HuggingFace or GGUF Zen weights on :36900 and discover inference nodes on LAN.",
        )}
        control={
          <Button variant="outline" size="sm" onClick={() => openLocalAiManager("engine")}>
            <T>Configure engine</T>
          </Button>
        }
      />
    </SettingsSection>
  );
}

async function streamOllamaPull(
  modelId: string,
  onProgress: (update: {
    readonly status?: string | undefined;
    readonly percent?: number | undefined;
  }) => void,
): Promise<
  { readonly ok: true } | { readonly ok: false; readonly status?: number; readonly error?: string }
> {
  try {
    const target = resolveOllamaPullTarget(modelId);
    const response = await fetch(`${LOCAL_OLLAMA_BASE_URL}/api/pull`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ model: target, stream: true }),
    });
    if (!response.ok || !response.body) {
      return { ok: false, status: response.status };
    }

    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let buffer = "";

    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      buffer += decoder.decode(value, { stream: true });
      const lines = buffer.split("\n");
      buffer = lines.pop() ?? "";
      for (const line of lines) {
        if (!line.trim()) continue;
        const payload = JSON.parse(line) as {
          readonly error?: string;
          readonly status?: string;
          readonly completed?: number;
          readonly total?: number;
        };
        if (payload.error) {
          return { ok: false, error: payload.error };
        }
        const percent =
          typeof payload.completed === "number" &&
          typeof payload.total === "number" &&
          payload.total > 0
            ? Math.min(99, Math.round((payload.completed / payload.total) * 100))
            : undefined;
        onProgress({ status: payload.status, percent });
      }
    }
    return { ok: true };
  } catch (error) {
    return { ok: false, error: error instanceof Error ? error.message : String(error) };
  }
}

async function requestEngineModelLoad(
  trimmed: string,
): Promise<{ readonly ok: true } | { readonly ok: false; readonly error?: string | undefined }> {
  try {
    const base = readCustomLocalEngineUrl() || LOCAL_HANZO_ENGINE_BASE_URL;
    const response = await fetch(`${base}/v1/engine/load`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        model_id: trimmed,
        format: trimmed.endsWith(".gguf") ? "gguf" : "auto",
      }),
    });
    if (!response.ok) {
      const text = await response.text();
      return { ok: false, error: text || `HTTP ${response.status}` };
    }
    return { ok: true };
  } catch (error) {
    return { ok: false, error: error instanceof Error ? error.message : undefined };
  }
}

async function discoverLanInferenceNodes(): Promise<readonly DesktopDiscoveredLanEngine[]> {
  try {
    if (window.desktopBridge?.discoverLanEngines) {
      return await window.desktopBridge.discoverLanEngines();
    }
    const local = await probeLocalAiStack();
    const fallback: DesktopDiscoveredLanEngine[] = [];
    if (local.engine.online) {
      fallback.push({
        name: "Localhost · Hanzo Engine",
        host: "127.0.0.1",
        port: 36900,
        baseUrl: local.engine.baseUrl,
        kind: "hanzo-engine",
        models: local.engine.models.map((m) => m.id),
        latencyMs: local.engine.latencyMs,
      });
    }
    if (local.ollama.online) {
      fallback.push({
        name: "Localhost · Ollama",
        host: "127.0.0.1",
        port: 11434,
        baseUrl: local.ollama.baseUrl,
        kind: "ollama",
        models: local.ollama.models.map((m) => m.id),
        latencyMs: local.ollama.latencyMs,
      });
    }
    return fallback;
  } catch {
    return [];
  }
}

export function LocalAiManagerModal() {
  const modal = useLocalAiModalState();
  if (!modal.open) return null;
  return (
    <LocalAiManagerModalBody
      key={modal.filterQuery ?? ""}
      initialFilterQuery={modal.filterQuery ?? ""}
      tab={modal.tab}
    />
  );
}

function LocalAiManagerModalBody({
  initialFilterQuery,
  tab,
}: {
  readonly initialFilterQuery: string;
  readonly tab: ReturnType<typeof useLocalAiModalState>["tab"];
}) {
  const gt = useGT();
  const [snapshot, setSnapshot] = useState<LocalAiStackSnapshot | null>(null);
  const [refreshing, setRefreshing] = useState(false);
  const [search, setSearch] = useState(initialFilterQuery);
  const [selectedModel, setSelectedModel] = useState(readSelectedLocalAiModel);

  const [pullingId, setPullingId] = useState<string | null>(null);
  const [pullPercent, setPullPercent] = useState(0);
  const [pullStatus, setPullStatus] = useState<string | null>(null);
  const [pullError, setPullError] = useState<string | null>(null);

  const [engineUrlInput, setEngineUrlInput] = useState(readCustomLocalEngineUrl);
  const [hanzodUrlInput, setHanzodUrlInput] = useState(readCustomHanzodUrl);
  const [hfRepoInput, setHfRepoInput] = useState("Qwen/Qwen3-8B");
  const [engineLoadMessage, setEngineLoadMessage] = useState<string | null>(null);
  const [loadingEngineModel, setLoadingEngineModel] = useState(false);

  const [lanEngines, setLanEngines] = useState<readonly DesktopDiscoveredLanEngine[]>([]);
  const [scanningLan, setScanningLan] = useState(false);

  const refreshStack = async () => {
    setRefreshing(true);
    const next = await probeLocalAiStack({
      engineBaseUrl: readCustomLocalEngineUrl() || LOCAL_HANZO_ENGINE_BASE_URL,
      hanzodBaseUrl: readCustomHanzodUrl() || LOCAL_HANZOD_RPC_BASE_URL,
    }).catch(() => null);
    if (next) {
      setSnapshot(next);
    }
    setRefreshing(false);
  };

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- probes local AI runtimes on dialog mount and handles Escape key dismissal
  useEffect(() => {
    let cancelled = false;
    void probeLocalAiStack({
      engineBaseUrl: readCustomLocalEngineUrl() || LOCAL_HANZO_ENGINE_BASE_URL,
      hanzodBaseUrl: readCustomHanzodUrl() || LOCAL_HANZOD_RPC_BASE_URL,
    }).then((next) => {
      if (!cancelled) {
        setSnapshot(next);
      }
    });
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        event.preventDefault();
        closeLocalAiManager();
      }
    };
    window.addEventListener("keydown", onKeyDown);
    return () => {
      cancelled = true;
      window.removeEventListener("keydown", onKeyDown);
    };
  }, []);

  const installedIds = new Set((snapshot?.installedModels ?? []).map((m) => m.id));
  const normalizedQuery = search.trim().toLowerCase();
  const visibleCatalog = ZEN_LOCAL_AI_CATALOG.filter((entry) => {
    if (!isAllowedLocalAiModel(entry.id)) return false;
    if (!normalizedQuery) return true;
    return (
      entry.title.toLowerCase().includes(normalizedQuery) ||
      entry.id.toLowerCase().includes(normalizedQuery) ||
      entry.ollamaTag.toLowerCase().includes(normalizedQuery) ||
      entry.description.toLowerCase().includes(normalizedQuery)
    );
  });

  const handleSelectModel = (modelId: string) => {
    if (!isAllowedLocalAiModel(modelId)) return;
    writeSelectedLocalAiModel(modelId);
    setSelectedModel(modelId);
  };

  const handlePullModel = async (modelId: string) => {
    if (pullingId !== null) return;
    setPullError(null);
    setPullingId(modelId);
    setPullPercent(2);
    setPullStatus(gt("Connecting to Ollama on 127.0.0.1:11434..."));

    const result = await streamOllamaPull(modelId, (update) => {
      if (update.status) setPullStatus(update.status);
      if (update.percent !== undefined) setPullPercent(update.percent);
    });
    if (result.ok) {
      setPullPercent(100);
      setPullStatus(gt("Installed {model}", { model: modelId }));
      handleSelectModel(modelId);
      await refreshStack();
    } else {
      setPullError(
        result.status !== undefined
          ? gt("Local Ollama runtime returned HTTP {status}. Start Ollama on :11434 and retry.", {
              status: result.status,
            })
          : (result.error ?? gt("Failed to pull model.")),
      );
    }
    setPullingId(null);
  };

  const handleDeleteModel = async (modelId: string) => {
    const target = resolveOllamaPullTarget(modelId);
    await fetch(`${LOCAL_OLLAMA_BASE_URL}/api/delete`, {
      method: "DELETE",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ model: target }),
    }).catch(() => undefined);
    await refreshStack();
  };

  const handleLoadIntoEngine = async () => {
    const trimmed = hfRepoInput.trim();
    if (!trimmed) return;
    if (!isAllowedLocalAiModel(trimmed)) {
      setEngineLoadMessage(gt("Legacy weights are blocked; use Zen weights."));
      return;
    }
    setLoadingEngineModel(true);
    setEngineLoadMessage(gt("Loading {model} on :36900...", { model: trimmed }));
    const result = await requestEngineModelLoad(trimmed);
    if (result.ok) {
      setEngineLoadMessage(gt("Loaded {model} on Hanzo Engine.", { model: trimmed }));
      await refreshStack();
    } else {
      setEngineLoadMessage(result.error ?? gt("Embedded Hanzo Engine (:36900) is not reachable."));
    }
    setLoadingEngineModel(false);
  };

  const handleScanLan = async () => {
    setScanningLan(true);
    const discovered = await discoverLanInferenceNodes();
    setLanEngines(discovered);
    setScanningLan(false);
  };

  return (
    <div
      role="dialog"
      aria-modal="true"
      aria-label={gt("Local AI, Node & Engine")}
      data-testid="local-ai-manager-modal"
      className="fixed inset-0 z-50 flex items-center justify-center bg-black/60 p-4 backdrop-blur-xs"
      onClick={(event) => {
        if (event.target === event.currentTarget) closeLocalAiManager();
      }}
    >
      <div className="flex max-h-[86vh] w-full max-w-3xl flex-col overflow-hidden rounded-2xl border border-border bg-background text-foreground shadow-2xl">
        <div className="flex items-center justify-between border-b border-border px-5 py-3.5">
          <div className="flex items-center gap-2.5">
            <div className="flex size-8 items-center justify-center rounded-lg bg-muted">
              <CpuIcon className="size-4 text-foreground" />
            </div>
            <div>
              <div className="text-sm font-semibold">
                <T>Local AI, Validator Node &amp; Engine</T>
              </div>
              <div className="text-xs text-muted-foreground">
                <T>Zen weights · Ollama (:11434) · Engine (:36900) · hanzod (:9630)</T>
              </div>
            </div>
          </div>
          <div className="flex items-center gap-2">
            <Button
              variant="outline"
              size="xs"
              disabled={refreshing}
              onClick={() => void refreshStack()}
            >
              <RefreshCwIcon className="size-3.5" />
              <T>Refresh</T>
            </Button>
            <button
              type="button"
              onClick={closeLocalAiManager}
              aria-label={gt("Close")}
              className="rounded-lg p-1.5 text-muted-foreground hover:bg-muted hover:text-foreground"
            >
              <XIcon className="size-4" />
            </button>
          </div>
        </div>

        <div className="flex flex-wrap items-center gap-2 border-b border-border bg-muted/20 px-5 py-2 text-xs">
          <span className="inline-flex items-center gap-1.5 rounded-full border border-border bg-background px-2.5 py-0.5">
            <span
              className={`size-2 rounded-full ${
                snapshot?.ollama.online ? "bg-success" : "bg-muted-foreground/40"
              }`}
            />
            <span>
              <code>Ollama (:11434)</code>:{" "}
              <strong>{snapshot?.ollama.online ? gt("online") : gt("offline")}</strong>
            </span>
          </span>
          <span className="inline-flex items-center gap-1.5 rounded-full border border-border bg-background px-2.5 py-0.5">
            <span
              className={`size-2 rounded-full ${
                snapshot?.engine.online ? "bg-success" : "bg-muted-foreground/40"
              }`}
            />
            <span>
              <code>Engine (:36900)</code>:{" "}
              <strong>{snapshot?.engine.online ? gt("online") : gt("standby")}</strong>
            </span>
          </span>
          <span className="inline-flex items-center gap-1.5 rounded-full border border-border bg-background px-2.5 py-0.5">
            <span
              className={`size-2 rounded-full ${
                snapshot?.hanzod.online ? "bg-success" : "bg-muted-foreground/40"
              }`}
            />
            <span>
              <code>hanzod (:9630)</code>:{" "}
              <strong>
                {snapshot?.hanzod.online ? `#${snapshot.hanzod.blockHeight ?? 0}` : gt("offline")}
              </strong>
            </span>
          </span>
        </div>

        <div className="flex border-b border-border px-5">
          {(
            [
              { id: "models", label: gt("Zen Models") },
              { id: "node", label: gt("Validator Node (:9630)") },
              { id: "engine", label: gt("Embedded Engine & LAN") },
            ] as const
          ).map((item) => (
            <button
              key={item.id}
              type="button"
              onClick={() => setLocalAiManagerTab(item.id)}
              className={`border-b-2 px-3.5 py-2.5 text-xs font-medium transition-colors ${
                tab === item.id
                  ? "border-foreground text-foreground"
                  : "border-transparent text-muted-foreground hover:text-foreground"
              }`}
            >
              {item.label}
            </button>
          ))}
        </div>

        <div className="flex-1 overflow-y-auto p-5">
          {tab === "models" ? (
            <div className="space-y-4">
              <div className="flex items-center gap-2">
                <input
                  type="text"
                  value={search}
                  onChange={(event) => setSearch(event.currentTarget.value)}
                  placeholder={gt("Filter Zen models...")}
                  aria-label={gt("Filter local models")}
                  className="w-full rounded-lg border border-border bg-background px-3 py-2 text-xs focus:outline-hidden"
                />
              </div>

              {pullError ? (
                <div className="rounded-lg border border-destructive/40 bg-destructive/10 px-3 py-2 text-xs text-destructive">
                  {pullError}
                </div>
              ) : null}

              {pullingId !== null ? (
                <div className="space-y-1.5 rounded-xl border border-border bg-muted/30 p-3 text-xs">
                  <div className="flex items-center justify-between font-medium">
                    <span>{pullStatus ?? pullingId}</span>
                    <span>{pullPercent}%</span>
                  </div>
                  <div className="h-1.5 w-full overflow-hidden rounded-full bg-muted">
                    <div
                      className="h-full bg-foreground transition-all"
                      style={{ width: `${pullPercent}%` }}
                    />
                  </div>
                </div>
              ) : null}

              <div className="divide-y divide-border rounded-xl border border-border">
                {visibleCatalog.map((model) => {
                  const isInstalled =
                    installedIds.has(model.id) || installedIds.has(model.ollamaTag);
                  const isSelected = selectedModel === model.id;
                  return (
                    <div
                      key={model.id}
                      className="flex flex-col gap-2.5 p-3.5 sm:flex-row sm:items-center sm:justify-between"
                    >
                      <div className="min-w-0 flex-1 space-y-1">
                        <div className="flex flex-wrap items-center gap-1.5">
                          <span className="text-sm font-semibold">{model.title}</span>
                          <span className="rounded bg-muted px-1.5 py-0.5 font-mono text-2xs text-muted-foreground">
                            {model.ollamaTag}
                          </span>
                          <span className="rounded bg-muted px-1.5 py-0.5 text-2xs text-muted-foreground">
                            {model.sizeLabel}
                          </span>
                          {model.recommended ? (
                            <span className="rounded-full bg-success/15 px-2 py-0.5 text-3xs font-semibold text-success-foreground">
                              <T>Recommended</T>
                            </span>
                          ) : null}
                          {isInstalled ? (
                            <span className="inline-flex items-center gap-1 rounded-full bg-success/15 px-2 py-0.5 text-3xs font-medium text-success-foreground">
                              <CheckIcon className="size-2.5" />
                              <T>Installed</T>
                            </span>
                          ) : null}
                        </div>
                        <p className="text-xs text-muted-foreground">{model.description}</p>
                      </div>
                      <div className="flex shrink-0 items-center gap-2">
                        {isInstalled ? (
                          <>
                            <Button
                              variant={isSelected ? "default" : "outline"}
                              size="xs"
                              onClick={() => handleSelectModel(model.id)}
                            >
                              {isSelected ? <T>Active default</T> : <T>Use model</T>}
                            </Button>
                            <button
                              type="button"
                              aria-label={gt("Remove {model}", { model: model.title })}
                              onClick={() => void handleDeleteModel(model.id)}
                              className="rounded-md p-1.5 text-muted-foreground hover:bg-destructive/10 hover:text-destructive"
                            >
                              <Trash2Icon className="size-3.5" />
                            </button>
                          </>
                        ) : (
                          <Button
                            variant="outline"
                            size="xs"
                            disabled={pullingId !== null}
                            onClick={() => void handlePullModel(model.id)}
                          >
                            <DownloadIcon className="size-3.5" />
                            <T>Pull Zen model</T>
                          </Button>
                        )}
                      </div>
                    </div>
                  );
                })}
              </div>
            </div>
          ) : tab === "node" ? (
            <div className="space-y-4">
              <div className="grid grid-cols-1 gap-3 sm:grid-cols-3">
                <div className="rounded-xl border border-border bg-muted/20 p-3.5">
                  <div className="text-2xs font-medium text-muted-foreground uppercase">
                    <T>Validator Status</T>
                  </div>
                  <div className="mt-1 flex items-center gap-2 text-base font-semibold">
                    <ServerIcon className="size-4" />
                    <span>{snapshot?.hanzod.online ? gt("Online & Synced") : gt("Offline")}</span>
                  </div>
                  <div className="mt-1 text-xs text-muted-foreground">
                    {snapshot?.hanzod.networkLabel ?? gt("Hanzo Mainnet (36963)")}
                  </div>
                </div>
                <div className="rounded-xl border border-border bg-muted/20 p-3.5">
                  <div className="text-2xs font-medium text-muted-foreground uppercase">
                    <T>Block Height</T>
                  </div>
                  <div className="mt-1 font-mono text-base font-semibold">
                    {snapshot?.hanzod.blockHeight !== null &&
                    snapshot?.hanzod.blockHeight !== undefined
                      ? `#${snapshot.hanzod.blockHeight.toLocaleString()}`
                      : "N/A"}
                  </div>
                  <div className="mt-1 text-xs text-muted-foreground">
                    {snapshot?.hanzod.latencyMs !== null && snapshot?.hanzod.latencyMs !== undefined
                      ? gt("{latency} ms RPC", { latency: snapshot.hanzod.latencyMs })
                      : gt("RPC 127.0.0.1:9630")}
                  </div>
                </div>
                <div className="rounded-xl border border-border bg-muted/20 p-3.5">
                  <div className="text-2xs font-medium text-muted-foreground uppercase">
                    <T>P2P Staking Mesh</T>
                  </div>
                  <div className="mt-1 font-mono text-base font-semibold">
                    {snapshot?.hanzod.stakingEndpoint ?? "127.0.0.1:9631"}
                  </div>
                  <div className="mt-1 text-xs text-muted-foreground">
                    <T>Health: /v1/health &amp; /ext/bc/C/rpc</T>
                  </div>
                </div>
              </div>

              <div className="space-y-2 rounded-xl border border-border p-4">
                <div className="text-xs font-semibold">
                  <T>Custom hanzod RPC Endpoint</T>
                </div>
                <div className="flex gap-2">
                  <input
                    type="text"
                    value={hanzodUrlInput}
                    onChange={(event) => setHanzodUrlInput(event.currentTarget.value)}
                    placeholder={LOCAL_HANZOD_RPC_BASE_URL}
                    aria-label={gt("Custom hanzod RPC URL")}
                    className="flex-1 rounded-lg border border-border bg-background px-3 py-1.5 font-mono text-xs focus:outline-hidden"
                  />
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={() => {
                      writeCustomHanzodUrl(hanzodUrlInput);
                      void refreshStack();
                    }}
                  >
                    <T>Save &amp; Probe</T>
                  </Button>
                </div>
              </div>
            </div>
          ) : (
            <div className="space-y-4">
              <div className="grid grid-cols-1 gap-3 sm:grid-cols-3">
                <div className="rounded-xl border border-border bg-muted/20 p-3.5">
                  <div className="text-2xs font-medium text-muted-foreground uppercase">
                    <T>Embedded Engine</T>
                  </div>
                  <div className="mt-1 text-base font-semibold">
                    {snapshot?.engine.online ? gt("Listening (:36900)") : gt("Standby")}
                  </div>
                  <div className="mt-1 truncate text-xs text-muted-foreground">
                    {snapshot?.engine.loadedModel ?? gt("No model loaded")}
                  </div>
                </div>
                <div className="rounded-xl border border-border bg-muted/20 p-3.5">
                  <div className="text-2xs font-medium text-muted-foreground uppercase">
                    <T>Accelerator</T>
                  </div>
                  <div className="mt-1 text-base font-semibold">
                    {snapshot?.engine.device ?? gt("Metal / CUDA")}
                  </div>
                  <div className="mt-1 text-xs text-muted-foreground">
                    {snapshot?.engine.memoryUsedMb
                      ? gt("{memory} MB VRAM", { memory: snapshot.engine.memoryUsedMb })
                      : gt("Zero-copy inference")}
                  </div>
                </div>
                <div className="rounded-xl border border-border bg-muted/20 p-3.5">
                  <div className="text-2xs font-medium text-muted-foreground uppercase">
                    <T>OpenAI Endpoint</T>
                  </div>
                  <div className="mt-1 truncate font-mono text-xs font-semibold">
                    {(readCustomLocalEngineUrl() || LOCAL_HANZO_ENGINE_BASE_URL) + "/v1"}
                  </div>
                  <div className="mt-1 text-xs text-muted-foreground">
                    <T>D-LSA / N-LSA ready</T>
                  </div>
                </div>
              </div>

              <div className="space-y-2 rounded-xl border border-border p-4">
                <div className="text-xs font-semibold">
                  <T>Load HuggingFace or GGUF Zen weights into :36900</T>
                </div>
                <div className="flex gap-2">
                  <input
                    type="text"
                    value={hfRepoInput}
                    onChange={(event) => setHfRepoInput(event.currentTarget.value)}
                    placeholder={gt("Qwen/Qwen3-8B")}
                    aria-label={gt("HuggingFace repo or GGUF path")}
                    className="flex-1 rounded-lg border border-border bg-background px-3 py-1.5 font-mono text-xs focus:outline-hidden"
                  />
                  <Button
                    variant="outline"
                    size="sm"
                    disabled={loadingEngineModel}
                    onClick={() => void handleLoadIntoEngine()}
                  >
                    <T>Load Model</T>
                  </Button>
                </div>
                {engineLoadMessage ? (
                  <p className="text-xs text-muted-foreground">{engineLoadMessage}</p>
                ) : null}
              </div>

              <div className="space-y-2 rounded-xl border border-border p-4">
                <div className="text-xs font-semibold">
                  <T>Custom Engine Base URL</T>
                </div>
                <div className="flex gap-2">
                  <input
                    type="text"
                    value={engineUrlInput}
                    onChange={(event) => setEngineUrlInput(event.currentTarget.value)}
                    placeholder={LOCAL_HANZO_ENGINE_BASE_URL}
                    aria-label={gt("Custom engine URL")}
                    className="flex-1 rounded-lg border border-border bg-background px-3 py-1.5 font-mono text-xs focus:outline-hidden"
                  />
                  <Button
                    variant="outline"
                    size="sm"
                    onClick={() => {
                      writeCustomLocalEngineUrl(engineUrlInput);
                      void refreshStack();
                    }}
                  >
                    <T>Save</T>
                  </Button>
                </div>
              </div>

              <div className="space-y-2.5 rounded-xl border border-border p-4">
                <div className="flex items-center justify-between">
                  <div>
                    <div className="text-xs font-semibold">
                      <T>LAN Inference Discovery</T>
                    </div>
                    <div className="text-xs text-muted-foreground">
                      <T>
                        Scan local network interfaces for Hanzo Engine (:36900) and Ollama (:11434).
                      </T>
                    </div>
                  </div>
                  <Button
                    variant="outline"
                    size="xs"
                    disabled={scanningLan}
                    onClick={() => void handleScanLan()}
                  >
                    <NetworkIcon className="size-3.5" />
                    {scanningLan ? <T>Scanning...</T> : <T>Scan LAN</T>}
                  </Button>
                </div>
                {lanEngines.length > 0 ? (
                  <div className="divide-y divide-border rounded-lg border border-border">
                    {lanEngines.map((item) => (
                      <div
                        key={`${item.host}:${item.port}`}
                        className="flex items-center justify-between gap-2 px-3 py-2 text-xs"
                      >
                        <div className="min-w-0 flex-1">
                          <div className="truncate font-medium">{item.name}</div>
                          <div className="truncate font-mono text-2xs text-muted-foreground">
                            {item.baseUrl} · {item.models.length} models
                          </div>
                        </div>
                        <Button
                          variant="ghost"
                          size="xs"
                          onClick={() => {
                            setEngineUrlInput(item.baseUrl);
                            writeCustomLocalEngineUrl(item.baseUrl);
                            void refreshStack();
                          }}
                        >
                          <T>Use</T>
                        </Button>
                      </div>
                    ))}
                  </div>
                ) : null}
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
