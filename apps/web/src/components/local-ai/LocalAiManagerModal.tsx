import type { DesktopDiscoveredLanEngine, EnvironmentId } from "@hanzodev/contracts";
import {
  HANZO_DRIVER_KIND,
  ProviderInstanceId,
  type ProviderInstanceConfig,
} from "@hanzodev/contracts";
import {
  BUILT_IN_LOCAL_ENGINES,
  engineCatalogAction,
  fetchEngineCatalog,
  localEngineAction,
  localEngineInstanceId,
  localEngineNativeUrl,
  localEngineProviderInstance,
  LOCAL_HANZOD_RPC_BASE_URL,
  probeHanzodValidatorStatus,
  probeLocalEngines,
  trimBaseUrl,
  type EngineCatalog,
  type HanzodValidatorStatus,
  type LocalEngine,
  type LocalEngineStatus,
} from "@hanzodev/shared/localAi";
import { T, useGT } from "gt-react";
import {
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
  useEnvironmentSettings,
  usePersistEnvironmentProviderInstanceMutation,
} from "../../hooks/useSettings";
import {
  closeLocalAiManager,
  openLocalAiManager,
  readCustomHanzodUrl,
  readCustomLocalEngines,
  setLocalAiManagerTab,
  useLocalAiModalState,
  writeCustomHanzodUrl,
  writeCustomLocalEngines,
} from "../../localAi/localAiState";
import { usePrimaryEnvironmentId } from "../../state/environments";
import { Button } from "../ui/button";
import { SettingsRow, SettingsSection } from "../settings/settingsLayout";
import { useSearchableSetting } from "../settings/settingsSearch";

interface LocalAiSnapshot {
  readonly engines: ReadonlyArray<LocalEngineStatus>;
  readonly hanzod: HanzodValidatorStatus;
}

async function discoverLanEngines(): Promise<LocalEngine[]> {
  try {
    const found = (await window.desktopBridge?.discoverLanEngines?.()) ?? [];
    return found.map((node: DesktopDiscoveredLanEngine) => ({
      name: node.name,
      kind: node.kind,
      baseUrl: node.baseUrl,
    }));
  } catch {
    return [];
  }
}

/** One bounded probe round: built-ins, saved custom endpoints, and desktop mDNS discoveries. */
async function probeEverything(): Promise<LocalAiSnapshot> {
  const known = [...BUILT_IN_LOCAL_ENGINES, ...readCustomLocalEngines()];
  const lan = (await discoverLanEngines()).filter(
    (node) => !known.some((engine) => engine.baseUrl === node.baseUrl),
  );
  const [engines, hanzod] = await Promise.all([
    probeLocalEngines([...known, ...lan]),
    probeHanzodValidatorStatus({ baseUrl: readCustomHanzodUrl() || LOCAL_HANZOD_RPC_BASE_URL }),
  ]);
  return { engines, hanzod };
}

function useLocalAiSnapshot() {
  const [snapshot, setSnapshot] = useState<LocalAiSnapshot | null>(null);
  const [refreshing, setRefreshing] = useState(false);
  const refresh = async () => {
    setRefreshing(true);
    setSnapshot(await probeEverything());
    setRefreshing(false);
  };
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- one probe round when the surface mounts; later rounds are explicit refreshes
  useEffect(() => {
    let cancelled = false;
    void probeEverything().then((next) => {
      if (!cancelled) setSnapshot(next);
    });
    return () => {
      cancelled = true;
    };
  }, []);
  return { snapshot, refreshing, refresh };
}

export function LocalAiSettingsSection() {
  const gt = useGT();
  const searchableSetting = useSearchableSetting();
  const { snapshot } = useLocalAiSnapshot();
  const online = snapshot?.engines.filter((status) => status.online) ?? [];
  const modelCount = online.reduce((total, status) => total + status.models.length, 0);
  const hanzodOnline = snapshot?.hanzod.online ?? false;

  return (
    <SettingsSection id="local-ai" title={gt("Local AI")}>
      <SettingsRow
        {...searchableSetting("local-ai-models")}
        devicePreference
        title={gt("Local AI engines")}
        description={gt(
          "Run models on this Mac or your network: Hanzo Engine, Ollama, LM Studio, or any OpenAI-compatible endpoint.",
        )}
        status={
          snapshot === null
            ? gt("Checking local engines...")
            : gt("{engines} online · {count} models", {
                engines: online.length,
                count: modelCount,
              })
        }
        control={
          <Button variant="outline" size="sm" onClick={() => openLocalAiManager("engines")}>
            <T>Manage engines</T>
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
    </SettingsSection>
  );
}

async function streamOllamaPull(
  engine: LocalEngine,
  model: string,
  onProgress: (update: { readonly status?: string; readonly percent?: number }) => void,
): Promise<{ readonly ok: true } | { readonly ok: false; readonly error: string }> {
  try {
    const response = await fetch(localEngineNativeUrl(engine, "/api/pull"), {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ model, stream: true }),
    });
    if (!response.ok || !response.body) return { ok: false, error: `HTTP ${response.status}` };
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
        if (payload.error) return { ok: false, error: payload.error };
        const percent =
          typeof payload.completed === "number" &&
          typeof payload.total === "number" &&
          payload.total > 0
            ? Math.min(99, Math.round((payload.completed / payload.total) * 100))
            : undefined;
        onProgress({
          ...(payload.status ? { status: payload.status } : {}),
          ...(percent !== undefined ? { percent } : {}),
        });
      }
    }
    return { ok: true };
  } catch (error) {
    return { ok: false, error: error instanceof Error ? error.message : String(error) };
  }
}

async function deleteOllamaModel(engine: LocalEngine, model: string): Promise<void> {
  await fetch(localEngineNativeUrl(engine, "/api/delete"), {
    method: "DELETE",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ model }),
  }).catch(() => undefined);
}

function pickerInstance(status: LocalEngineStatus): ProviderInstanceConfig {
  const entry = localEngineProviderInstance(status);
  return {
    driver: HANZO_DRIVER_KIND,
    displayName: entry.displayName,
    enabled: true,
    config: entry.config,
  };
}

/** Adds or removes the engine in the composer model picker, the one place models are chosen. */
function PickerToggle({
  environmentId,
  status,
}: {
  readonly environmentId: EnvironmentId;
  readonly status: LocalEngineStatus;
}) {
  const instances = useEnvironmentSettings(environmentId, (settings) => settings.providerInstances);
  const persist = usePersistEnvironmentProviderInstanceMutation(environmentId);
  const instanceId = ProviderInstanceId.make(localEngineInstanceId(status.engine));
  const listed = instances[instanceId] !== undefined;
  // Hanzo Engine's models join the picker (as "Local · Private") unless the user took them out.
  const dismissKey = `hanzo.dev.localAi.pickerDismissed.${instanceId}`;
  const dismissed = (() => {
    try {
      return window.localStorage.getItem(dismissKey) === "1";
    } catch {
      return false;
    }
  })();
  const joins =
    status.engine.kind === "hanzo-engine" &&
    status.online &&
    status.models.length > 0 &&
    !dismissed;
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- keeps the picker's model list equal to what the engine serves after each explicit probe
  useEffect(() => {
    if ((listed || joins) && status.online) {
      void persist({
        operation: "upsert",
        instanceId,
        instance: pickerInstance(status),
      });
    }
    // Only a new probe result re-syncs; persisting is not itself a trigger.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [status]);
  return (
    <Button
      variant={listed || joins ? "default" : "outline"}
      size="xs"
      disabled={!status.online && !listed}
      onClick={() => {
        try {
          if (listed || joins) window.localStorage.setItem(dismissKey, "1");
          else window.localStorage.removeItem(dismissKey);
        } catch {
          // Restricted storage: the choice holds for this session only.
        }
        void persist(
          listed || joins
            ? { operation: "remove", instanceId }
            : { operation: "upsert", instanceId, instance: pickerInstance(status) },
        );
      }}
    >
      {listed || joins ? <T>In model picker</T> : <T>Add to model picker</T>}
    </Button>
  );
}

const formatGb = (bytes: number): string => `${(bytes / 2 ** 30).toFixed(1)} GB`;

/**
 * The Zen models Hanzo Engine can run on this machine, with a one-click download of the one this Mac's
 * memory suits. Weights land in the shared Hugging Face cache and run offline; nothing is sent out.
 */
function HanzoEngineCatalog({ onChanged }: { readonly onChanged: () => Promise<void> }) {
  const gt = useGT();
  const [catalog, setCatalog] = useState<EngineCatalog | null>(null);
  const [message, setMessage] = useState<string | null>(null);
  const pulling = catalog?.models.some((model) => model.pull.state === "pulling") ?? false;

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- reads hanzod's catalog on mount, and again every second only while a download runs
  useEffect(() => {
    let cancelled = false;
    const read = () =>
      void fetchEngineCatalog().then((next) => {
        if (!cancelled) setCatalog(next);
      });
    read();
    if (!pulling) return () => void (cancelled = true);
    const timer = setInterval(read, 1_000);
    return () => {
      cancelled = true;
      clearInterval(timer);
    };
  }, [pulling]);

  if (catalog === null) return null;
  const pull = async (id: string) => {
    setMessage((await engineCatalogAction("pull", id)) ?? null);
    setCatalog(await fetchEngineCatalog());
  };
  const use = async (id: string) => {
    setMessage((await engineCatalogAction("select", id)) ?? null);
    setCatalog(await fetchEngineCatalog());
    await onChanged();
  };
  return (
    <div className="space-y-1.5" data-testid="engine-catalog">
      <div className="text-xs font-medium">
        <T>Run on this Mac, privately</T>
      </div>
      <ul className="divide-y divide-border rounded-lg border border-border text-xs">
        {catalog.models.map((model) => {
          const recommended = model.id === catalog.recommended;
          const percent =
            model.pull.total > 0 ? Math.round((model.pull.done / model.pull.total) * 100) : 0;
          return (
            <li key={model.id} className="flex items-center justify-between gap-2 px-3 py-1.5">
              <span className="min-w-0">
                <span className="font-mono">{model.id}</span>
                <span className="ml-2 text-muted-foreground">{formatGb(model.bytes)}</span>
                {recommended ? (
                  <span className="ml-2 rounded-full bg-success/15 px-2 py-0.5 text-3xs font-semibold text-success-foreground">
                    <T>Recommended</T>
                  </span>
                ) : null}
              </span>
              {model.installed ? (
                <Button
                  variant={model.selected ? "default" : "outline"}
                  size="xs"
                  disabled={model.selected}
                  onClick={() => void use(model.id)}
                >
                  {model.selected ? <T>In use</T> : <T>Use</T>}
                </Button>
              ) : (
                <Button
                  variant={recommended ? "default" : "outline"}
                  size="xs"
                  disabled={pulling}
                  onClick={() => void pull(model.id)}
                >
                  <DownloadIcon className="size-3" />
                  {model.pull.state === "pulling" ? `${percent}%` : <T>Download</T>}
                </Button>
              )}
            </li>
          );
        })}
      </ul>
      {message ? <p className="text-xs text-destructive">{message}</p> : null}
      {catalog.models.some((model) => model.pull.state === "failed") ? (
        <p className="text-xs text-destructive">
          {catalog.models.find((model) => model.pull.state === "failed")?.pull.error}
        </p>
      ) : null}
      <p className="text-2xs text-muted-foreground">
        {gt("Weights stay in your Hugging Face cache and run offline.")}
      </p>
    </div>
  );
}

function EngineCard({
  status,
  environmentId,
  removable,
  onRemove,
  onChanged,
}: {
  readonly status: LocalEngineStatus;
  readonly environmentId: EnvironmentId | null;
  readonly removable: boolean;
  readonly onRemove: () => void;
  readonly onChanged: () => Promise<void>;
}) {
  const gt = useGT();
  const { engine } = status;
  const action = localEngineAction(engine.kind);
  const [input, setInput] = useState("");
  const [busy, setBusy] = useState(false);
  const [percent, setPercent] = useState<number | null>(null);
  const [message, setMessage] = useState<string | null>(null);

  const run = async () => {
    const model = input.trim();
    if (!model || busy) return;
    setBusy(true);
    setPercent(null);
    setMessage(null);
    const result = await streamOllamaPull(engine, model, (update) => {
      if (update.status) setMessage(update.status);
      if (update.percent !== undefined) setPercent(update.percent);
    });
    if (result.ok) {
      setInput("");
      setMessage(gt("Ready: {model}", { model }));
      await onChanged();
    } else {
      setMessage(result.error);
    }
    setPercent(null);
    setBusy(false);
  };

  return (
    <div className="space-y-3 rounded-xl border border-border p-4" data-testid="local-engine-card">
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="flex min-w-0 items-center gap-2">
          <span
            role="img"
            aria-label={status.online ? gt("online") : gt("offline")}
            className={`size-2 shrink-0 rounded-full ${status.online ? "bg-success" : "bg-muted-foreground/40"}`}
          />
          <div className="min-w-0">
            <div className="truncate text-sm font-semibold">{engine.name}</div>
            <div className="truncate font-mono text-2xs text-muted-foreground">
              {status.baseUrl}
              {status.latencyMs !== null ? ` · ${status.latencyMs} ms` : ""}
            </div>
          </div>
        </div>
        <div className="flex items-center gap-2">
          {environmentId !== null ? (
            <PickerToggle environmentId={environmentId} status={status} />
          ) : null}
          {removable ? (
            <button
              type="button"
              aria-label={gt("Remove {name}", { name: engine.name })}
              onClick={onRemove}
              className="rounded-md p-1.5 text-muted-foreground hover:bg-destructive/10 hover:text-destructive"
            >
              <Trash2Icon className="size-3.5" />
            </button>
          ) : null}
        </div>
      </div>

      {status.online ? (
        status.models.length > 0 ? (
          <ul className="divide-y divide-border rounded-lg border border-border text-xs">
            {status.models.map((model) => (
              <li key={model.id} className="flex items-center justify-between gap-2 px-3 py-1.5">
                <span className="flex min-w-0 items-center gap-1.5">
                  <span className="truncate font-mono">{model.id}</span>
                  {model.zen ? (
                    <span className="rounded-full bg-success/15 px-2 py-0.5 text-3xs font-semibold text-success-foreground">
                      Zen
                    </span>
                  ) : null}
                </span>
                {engine.kind === "ollama" ? (
                  <button
                    type="button"
                    aria-label={gt("Remove {model}", { model: model.id })}
                    onClick={() => void deleteOllamaModel(engine, model.id).then(onChanged)}
                    className="rounded-md p-1 text-muted-foreground hover:bg-destructive/10 hover:text-destructive"
                  >
                    <Trash2Icon className="size-3" />
                  </button>
                ) : null}
              </li>
            ))}
          </ul>
        ) : (
          <p className="text-xs text-muted-foreground">
            <T>No models served yet.</T>
          </p>
        )
      ) : (
        <p className="text-xs text-muted-foreground">
          <T>Not reachable. Start the engine, then refresh.</T>
        </p>
      )}

      {engine.kind === "hanzo-engine" ? <HanzoEngineCatalog onChanged={onChanged} /> : null}

      {action !== null && status.online ? (
        <div className="space-y-1.5">
          <div className="flex gap-2">
            <input
              type="text"
              value={input}
              onChange={(event) => setInput(event.currentTarget.value)}
              placeholder={gt("Model tag to pull, e.g. llama3.2")}
              aria-label={gt("Model to pull")}
              className="flex-1 rounded-lg border border-border bg-background px-3 py-1.5 font-mono text-xs focus:outline-hidden"
            />
            <Button
              variant="outline"
              size="sm"
              disabled={busy || !input.trim()}
              onClick={() => void run()}
            >
              <DownloadIcon className="size-3.5" />
              <T>Pull</T>
            </Button>
          </div>
          {percent !== null ? (
            <div className="h-1.5 w-full overflow-hidden rounded-full bg-muted">
              <div
                className="h-full bg-foreground transition-all"
                style={{ width: `${percent}%` }}
              />
            </div>
          ) : null}
          {message ? <p className="text-xs text-muted-foreground">{message}</p> : null}
        </div>
      ) : null}
    </div>
  );
}

export function LocalAiManagerModal() {
  const modal = useLocalAiModalState();
  if (!modal.open) return null;
  return <LocalAiManagerModalBody tab={modal.tab} />;
}

function LocalAiManagerModalBody({
  tab,
}: {
  readonly tab: ReturnType<typeof useLocalAiModalState>["tab"];
}) {
  const gt = useGT();
  const environmentId = usePrimaryEnvironmentId();
  const { snapshot, refreshing, refresh } = useLocalAiSnapshot();
  const [hanzodUrlInput, setHanzodUrlInput] = useState(readCustomHanzodUrl);
  const [customName, setCustomName] = useState("");
  const [customUrl, setCustomUrl] = useState("");

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- Escape dismisses the dialog
  useEffect(() => {
    const onKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        event.preventDefault();
        closeLocalAiManager();
      }
    };
    window.addEventListener("keydown", onKeyDown);
    return () => window.removeEventListener("keydown", onKeyDown);
  }, []);

  const addCustomEngine = () => {
    const baseUrl = trimBaseUrl(customUrl);
    if (!customName.trim() || !/^https?:\/\//iu.test(baseUrl)) return;
    writeCustomLocalEngines([
      ...readCustomLocalEngines().filter((engine) => engine.baseUrl !== baseUrl),
      { name: customName.trim(), kind: "openai", baseUrl },
    ]);
    setCustomName("");
    setCustomUrl("");
    void refresh();
  };

  const removeCustomEngine = (baseUrl: string) => {
    writeCustomLocalEngines(
      readCustomLocalEngines().filter((engine) => engine.baseUrl !== baseUrl),
    );
    void refresh();
  };

  const customUrls = new Set(readCustomLocalEngines().map((engine) => engine.baseUrl));

  return (
    <div
      role="dialog"
      aria-modal="true"
      aria-label={gt("Local AI")}
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
                <T>Local AI</T>
              </div>
              <div className="text-xs text-muted-foreground">
                <T>
                  Run models on this Mac or your network: Hanzo Engine, Ollama, LM Studio, or any
                  OpenAI-compatible endpoint.
                </T>
              </div>
            </div>
          </div>
          <div className="flex items-center gap-2">
            <Button
              variant="outline"
              size="xs"
              disabled={refreshing}
              onClick={() => void refresh()}
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

        <div className="flex border-b border-border px-5">
          {(
            [
              { id: "engines", label: gt("Engines") },
              { id: "node", label: gt("Validator Node (:9630)") },
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
          {tab === "engines" ? (
            <div className="space-y-4">
              {snapshot === null ? (
                <p className="text-xs text-muted-foreground">
                  <T>Checking engines...</T>
                </p>
              ) : (
                snapshot.engines.map((status) => (
                  <EngineCard
                    key={status.engine.baseUrl}
                    status={status}
                    environmentId={environmentId}
                    removable={customUrls.has(status.engine.baseUrl)}
                    onRemove={() => removeCustomEngine(status.engine.baseUrl)}
                    onChanged={refresh}
                  />
                ))
              )}

              <div className="space-y-2 rounded-xl border border-border p-4">
                <div className="flex items-center gap-1.5 text-xs font-semibold">
                  <NetworkIcon className="size-3.5" />
                  <T>Custom OpenAI-compatible endpoint</T>
                </div>
                <div className="flex gap-2">
                  <input
                    type="text"
                    value={customName}
                    onChange={(event) => setCustomName(event.currentTarget.value)}
                    placeholder={gt("Name")}
                    aria-label={gt("Endpoint name")}
                    className="w-40 rounded-lg border border-border bg-background px-3 py-1.5 text-xs focus:outline-hidden"
                  />
                  <input
                    type="text"
                    value={customUrl}
                    onChange={(event) => setCustomUrl(event.currentTarget.value)}
                    placeholder="http://127.0.0.1:8000/v1"
                    aria-label={gt("Endpoint URL")}
                    className="flex-1 rounded-lg border border-border bg-background px-3 py-1.5 font-mono text-xs focus:outline-hidden"
                  />
                  <Button variant="outline" size="sm" onClick={addCustomEngine}>
                    <T>Add</T>
                  </Button>
                </div>
                <p className="text-xs text-muted-foreground">
                  <T>
                    Nodes on your network that advertise over mDNS appear above in the desktop app.
                  </T>
                </p>
              </div>
            </div>
          ) : (
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
                      void refresh();
                    }}
                  >
                    <T>Save &amp; Probe</T>
                  </Button>
                </div>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
