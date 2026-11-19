// @effect-diagnostics globalFetchInEffect:off nodeBuiltinImport:off -- Local loopback/LAN runtime discovery probes HTTP endpoints directly with short AbortSignal timeouts.
import * as NodeOS from "node:os";
import {
  DesktopDiscoveredLanEngineSchema,
  type DesktopDiscoveredLanEngine,
} from "@hanzodev/contracts";
import {
  probeHanzoEngineStatus,
  probeOllamaStatus,
} from "@hanzodev/shared/localAi";
import * as Effect from "effect/Effect";
import * as Schema from "effect/Schema";

import * as IpcChannels from "../channels.ts";
import * as DesktopIpc from "../DesktopIpc.ts";

function candidateLanHosts(): ReadonlyArray<{ readonly host: string; readonly label: string }> {
  const seen = new Set<string>(["127.0.0.1"]);
  const hosts: Array<{ readonly host: string; readonly label: string }> = [
    { host: "127.0.0.1", label: "Localhost" },
  ];
  const interfaces = NodeOS.networkInterfaces();
  for (const [name, entries] of Object.entries(interfaces)) {
    for (const entry of entries ?? []) {
      if (entry.family !== "IPv4" || entry.internal || seen.has(entry.address)) continue;
      seen.add(entry.address);
      hosts.push({ host: entry.address, label: `${NodeOS.hostname()} (${name})` });
    }
  }
  return hosts.slice(0, 6);
}

export const discoverLanEngines = DesktopIpc.makeIpcMethod({
  channel: IpcChannels.DISCOVER_LAN_ENGINES_CHANNEL,
  payload: Schema.Void,
  result: Schema.Array(DesktopDiscoveredLanEngineSchema),
  handler: Effect.fn("desktop.ipc.localAi.discoverLanEngines")(function* () {
    const candidates = candidateLanHosts();
    const discovered = yield* Effect.promise(async () => {
      const results: DesktopDiscoveredLanEngine[] = [];
      await Promise.all(
        candidates.flatMap(({ host, label }) => [
          probeHanzoEngineStatus({
            baseUrl: `http://${host}:36900`,
            timeoutMs: 900,
          }).then((status) => {
            if (!status.online) return;
            results.push({
              name: `${label} · Hanzo Engine`,
              host,
              port: 36900,
              baseUrl: status.baseUrl,
              kind: "hanzo-engine",
              models: status.models.map((m) => m.id),
              latencyMs: status.latencyMs,
            });
          }),
          probeOllamaStatus({
            baseUrl: `http://${host}:11434`,
            timeoutMs: 900,
          }).then((status) => {
            if (!status.online) return;
            results.push({
              name: `${label} · Ollama`,
              host,
              port: 11434,
              baseUrl: status.baseUrl,
              kind: "ollama",
              models: status.models.map((m) => m.id),
              latencyMs: status.latencyMs,
            });
          }),
        ]),
      );
      return results.toSorted((a, b) => a.host.localeCompare(b.host) || a.port - b.port);
    });
    return discovered;
  }),
});
