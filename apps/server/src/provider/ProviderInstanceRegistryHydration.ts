import {
  defaultInstanceIdForDriver,
  ProviderDriverKind,
  ProviderInstanceId,
  providerInstanceConfigEnabledFlag,
  resolveProviderInstanceEnabled,
  type ProviderInstanceConfig,
  type ProviderInstanceConfigMap,
  ServerSettings,
} from "@hanzodev/contracts";
import * as NodeUtil from "node:util";

import * as HostProcess from "@hanzodev/shared/HostProcess";
import type * as SecureRandom from "effect/crypto/SecureRandom";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Layer from "effect/Layer";
import type * as SqlClient from "effect/sql/SqlClient";
import * as Path from "effect/Path";
import * as Stream from "effect/Stream";

import * as ManagedProviders from "../policy/managedProviders.ts";
import * as Settings from "../serverSettings.ts";
import * as SecretResolver from "../secrets/SecretResolver.ts";
import { BUILT_IN_DRIVERS, type BuiltInDriversEnv } from "./builtInDrivers.ts";
import * as ProviderInstanceRegistry from "./ProviderInstanceRegistry.ts";
import * as ProviderInstanceRegistryMutator from "./ProviderInstanceRegistryMutator.ts";
import * as ProviderOrchestrationAdapterInfrastructure from "./ProviderOrchestrationAdapterInfrastructure.ts";
import * as AcpRegistrySupport from "@hanzodev/provider-acp-registry/server/AcpRegistrySupport";
import * as ProviderHostLive from "./ProviderHostLive.ts";
import type * as ProviderHost from "@hanzodev/provider-core/server/ProviderHost";
import type * as BackgroundPolicy from "../background/BackgroundPolicy.ts";
import * as ServerConfig from "../config.ts";
import type * as PortScanner from "../preview/PortScanner.ts";

type ProviderInstanceRegistryHydrationEnv =
  | Exclude<
      BuiltInDriversEnv,
      | ProviderOrchestrationAdapterInfrastructure.ProviderOrchestrationAdapterInfrastructure
      | AcpRegistrySupport.AcpRegistryCatalog
      | ProviderHost.ProviderHost
    >
  | Settings.ServerSettingsService
  | BackgroundPolicy.BackgroundPolicy
  | ServerConfig.ServerConfig
  | Path.Path
  | FileSystem.FileSystem
  // Secret provider reads.
  | SecureRandom.SecureRandom
  // The Claude runner counts its CLIs as running agents for port discovery.
  | PortScanner.PortDiscovery
  // Adapter ids read which subagent threads predate short ids.
  | SqlClient.SqlClient;

export const deriveProviderInstanceConfigMap = (
  settings: ServerSettings,
): ProviderInstanceConfigMap => {
  const merged: Record<string, ProviderInstanceConfig> = { ...settings.providerInstances };

  for (const driver of BUILT_IN_DRIVERS) {
    if (driver.metadata.hasDefaultInstance === false) continue;
    const instanceId = defaultInstanceIdForDriver(driver.driverKind);
    if (instanceId in merged) continue;
    merged[instanceId] = { driver: driver.driverKind };
  }

  // Probe project-enabled instances even if disabled machine-wide; project threads can still route to them.
  for (const overrides of Object.values(settings.projectSettingsOverrides)) {
    for (const [rawInstanceId, projectOverride] of Object.entries(
      overrides?.providerInstances ?? {},
    )) {
      if (projectOverride.enabled !== true) continue;
      const instanceId = rawInstanceId as ProviderInstanceId;
      const existing = merged[instanceId];
      if (existing === undefined || resolveProviderInstanceEnabled(existing)) continue;
      merged[instanceId] = {
        ...existing,
        enabled: true,
        ...(providerInstanceConfigEnabledFlag(existing.config) === false
          ? { config: { ...(existing.config as Record<string, unknown>), enabled: true } }
          : {}),
      };
    }
  }

  return merged as ProviderInstanceConfigMap;
};

export const DEV_REPLAY_INSTANCE_ID = ProviderInstanceId.make("devReplay");
export const HANZO_DEFAULT_INSTANCE_ID = ProviderInstanceId.make("hanzo");
export const HANZO_GEN_DEFAULT_INSTANCE_ID = ProviderInstanceId.make("hanzoGen");
export const CODEX_DEFAULT_INSTANCE_ID = ProviderInstanceId.make("codex");
export const OPENAI_CODEX_INSTANCE_ID = ProviderInstanceId.make("openai-codex");

/**
 * Configures the primary default provider instances (`hanzo` and `hanzoGen`) to use the native
 * Hanzo CLIs (`dev` and `gen`) when installed on the host, sets `hanzo` + `enso-auto` as the
 * default model selection, and keeps OpenAI Codex cleanly configured on `codex`.
 */
export const seedHanzoCloudDefaultInstance = Effect.fn(
  "ProviderInstanceRegistryHydration.seedHanzoCloudDefaultInstance",
)(function* () {
  const fs = yield* FileSystem.FileSystem;
  const path = yield* Path.Path;
  const home = yield* HostProcess.HomeDirectory;
  const candidates = [
    path.join(home, ".local", "bin", "dev"),
    path.join(home, ".hanzo", "bin", "dev"),
    "/opt/homebrew/bin/dev",
    "/usr/local/bin/dev",
  ];
  let devBinaryPath: string | null = null;
  for (const candidate of candidates) {
    if (yield* fs.exists(candidate).pipe(Effect.orElseSucceed(() => false))) {
      devBinaryPath = candidate;
      break;
    }
  }
  if (!devBinaryPath) return;

  const genCandidates = [
    path.join(home, ".local", "bin", "gen"),
    path.join(home, ".local", "bin", "hanzo-gen"),
    "/opt/homebrew/bin/gen",
    "/usr/local/bin/gen",
  ];
  let genBinaryPath: string | null = null;
  for (const candidate of genCandidates) {
    if (yield* fs.exists(candidate).pipe(Effect.orElseSucceed(() => false))) {
      genBinaryPath = candidate;
      break;
    }
  }

  const hanzoDevHome = path.join(home, ".hanzo", "dev");
  const serverSettings = yield* Settings.ServerSettingsService;
  const currentSettings = yield* serverSettings.getSettings;

  const existingHanzo = currentSettings.providerInstances[HANZO_DEFAULT_INSTANCE_ID];
  const existingHanzoConfig = (existingHanzo?.config as Record<string, unknown> | undefined) ?? {};
  const existingHanzoBinary =
    typeof existingHanzoConfig.binaryPath === "string" ? existingHanzoConfig.binaryPath : "";

  if (
    !existingHanzo ||
    !existingHanzoBinary ||
    existingHanzo.displayName === "Hanzo Cloud" ||
    existingHanzoBinary.endsWith("/hanzo-dev")
  ) {
    const hanzoInstance: ProviderInstanceConfig = {
      ...existingHanzo,
      displayName: "Hanzo",
      driver: ProviderDriverKind.make("hanzo"),
      enabled: existingHanzo?.enabled ?? true,
      config: {
        ...existingHanzoConfig,
        setupMode: "existing",
        enabled: existingHanzo?.enabled ?? true,
        binaryPath: devBinaryPath,
        homePath: hanzoDevHome,
      },
    };
    yield* serverSettings.updateProviderInstance({
      operation: "upsert",
      instanceId: HANZO_DEFAULT_INSTANCE_ID,
      instance: hanzoInstance,
    });
  }

  if (genBinaryPath) {
    const existingGen = currentSettings.providerInstances[HANZO_GEN_DEFAULT_INSTANCE_ID];
    if (!existingGen) {
      yield* serverSettings.updateProviderInstance({
        operation: "upsert",
        instanceId: HANZO_GEN_DEFAULT_INSTANCE_ID,
        instance: {
          displayName: "Hanzo Gen",
          driver: ProviderDriverKind.make("hanzoGen"),
          enabled: true,
          config: {
            setupMode: "existing",
            enabled: true,
            binaryPath: genBinaryPath,
            homePath: hanzoDevHome,
          },
        },
      });
    }
  }

  // Migrate any earlier `codex` slot or stale `Hanzo Cloud` custom instance.
  const existingCodex = currentSettings.providerInstances[CODEX_DEFAULT_INSTANCE_ID];
  if (existingCodex?.displayName === "Hanzo Cloud") {
    const codexBinary = (yield* fs
      .exists("/opt/homebrew/bin/codex")
      .pipe(Effect.orElseSucceed(() => false)))
      ? "/opt/homebrew/bin/codex"
      : "codex";
    yield* serverSettings.updateProviderInstance({
      operation: "upsert",
      instanceId: CODEX_DEFAULT_INSTANCE_ID,
      instance: {
        displayName: "Codex",
        driver: ProviderDriverKind.make("codex"),
        enabled: true,
        config: {
          setupMode: "existing",
          enabled: true,
          binaryPath: codexBinary,
          homePath: path.join(home, ".codex"),
        },
      },
    });
  }
  for (const [id, entry] of Object.entries(currentSettings.providerInstances)) {
    if (
      id === OPENAI_CODEX_INSTANCE_ID ||
      (id !== HANZO_DEFAULT_INSTANCE_ID &&
        id !== CODEX_DEFAULT_INSTANCE_ID &&
        entry.displayName === "Hanzo Cloud")
    ) {
      yield* serverSettings.updateProviderInstance({
        operation: "remove",
        instanceId: ProviderInstanceId.make(id),
      });
    }
  }

  // Default new threads to Hanzo (`enso-auto`) and repair any stale `{ instanceId: "codex", model: "enso-*" }` pairing.
  const currentDefault = currentSettings.defaultModelSelection;
  const pointsHanzoModelAtCodex =
    currentDefault?.instanceId === CODEX_DEFAULT_INSTANCE_ID &&
    (currentDefault.model.startsWith("enso") ||
      currentDefault.model.startsWith("zen") ||
      currentDefault.model.startsWith("hanzo/"));
  const needsDarkDefault = currentSettings.defaultAppearance === "";
  const needsRemoteDefaults =
    !currentSettings.linkedEnvironmentsEnabled ||
    !currentSettings.enableDeviceSupport ||
    !currentSettings.enableAgentDeviceAccess;

  if (!currentDefault || pointsHanzoModelAtCodex || needsDarkDefault || needsRemoteDefaults) {
    yield* serverSettings
      .updateSettings({
        ...(!currentDefault || pointsHanzoModelAtCodex
          ? {
              defaultModelSelection: {
                instanceId: HANZO_DEFAULT_INSTANCE_ID,
                model: currentDefault?.model && pointsHanzoModelAtCodex ? currentDefault.model : "enso-auto",
              },
            }
          : {}),
        ...(needsDarkDefault
          ? {
              defaultAppearance: "dark",
              defaultAppearanceSetAt: "2026-11-19T00:00:00.000Z",
            }
          : {}),
        linkedEnvironmentsEnabled: true,
        enableDeviceSupport: true,
        enableAgentDeviceAccess: true,
      })
      .pipe(Effect.ignore);
  }
});

/**
 * Dev servers run from source keep a free ACP agent that replays recorded turns, so UI checks
 * need no paid model. Bundled servers, such as desktop dev, have no agent script and skip it.
 */
export const seedDevReplayInstance = Effect.fn(
  "ProviderInstanceRegistryHydration.seedDevReplayInstance",
)(function* () {
  const config = yield* ServerConfig.ServerConfig;
  if (config.devUrl === undefined) return;
  const path = yield* Path.Path;
  const agentPath = yield* path.fromFileUrl(
    new URL("../../scripts/dev-replay-agent.ts", import.meta.url),
  );
  if (!(yield* (yield* FileSystem.FileSystem).exists(agentPath))) return;
  const commandPath = yield* HostProcess.ExecutablePath;
  const serverSettings = yield* Settings.ServerSettingsService;
  const existing = (yield* serverSettings.getSettings).providerInstances[DEV_REPLAY_INSTANCE_ID];
  const existingConfig = existing?.config as Record<string, unknown> | undefined;
  // Rewritten only when stale: the node and checkout paths can move.
  if (
    existingConfig?.commandPath === commandPath &&
    (existingConfig.commandArgs as ReadonlyArray<unknown> | undefined)?.[0] === agentPath
  ) {
    return;
  }
  // Keeps user edits, such as turning the provider off.
  const instance: ProviderInstanceConfig = {
    displayName: "Replay (dev)",
    ...existing,
    driver: ProviderDriverKind.make("acpRegistry"),
    config: { ...existingConfig, source: "local", commandPath, commandArgs: [agentPath] },
  };
  yield* serverSettings.updateProviderInstance({
    operation: "upsert",
    instanceId: DEV_REPLAY_INSTANCE_ID,
    instance,
  });
});

// A label is display-only, so only kind and config can change what a vault returns.
const vaultConfigs = (settings: ServerSettings) =>
  Object.entries(settings.secretProviders).map(([id, { kind, config }]) => [id, kind, config]);

export const layerSettingsWatcher = (
  settingsChanges: Stream.Stream<ServerSettings>,
  // The settings the registry was built from: a change queued during startup differs from these.
  initialSettings: ServerSettings | undefined,
) =>
  Layer.effectDiscard(
    Effect.gen(function* () {
      const mutator = yield* ProviderInstanceRegistryMutator.ProviderInstanceRegistryMutator;
      const serverSettings = yield* Settings.ServerSettingsService;
      let secretProviders = initialSettings && vaultConfigs(initialSettings);
      yield* settingsChanges.pipe(
        Stream.runForEach(() =>
          // The subscription predates the seed read, so a queued change can be
          // older than the seed. A failed current read must leave the registry intact.
          serverSettings.getSettings.pipe(
            Effect.flatMap((current) =>
              Effect.gen(function* () {
                yield* mutator.reconcile(deriveProviderInstanceConfigMap(current));
                // Reconcile leaves instances parked on an unreadable secret alone,
                // and a corrected secret provider is what can make them readable.
                const next = vaultConfigs(current);
                if (!NodeUtil.isDeepStrictEqual(secretProviders, next)) {
                  secretProviders = next;
                  yield* mutator.reloadSecrets();
                }
              }),
            ),
            Effect.catchCause((cause) =>
              Effect.logError("ProviderInstanceRegistry reconcile failed", cause),
            ),
          ),
        ),
        Effect.forkScoped,
      );
    }),
  );

export const layer: Layer.Layer<
  | ProviderInstanceRegistry.ProviderInstanceRegistry
  | ProviderInstanceRegistryMutator.ProviderInstanceRegistryMutator,
  never,
  ProviderInstanceRegistryHydrationEnv
> = Layer.unwrap(
  Effect.gen(function* () {
    const serverSettings = yield* Settings.ServerSettingsService;
    yield* seedHanzoCloudDefaultInstance().pipe(
      Effect.catchCause((cause) => Effect.logWarning("Hanzo Cloud default seed unavailable", cause)),
    );
    yield* seedDevReplayInstance().pipe(
      Effect.catchCause((cause) => Effect.logWarning("Dev replay agent unavailable", cause)),
    );
    // Subscribe before the seed read so saves during registry construction are queued.
    const settingsChanges = yield* serverSettings.subscribeChanges;
    const initialSettings: ServerSettings | undefined = yield* serverSettings.getSettings.pipe(
      Effect.orElseSucceed(() => undefined),
    );
    const initialConfigMap =
      initialSettings === undefined
        ? ({} as ProviderInstanceConfigMap)
        : deriveProviderInstanceConfigMap(initialSettings);

    // Drivers launch from the host environment, minus launch overrides a policy ignores.
    const hostEnvironment = ManagedProviders.providerHostEnvironment(
      serverSettings.managedPolicy,
      yield* HostProcess.Environment,
    );
    // Instances of drivers the policy leaves out stay listed, as unavailable, so their threads
    // explain why instead of losing their provider.
    const layerMutable = ProviderInstanceRegistry.layer({
      drivers: BUILT_IN_DRIVERS,
      configMap: initialConfigMap,
      blockedReason: (driver) =>
        ManagedProviders.disallowedDriverReason(serverSettings.managedPolicy, driver),
    }).pipe(
      Layer.provide(ProviderOrchestrationAdapterInfrastructure.layer),
      Layer.provide(AcpRegistrySupport.layerFromHost),
      Layer.provide(ProviderHostLive.layer),
      Layer.provide(Layer.succeed(HostProcess.Environment, hostEnvironment)),
      Layer.provide(SecretResolver.layer),
    );

    return layerSettingsWatcher(settingsChanges, initialSettings).pipe(
      Layer.provideMerge(layerMutable),
    );
  }),
) as Layer.Layer<
  | ProviderInstanceRegistry.ProviderInstanceRegistry
  | ProviderInstanceRegistryMutator.ProviderInstanceRegistryMutator,
  never,
  ProviderInstanceRegistryHydrationEnv
>;
