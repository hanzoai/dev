import {
  HANZO_GEN_DRIVER_KIND,
  HanzoGenSettings,
  type CodexSettings,
  type ServerProvider,
} from "@hanzodev/contracts";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Path from "effect/Path";
import * as Schema from "effect/Schema";
import * as Stream from "effect/Stream";

import type * as CodexUsageHistory from "../../usage/CodexUsageHistory.ts";
import type { ProviderDriver, ProviderInstance } from "@hanzodev/provider-core/server/driver";
import { CodexDriver, type CodexDriverEnv } from "./CodexDriver.ts";
import { codexUsageReader } from "./codexUsage.ts";
import {
  CURATED_HANZO_MODELS,
  mergeCuratedHanzoModels,
  resolveDefaultHanzoDevBinary,
  resolveDefaultHanzoGenBinary,
  resolveHanzoProviderAuth,
} from "../hanzoLocalIdentity.ts";

const decodeHanzoGenSettings = Schema.decodeSync(HanzoGenSettings);

function enrichHanzoGenSnapshot(
  snapshot: ServerProvider,
  fallbackAuth: ServerProvider["auth"],
  genInstalled: boolean,
): ServerProvider {
  const {
    compatibilityAdvisory: _compat,
    versionAdvisory: _versionAdv,
    updateRequiredModels: _updateReq,
    message: rawMessage,
    ...cleanSnapshot
  } = snapshot;
  const models = mergeCuratedHanzoModels(
    snapshot.models.length > 0 ? snapshot.models : CURATED_HANZO_MODELS,
  );
  const auth =
    snapshot.auth.status === "authenticated"
      ? snapshot.auth
      : fallbackAuth.status === "authenticated"
        ? fallbackAuth
        : snapshot.auth;
  const installed = genInstalled || snapshot.installed;
  const status =
    installed && (snapshot.status === "ready" || auth.status === "authenticated")
      ? ("ready" as const)
      : snapshot.status;
  return {
    ...cleanSnapshot,
    driver: HANZO_GEN_DRIVER_KIND,
    displayName: snapshot.displayName ?? "Hanzo Gen",
    installed,
    status,
    auth,
    models,
    updateRequiredModels: [],
    ...(status === "ready" ? {} : rawMessage ? { message: rawMessage } : {}),
  };
}

export type HanzoGenDriverEnv = CodexDriverEnv;

export const HanzoGenDriver: ProviderDriver<
  HanzoGenSettings,
  HanzoGenDriverEnv,
  Path.Path | CodexUsageHistory.CodexUsageHistory
> = {
  driverKind: HANZO_GEN_DRIVER_KIND,
  metadata: {
    displayName: "Hanzo Gen",
    supportsMultipleInstances: true,
  },
  configSchema: HanzoGenSettings,
  defaultConfig: (): HanzoGenSettings => decodeHanzoGenSettings({}),
  usage: codexUsageReader,
  create: Effect.fn("HanzoGenDriver.create")(function* ({
    instanceId,
    displayName,
    accentColor,
    environment,
    enabled,
    config,
  }) {
    const fs = yield* FileSystem.FileSystem;
    const path = yield* Path.Path;
    const home = yield* HostProcess.HomeDirectory;

    const genBinary = yield* resolveDefaultHanzoGenBinary();
    // `gen` (hanzoai/gen) wraps `dev` as its native JSON-RPC app-server backend; run `dev serve`
    // with HANZO_GEN_BINARY set so Hanzo Gen sessions share the 1M gateway & MCP toolset.
    const appServerBinary = yield* resolveDefaultHanzoDevBinary();
    const resolvedHome = config.homePath.trim() || path.join(home, ".hanzo", "dev");

    const codexCompatibleConfig: CodexSettings = {
      ...config,
      setupMode: "existing",
      enabled,
      binaryPath: appServerBinary,
      homePath: resolvedHome,
    };

    const resolvedAuth = yield* resolveHanzoProviderAuth().pipe(
      Effect.provideService(FileSystem.FileSystem, fs),
      Effect.provideService(Path.Path, path),
      Effect.provideService(HostProcess.HomeDirectory, home),
    );

    const baseInstance = yield* CodexDriver.create({
      instanceId,
      displayName: displayName ?? "Hanzo Gen",
      accentColor,
      environment: [
        ...(environment ?? []),
        { name: "HANZO_GEN", value: "1", sensitive: false },
        ...(genBinary ? [{ name: "HANZO_GEN_BINARY", value: genBinary, sensitive: false }] : []),
      ],
      enabled,
      config: codexCompatibleConfig,
    });

    const genInstalled = genBinary !== null;
    const wrapSnapshotEffect = (eff: Effect.Effect<ServerProvider>) =>
      eff.pipe(Effect.map((snap) => enrichHanzoGenSnapshot(snap, resolvedAuth, genInstalled)));

    const wrappedSnapshot: ProviderInstance["snapshot"] = {
      ...baseInstance.snapshot,
      getSnapshot: wrapSnapshotEffect(baseInstance.snapshot.getSnapshot),
      refresh: wrapSnapshotEffect(baseInstance.snapshot.refresh),
      streamChanges: baseInstance.snapshot.streamChanges.pipe(
        Stream.map((snap) => enrichHanzoGenSnapshot(snap, resolvedAuth, genInstalled)),
      ),
    };

    return {
      ...baseInstance,
      driverKind: HANZO_GEN_DRIVER_KIND,
      displayName: displayName ?? "Hanzo Gen",
      snapshot: wrappedSnapshot,
      orchestrationAdapter: {
        ...baseInstance.orchestrationAdapter,
        driver: HANZO_GEN_DRIVER_KIND,
      },
      ...(baseInstance.snapshotForCwd
        ? {
            snapshotForCwd: (cwd: string) =>
              baseInstance
                .snapshotForCwd!(cwd)
                .pipe(Effect.map((snap) => enrichHanzoGenSnapshot(snap, resolvedAuth, genInstalled))),
          }
        : {}),
    } satisfies ProviderInstance;
  }),
};
