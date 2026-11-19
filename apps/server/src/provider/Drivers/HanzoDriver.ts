import {
  HANZO_DRIVER_KIND,
  HanzoSettings,
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
import type { ProviderUsageReader } from "@hanzodev/provider-core/server/usage";
import { CodexDriver, type CodexDriverEnv } from "./CodexDriver.ts";
import { codexUsageReader } from "./codexUsage.ts";
import {
  CURATED_HANZO_MODELS,
  mergeCuratedHanzoModels,
  resolveDefaultHanzoDevBinary,
  resolveHanzoProviderAuth,
} from "../hanzoLocalIdentity.ts";

const decodeHanzoSettings = Schema.decodeSync(HanzoSettings);

function enrichHanzoSnapshot(
  snapshot: ServerProvider,
  fallbackAuth: ServerProvider["auth"],
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
  const status =
    snapshot.installed && (snapshot.status === "ready" || auth.status === "authenticated")
      ? ("ready" as const)
      : snapshot.status;
  return {
    ...cleanSnapshot,
    driver: HANZO_DRIVER_KIND,
    displayName: snapshot.displayName ?? "Hanzo",
    status,
    auth,
    models,
    updateRequiredModels: [],
    ...(status === "ready" ? {} : rawMessage ? { message: rawMessage } : {}),
  };
}

export const hanzoUsageReader: ProviderUsageReader<
  HanzoSettings,
  Path.Path | CodexUsageHistory.CodexUsageHistory
> = {
  ...(codexUsageReader as Extract<typeof codexUsageReader, { readonly kind: "transcripts" }>),
  directories: Effect.fn("hanzoUsageReader.directories")(function* ({ config, environment }) {
    if (config === undefined) return [];
    const path = yield* Path.Path;
    const home = yield* HostProcess.HomeDirectory;
    const envHome = environment.HANZO_DEV_HOME?.trim() || environment.HANZO_HOME?.trim();
    const resolvedHome = config.homePath.trim() || envHome || path.join(home, ".hanzo", "dev");
    return [{ dir: path.resolve(resolvedHome, "sessions") }];
  }),
};

export type HanzoDriverEnv = CodexDriverEnv;

export const HanzoDriver: ProviderDriver<
  HanzoSettings,
  HanzoDriverEnv,
  Path.Path | CodexUsageHistory.CodexUsageHistory
> = {
  driverKind: HANZO_DRIVER_KIND,
  metadata: {
    displayName: "Hanzo",
    supportsMultipleInstances: true,
  },
  configSchema: HanzoSettings,
  defaultConfig: (): HanzoSettings => decodeHanzoSettings({}),
  usage: hanzoUsageReader,
  create: Effect.fn("HanzoDriver.create")(function* ({
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

    const configuredBinary = config.binaryPath.trim();
    const resolvedBinary =
      !configuredBinary || configuredBinary === "dev" || configuredBinary === "hanzo-dev"
        ? yield* resolveDefaultHanzoDevBinary()
        : configuredBinary;
    const resolvedHome = config.homePath.trim() || path.join(home, ".hanzo", "dev");

    const codexCompatibleConfig: CodexSettings = {
      ...config,
      setupMode: "existing",
      enabled,
      binaryPath: resolvedBinary,
      homePath: resolvedHome,
    };

    const resolvedAuth = yield* resolveHanzoProviderAuth().pipe(
      Effect.provideService(FileSystem.FileSystem, fs),
      Effect.provideService(Path.Path, path),
      Effect.provideService(HostProcess.HomeDirectory, home),
    );

    const baseInstance = yield* CodexDriver.create({
      instanceId,
      displayName: displayName ?? "Hanzo",
      accentColor,
      environment,
      enabled,
      config: codexCompatibleConfig,
    });

    const wrapSnapshotEffect = (eff: Effect.Effect<ServerProvider>) =>
      eff.pipe(Effect.map((snap) => enrichHanzoSnapshot(snap, resolvedAuth)));

    const wrappedSnapshot: ProviderInstance["snapshot"] = {
      ...baseInstance.snapshot,
      getSnapshot: wrapSnapshotEffect(baseInstance.snapshot.getSnapshot),
      refresh: wrapSnapshotEffect(baseInstance.snapshot.refresh),
      streamChanges: baseInstance.snapshot.streamChanges.pipe(
        Stream.map((snap) => enrichHanzoSnapshot(snap, resolvedAuth)),
      ),
    };

    return {
      ...baseInstance,
      driverKind: HANZO_DRIVER_KIND,
      displayName: displayName ?? "Hanzo",
      snapshot: wrappedSnapshot,
      orchestrationAdapter: {
        ...baseInstance.orchestrationAdapter,
        driver: HANZO_DRIVER_KIND,
      },
      snapshotForCwd: (_cwd: string) => wrappedSnapshot.getSnapshot,
    } satisfies ProviderInstance;
  }),
};
