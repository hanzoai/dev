import {
  DEFAULT_SERVER_SETTINGS,
  ProjectId,
  ProviderDriverKind,
  ProviderInstanceId,
  SecretProviderId,
  SecretProviderKind,
  type ServerSettings,
} from "@hanzodev/contracts";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as NodeServices from "@effect/platform-node/NodeServices";
import { it as effectIt } from "@effect/vitest";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Layer from "effect/Layer";
import * as Path from "effect/Path";
import * as Queue from "effect/Queue";
import * as Stream from "effect/Stream";
import { describe, expect, it } from "vite-plus/test";

import * as ServerConfig from "../config.ts";
import { ServerSettingsService } from "../serverSettings.ts";
import {
  DEV_REPLAY_INSTANCE_ID,
  deriveProviderInstanceConfigMap,
  layerSettingsWatcher,
  seedDevReplayInstance,
  seedHanzoCloudDefaultInstance,
  seedInstalledCliProviderInstances,
} from "./ProviderInstanceRegistryHydration.ts";
import { ProviderInstanceRegistryMutator } from "./ProviderInstanceRegistryMutator.ts";

const projectId = ProjectId.make("project-a");
const instanceId = ProviderInstanceId.make("codex_work");

function settingsWith(overrides: Partial<ServerSettings>): ServerSettings {
  return { ...DEFAULT_SERVER_SETTINGS, ...overrides };
}

describe("deriveProviderInstanceConfigMap", () => {
  it("turns a machine-disabled instance on when a project enables it", () => {
    const settings = settingsWith({
      providerInstances: {
        [instanceId]: { driver: ProviderDriverKind.make("codex"), enabled: false },
      },
      projectSettingsOverrides: {
        [projectId]: { providerInstances: { [instanceId]: { enabled: true } } },
      },
    });

    const merged = deriveProviderInstanceConfigMap(settings);

    expect(merged[instanceId]).toEqual({ driver: ProviderDriverKind.make("codex"), enabled: true });
  });

  it("clears a nested config.enabled:false flag when a project enables the instance", () => {
    const settings = settingsWith({
      providerInstances: {
        [instanceId]: {
          driver: ProviderDriverKind.make("codex"),
          config: { enabled: false },
        },
      },
      projectSettingsOverrides: {
        [projectId]: { providerInstances: { [instanceId]: { enabled: true } } },
      },
    });

    const merged = deriveProviderInstanceConfigMap(settings);

    expect(merged[instanceId]).toEqual({
      driver: ProviderDriverKind.make("codex"),
      enabled: true,
      config: { enabled: true },
    });
  });

  it("does not revive an instance a project override references but settings no longer configure", () => {
    const settings = settingsWith({
      providerInstances: {},
      projectSettingsOverrides: {
        [projectId]: { providerInstances: { [instanceId]: { enabled: true } } },
      },
    });

    const merged = deriveProviderInstanceConfigMap(settings);

    expect(merged[instanceId]).toBeUndefined();
  });

  it("ignores a project override that disables an instance, leaving the machine's config in charge", () => {
    const settings = settingsWith({
      providerInstances: {
        [instanceId]: { driver: ProviderDriverKind.make("codex"), enabled: true },
      },
      projectSettingsOverrides: {
        [projectId]: { providerInstances: { [instanceId]: { enabled: false } } },
      },
    });

    const merged = deriveProviderInstanceConfigMap(settings);

    expect(merged[instanceId]).toBe(settings.providerInstances[instanceId]);
  });
});

describe("seedDevReplayInstance", () => {
  const seed = (devUrl: URL | undefined) =>
    Effect.gen(function* () {
      const config = yield* ServerConfig.ServerConfig;
      yield* seedDevReplayInstance().pipe(
        Effect.provideService(ServerConfig.ServerConfig, { ...config, devUrl }),
      );
      const settings = yield* ServerSettingsService;
      return (yield* settings.getSettings).providerInstances[DEV_REPLAY_INSTANCE_ID];
    }).pipe(
      Effect.provide(
        Layer.mergeAll(
          ServerConfig.layerTest(process.cwd(), { prefix: "hanzo-dev-replay-seed-" }),
          ServerSettingsService.layerTest({
            providerInstances: {
              [DEV_REPLAY_INSTANCE_ID]: {
                driver: ProviderDriverKind.make("acpRegistry"),
                displayName: "My replay",
                enabled: false,
                config: { source: "local", commandPath: "/old/node" },
              },
            },
          }),
        ).pipe(Layer.provideMerge(NodeServices.layer)),
      ),
    );

  effectIt.effect("leaves settings alone outside dev", () =>
    seed(undefined).pipe(
      Effect.map((instance) =>
        expect(instance?.config).toEqual({ source: "local", commandPath: "/old/node" }),
      ),
    ),
  );

  effectIt.effect("points a dev server at the replay agent and keeps user edits", () =>
    seed(new URL("http://localhost:5733")).pipe(
      Effect.map((instance) => {
        expect(instance?.displayName).toBe("My replay");
        expect(instance?.enabled).toBe(false);
        expect(instance?.config).toMatchObject({
          source: "local",
          commandArgs: [expect.stringMatching(/scripts[/\\]dev-replay-agent\.ts$/)],
        });
      }),
    ),
  );
});

describe("layerSettingsWatcher", () => {
  const provider = (label: string) => ({
    [SecretProviderId.make("personal")]: {
      kind: SecretProviderKind.make("one_password"),
      label,
      config: { account: "my" },
    },
  });

  const watch = (initial: ServerSettings | undefined) =>
    Effect.gen(function* () {
      const changes = yield* Queue.unbounded<ServerSettings>();
      const reconciled = yield* Queue.unbounded<void>();
      let reloads = 0;
      const mutator = Layer.succeed(ProviderInstanceRegistryMutator, {
        reconcile: () => Queue.offer(reconciled, undefined),
        reloadSecrets: () => Effect.sync(() => void reloads++),
        rebuild: () => Effect.void,
        applySecret: () => Effect.succeed([]),
      });
      yield* Layer.build(
        layerSettingsWatcher(Stream.fromQueue(changes), initial).pipe(Layer.provide(mutator)),
      );
      return {
        reloads: () => reloads,
        // Changes run one at a time, so the repeat's reconcile means the first one's reload decision is done.
        change: (settings: ServerSettings) =>
          Queue.offerAll(changes, [settings, settings]).pipe(
            Effect.andThen(Queue.take(reconciled)),
            Effect.andThen(Queue.take(reconciled)),
          ),
      };
    });

  effectIt.effect(
    "retries unreadable secrets when a secret provider changes, not when it is renamed",
    () =>
      Effect.gen(function* () {
        const settings = yield* ServerSettingsService;
        const { reloads, change } = yield* watch(yield* settings.getSettings);

        yield* change(yield* settings.updateSettings({ secretProviders: provider("Personal") }));
        expect(reloads()).toBe(1);

        // A label is not read by any vault, so renaming a provider retries nothing.
        yield* change(yield* settings.updateSettings({ secretProviders: provider("Renamed") }));
        expect(reloads()).toBe(1);
      }).pipe(Effect.scoped, Effect.provide(ServerSettingsService.layerTest())),
  );

  effectIt.effect(
    "compares a change queued during startup with the settings the registry started from",
    () =>
      Effect.gen(function* () {
        const settings = yield* ServerSettingsService;
        const started = yield* settings.getSettings;
        const saved = yield* settings.updateSettings({ secretProviders: provider("Personal") });
        const { reloads, change } = yield* watch(started);

        yield* change(saved);

        expect(reloads()).toBe(1);
      }).pipe(Effect.scoped, Effect.provide(ServerSettingsService.layerTest())),
  );
});

describe("seedHanzoCloudDefaultInstance", () => {
  effectIt.effect(
    "finds only the unified hanzo CLI, migrates legacy dev binaryPath, and removes hanzoGen",
    () =>
      Effect.gen(function* () {
        const fs = yield* FileSystem.FileSystem;
        const path = yield* Path.Path;
        const tmpHome = yield* fs.makeTempDirectoryScoped({ prefix: "hanzo-seed-home-" });
        const localBin = path.join(tmpHome, ".local", "bin");
        yield* fs.makeDirectory(localBin, { recursive: true });
        const hanzoPath = path.join(localBin, "hanzo");
        yield* fs.writeFileString(hanzoPath, "#!/bin/sh\n");
        yield* fs.writeFileString(path.join(localBin, "dev"), "#!/bin/sh\n");
        yield* fs.writeFileString(path.join(localBin, "gen"), "#!/bin/sh\n");

        yield* seedHanzoCloudDefaultInstance().pipe(
          Effect.provideService(HostProcess.HomeDirectory, tmpHome),
        );

        const settings = yield* (yield* ServerSettingsService).getSettings;
        expect(settings.providerInstances[ProviderInstanceId.make("hanzo")]).toMatchObject({
          driver: "hanzo",
          displayName: "Hanzo",
          enabled: true,
          config: { binaryPath: hanzoPath },
        });
        expect(settings.providerInstances[ProviderInstanceId.make("hanzoGen")]).toBeUndefined();
        expect(settings.defaultModelSelection).toEqual({
          instanceId: ProviderInstanceId.make("hanzo"),
          model: "enso-auto",
        });
      }).pipe(
        Effect.scoped,
        Effect.provide(
          Layer.mergeAll(
            ServerSettingsService.layerTest({
              defaultModelSelection: {
                instanceId: ProviderInstanceId.make("hanzoGen"),
                model: "enso-auto",
              },
              providerInstances: {
                [ProviderInstanceId.make("hanzo")]: {
                  driver: ProviderDriverKind.make("hanzo"),
                  displayName: "Hanzo",
                  enabled: true,
                  config: { setupMode: "existing", binaryPath: "/Users/z/.local/bin/dev" },
                },
                [ProviderInstanceId.make("hanzoGen")]: {
                  driver: ProviderDriverKind.make("hanzoGen"),
                  displayName: "Hanzo Gen",
                  enabled: true,
                  config: { setupMode: "existing", binaryPath: "/tmp/gen" },
                },
              },
            }),
            NodeServices.layer,
          ),
        ),
      ),
  );
});

describe("seedInstalledCliProviderInstances", () => {
  effectIt.effect(
    "auto-discovers and enables installed CLIs (antigravity, opencode, cursor, grok) while respecting explicit disablement",
    () =>
      Effect.gen(function* () {
        const fs = yield* FileSystem.FileSystem;
        const path = yield* Path.Path;
        const tmpHome = yield* fs.makeTempDirectoryScoped({ prefix: "hanzo-cli-discover-" });
        const localBin = path.join(tmpHome, ".local", "bin");
        const opencodeBin = path.join(tmpHome, ".opencode", "bin");
        yield* fs.makeDirectory(localBin, { recursive: true });
        yield* fs.makeDirectory(opencodeBin, { recursive: true });

        const agyPath = path.join(localBin, "agy");
        const opencodePath = path.join(opencodeBin, "opencode");
        const cursorAgentPath = path.join(localBin, "cursor-agent");
        const grokPath = path.join(localBin, "grok");
        const kiroPath = path.join(localBin, "kiro-cli");
        yield* fs.writeFileString(agyPath, "#!/bin/sh\n");
        yield* fs.writeFileString(opencodePath, "#!/bin/sh\n");
        yield* fs.writeFileString(cursorAgentPath, "#!/bin/sh\n");
        yield* fs.writeFileString(grokPath, "#!/bin/sh\n");
        yield* fs.writeFileString(kiroPath, "#!/bin/sh\n");

        yield* seedInstalledCliProviderInstances().pipe(
          Effect.provideService(HostProcess.HomeDirectory, tmpHome),
          Effect.provideService(HostProcess.Environment, { PATH: "" }),
        );

        const settings = yield* (yield* ServerSettingsService).getSettings;
        expect(settings.providerInstances[ProviderInstanceId.make("antigravity")]).toMatchObject({
          driver: "antigravity",
          displayName: "Antigravity",
          enabled: true,
          config: { binaryPath: agyPath },
        });
        expect(settings.providerInstances[ProviderInstanceId.make("opencode")]).toMatchObject({
          driver: "opencode",
          displayName: "OpenCode",
          enabled: true,
          config: { binaryPath: opencodePath },
        });
        expect(settings.providerInstances[ProviderInstanceId.make("cursor")]).toMatchObject({
          driver: "cursor",
          displayName: "Cursor",
          enabled: true,
        });
        expect(settings.providerInstances[ProviderInstanceId.make("grok")]).toMatchObject({
          driver: "grok",
          displayName: "Grok",
          enabled: true,
          config: { binaryPath: grokPath },
        });
        // Explicitly disabled provider stays disabled even when its CLI is installed.
        expect(settings.providerInstances[ProviderInstanceId.make("kiro")]?.enabled).toBe(false);
        // Uninstalled provider is not seeded.
        expect(settings.providerInstances[ProviderInstanceId.make("muse")]).toBeUndefined();
      }).pipe(
        Effect.scoped,
        Effect.provide(
          Layer.mergeAll(
            ServerSettingsService.layerTest({
              providerInstances: {
                [ProviderInstanceId.make("kiro")]: {
                  driver: ProviderDriverKind.make("kiro"),
                  displayName: "Kiro",
                  enabled: false,
                },
              },
            }),
            NodeServices.layer,
          ),
        ),
      ),
  );
});
