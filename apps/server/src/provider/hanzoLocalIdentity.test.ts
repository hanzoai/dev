import { DEFAULT_ENABLED_DEFAULT_INSTANCES, ProviderInstanceId } from "@hanzodev/contracts";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as NodeServices from "@effect/platform-node/NodeServices";
import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Layer from "effect/Layer";
import * as Path from "effect/Path";
import { describe, expect, it } from "vite-plus/test";

import { BUILT_IN_DRIVERS } from "./builtInDrivers.ts";
import {
  CURATED_HANZO_MODELS,
  readLocalHanzoIdentities,
  resolveHanzoProviderAuth,
} from "./hanzoLocalIdentity.ts";
import { orderProviderSnapshots } from "./providerStatusCache.ts";

describe("Native Hanzo Agent & Local IAM Multi-Account Identity", () => {
  it("registers HanzoDriver and HanzoGenDriver as the #1 and #2 built-in drivers", () => {
    expect(BUILT_IN_DRIVERS[0]?.driverKind).toBe("hanzo");
    expect(BUILT_IN_DRIVERS[0]?.metadata.displayName).toBe("Hanzo");
    expect(BUILT_IN_DRIVERS[1]?.driverKind).toBe("hanzoGen");
    expect(BUILT_IN_DRIVERS[1]?.metadata.displayName).toBe("Hanzo Gen");
    expect(DEFAULT_ENABLED_DEFAULT_INSTANCES.has(ProviderInstanceId.make("hanzo"))).toBe(true);
    expect(DEFAULT_ENABLED_DEFAULT_INSTANCES.has(ProviderInstanceId.make("hanzoGen"))).toBe(true);
    expect(CURATED_HANZO_MODELS[0]?.slug).toBe("enso-auto");
    expect(CURATED_HANZO_MODELS.some((m) => m.slug === "zen6-coder")).toBe(true);
  });

  it("orders Hanzo snapshots ahead of Codex and Claude", () => {
    const ordered = orderProviderSnapshots([
      {
        instanceId: ProviderInstanceId.make("claudeAgent"),
        driver: "claudeAgent",
        displayName: "Claude",
        enabled: true,
        installed: true,
        status: "ready",
        checkedAt: "2026-11-19T00:00:00.000Z",
        models: [],
        slashCommands: [],
        skills: [],
      },
      {
        instanceId: ProviderInstanceId.make("codex"),
        driver: "codex",
        displayName: "Codex",
        enabled: true,
        installed: true,
        status: "ready",
        checkedAt: "2026-11-19T00:00:00.000Z",
        models: [],
        slashCommands: [],
        skills: [],
      },
      {
        instanceId: ProviderInstanceId.make("hanzo"),
        driver: "hanzo",
        displayName: "Hanzo",
        enabled: true,
        installed: true,
        status: "ready",
        checkedAt: "2026-11-19T00:00:00.000Z",
        models: CURATED_HANZO_MODELS,
        slashCommands: [],
        skills: [],
      },
    ]);

    expect(ordered.map((p) => p.driver)).toEqual(["hanzo", "codex", "claudeAgent"]);
  });

  it("reads multiple accounts from ~/.hanzo/identities.json and resolves active Hanzo IAM auth", async () => {
    const program = Effect.scoped(
      Effect.gen(function* () {
        const fs = yield* FileSystem.FileSystem;
        const path = yield* Path.Path;
        const tmpHome = yield* fs.makeTempDirectoryScoped({ prefix: "hanzo-identities-test-" });
        const hanzoDir = path.join(tmpHome, ".hanzo");
        yield* fs.makeDirectory(hanzoDir, { recursive: true });
        yield* fs.writeFileString(
          path.join(hanzoDir, "identities.json"),
          JSON.stringify({
            active: "hanzo/z",
            identities: {
              "hanzo/z": {
                owner: "hanzo",
                subject: "z",
                email: "z@hanzo.ai",
                issuer: "https://hanzo.id",
                access_token: "tok_hanzo_z",
                refresh_token: "ref_hanzo_z",
              },
              "admin/z": {
                owner: "admin",
                subject: "z",
                email: "admin@hanzo.ai",
                issuer: "https://hanzo.id",
                access_token: "tok_admin_z",
              },
            },
          }),
        );

        const homeLayer = Layer.succeed(HostProcess.HomeDirectory, tmpHome);
        const snapshot = yield* readLocalHanzoIdentities().pipe(Effect.provide(homeLayer));
        const auth = yield* resolveHanzoProviderAuth().pipe(Effect.provide(homeLayer));
        return { snapshot, auth };
      }),
    ).pipe(Effect.provide(NodeServices.layer));

    const { snapshot, auth } = await Effect.runPromise(program);
    expect(snapshot.activeAccountId).toBe("hanzo/z");
    expect(snapshot.accounts.map((a) => a.id)).toEqual(["hanzo/z", "admin/z"]);
    expect(auth).toEqual({
      status: "authenticated",
      type: "hanzo",
      label: "Hanzo IAM (hanzo/z +1)",
      email: "z@hanzo.ai",
      workspaceId: "hanzo",
    });
  });
});
