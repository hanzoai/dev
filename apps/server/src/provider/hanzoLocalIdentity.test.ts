import { ProviderDriverKind, ProviderInstanceId } from "@hanzodev/contracts";
import { DEFAULT_ENABLED_DEFAULT_INSTANCES } from "@hanzodev/contracts/settings";
import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as NodeServices from "@effect/platform-node/NodeServices";
import { it as effectIt } from "@effect/vitest";
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
  it("registers HanzoDriver as default #1 and hides HanzoGenDriver from default instances", () => {
    expect(BUILT_IN_DRIVERS[0]?.driverKind).toBe("hanzo");
    expect(BUILT_IN_DRIVERS[0]?.metadata.displayName).toBe("Hanzo");
    expect(BUILT_IN_DRIVERS[1]?.driverKind).toBe("hanzoGen");
    expect(BUILT_IN_DRIVERS[1]?.metadata.displayName).toBe("Hanzo Gen");
    expect(BUILT_IN_DRIVERS[1]?.metadata.hasDefaultInstance).toBe(false);
    expect(DEFAULT_ENABLED_DEFAULT_INSTANCES.has(ProviderInstanceId.make("hanzo"))).toBe(true);
    expect(DEFAULT_ENABLED_DEFAULT_INSTANCES.has(ProviderInstanceId.make("hanzoGen"))).toBe(false);
    expect(CURATED_HANZO_MODELS[0]?.slug).toBe("enso-auto");
    expect(CURATED_HANZO_MODELS.some((m) => m.slug === "zen6-coder")).toBe(true);
  });

  it("orders Hanzo snapshots ahead of Codex and Claude", () => {
    const ordered = orderProviderSnapshots([
      {
        instanceId: ProviderInstanceId.make("claudeAgent"),
        driver: ProviderDriverKind.make("claudeAgent"),
        displayName: "Claude",
        enabled: true,
        installed: true,
        version: "1.0.0",
        status: "ready",
        auth: { status: "authenticated" },
        checkedAt: "2026-11-19T00:00:00.000Z",
        models: [],
        slashCommands: [],
        skills: [],
      },
      {
        instanceId: ProviderInstanceId.make("codex"),
        driver: ProviderDriverKind.make("codex"),
        displayName: "Codex",
        enabled: true,
        installed: true,
        version: "1.0.0",
        status: "ready",
        auth: { status: "authenticated" },
        checkedAt: "2026-11-19T00:00:00.000Z",
        models: [],
        slashCommands: [],
        skills: [],
      },
      {
        instanceId: ProviderInstanceId.make("hanzo"),
        driver: ProviderDriverKind.make("hanzo"),
        displayName: "Hanzo",
        enabled: true,
        installed: true,
        version: "1.0.0",
        status: "ready",
        auth: { status: "authenticated" },
        checkedAt: "2026-11-19T00:00:00.000Z",
        models: CURATED_HANZO_MODELS,
        slashCommands: [],
        skills: [],
      },
    ]);

    expect(ordered.map((p) => p.driver)).toEqual(["hanzo", "codex", "claudeAgent"]);
  });

  effectIt.effect("reads the machine vault and resolves the active Hanzo IAM auth", () =>
    Effect.gen(function* () {
      const fs = yield* FileSystem.FileSystem;
      const path = yield* Path.Path;
      const tmpHome = yield* fs.makeTempDirectoryScoped({ prefix: "hanzo-identities-test-" });
      const hanzoDir = path.join(tmpHome, ".local", "share", "hanzo");
      yield* fs.makeDirectory(hanzoDir, { recursive: true });
      const jwt = (claims: object) =>
        `${Buffer.from("{}").toString("base64url")}.${Buffer.from(JSON.stringify(claims)).toString("base64url")}.s`;
      const set = (claims: object) =>
        JSON.stringify({ access_token: jwt(claims), refresh_token: "r" });
      yield* fs.writeFileString(
        path.join(hanzoDir, "credentials"),
        JSON.stringify({
          "hanzo/hanzo/z": set({ owner: "hanzo", name: "z", email: "z@hanzo.ai" }),
          "hanzo/admin/z": set({ owner: "admin", name: "z", email: "z@hanzo.ai" }),
        }),
      );
      yield* fs.writeFileString(
        path.join(hanzoDir, "config.toml"),
        '[auth.active]\nhanzo = "hanzo/z"\n',
      );

      const layer = Layer.mergeAll(
        Layer.succeed(HostProcess.HomeDirectory, tmpHome),
        Layer.succeed(HostProcess.Platform, "linux"),
        Layer.succeed(HostProcess.Environment, {}),
      );
      const snapshot = yield* readLocalHanzoIdentities().pipe(Effect.provide(layer));
      const auth = yield* resolveHanzoProviderAuth().pipe(Effect.provide(layer));

      expect(snapshot.activeAccountId).toBe("hanzo/z");
      expect(snapshot.accounts.map((a) => a.id)).toEqual(["hanzo/z", "admin/z"]);
      expect(auth).toEqual({
        status: "authenticated",
        type: "hanzo",
        label: "Hanzo IAM (hanzo/z +1)",
        email: "z@hanzo.ai",
        workspaceId: "hanzo",
      });
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  );
});
