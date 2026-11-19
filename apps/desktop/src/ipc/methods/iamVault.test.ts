// @effect-diagnostics nodeBuiltinImport:off -- Uses a temporary directory to test encrypted IAM vault persistence.
import * as NodeFS from "node:fs";
import * as NodeOS from "node:os";
import * as NodePath from "node:path";
import * as NodeServices from "@effect/platform-node/NodeServices";
import { assert, describe, it } from "@effect/vitest";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import { afterAll } from "vite-plus/test";

import * as DesktopConfig from "../../app/DesktopConfig.ts";
import * as DesktopEnvironment from "../../app/DesktopEnvironment.ts";
import * as ElectronSafeStorage from "../../electron/ElectronSafeStorage.ts";
import { readIamVaultFromSafeStorage, writeIamVaultToSafeStorage } from "./iamVault.ts";

const tempDir = NodeFS.mkdtempSync(NodePath.join(NodeOS.tmpdir(), "hanzo-iam-vault-test-"));
afterAll(() => {
  NodeFS.rmSync(tempDir, { recursive: true, force: true });
});

const safeStorageLayer = Layer.succeed(ElectronSafeStorage.ElectronSafeStorage, {
  isEncryptionAvailable: Effect.succeed(true),
  encryptString: (value) =>
    Effect.succeed(new TextEncoder().encode([...value].reverse().join(""))),
  decryptString: (bytes) =>
    Effect.succeed([...new TextDecoder().decode(bytes)].reverse().join("")),
  selectedStorageBackend: Effect.succeedSome("keychain"),
});

const envLayer = DesktopEnvironment.layer({
  dirname: "/repo/apps/desktop/dist-electron",
  homeDirectory: tempDir,
  platform: "darwin",
  processArch: "arm64",
  appVersion: "1.0.0",
  appPath: "/repo",
  isPackaged: true,
  resourcesPath: "/repo/resources",
  runningUnderArm64Translation: false,
}).pipe(Layer.provide(Layer.mergeAll(NodeServices.layer, DesktopConfig.layerTest({}))));

const testLayer = Layer.mergeAll(NodeServices.layer, safeStorageLayer, envLayer);

describe("iamVault OS keychain persistence", () => {
  it.effect("encrypts IAM tokens on disk via safeStorage, decrypts on read, and clears on sign-out", () =>
    Effect.gen(function* () {
      const env = yield* DesktopEnvironment.DesktopEnvironment;
      const sampleJson = JSON.stringify({
        activeAccountId: "hanzo/z",
        activeOrgId: "hanzo",
        accounts: [{ id: "hanzo/z", accessToken: "secret_bearer_token_123" }],
      });

      const saved = yield* writeIamVaultToSafeStorage(sampleJson);
      assert.isTrue(saved);

      const diskPath = NodePath.join(env.stateDir, "iam", "iam-vault.json");
      const rawDisk = NodeFS.readFileSync(diskPath, "utf8");
      assert.notInclude(rawDisk, "secret_bearer_token_123");

      const restored = yield* readIamVaultFromSafeStorage();
      assert.strictEqual(restored, sampleJson);

      const cleared = yield* writeIamVaultToSafeStorage(null);
      assert.isTrue(cleared);
      assert.isNull(yield* readIamVaultFromSafeStorage());
    }).pipe(Effect.provide(testLayer)),
  );
});
