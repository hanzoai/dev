import * as Effect from "effect/Effect";
import * as FileSystem from "effect/FileSystem";
import * as Schema from "effect/Schema";

import * as DesktopEnvironment from "../../app/DesktopEnvironment.ts";
import * as ElectronSafeStorage from "../../electron/ElectronSafeStorage.ts";
import * as IpcChannels from "../channels.ts";
import * as DesktopIpc from "../DesktopIpc.ts";

const IAM_VAULT_DIRECTORY_NAME = "iam";
const IAM_VAULT_FILE_NAME = "iam-vault.json";

const StoredIamVaultEnvelope = Schema.Struct({
  version: Schema.Literal(1),
  encrypted: Schema.Boolean,
  payload: Schema.String,
});
const StoredIamVaultEnvelopeJson = Schema.fromJsonString(StoredIamVaultEnvelope);
const decodeEnvelope = Schema.decodeEffect(StoredIamVaultEnvelopeJson);
const encodeEnvelope = Schema.encodeEffect(StoredIamVaultEnvelopeJson);

export const readIamVaultFromSafeStorage = Effect.fn("desktop.ipc.iamVault.read")(function* () {
  const environment = yield* DesktopEnvironment.DesktopEnvironment;
  const fs = yield* FileSystem.FileSystem;
  const safeStorage = yield* ElectronSafeStorage.ElectronSafeStorage;
  const vaultPath = environment.path.join(
    environment.stateDir,
    IAM_VAULT_DIRECTORY_NAME,
    IAM_VAULT_FILE_NAME,
  );

  const exists = yield* fs.exists(vaultPath).pipe(Effect.orElseSucceed(() => false));
  if (!exists) return null;

  const raw = yield* fs.readFileString(vaultPath).pipe(Effect.orElseSucceed(() => ""));
  if (!raw.trim()) return null;

  const parsed = yield* decodeEnvelope(raw).pipe(Effect.orElseSucceed(() => null));
  if (!parsed) return null;

  if (!parsed.encrypted) {
    return parsed.payload;
  }

  const encryptedBytes = new Uint8Array(Buffer.from(parsed.payload, "base64"));
  if (encryptedBytes.byteLength === 0) return null;

  return yield* safeStorage.decryptString(encryptedBytes).pipe(Effect.orElseSucceed(() => null));
});

export const writeIamVaultToSafeStorage = Effect.fn("desktop.ipc.iamVault.write")(function* (
  vaultJson: string | null,
) {
  const environment = yield* DesktopEnvironment.DesktopEnvironment;
  const fs = yield* FileSystem.FileSystem;
  const safeStorage = yield* ElectronSafeStorage.ElectronSafeStorage;
  const dirPath = environment.path.join(environment.stateDir, IAM_VAULT_DIRECTORY_NAME);
  const vaultPath = environment.path.join(dirPath, IAM_VAULT_FILE_NAME);

  if (vaultJson === null || !vaultJson.trim()) {
    yield* fs.remove(vaultPath, { force: true }).pipe(Effect.ignore);
    return true;
  }

  const encryptionAvailable = yield* safeStorage.isEncryptionAvailable.pipe(
    Effect.orElseSucceed(() => false),
  );
  const envelope = encryptionAvailable
    ? yield* safeStorage.encryptString(vaultJson).pipe(
        Effect.map((bytes) => ({
          version: 1 as const,
          encrypted: true,
          payload: Buffer.from(bytes).toString("base64"),
        })),
        Effect.orElseSucceed(() => ({
          version: 1 as const,
          encrypted: false,
          payload: vaultJson,
        })),
      )
    : {
        version: 1 as const,
        encrypted: false,
        payload: vaultJson,
      };

  const encoded = yield* encodeEnvelope(envelope).pipe(Effect.orElseSucceed(() => null));
  if (encoded === null) return false;

  const ensured = yield* fs
    .makeDirectory(dirPath, { recursive: true, mode: 0o700 })
    .pipe(Effect.as(true), Effect.orElseSucceed(() => false));
  if (!ensured) return false;

  return yield* fs
    .writeFileString(vaultPath, encoded, { mode: 0o600 })
    .pipe(Effect.as(true), Effect.orElseSucceed(() => false));
});

export const getIamVault = DesktopIpc.makeIpcMethod({
  channel: IpcChannels.GET_IAM_VAULT_CHANNEL,
  payload: Schema.Void,
  result: Schema.NullOr(Schema.String),
  handler: () => readIamVaultFromSafeStorage(),
});

export const setIamVault = DesktopIpc.makeIpcMethod({
  channel: IpcChannels.SET_IAM_VAULT_CHANNEL,
  payload: Schema.NullOr(Schema.String),
  result: Schema.Boolean,
  handler: (vaultJson) => writeIamVaultToSafeStorage(vaultJson),
});
