import { msg } from "gt-react";
import { m } from "../i18n/messages";
import { DpopProof } from "@hanzodev/client-runtime/relay";
import { computeDpopJwkThumbprint, DpopPublicJwk } from "@hanzodev/shared/dpop";
import * as Digest from "effect/crypto/Digest";
import * as SecureRandom from "effect/crypto/SecureRandom";
import * as Signature from "effect/crypto/Signature";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Schema from "effect/Schema";

export interface BrowserDpopKey {
  readonly privateKey: CryptoKey;
  readonly publicJwk: DpopPublicJwk;
  readonly thumbprint: string;
}

export class BrowserDpopError extends Schema.TaggedError<BrowserDpopError>()("BrowserDpopError", {
  message: Schema.String,
  cause: Schema.optional(Schema.Defect()),
}) {}

const isBrowserDpopError = Schema.is(BrowserDpopError);

const DPOP_DATABASE_NAME = "hanzodev:cloud-auth";
const DPOP_DATABASE_VERSION = 1;
const DPOP_KEY_STORE_NAME = "keys";
const DPOP_KEY_ID = "relay-dpop-proof-key";
const DPOP_JWK_KEY_ID = "relay-dpop-proof-key-jwk";
const decodeDpopPublicJwk = Schema.decodeUnknownEffect(DpopPublicJwk);

interface StoredJwkDpopKey {
  readonly privateJwk: JsonWebKey;
  readonly publicJwk: DpopPublicJwk;
  readonly thumbprint: string;
}

/** WKWebView wraps IndexedDB CryptoKeys in macOS Keychain, prompting on rebuilt Tauri apps. */
function shouldAvoidIndexedDbCryptoKey(): boolean {
  if (typeof globalThis === "undefined") return false;
  const g = globalThis as Record<string, unknown>;
  if ("__TAURI_INTERNALS__" in g || "__TAURI__" in g) return true;
  if (typeof location !== "undefined" && location.protocol === "tauri:") return true;
  return false;
}

export const layer = Layer.mergeAll(Digest.layerWebCrypto, Signature.layerWebCrypto).pipe(
  Layer.provideMerge(SecureRandom.layerWebCrypto),
);

function dpopError(message: string, cause?: unknown) {
  return new BrowserDpopError({ message, ...(cause === undefined ? {} : { cause }) });
}

function openDpopDatabase(): Effect.Effect<IDBDatabase, BrowserDpopError> {
  return Effect.callback<IDBDatabase, BrowserDpopError>((resume) => {
    const request = indexedDB.open(DPOP_DATABASE_NAME, DPOP_DATABASE_VERSION);
    request.addEventListener("error", () =>
      resume(
        Effect.fail(
          dpopError(m(msg("Could not open DPoP key storage.")), request.error ?? undefined),
        ),
      ),
    );
    request.addEventListener("upgradeneeded", () => {
      if (!request.result.objectStoreNames.contains(DPOP_KEY_STORE_NAME)) {
        request.result.createObjectStore(DPOP_KEY_STORE_NAME);
      }
    });
    request.addEventListener("success", () => resume(Effect.succeed(request.result)));
  });
}

function readStoredEntry<T>(keyId: string): Effect.Effect<T | null, BrowserDpopError> {
  if (typeof indexedDB === "undefined") {
    return Effect.succeed(null);
  }
  return Effect.acquireUseRelease(
    openDpopDatabase(),
    (database) =>
      Effect.callback<T | null, BrowserDpopError>((resume) => {
        const request = database
          .transaction(DPOP_KEY_STORE_NAME, "readonly")
          .objectStore(DPOP_KEY_STORE_NAME)
          .get(keyId);
        request.addEventListener("error", () =>
          resume(
            Effect.fail(dpopError(m(msg("Could not read DPoP key.")), request.error ?? undefined)),
          ),
        );
        request.addEventListener("success", () =>
          resume(Effect.succeed((request.result as T | undefined) ?? null)),
        );
      }),
    (database) => Effect.sync(() => database.close()),
  );
}

function writeStoredEntry(keyId: string, value: unknown): Effect.Effect<void, BrowserDpopError> {
  if (typeof indexedDB === "undefined") {
    return Effect.void;
  }
  return Effect.acquireUseRelease(
    openDpopDatabase(),
    (database) =>
      Effect.callback<void, BrowserDpopError>((resume) => {
        const transaction = database.transaction(DPOP_KEY_STORE_NAME, "readwrite");
        transaction.addEventListener("error", () =>
          resume(
            Effect.fail(
              dpopError(m(msg("Could not write DPoP key.")), transaction.error ?? undefined),
            ),
          ),
        );
        transaction.addEventListener("complete", () => resume(Effect.void));
        transaction.objectStore(DPOP_KEY_STORE_NAME).put(value, keyId);
      }),
    (database) => Effect.sync(() => database.close()),
  );
}

function importDpopPrivateKey(privateJwk: JsonWebKey): Effect.Effect<CryptoKey, BrowserDpopError> {
  return Effect.tryPromise({
    try: () =>
      crypto.subtle.importKey("jwk", privateJwk, { name: "ECDSA", namedCurve: "P-256" }, false, [
        "sign",
      ]),
    catch: (cause) => dpopError(m(msg("Could not import DPoP private key.")), cause),
  });
}

const generateBrowserDpopKeyWithJwk = Effect.gen(function* () {
  const generated = yield* Effect.tryPromise({
    try: () =>
      crypto.subtle.generateKey({ name: "ECDSA", namedCurve: "P-256" }, true, [
        "sign",
        "verify",
      ]) as Promise<CryptoKeyPair>,
    catch: (cause) => dpopError(m(msg("Could not generate DPoP proof key.")), cause),
  });
  const privateJwk = yield* Effect.tryPromise({
    try: () => crypto.subtle.exportKey("jwk", generated.privateKey),
    catch: (cause) => dpopError(m(msg("Could not export DPoP private key.")), cause),
  });
  const publicJwk = yield* Effect.tryPromise({
    try: () => crypto.subtle.exportKey("jwk", generated.publicKey),
    catch: (cause) => dpopError(m(msg("Could not export DPoP public key.")), cause),
  }).pipe(
    Effect.flatMap((jwk) => decodeDpopPublicJwk(jwk)),
    Effect.mapError((cause) =>
      isBrowserDpopError(cause)
        ? cause
        : dpopError(m(msg("Generated DPoP public key is invalid.")), cause),
    ),
  );
  const privateKey = yield* importDpopPrivateKey(privateJwk);
  const thumbprint = computeDpopJwkThumbprint(publicJwk);
  return {
    browserKey: { privateKey, publicJwk, thumbprint } satisfies BrowserDpopKey,
    storedJwk: { privateJwk, publicJwk, thumbprint } satisfies StoredJwkDpopKey,
  };
});

/** The stored key, or a new one persisted on first use, as a DPoP proof key. */
export const loadOrCreateProofKey = Effect.fn("dpop.loadOrCreateProofKey")(function* () {
  const signature = yield* Signature.Signature;
  let browserKey: BrowserDpopKey;
  if (shouldAvoidIndexedDbCryptoKey()) {
    const storedJwk = yield* readStoredEntry<StoredJwkDpopKey>(DPOP_JWK_KEY_ID);
    if (storedJwk !== null) {
      browserKey = {
        privateKey: yield* importDpopPrivateKey(storedJwk.privateJwk),
        publicJwk: storedJwk.publicJwk,
        thumbprint: storedJwk.thumbprint,
      };
    } else {
      const created = yield* generateBrowserDpopKeyWithJwk;
      yield* writeStoredEntry(DPOP_JWK_KEY_ID, created.storedJwk);
      browserKey = created.browserKey;
    }
  } else {
    const stored = yield* readStoredEntry<BrowserDpopKey>(DPOP_KEY_ID);
    if (stored !== null) {
      browserKey = stored;
    } else {
      const created = yield* generateBrowserDpopKeyWithJwk;
      yield* writeStoredEntry(DPOP_KEY_ID, created.browserKey);
      browserKey = created.browserKey;
    }
  }
  const privateKey = yield* Signature.privateKeyFromCryptoKey(
    Signature.ecdsaP256,
    browserKey.privateKey,
  ).pipe(Effect.mapError((cause) => dpopError(m(msg("Could not load DPoP proof key.")), cause)));
  return {
    publicJwk: browserKey.publicJwk,
    sign: (signingInput) =>
      signature.sign(privateKey, signingInput).pipe(
        Effect.mapError(
          (cause) =>
            new DpopProof.DpopProofError({
              message: m(msg("Could not sign DPoP proof.")),
              cause,
            }),
        ),
      ),
  } satisfies DpopProof.DpopProofKey;
});
