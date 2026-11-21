// @effect-diagnostics nodeBuiltinImport:off - The device name and platform come from the OS.
import * as NodeOS from "node:os";

import {
  type Desktop,
  DesktopLinkError,
  type DesktopLinkInput,
  type DesktopLinkStatus,
  type DesktopProviderState,
  type ServerProvider,
} from "@hanzodev/contracts";
import * as Clock from "effect/Clock";
import * as Context from "effect/Context";
import * as DateTime from "effect/DateTime";
import * as Duration from "effect/Duration";
import * as Effect from "effect/Effect";
import * as Base64Url from "effect/encoding/Base64Url";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";
import * as Ref from "effect/Ref";
import * as Schema from "effect/Schema";
import * as Semaphore from "effect/Semaphore";
import * as HttpClient from "effect/http/HttpClient";
import * as HttpClientRequest from "effect/http/HttpClientRequest";

import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as ServerSecretStore from "../auth/ServerSecretStore.ts";
import packageJson from "../../package.json" with { type: "json" };
import * as ProviderRegistry from "../provider/ProviderRegistry.ts";
import * as CliTokenManager from "./CliTokenManager.ts";
import { apiBaseUrlConfig } from "./publicConfig.ts";

const DEVICE_KEY_SECRET = "desktop-device-key";
const LINK_RECORD_SECRET = "desktop-link";
const DEFAULT_HEARTBEAT_INTERVAL_SECONDS = 30;
// HIP-1350: online means a heartbeat in the last 90 seconds.
const ONLINE_WINDOW_MS = 90_000;
const REQUEST_TIMEOUT = Duration.seconds(10);
const MAX_PLAN_LENGTH = 40;
const DEFAULT_LEVEL = "metadata" as const;

const LinkRecord = Schema.Struct({
  id: Schema.String,
  name: Schema.String,
  linkedAt: Schema.String,
});
type LinkRecord = typeof LinkRecord.Type;
const LinkRecordJson = Schema.fromJsonString(LinkRecord);
const decodeLinkRecord = Schema.decodeUnknownOption(LinkRecordJson);
const encodeLinkRecord = Schema.encodeSync(LinkRecordJson);

const DeviceKeyJwk = Schema.Struct({
  kty: Schema.Literal("OKP"),
  crv: Schema.Literal("Ed25519"),
  x: Schema.String,
  d: Schema.String,
});
const DeviceKeyJwkJson = Schema.fromJsonString(DeviceKeyJwk);
const decodeDeviceKey = Schema.decodeUnknownOption(DeviceKeyJwkJson);
const encodeDeviceKey = Schema.encodeSync(DeviceKeyJwkJson);

const LinkReply = Schema.Struct({
  id: Schema.String,
  interval: Schema.optional(Schema.Number),
  level: Schema.optional(Schema.Literals(["metadata", "metadata+content"])),
});
const BeatReply = Schema.Struct({
  interval: Schema.optional(Schema.Number),
  level: Schema.optional(Schema.Literals(["metadata", "metadata+content"])),
  revoked: Schema.optional(Schema.Boolean),
});
const WireProvider = Schema.Struct({
  provider: Schema.String,
  instance: Schema.String,
  kind: Schema.Literals(["login", "key"]),
  state: Schema.Literals(["ready", "needs_login", "limited"]),
  plan: Schema.optional(Schema.String),
});
const WireDesktop = Schema.Struct({
  id: Schema.String,
  kind: Schema.Literals(["desktop", "browser"]),
  name: Schema.String,
  os: Schema.String,
  arch: Schema.String,
  version: Schema.String,
  state: Schema.Literals(["online", "offline", "revoked"]),
  last_seen: Schema.optional(Schema.String),
  linked_at: Schema.String,
  providers: Schema.optional(Schema.Array(WireProvider)),
});
const WireDesktopList = Schema.Struct({ sources: Schema.Array(WireDesktop) });

const decodeLinkReply = Schema.decodeUnknownEffect(LinkReply);
const decodeBeatReply = Schema.decodeUnknownEffect(BeatReply);
const decodeDesktopList = Schema.decodeUnknownEffect(WireDesktopList);

const toDesktop = (wire: typeof WireDesktop.Type): Desktop => ({
  id: wire.id,
  kind: wire.kind,
  name: wire.name,
  os: wire.os,
  arch: wire.arch,
  version: wire.version,
  state: wire.state,
  lastSeen: wire.last_seen ?? null,
  linkedAt: wire.linked_at,
  providers: (wire.providers ?? []).map((provider) => ({
    provider: provider.provider,
    instance: provider.instance,
    kind: provider.kind,
    state: provider.state,
    ...(provider.plan === undefined || provider.plan === "" ? {} : { plan: provider.plan }),
  })),
});

const fail = (reason: DesktopLinkError["reason"], message: string, cause?: unknown) =>
  new DesktopLinkError({ reason, message, ...(cause === undefined ? {} : { cause }) });

const storageFailure = (cause: unknown) =>
  fail("storage", "Could not read or write this desktop's link on disk.", cause);

const unreachable = (cause: unknown) =>
  fail("unreachable", "Could not reach Hanzo. Check the connection and try again.", cause);

/** The state a heartbeat reports for one provider, or null when it has nothing to report. */
const providerState = (provider: ServerProvider): DesktopProviderState | null => {
  if (!provider.enabled || !provider.installed) return null;
  if (provider.availability === "unavailable") return null;
  if (provider.status !== "ready" && provider.status !== "warning") return null;
  const usage = provider.usageLimits;
  const limited =
    usage !== undefined &&
    (usage.zeroSpendCap === true || usage.windows.some((window) => window.usedPercent >= 100));
  const kind = provider.auth.type === "apiKey" ? "key" : "login";
  const plan = kind === "login" ? provider.auth.type?.slice(0, MAX_PLAN_LENGTH) : undefined;
  return {
    provider: provider.driver,
    instance: provider.instanceId,
    kind,
    state:
      provider.auth.status === "unauthenticated" ? "needs_login" : limited ? "limited" : "ready",
    ...(plan === undefined ? {} : { plan }),
  };
};

const ownerOf = (accessToken: string): string | null => {
  const payload = accessToken.split(".")[1];
  if (payload === undefined) return null;
  const decoded = Base64Url.decodeString(payload);
  if (decoded._tag !== "Success") return null;
  try {
    const claims: unknown = JSON.parse(decoded.success);
    const owner =
      typeof claims === "object" && claims !== null && "owner" in claims ? claims.owner : null;
    return typeof owner === "string" && owner.length > 0 ? owner : null;
  } catch {
    return null;
  }
};

/** The bytes a heartbeat's `sig` covers; cloud's `beatMessage` builds the same string. */
const beatMessage = (beat: {
  readonly id: string;
  readonly at: string;
  readonly version: string;
  readonly zap: string;
  readonly busy: number;
  readonly providers: ReadonlyArray<DesktopProviderState>;
}) =>
  [
    "hanzo-source-heartbeat/1",
    beat.id,
    beat.at,
    beat.version,
    beat.zap,
    String(beat.busy),
    ...beat.providers.map((p) => [p.provider, p.instance, p.kind, p.state, p.plan ?? ""].join(",")),
  ]
    .map((line) => `${line}\n`)
    .join("");

const iso = (epochMs: number) => DateTime.formatIso(DateTime.makeUnsafe(epochMs));
// Pretty-printed so the Settings view shows the very bytes that are sent.
const encodeBody = (value: unknown) => JSON.stringify(value, null, 2);

const subtle = () => globalThis.crypto.subtle;
export class DesktopLink extends Context.Service<
  DesktopLink,
  {
    readonly status: Effect.Effect<DesktopLinkStatus, DesktopLinkError>;
    /** Registers this install, idempotently on its device key. */
    readonly link: (input: DesktopLinkInput) => Effect.Effect<DesktopLinkStatus, DesktopLinkError>;
    readonly unlink: Effect.Effect<DesktopLinkStatus, DesktopLinkError>;
    readonly rename: (name: string) => Effect.Effect<DesktopLinkStatus, DesktopLinkError>;
    readonly list: Effect.Effect<ReadonlyArray<Desktop>, DesktopLinkError>;
    readonly revoke: (id: string) => Effect.Effect<void, DesktopLinkError>;
  }
>()("hanzo/cloud/DesktopLink") {}

interface Runtime {
  readonly lastHeartbeatAtMs: number | null;
  readonly lastBody: string | null;
  readonly level: DesktopLinkStatus["level"];
  readonly intervalSeconds: number;
}

const make = Effect.gen(function* () {
  const secrets = yield* ServerSecretStore.ServerSecretStore;
  const tokens = yield* CliTokenManager.CloudCliTokenManager;
  const registry = yield* ProviderRegistry.ProviderRegistry;
  const httpClient = yield* HttpClient.HttpClient;
  const apiBaseUrl = yield* apiBaseUrlConfig.pipe(Effect.orDie);
  const platform = yield* HostProcess.Platform;
  const arch = yield* HostProcess.Architecture;
  const version = packageJson.version;
  const lock = yield* Semaphore.make(1);
  const runtime = yield* Ref.make<Runtime>({
    lastHeartbeatAtMs: null,
    lastBody: null,
    level: DEFAULT_LEVEL,
    intervalSeconds: DEFAULT_HEARTBEAT_INTERVAL_SECONDS,
  });

  const readRecord = secrets.get(LINK_RECORD_SECRET).pipe(
    Effect.map((bytes) =>
      Option.flatMap(bytes, (value) => decodeLinkRecord(new TextDecoder().decode(value))),
    ),
    Effect.mapError(storageFailure),
  );

  const writeRecord = (record: LinkRecord) =>
    secrets
      .set(LINK_RECORD_SECRET, new TextEncoder().encode(encodeLinkRecord(record)))
      .pipe(Effect.mapError(storageFailure));

  const clearLocal = Effect.gen(function* () {
    yield* secrets.remove(LINK_RECORD_SECRET);
    yield* secrets.remove(DEVICE_KEY_SECRET);
    yield* Ref.set(runtime, {
      lastHeartbeatAtMs: null,
      lastBody: null,
      level: DEFAULT_LEVEL,
      intervalSeconds: DEFAULT_HEARTBEAT_INTERVAL_SECONDS,
    });
  }).pipe(Effect.mapError(storageFailure), Effect.withSpan("DesktopLink.clearLocal"));

  const readDeviceKey = Effect.gen(function* () {
    const stored = yield* secrets.get(DEVICE_KEY_SECRET).pipe(Effect.mapError(storageFailure));
    if (Option.isSome(stored)) {
      const key = decodeDeviceKey(new TextDecoder().decode(stored.value));
      if (Option.isSome(key)) return key.value;
    }
    const generated = yield* Effect.tryPromise({
      try: async () => {
        const pair = await subtle().generateKey("Ed25519", true, ["sign", "verify"]);
        if (!("privateKey" in pair)) return null;
        return await subtle().exportKey("jwk", pair.privateKey);
      },
      catch: storageFailure,
    });
    const key = decodeDeviceKey(encodeBody(generated));
    if (Option.isNone(key)) {
      return yield* storageFailure("The platform produced an unexpected device key.");
    }
    yield* secrets
      .set(DEVICE_KEY_SECRET, new TextEncoder().encode(encodeDeviceKey(key.value)))
      .pipe(Effect.mapError(storageFailure));
    return key.value;
  }).pipe(Effect.withSpan("DesktopLink.readDeviceKey"));

  const signBeat = Effect.fn("DesktopLink.signBeat")(function* (
    key: typeof DeviceKeyJwk.Type,
    message: string,
  ) {
    const signature = yield* Effect.tryPromise({
      try: async () => {
        const privateKey = await subtle().importKey("jwk", key, "Ed25519", false, ["sign"]);
        return new Uint8Array(
          await subtle().sign("Ed25519", privateKey, new TextEncoder().encode(message)),
        );
      },
      catch: storageFailure,
    });
    return Base64Url.encode(signature);
  });

  /** One call to the registry. The status code is the caller's to read, the body is JSON. */
  const call = Effect.fn("DesktopLink.call")(function* (input: {
    readonly method: "GET" | "POST" | "DELETE";
    readonly path: string;
    readonly accessToken: string;
    readonly body?: string;
  }) {
    const url = `${apiBaseUrl}${input.path}`;
    const base =
      input.method === "GET"
        ? HttpClientRequest.get(url)
        : input.method === "POST"
          ? HttpClientRequest.post(url)
          : HttpClientRequest.delete(url);
    const request = base.pipe(
      HttpClientRequest.bearerToken(input.accessToken),
      input.body === undefined
        ? (self) => self
        : HttpClientRequest.bodyText(input.body, "application/json"),
    );
    return yield* httpClient.execute(request).pipe(
      Effect.flatMap((response) =>
        response.text.pipe(
          Effect.map((text) => ({
            status: response.status,
            json: ((): unknown => {
              try {
                return text.length === 0 ? null : JSON.parse(text);
              } catch {
                return null;
              }
            })(),
          })),
        ),
      ),
      Effect.timeout(REQUEST_TIMEOUT),
      Effect.mapError(unreachable),
    );
  });

  const reject = (status: number) =>
    status === 401
      ? fail("sign_in_required", "Hanzo did not accept this sign-in. Sign in again.")
      : status === 409
        ? fail("limit", "This account has reached its limit of linked desktops. Revoke one first.")
        : fail("rejected", `Hanzo refused the request (${status}).`);

  const requireToken = tokens.getExisting.pipe(
    Effect.mapError(() => fail("sign_in_required", "Sign in to Hanzo to link this desktop.")),
    Effect.flatMap(
      Option.match({
        onNone: () =>
          Effect.fail(fail("sign_in_required", "Sign in to Hanzo to link this desktop.")),
        onSome: Effect.succeed,
      }),
    ),
  );

  const reportedProviders = registry.getProviders.pipe(
    Effect.map((providers) =>
      providers.flatMap((provider) => {
        const state = providerState(provider);
        return state === null ? [] : [state];
      }),
    ),
  );

  // Before the first heartbeat there is no id or signature, so the preview shows the rest.
  const beatPreview = (providers: ReadonlyArray<DesktopProviderState>) =>
    Clock.currentTimeMillis.pipe(
      Effect.map((now) => encodeBody({ at: iso(now), version, zap: "", busy: 0, providers })),
    );

  const signedBeat = Effect.fn("DesktopLink.signedBeat")(function* (
    id: string,
    key: typeof DeviceKeyJwk.Type,
    providers: ReadonlyArray<DesktopProviderState>,
  ) {
    const at = iso(yield* Clock.currentTimeMillis);
    const beat = { id, at, version, zap: "", busy: 0, providers };
    const sig = yield* signBeat(key, beatMessage(beat));
    return encodeBody({ ...beat, sig });
  });

  const buildStatus = Effect.gen(function* () {
    const record = yield* readRecord;
    const stored = yield* tokens.getExisting.pipe(Effect.orElseSucceed(() => Option.none()));
    const now = yield* Clock.currentTimeMillis;
    const state = yield* Ref.get(runtime);
    const providers = yield* reportedProviders;
    const sent = state.lastBody !== null;
    const desktop: Desktop | null = Option.isNone(record)
      ? null
      : {
          id: record.value.id,
          kind: "desktop",
          name: record.value.name,
          os: platform,
          arch: arch,
          version,
          state:
            state.lastHeartbeatAtMs !== null && now - state.lastHeartbeatAtMs <= ONLINE_WINDOW_MS
              ? "online"
              : "offline",
          lastSeen: state.lastHeartbeatAtMs === null ? null : iso(state.lastHeartbeatAtMs),
          linkedAt: record.value.linkedAt,
          providers,
        };
    return {
      signedIn: Option.isSome(stored),
      identity: Option.isSome(stored) ? (stored.value.identity ?? null) : null,
      organization: Option.isSome(stored) ? ownerOf(stored.value.accessToken) : null,
      desktop,
      lastHeartbeatAt: state.lastHeartbeatAtMs === null ? null : iso(state.lastHeartbeatAtMs),
      level: state.level,
      providers,
      report: { body: state.lastBody ?? (yield* beatPreview(providers)), sent },
    } satisfies DesktopLinkStatus;
  });

  const beat = Effect.gen(function* () {
    const record = yield* readRecord;
    if (Option.isNone(record)) return;
    const stored = yield* tokens.getExisting.pipe(Effect.orElseSucceed(() => Option.none()));
    if (Option.isNone(stored)) return;
    const key = yield* readDeviceKey;
    const body = yield* signedBeat(record.value.id, key, yield* reportedProviders);
    const response = yield* call({
      method: "POST",
      path: `/v1/sources/${encodeURIComponent(record.value.id)}/heartbeat`,
      accessToken: stored.value.accessToken,
      body,
    });
    const now = yield* Clock.currentTimeMillis;
    if (response.status === 410) {
      yield* clearLocal;
      return;
    }
    // 409 and 429 mean this beat was not newer or came too soon; the next one is fine.
    if (response.status === 409 || response.status === 429) return;
    if (response.status !== 200) return yield* reject(response.status);
    yield* Ref.update(runtime, (state) => ({ ...state, lastHeartbeatAtMs: now, lastBody: body }));
    const reply = yield* decodeBeatReply(response.json).pipe(
      Effect.mapError((cause) => fail("rejected", "Hanzo sent an unreadable reply.", cause)),
    );
    if (reply.revoked === true) {
      yield* clearLocal;
      return;
    }
    yield* Ref.update(runtime, (state) => ({
      ...state,
      level: reply.level ?? state.level,
      intervalSeconds: reply.interval ?? state.intervalSeconds,
    }));
  }).pipe(Effect.withSpan("DesktopLink.beat"));

  const register = Effect.fn("DesktopLink.register")(function* (name: string, accessToken: string) {
    const key = yield* readDeviceKey;
    const response = yield* call({
      method: "POST",
      path: "/v1/sources",
      accessToken,
      body: encodeBody({
        kind: "desktop",
        name,
        os: platform,
        arch,
        version,
        key: key.x,
      }),
    });
    if (response.status !== 200 && response.status !== 201) {
      return yield* reject(response.status);
    }
    const reply = yield* decodeLinkReply(response.json).pipe(
      Effect.mapError((cause) => fail("rejected", "Hanzo sent an unreadable reply.", cause)),
    );
    return reply;
  });

  const link = Effect.fn("DesktopLink.link")(function* (input: DesktopLinkInput) {
    return yield* lock.withPermits(1)(
      Effect.gen(function* () {
        if (input.token !== undefined) {
          yield* tokens
            .store({
              accessToken: input.token.accessToken,
              refreshToken: input.token.refreshToken,
              expiresAtEpochMs: input.token.expiresAtEpochMs,
            })
            .pipe(Effect.mapError(storageFailure));
        }
        const stored = yield* requireToken;
        const existing = yield* readRecord;
        const name = Option.isSome(existing) ? existing.value.name : NodeOS.hostname();
        const reply = yield* register(name, stored.accessToken);
        const now = yield* Clock.currentTimeMillis;
        yield* writeRecord({
          id: reply.id,
          name,
          linkedAt:
            Option.isSome(existing) && existing.value.id === reply.id
              ? existing.value.linkedAt
              : iso(now),
        });
        yield* Ref.update(runtime, (state) => ({
          ...state,
          level: reply.level ?? state.level,
          intervalSeconds: reply.interval ?? state.intervalSeconds,
        }));
        yield* beat;
        return yield* buildStatus;
      }),
    );
  });

  const revokeRemote = Effect.fn("DesktopLink.revokeRemote")(function* (id: string) {
    const stored = yield* requireToken;
    const response = yield* call({
      method: "DELETE",
      path: `/v1/sources/${encodeURIComponent(id)}`,
      accessToken: stored.accessToken,
    });
    // A desktop that is already revoked or gone is the state the caller asked for.
    if (response.status !== 404 && response.status !== 410 && !(response.status < 300)) {
      return yield* reject(response.status);
    }
  });

  const unlink = Effect.fn("DesktopLink.unlink")(function* () {
    return yield* lock.withPermits(1)(
      Effect.gen(function* () {
        const record = yield* readRecord;
        if (Option.isSome(record)) {
          yield* revokeRemote(record.value.id);
          yield* clearLocal;
        }
        return yield* buildStatus;
      }),
    );
  });

  const rename = Effect.fn("DesktopLink.rename")(function* (name: string) {
    return yield* lock.withPermits(1)(
      Effect.gen(function* () {
        const record = yield* readRecord;
        if (Option.isNone(record)) {
          return yield* fail("not_linked", "This desktop is not linked to Hanzo.");
        }
        const stored = yield* requireToken;
        // Registering again is idempotent on the device key and carries the new name.
        const reply = yield* register(name, stored.accessToken);
        yield* writeRecord({ ...record.value, id: reply.id, name });
        return yield* buildStatus;
      }),
    );
  });

  const list = Effect.fn("DesktopLink.list")(function* () {
    const stored = yield* requireToken;
    const response = yield* call({
      method: "GET",
      path: "/v1/sources",
      accessToken: stored.accessToken,
    });
    if (response.status !== 200) return yield* reject(response.status);
    const wire = yield* decodeDesktopList(response.json).pipe(
      Effect.mapError((cause) => fail("rejected", "Hanzo sent an unreadable reply.", cause)),
    );
    return wire.sources.map(toDesktop);
  });

  const revoke = Effect.fn("DesktopLink.revoke")(function* (id: string) {
    yield* lock.withPermits(1)(
      Effect.gen(function* () {
        yield* revokeRemote(id);
        const record = yield* readRecord;
        if (Option.isSome(record) && record.value.id === id) yield* clearLocal;
      }),
    );
  });

  yield* lock
    .withPermits(1)(beat)
    .pipe(
      Effect.catch((error) => Effect.logWarning("Desktop heartbeat failed.", error.message)),
      Effect.andThen(
        Ref.get(runtime).pipe(
          Effect.flatMap((state) => Effect.sleep(Duration.seconds(state.intervalSeconds))),
        ),
      ),
      Effect.forever,
      Effect.forkScoped,
    );

  return DesktopLink.of({
    status: buildStatus,
    link,
    unlink: unlink(),
    rename,
    list: list(),
    revoke,
  });
});

export const layer = Layer.effect(DesktopLink, make);
