// @effect-diagnostics nodeBuiltinImport:off - The device name and platform come from the OS.
import * as NodeOS from "node:os";

import {
  type Desktop,
  DesktopLinkError,
  type DesktopLinkInput,
  type DesktopLinkStatus,
  type DesktopOutbox,
  type DesktopProviderRow,
  DESKTOP_ROUTABLE_PROVIDERS,
  type DesktopProviderState,
  type ServerProvider,
} from "@hanzodev/contracts";
import { UsageReport, type ProviderSnapshot, type UsageSummary } from "@hanzodev/usage";
import * as Clock from "effect/Clock";
import * as Context from "effect/Context";
import * as DateTime from "effect/DateTime";
import * as Duration from "effect/Duration";
import * as FileSystem from "effect/FileSystem";
import * as Path from "effect/Path";
import * as Effect from "effect/Effect";
import * as Base64Url from "effect/encoding/Base64Url";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";
import * as Ref from "effect/Ref";
import * as Schema from "effect/Schema";
import * as Semaphore from "effect/Semaphore";

import * as HostProcess from "@hanzodev/shared/HostProcess";
import * as ServerSecretStore from "../auth/ServerSecretStore.ts";
import packageJson from "../../package.json" with { type: "json" };
import * as ServerConfig from "../config.ts";
import { catalogId } from "../usage/HostReadings.ts";
import * as ProviderRegistry from "../provider/ProviderRegistry.ts";
import { CLOUD_LINKED_USER_ID } from "./config.ts";
import * as CliTokenManager from "./CliTokenManager.ts";
import * as SourceUsageSummary from "./SourceUsageSummary.ts";
import { fail, makeSourceApi, reject, storageFailure } from "./SourceApi.ts";

const DEVICE_KEY_SECRET = "desktop-device-key";
const LINK_RECORD_SECRET = "desktop-link";
const DEFAULT_HEARTBEAT_INTERVAL_SECONDS = 30;
// HIP-1350: online means a heartbeat in the last 90 seconds.
const ONLINE_WINDOW_MS = 90_000;
const MAX_PLAN_LENGTH = 40;
const DEFAULT_LEVEL = "metadata" as const;
// HIP-1350: at most one usage report per source per 10 s.
const MIN_REPORT_GAP_MS = 10_000;
// An unchanged report is still sent this often, so cloud keeps its snapshots fresh.
const REPORT_REFRESH_MS = 300_000;
const OUTBOX_LIMIT = 100;
// The cost window a report covers; cost rows replace by day, so resending is safe.
const REPORT_WINDOW_MS = 30 * 86_400_000;
const OUTBOX_FILE = "source-outbox.json";
// Hanzo's own providers need no subscription login and are not gated by the share switch.
const OWN_PROVIDERS: ReadonlySet<string> = new Set(["hanzo", "hanzogen", "devreplay"]);
const SHARED_FILE = "source-shared.json";
const USAGE_DOMAIN = "hanzo-source-usage/1";

const OutboxBatchRecord = Schema.Struct({
  seq: Schema.Number,
  at: Schema.String,
  state: Schema.Literals(["pending", "sent", "superseded", "rejected"]),
  body: Schema.String,
});
const OutboxFile = Schema.Struct({
  sendNothing: Schema.Boolean,
  nextSeq: Schema.Number,
  batches: Schema.Array(OutboxBatchRecord),
});
type OutboxFile = typeof OutboxFile.Type;
const OutboxFileJson = Schema.fromJsonString(OutboxFile);
const decodeOutboxFile = Schema.decodeUnknownOption(OutboxFileJson);
const encodeOutboxFile = Schema.encodeSync(OutboxFileJson);
// Only the user's explicit choices are stored; an instance with none takes the default (on).
const SharedFile = Schema.Struct({ choices: Schema.Record(Schema.String, Schema.Boolean) });
const SharedFileJson = Schema.fromJsonString(SharedFile);
const decodeSharedFile = Schema.decodeUnknownOption(SharedFileJson);
const encodeSharedFile = Schema.encodeSync(SharedFileJson);

/** The heartbeat's provider entry: state only, plus whether the user shares it. */
const wireProvider = (row: DesktopProviderRow): DesktopProviderState => ({
  provider: row.provider,
  instance: row.instance,
  kind: row.kind,
  state: row.state,
  ...(row.plan === undefined ? {} : { plan: row.plan }),
  shared: row.shared,
});

const EMPTY_OUTBOX: OutboxFile = { sendNothing: false, nextSeq: 1, batches: [] };
const decodeUsageReport = Schema.decodeUnknownOption(Schema.fromJsonString(UsageReport));

/** A provider's free-text error becomes one of a few fixed codes, so no text leaves the desktop. */
const errorCode = (text: string): string =>
  /sign.?in|log.?in|auth|unauthori[sz]ed|expired|40[13]/i.test(text)
    ? "auth_failed"
    : /rate|limit|429/i.test(text)
      ? "rate_limited"
      : /timeout|timed out|network|fetch|econn|enotfound|offline/i.test(text)
        ? "network_error"
        : "refresh_failed";

const reportSnapshot = (snapshot: ProviderSnapshot): ProviderSnapshot => {
  const { account: _account, error, ...rest } = snapshot;
  return error === undefined ? rest : { ...rest, error: errorCode(error) };
};

/** The usage model's summary as a report: no account, error as a code. */
const usageReport = (id: string, summary: UsageSummary, fromMs: number, toMs: number) =>
  ({
    version: 1,
    deviceId: id,
    from: DateTime.formatIso(DateTime.makeUnsafe(fromMs)),
    to: DateTime.formatIso(DateTime.makeUnsafe(toMs)),
    cost: summary.cost,
    snapshots: summary.snapshots.map(reportSnapshot),
  }) satisfies UsageReport;

/** What `x-hanzo-sig` covers on a usage report; cloud builds the same string. */
const usageMessage = (id: string, bodySha256Hex: string) =>
  `${USAGE_DOMAIN}\n${id}\n${bodySha256Hex}\n`;

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
  shared: Schema.optional(Schema.Boolean),
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
    shared: provider.shared ?? false,
    ...(provider.plan === undefined || provider.plan === "" ? {} : { plan: provider.plan }),
  })),
});

/** The state a heartbeat reports for one provider, or null when it has nothing to report. */
const providerState = (
  provider: ServerProvider,
  choices: Readonly<Record<string, boolean>>,
): DesktopProviderRow | null => {
  if (!provider.enabled || !provider.installed) return null;
  if (provider.availability === "unavailable") return null;
  if (provider.status !== "ready" && provider.status !== "warning") return null;
  const usage = provider.usageLimits;
  const limited =
    usage !== undefined &&
    (usage.zeroSpendCap === true || usage.windows.some((window) => window.usedPercent >= 100));
  const kind = provider.auth.type === "apiKey" ? "key" : "login";
  // A key is never shared with a surface; only a login of a provider whose class includes desktop.
  const shareable =
    kind === "login" &&
    (DESKTOP_ROUTABLE_PROVIDERS as ReadonlyArray<string>).includes(catalogId(provider.driver));
  const plan = kind === "login" ? provider.auth.type?.slice(0, MAX_PLAN_LENGTH) : undefined;
  return {
    provider: catalogId(provider.driver),
    instance: provider.instanceId,
    kind,
    state:
      provider.auth.status === "unauthenticated" ? "needs_login" : limited ? "limited" : "ready",
    ...(plan === undefined ? {} : { plan }),
    shared: shareable && (choices[provider.instanceId] ?? true),
    shareable,
  };
};

const claimOf = (accessToken: string, name: string): string | null => {
  const payload = accessToken.split(".")[1];
  if (payload === undefined) return null;
  const decoded = Base64Url.decodeString(payload);
  if (decoded._tag !== "Success") return null;
  try {
    const claims: unknown = JSON.parse(decoded.success);
    const value =
      typeof claims === "object" && claims !== null && name in claims
        ? (claims as Record<string, unknown>)[name]
        : null;
    return typeof value === "string" && value.length > 0 ? value : null;
  } catch {
    return null;
  }
};

const ownerOf = (accessToken: string): string | null => claimOf(accessToken, "owner");

/** Whether the account Hanzo Link admits is the account this desktop's token belongs to. */
const principalMatches = (linkedUser: string, accessToken: string): boolean => {
  const owner = claimOf(accessToken, "owner");
  const subject = claimOf(accessToken, "sub");
  return (
    subject !== null &&
    (linkedUser === subject || (owner !== null && linkedUser === `${owner}/${subject}`))
  );
};

/** The bytes a heartbeat's `sig` covers; cloud's `beatMessage` builds the same string. */
export const beatMessage = (beat: {
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
    ...beat.providers.map((p) =>
      [p.provider, p.instance, p.kind, p.state, p.plan ?? "", String(p.shared)].join(","),
    ),
  ]
    .map((line) => `${line}\n`)
    .join("");

const iso = (epochMs: number) => DateTime.formatIso(DateTime.makeUnsafe(epochMs));
// Pretty-printed so the Settings view shows the very bytes that are sent.
const encodeBody = (value: unknown) => JSON.stringify(value, null, 2);

const subtle = () => globalThis.crypto.subtle;
const bytesToHex = (bytes: ArrayBuffer) =>
  Array.from(new Uint8Array(bytes), (byte) => byte.toString(16).padStart(2, "0")).join("");
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
    /** The usage reports written to the local outbox, newest first, and the Send nothing switch. */
    readonly outbox: Effect.Effect<DesktopOutbox, DesktopLinkError>;
    /** Share one login instance with the user's other surfaces, or stop. Effective at once. */
    readonly setShared: (
      instance: string,
      shared: boolean,
    ) => Effect.Effect<DesktopLinkStatus, DesktopLinkError>;
    /**
     * Refuses a routed run on a provider instance the user has not shared. Fail closed: only
     * Hanzo's own providers and keys run for another surface without the share switch.
     */
    readonly assertRoutedRun: (instance: string) => Effect.Effect<void, DesktopLinkError>;
    readonly setSendNothing: (
      sendNothing: boolean,
    ) => Effect.Effect<DesktopOutbox, DesktopLinkError>;
  }
>()("hanzo/cloud/DesktopLink") {}

interface Runtime {
  readonly lastReportAtMs: number | null;
  readonly lastHeartbeatAtMs: number | null;
  readonly lastBody: string | null;
  readonly level: DesktopLinkStatus["level"];
  readonly intervalSeconds: number;
}

const make = Effect.gen(function* () {
  const secrets = yield* ServerSecretStore.ServerSecretStore;
  const tokens = yield* CliTokenManager.CloudCliTokenManager;
  const registry = yield* ProviderRegistry.ProviderRegistry;
  const { call, requireToken } = yield* makeSourceApi;
  const platform = yield* HostProcess.Platform;
  const arch = yield* HostProcess.Architecture;
  const version = packageJson.version;
  const lock = yield* Semaphore.make(1);
  const fileSystem = yield* FileSystem.FileSystem;
  const path = yield* Path.Path;
  const config = yield* ServerConfig.ServerConfig;
  const usageSummary = yield* SourceUsageSummary.SourceUsageSummary;
  const usageLock = yield* Semaphore.make(1);
  const deviceCode = yield* Ref.make<DesktopLinkStatus["deviceCode"]>(null);
  const outboxPath = path.join(config.stateDir, OUTBOX_FILE);
  const runtime = yield* Ref.make<Runtime>({
    lastReportAtMs: null,
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
    yield* fileSystem.remove(outboxPath, { force: true });
    yield* Ref.set(runtime, {
      lastReportAtMs: null,
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

  const sharedPath = path.join(config.stateDir, SHARED_FILE);
  const readShared = fileSystem.readFileString(sharedPath).pipe(
    Effect.map((text) => Option.getOrElse(decodeSharedFile(text), () => ({ choices: {} })).choices),
    Effect.catch((cause) =>
      cause.reason._tag === "NotFound"
        ? Effect.succeed<Readonly<Record<string, boolean>>>({})
        : Effect.fail(storageFailure(cause)),
    ),
  );

  const reportedProviders = Effect.gen(function* () {
    const shared = yield* readShared;
    const providers = yield* registry.getProviders;
    return providers.flatMap((provider) => {
      const row = providerState(provider, shared);
      return row === null ? [] : [row];
    });
  });

  // Before the first heartbeat there is no id or signature, so the preview shows the rest.
  const beatPreview = (rows: ReadonlyArray<DesktopProviderRow>) =>
    Clock.currentTimeMillis.pipe(
      Effect.map((now) =>
        encodeBody({ at: iso(now), version, zap: "", busy: 0, providers: rows.map(wireProvider) }),
      ),
    );

  const signedBeat = Effect.fn("DesktopLink.signedBeat")(function* (
    id: string,
    key: typeof DeviceKeyJwk.Type,
    rows: ReadonlyArray<DesktopProviderRow>,
  ) {
    const providers = rows.map(wireProvider);
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
      deviceCode: yield* Ref.get(deviceCode),
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
        const held = yield* tokens.getExisting.pipe(Effect.orElseSucceed(() => Option.none()));
        if (Option.isNone(held) && input.signIn === "browser") {
          yield* tokens.signInWithBrowser.pipe(
            Effect.mapError((cause) =>
              fail("sign_in_required", "Sign-in did not finish in the browser.", cause),
            ),
          );
        }
        if (Option.isNone(held) && input.signIn === "device") {
          yield* tokens
            .signInWithDeviceCode((prompt) =>
              Ref.set(deviceCode, {
                verificationUri: prompt.verificationUriComplete ?? prompt.verificationUri,
                userCode: prompt.userCode,
              }),
            )
            .pipe(
              Effect.ensuring(Ref.set(deviceCode, null)),
              Effect.mapError((cause) =>
                fail("sign_in_required", "Sign-in with a code did not finish.", cause),
              ),
            );
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

  const readOutbox = fileSystem.readFileString(outboxPath).pipe(
    Effect.map((text) => Option.getOrElse(decodeOutboxFile(text), () => EMPTY_OUTBOX)),
    Effect.catch((cause) =>
      cause.reason._tag === "NotFound"
        ? Effect.succeed(EMPTY_OUTBOX)
        : Effect.fail(storageFailure(cause)),
    ),
  );

  // Written to a temporary file and renamed, so a reader never sees half an outbox.
  const writeOutbox = (outbox: OutboxFile) =>
    fileSystem.makeDirectory(config.stateDir, { recursive: true }).pipe(
      Effect.andThen(
        fileSystem.writeFileString(`${outboxPath}.tmp`, encodeOutboxFile(outbox), {
          mode: 0o600,
        }),
      ),
      Effect.andThen(fileSystem.rename(`${outboxPath}.tmp`, outboxPath)),
      Effect.mapError(storageFailure),
    );

  const outboxView = (outbox: OutboxFile): DesktopOutbox => ({
    sendNothing: outbox.sendNothing,
    batches: outbox.batches.toReversed().slice(0, OUTBOX_LIMIT),
  });

  const setState = (
    outbox: OutboxFile,
    seq: number,
    state: OutboxFile["batches"][number]["state"],
  ) => ({
    ...outbox,
    batches: outbox.batches.map((batch) => (batch.seq === seq ? { ...batch, state } : batch)),
  });

  const sendBatch = Effect.fn("DesktopLink.sendBatch")(function* (
    id: string,
    accessToken: string,
    batch: OutboxFile["batches"][number],
  ) {
    const key = yield* readDeviceKey;
    const digest = yield* Effect.tryPromise({
      try: async () =>
        bytesToHex(await subtle().digest("SHA-256", new TextEncoder().encode(batch.body))),
      catch: storageFailure,
    });
    const sig = yield* signBeat(key, usageMessage(id, digest));
    return yield* call({
      method: "POST",
      path: `/v1/sources/${encodeURIComponent(id)}/usage`,
      accessToken,
      body: batch.body,
      headers: { "x-hanzo-sig": sig },
    });
  });

  // Writes the batch to the outbox before sending it; the outbox holds the bytes that were sent.
  const reportUsage = Effect.gen(function* () {
    const record = yield* readRecord;
    if (Option.isNone(record)) return;
    const stored = yield* tokens.getExisting.pipe(Effect.orElseSucceed(() => Option.none()));
    if (Option.isNone(stored)) return;
    let outbox = yield* readOutbox;
    if (outbox.sendNothing) return;
    const now = yield* Clock.currentTimeMillis;
    const state = yield* Ref.get(runtime);
    if (state.lastReportAtMs !== null && now - state.lastReportAtMs < MIN_REPORT_GAP_MS) return;
    const id = record.value.id;
    const newest = outbox.batches.at(-1);
    let batch = newest !== undefined && newest.state === "pending" ? newest : undefined;
    if (batch === undefined) {
      const summary = yield* usageSummary.read.pipe(
        Effect.mapError((cause) => fail("unreachable", "Could not read usage.", cause)),
      );
      const body = encodeBody(usageReport(id, summary, now - REPORT_WINDOW_MS, now));
      const previous = newest === undefined ? Option.none() : decodeUsageReport(newest.body);
      const unchanged =
        Option.isSome(previous) &&
        encodeBody({ cost: previous.value.cost, snapshots: previous.value.snapshots }) ===
          encodeBody({ cost: summary.cost, snapshots: summary.snapshots.map(reportSnapshot) });
      if (unchanged && newest !== undefined && now - Date.parse(newest.at) < REPORT_REFRESH_MS) {
        return;
      }
      batch = { seq: outbox.nextSeq, at: iso(now), state: "pending", body };
      outbox = {
        ...outbox,
        nextSeq: outbox.nextSeq + 1,
        batches: [...outbox.batches, batch].slice(-OUTBOX_LIMIT),
      };
      yield* writeOutbox(outbox);
    }
    yield* Ref.update(runtime, (current) => ({ ...current, lastReportAtMs: now }));
    const response = yield* sendBatch(id, stored.value.accessToken, batch);
    if (response.status === 410) {
      yield* clearLocal;
      return;
    }
    if (response.status === 200) {
      const sent = setState(outbox, batch.seq, "sent");
      yield* writeOutbox({
        ...sent,
        batches: sent.batches.map((entry) =>
          entry.state === "pending" ? { ...entry, state: "superseded" } : entry,
        ),
      });
      return;
    }
    // Cloud refuses a bad payload whole; sending the same bytes again cannot help.
    // 413 means over 1 MiB: the same bytes can never be accepted either.
    if (response.status === 400 || response.status === 413) {
      yield* writeOutbox(setState(outbox, batch.seq, "rejected"));
      return;
    }
    if (response.status !== 429 && response.status !== 409) return yield* reject(response.status);
  }).pipe(Effect.withSpan("DesktopLink.reportUsage"));

  const setShared = Effect.fn("DesktopLink.setShared")(function* (
    instance: string,
    shared: boolean,
  ) {
    const rows = yield* reportedProviders;
    const row = rows.find((candidate) => candidate.instance === instance);
    if (row === undefined || (shared && !row.shareable)) {
      return yield* fail("rejected", "This provider cannot be shared with other surfaces.");
    }
    const current = { ...(yield* readShared), [instance]: shared };
    yield* fileSystem.makeDirectory(config.stateDir, { recursive: true }).pipe(
      Effect.andThen(
        fileSystem.writeFileString(sharedPath, encodeSharedFile({ choices: current }), {
          mode: 0o600,
        }),
      ),
      Effect.mapError(storageFailure),
    );
    // Tell Hanzo now, so a turned-off login stops being offered without waiting for the next beat.
    yield* lock.withPermits(1)(beat).pipe(Effect.ignore);
    return yield* buildStatus;
  });

  // A routed run comes through Hanzo Link, which admits only the account this environment is
  // linked to. It runs here only if that is also the account this desktop is linked as.
  const assertRoutedRun = Effect.fn("DesktopLink.assertRoutedRun")(function* (instance: string) {
    const refuse = () =>
      fail("not_shared", "This provider is not shared with your other surfaces.");
    const record = yield* readRecord;
    const stored = yield* tokens.getExisting.pipe(Effect.orElseSucceed(() => Option.none()));
    const linkedUser = yield* secrets
      .get(CLOUD_LINKED_USER_ID)
      .pipe(Effect.orElseSucceed(() => Option.none<Uint8Array>()));
    if (Option.isNone(record) || Option.isNone(stored) || Option.isNone(linkedUser)) {
      return yield* refuse();
    }
    if (!principalMatches(new TextDecoder().decode(linkedUser.value), stored.value.accessToken)) {
      return yield* refuse();
    }
    const rows = yield* reportedProviders;
    const row = rows.find((candidate) => candidate.instance === instance);
    const open =
      row !== undefined &&
      (row.kind === "key" || OWN_PROVIDERS.has(row.provider) || (row.shareable && row.shared));
    if (!open) return yield* refuse();
  });

  const outboxStatus = readOutbox.pipe(Effect.map(outboxView));

  const setSendNothing = Effect.fn("DesktopLink.setSendNothing")(function* (sendNothing: boolean) {
    return yield* usageLock.withPermits(1)(
      Effect.gen(function* () {
        const outbox = yield* readOutbox;
        const next = { ...outbox, sendNothing };
        yield* writeOutbox(next);
        return outboxView(next);
      }),
    );
  });

  yield* lock
    .withPermits(1)(beat)
    .pipe(
      Effect.catch((error) => Effect.logWarning("Desktop heartbeat failed.", error.message)),
      Effect.andThen(
        usageLock
          .withPermits(1)(reportUsage)
          .pipe(Effect.catch((error) => Effect.logWarning("Usage report failed.", error.message))),
      ),
      Effect.andThen(
        Ref.get(runtime).pipe(
          Effect.flatMap((state) => Effect.sleep(Duration.seconds(state.intervalSeconds))),
        ),
      ),
      Effect.forever,
      Effect.forkScoped,
    );

  return DesktopLink.of({
    outbox: outboxStatus,
    assertRoutedRun,
    setSendNothing,
    setShared,
    status: buildStatus,
    link,
    unlink: unlink(),
    rename,
    list: list(),
    revoke,
  });
});

export const layer = Layer.effect(DesktopLink, make);
