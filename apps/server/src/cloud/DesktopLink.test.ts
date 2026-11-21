import { ServerProvider } from "@hanzodev/contracts";
import { assert, it } from "@effect/vitest";
import * as Effect from "effect/Effect";
import * as Base64Url from "effect/encoding/Base64Url";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";
import * as Queue from "effect/Queue";
import * as Schema from "effect/Schema";
import * as TestClock from "effect/testing/TestClock";
import * as HttpClient from "effect/http/HttpClient";
import * as HttpClientResponse from "effect/http/HttpClientResponse";

import * as ServerSecretStore from "../auth/ServerSecretStore.ts";
import * as ProviderRegistry from "../provider/ProviderRegistry.ts";
import * as CliTokenManager from "./CliTokenManager.ts";
import * as DesktopLink from "./DesktopLink.ts";

interface Recorded {
  readonly method: string;
  readonly path: string;
  readonly headers: Readonly<Record<string, string>>;
  readonly body: string;
}

const SECRET_EMAIL = "someone@example.test";
const SECRET_TOKEN = "access-token-secret";

const decodeProvider = Schema.decodeUnknownSync(ServerProvider);

const providers = [
  {
    instanceId: "claude",
    driver: "claude",
    enabled: true,
    installed: true,
    version: "2.0.0",
    status: "ready",
    auth: { status: "authenticated", type: "max", label: "Claude Max", email: SECRET_EMAIL },
    checkedAt: "2026-11-20T10:00:00Z",
    models: [],
  },
  {
    instanceId: "codex",
    driver: "codex",
    enabled: true,
    installed: true,
    version: "1.0.0",
    status: "ready",
    auth: { status: "unauthenticated" },
    checkedAt: "2026-11-20T10:00:00Z",
    models: [],
  },
  {
    instanceId: "cursor",
    driver: "cursor",
    enabled: false,
    installed: true,
    version: "1.0.0",
    status: "disabled",
    auth: { status: "unknown" },
    checkedAt: "2026-11-20T10:00:00Z",
    models: [],
  },
].map((provider) => decodeProvider(provider));

const harness = Effect.gen(function* () {
  const requests = yield* Queue.unbounded<Recorded>();
  const secrets = new Map<string, Uint8Array>();
  const replies = { heartbeat: 200, link: 201 };
  const layer = DesktopLink.layer.pipe(
    Layer.provide(
      Layer.mergeAll(
        Layer.mock(ServerSecretStore.ServerSecretStore)({
          get: (name) => Effect.sync(() => Option.fromNullishOr(secrets.get(name))),
          set: (name, value) => Effect.sync(() => void secrets.set(name, value)),
          remove: (name) => Effect.sync(() => void secrets.delete(name)),
        }),
        Layer.mock(CliTokenManager.CloudCliTokenManager)({
          getExisting: Effect.succeedSome({
            accessToken: SECRET_TOKEN,
            refreshToken: "refresh-token-secret",
            expiresAtEpochMs: 9_999_999_999_999,
            identity: SECRET_EMAIL,
          }),
        }),
        Layer.mock(ProviderRegistry.ProviderRegistry)({ getProviders: Effect.succeed(providers) }),
        Layer.succeed(
          HttpClient.HttpClient,
          HttpClient.make((request) =>
            Effect.gen(function* () {
              const url = new URL(request.url);
              const body =
                request.body._tag === "Uint8Array"
                  ? new TextDecoder().decode(request.body.body)
                  : "";
              yield* Queue.offer(requests, {
                method: request.method,
                path: url.pathname,
                headers: request.headers as Readonly<Record<string, string>>,
                body,
              });
              const heartbeat = url.pathname.endsWith("/heartbeat");
              const status = heartbeat ? replies.heartbeat : replies.link;
              const reply = heartbeat
                ? { interval: 30, level: "metadata", revoked: false }
                : { id: "src_1", interval: 30, level: "metadata" };
              return HttpClientResponse.fromWeb(
                request,
                new Response(status === 410 ? "" : JSON.stringify(reply), { status }),
              );
            }),
          ),
        ),
      ),
    ),
  );
  return { requests, secrets, replies, layer };
});

it.effect("relinking the same install keeps one device key and one id", () =>
  Effect.gen(function* () {
    const { requests, secrets, layer } = yield* harness;
    yield* Effect.gen(function* () {
      const desktopLink = yield* DesktopLink.DesktopLink;
      // The heartbeat loop's first pass runs before anything is linked.
      yield* Effect.yieldNow;
      const first = yield* desktopLink.link({});
      const second = yield* desktopLink.link({});
      assert.equal(first.desktop?.id, "src_1");
      assert.equal(second.desktop?.id, "src_1");
      const linkBodies: Array<{ readonly key: string }> = [];
      for (let index = 0; index < 4; index += 1) {
        const request = yield* Queue.take(requests);
        if (request.path === "/v1/sources") linkBodies.push(JSON.parse(request.body));
      }
      assert.equal(linkBodies.length, 2);
      assert.equal((linkBodies[0] as { readonly kind?: string }).kind, "desktop");
      assert.equal(linkBodies[0]?.key, linkBodies[1]?.key);
      assert.equal(secrets.has("desktop-device-key"), true);
    }).pipe(Effect.provide(layer));
  }),
);

it.effect("a heartbeat reports provider state only and is signed by the device key", () =>
  Effect.gen(function* () {
    const { requests, layer } = yield* harness;
    yield* Effect.gen(function* () {
      const desktopLink = yield* DesktopLink.DesktopLink;
      // The heartbeat loop's first pass runs before anything is linked.
      yield* Effect.yieldNow;
      const status = yield* desktopLink.link({});
      const registration = yield* Queue.take(requests);
      const heartbeat = yield* Queue.take(requests);
      assert.equal(heartbeat.path, "/v1/sources/src_1/heartbeat");
      assert.deepEqual(Object.keys(JSON.parse(heartbeat.body)).toSorted(), [
        "at",
        "busy",
        "id",
        "providers",
        "sig",
        "version",
        "zap",
      ]);
      assert.deepEqual(JSON.parse(heartbeat.body).providers, [
        { provider: "claude", instance: "claude", kind: "login", state: "ready", plan: "max" },
        { provider: "codex", instance: "codex", kind: "login", state: "needs_login" },
      ]);
      assert.notInclude(heartbeat.body, SECRET_EMAIL);
      assert.notInclude(heartbeat.body, SECRET_TOKEN);
      assert.equal(status.report.body, heartbeat.body);
      assert.equal(status.report.sent, true);

      const publicKey = JSON.parse(registration.body).key as string;
      const verifyKey = yield* Effect.promise(() =>
        globalThis.crypto.subtle.importKey(
          "jwk",
          { kty: "OKP", crv: "Ed25519", x: publicKey },
          "Ed25519",
          false,
          ["verify"],
        ),
      );
      const sig = JSON.parse(heartbeat.body).sig as string;
      assert.notInclude(sig, "=");
      const decoded = Base64Url.decode(sig);
      assert.equal(decoded._tag, "Success");
      const signature = decoded._tag === "Success" ? decoded.success : new Uint8Array();
      // Cloud's beatMessage (apps/source/source.go), written out by hand for these fields.
      const expected =
        "hanzo-source-heartbeat/1\n" +
        "src_1\n" +
        "1970-01-01T00:00:00.000Z\n" +
        `${JSON.parse(heartbeat.body).version}\n` +
        "\n" +
        "0\n" +
        "claude,claude,login,ready,max\n" +
        "codex,codex,login,needs_login,\n";
      const verified = yield* Effect.promise(() =>
        globalThis.crypto.subtle.verify(
          "Ed25519",
          verifyKey,
          signature,
          new TextEncoder().encode(expected),
        ),
      );
      assert.equal(verified, true);
    }).pipe(Effect.provide(layer));
  }),
);

it.effect("a 410 on the next heartbeat unlinks and clears the local state", () =>
  Effect.gen(function* () {
    const { requests, secrets, replies, layer } = yield* harness;
    yield* Effect.gen(function* () {
      const desktopLink = yield* DesktopLink.DesktopLink;
      // The heartbeat loop's first pass runs before anything is linked.
      yield* Effect.yieldNow;
      yield* desktopLink.link({});
      yield* Queue.take(requests);
      yield* Queue.take(requests);
      replies.heartbeat = 410;
      yield* TestClock.adjust("30 seconds");
      const heartbeat = yield* Queue.take(requests);
      assert.equal(heartbeat.path, "/v1/sources/src_1/heartbeat");
      const status = yield* desktopLink.status;
      assert.equal(status.desktop, null);
      assert.equal(secrets.has("desktop-link"), false);
      assert.equal(secrets.has("desktop-device-key"), false);
    }).pipe(Effect.provide(layer));
  }),
);

it.effect("unlinking revokes the desktop at Hanzo and forgets the link", () =>
  Effect.gen(function* () {
    const { requests, secrets, layer } = yield* harness;
    yield* Effect.gen(function* () {
      const desktopLink = yield* DesktopLink.DesktopLink;
      // The heartbeat loop's first pass runs before anything is linked.
      yield* Effect.yieldNow;
      yield* desktopLink.link({});
      yield* Queue.take(requests);
      yield* Queue.take(requests);
      const status = yield* desktopLink.unlink;
      const revoke = yield* Queue.take(requests);
      assert.deepInclude(revoke, { method: "DELETE", path: "/v1/sources/src_1" });
      assert.equal(status.desktop, null);
      assert.equal(secrets.has("desktop-link"), false);
    }).pipe(Effect.provide(layer));
  }),
);
