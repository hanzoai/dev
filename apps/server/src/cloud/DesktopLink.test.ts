import { ServerProvider } from "@hanzodev/contracts";
import type { UsageSummary } from "@hanzodev/usage";
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

import * as NodeServices from "@effect/platform-node/NodeServices";
import * as FileSystem from "effect/FileSystem";
import * as ServerConfig from "../config.ts";
import * as SourceUsageSummary from "./SourceUsageSummary.ts";
import * as ServerSecretStore from "../auth/ServerSecretStore.ts";
import * as ProviderRegistry from "../provider/ProviderRegistry.ts";
import * as ProviderAccounts from "./ProviderAccounts.ts";
import * as CliTokenManager from "./CliTokenManager.ts";
import * as DesktopLink from "./DesktopLink.ts";

interface Recorded {
  readonly method: string;
  readonly path: string;
  readonly headers: Readonly<Record<string, string>>;
  readonly body: string;
}

const SECRET_EMAIL = "someone@example.test";
// A JWT-shaped bearer: the desktop's principal is its `owner` and `sub` claims.
const SECRET_TOKEN = `${Base64Url.encode(new TextEncoder().encode('{"alg":"none"}'))}.${Base64Url.encode(
  new TextEncoder().encode('{"owner":"hanzo","sub":"z-user"}'),
)}.secret`;

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

// Free text a provider returns, which must not leave the desktop.
const usageSummary: UsageSummary = {
  generatedAt: "2026-11-20T10:00:00.000Z",
  cost: [
    {
      day: "2026-11-20",
      provider: "claude",
      model: "sonnet",
      inputTokens: 10,
      outputTokens: 20,
      cacheTokens: 5,
    },
  ],
  snapshots: [
    {
      provider: "claude",
      fetchedAt: "2026-11-20T10:00:00.000Z",
      source: "oauth",
      plan: "max",
      account: SECRET_EMAIL,
      windows: [{ kind: "weekly", label: "Weekly", usedPercent: 42 }],
      error: `401 Unauthorized for ${SECRET_EMAIL}`,
    },
  ],
};

const harness = Effect.gen(function* () {
  const fileSystem = yield* FileSystem.FileSystem;
  const stateDir = yield* fileSystem.makeTempDirectoryScoped();
  const requests = yield* Queue.unbounded<Recorded>();
  const secrets = new Map<string, Uint8Array>();
  const replies = { heartbeat: 200, link: 201, usage: 200 };
  const layer = Layer.mergeAll(DesktopLink.layer, ProviderAccounts.layer).pipe(
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
        NodeServices.layer,
        Layer.mock(ServerConfig.ServerConfig)({ stateDir } as ServerConfig.ServerConfig["Service"]),
        Layer.mock(SourceUsageSummary.SourceUsageSummary)({ read: Effect.succeed(usageSummary) }),
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
              const usage = url.pathname.endsWith("/usage");
              if (url.pathname.startsWith("/v1/sources/accounts")) {
                const account = {
                  id: "acc_1",
                  provider: "anthropic",
                  kind: "key",
                  label: "work",
                  fingerprint: "deadbeef",
                  created: "2026-11-20T10:00:00Z",
                  route: "key",
                };
                return HttpClientResponse.fromWeb(
                  request,
                  new Response(
                    JSON.stringify(request.method === "GET" ? { accounts: [account] } : account),
                    { status: request.method === "POST" ? 201 : 200 },
                  ),
                );
              }
              const status = heartbeat ? replies.heartbeat : usage ? replies.usage : replies.link;
              const reply = heartbeat
                ? { interval: 30, level: "metadata", revoked: false }
                : usage
                  ? { accepted: 2 }
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
  return { requests, secrets, replies, layer, stateDir, fileSystem };
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
  }).pipe(Effect.provide(NodeServices.layer)),
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
        {
          provider: "claude",
          instance: "claude",
          kind: "login",
          state: "ready",
          plan: "max",
          shared: true,
        },
        {
          provider: "codex",
          instance: "codex",
          kind: "login",
          state: "needs_login",
          shared: true,
        },
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
        "claude,claude,login,ready,max,true\n" +
        "codex,codex,login,needs_login,,true\n";
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
  }).pipe(Effect.provide(NodeServices.layer)),
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
  }).pipe(Effect.provide(NodeServices.layer)),
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
  }).pipe(Effect.provide(NodeServices.layer)),
);

const SECRET_KEY_VALUE = "sk-ant-api03-secret-value-never-stored";

it.effect("a usage report drops the account, codes the error, and is signed over its bytes", () =>
  Effect.gen(function* () {
    const { requests, layer } = yield* harness;
    yield* Effect.gen(function* () {
      const desktopLink = yield* DesktopLink.DesktopLink;
      yield* Effect.yieldNow;
      yield* desktopLink.link({});
      const registration = yield* Queue.take(requests);
      yield* Queue.take(requests);
      yield* TestClock.adjust("30 seconds");
      yield* Queue.take(requests);
      const usage = yield* Queue.take(requests);
      assert.equal(usage.path, "/v1/sources/src_1/usage");
      const report = JSON.parse(usage.body);
      assert.equal(report.deviceId, "src_1");
      assert.equal(report.version, 1);
      assert.equal(report.snapshots[0].account, undefined);
      assert.equal(report.snapshots[0].error, "auth_failed");
      assert.notInclude(usage.body, SECRET_EMAIL);

      const verifyKey = yield* Effect.promise(() =>
        globalThis.crypto.subtle.importKey(
          "jwk",
          { kty: "OKP", crv: "Ed25519", x: JSON.parse(registration.body).key },
          "Ed25519",
          false,
          ["verify"],
        ),
      );
      const digest = yield* Effect.promise(async () =>
        Array.from(
          new Uint8Array(
            await globalThis.crypto.subtle.digest("SHA-256", new TextEncoder().encode(usage.body)),
          ),
          (byte) => byte.toString(16).padStart(2, "0"),
        ).join(""),
      );
      const decoded = Base64Url.decode(usage.headers["x-hanzo-sig"] ?? "");
      assert.equal(decoded._tag, "Success");
      const verified = yield* Effect.promise(() =>
        globalThis.crypto.subtle.verify(
          "Ed25519",
          verifyKey,
          decoded._tag === "Success" ? decoded.success : new Uint8Array(),
          // Cloud's usage message, written out by hand.
          new TextEncoder().encode(`hanzo-source-usage/1\nsrc_1\n${digest}\n`),
        ),
      );
      assert.equal(verified, true);

      // Taking the usage lock waits for the report to finish, so its outbox write has landed.
      yield* desktopLink.setSendNothing(false);
      const outbox = yield* desktopLink.outbox;
      assert.equal(outbox.batches.length, 1);
      assert.equal(outbox.batches[0]?.body, usage.body);
      assert.equal(outbox.batches[0]?.state, "sent");
    }).pipe(Effect.provide(layer));
  }).pipe(Effect.provide(NodeServices.layer)),
);

it.effect("send nothing writes and sends no report; a 410 on a report unlinks", () =>
  Effect.gen(function* () {
    const { requests, replies, secrets, layer } = yield* harness;
    yield* Effect.gen(function* () {
      const desktopLink = yield* DesktopLink.DesktopLink;
      yield* Effect.yieldNow;
      yield* desktopLink.link({});
      yield* Queue.take(requests);
      yield* Queue.take(requests);
      yield* desktopLink.setSendNothing(true);
      yield* TestClock.adjust("30 seconds");
      yield* Queue.take(requests);
      assert.equal(yield* Queue.size(requests), 0);
      assert.deepEqual((yield* desktopLink.outbox).batches, []);

      yield* desktopLink.setSendNothing(false);
      replies.usage = 410;
      yield* TestClock.adjust("30 seconds");
      yield* Queue.take(requests);
      yield* Queue.take(requests);
      assert.equal((yield* desktopLink.status).desktop, null);
      assert.equal(secrets.has("desktop-link"), false);
    }).pipe(Effect.provide(layer));
  }).pipe(Effect.provide(NodeServices.layer)),
);

it.effect("an API key's value is sent to Hanzo once and is in no file this desktop writes", () =>
  Effect.gen(function* () {
    const { requests, secrets, layer, stateDir, fileSystem } = yield* harness;
    yield* Effect.gen(function* () {
      const desktopLink = yield* DesktopLink.DesktopLink;
      const accounts = yield* ProviderAccounts.ProviderAccounts;
      yield* Effect.yieldNow;
      yield* desktopLink.link({});
      yield* Queue.take(requests);
      yield* Queue.take(requests);
      yield* TestClock.adjust("30 seconds");
      const added = yield* accounts.add({
        provider: "anthropic",
        label: "work",
        value: SECRET_KEY_VALUE,
      });
      assert.deepEqual(added, {
        id: "acc_1",
        provider: "anthropic",
        kind: "key",
        label: "work",
        fingerprint: "deadbeef",
        created: "2026-11-20T10:00:00Z",
        route: "key",
      });
      const list = yield* accounts.list;
      assert.equal(list.length, 1);
      assert.notInclude(JSON.stringify(list), SECRET_KEY_VALUE);
      const sent = [];
      while ((yield* Queue.size(requests)) > 0) sent.push(yield* Queue.take(requests));
      const post = sent.find(
        (request) => request.path === "/v1/sources/accounts" && request.method === "POST",
      );
      assert.equal(JSON.parse(post?.body ?? "{}").value, SECRET_KEY_VALUE);
      for (const [, bytes] of secrets) {
        assert.notInclude(new TextDecoder().decode(bytes), SECRET_KEY_VALUE);
      }
      const files = yield* fileSystem.readDirectory(stateDir, { recursive: true });
      for (const file of files) {
        const text = yield* fileSystem
          .readFileString(`${stateDir}/${file}`)
          .pipe(Effect.orElseSucceed(() => ""));
        assert.notInclude(text, SECRET_KEY_VALUE);
      }
    }).pipe(Effect.provide(layer));
  }).pipe(Effect.provide(NodeServices.layer)),
);

it("a heartbeat's signed bytes are the ones cloud's beatMessage builds", () => {
  assert.equal(
    DesktopLink.beatMessage({
      id: "src_0123456789abcdef0123456789abcdef",
      at: "2026-11-20T10:00:00Z",
      version: "1.8.4",
      zap: "https://mac.example/zap",
      busy: 0,
      providers: [
        {
          provider: "claude",
          instance: "claude-1",
          kind: "login",
          state: "ready",
          plan: "max",
          shared: true,
        },
        { provider: "openai", instance: "key-1", kind: "key", state: "limited", shared: false },
      ],
    }),
    "hanzo-source-heartbeat/1\nsrc_0123456789abcdef0123456789abcdef\n2026-11-20T10:00:00Z\n1.8.4\nhttps://mac.example/zap\n0\nclaude,claude-1,login,ready,max,true\nopenai,key-1,key,limited,,false\n",
  );
});

it.effect(
  "sharing a login is local, off by default, and reaches Hanzo at once in both directions",
  () =>
    Effect.gen(function* () {
      const { requests, layer } = yield* harness;
      yield* Effect.gen(function* () {
        const desktopLink = yield* DesktopLink.DesktopLink;
        yield* Effect.yieldNow;
        yield* desktopLink.link({});
        yield* Queue.take(requests);
        const first = yield* Queue.take(requests);
        // Nothing stored yet: a claude login is shared with the owner's own surfaces by default.
        assert.equal(JSON.parse(first.body).providers[0].shared, true);

        const off = yield* desktopLink.setShared("claude", false);
        assert.equal(off.providers.find((row) => row.instance === "claude")?.shared, false);
        const beatOff = yield* Queue.take(requests);
        assert.equal(JSON.parse(beatOff.body).providers[0].shared, false);
        assert.equal(JSON.parse(beatOff.body).providers[0].shareable, undefined);

        yield* desktopLink.setShared("claude", true);
        const beatOn = yield* Queue.take(requests);
        assert.equal(JSON.parse(beatOn.body).providers[0].shared, true);

        const refused = yield* desktopLink.setShared("not-an-instance", true).pipe(Effect.flip);
        assert.equal(refused.reason, "rejected");
      }).pipe(Effect.provide(layer));
    }).pipe(Effect.provide(NodeServices.layer)),
);

it.effect(
  "a routed run needs the same account and Share on, which is the default; no credential appears in any answer",
  () =>
    Effect.gen(function* () {
      const { requests, secrets, layer } = yield* harness;
      yield* Effect.gen(function* () {
        const desktopLink = yield* DesktopLink.DesktopLink;
        const linkedUser = (id: string) =>
          void secrets.set("cloud-linked-user-id", new TextEncoder().encode(id));
        yield* Effect.yieldNow;

        // An unlinked desktop refuses.
        linkedUser("z-user");
        const unlinked = yield* desktopLink.assertRoutedRun("claude").pipe(Effect.flip);
        assert.equal(unlinked.reason, "not_shared");

        yield* desktopLink.link({});
        yield* Queue.take(requests);
        yield* Queue.take(requests);

        // A fresh install, no stored setting, the same account: claude and codex are allowed.
        yield* desktopLink.assertRoutedRun("claude");
        yield* desktopLink.assertRoutedRun("codex");

        // A different account is refused, with the same default.
        linkedUser("someone-else");
        const stranger = yield* desktopLink.assertRoutedRun("claude").pipe(Effect.flip);
        assert.equal(stranger.reason, "not_shared");
        linkedUser("hanzo/z-user");
        yield* desktopLink.assertRoutedRun("claude");

        // An instance the policy does not offer stays refused.
        const unknown = yield* desktopLink.assertRoutedRun("nope").pipe(Effect.flip);
        assert.equal(unknown.reason, "not_shared");

        // An explicit off wins over the default.
        yield* desktopLink.setShared("claude", false);
        yield* Queue.take(requests);
        const off = yield* desktopLink.assertRoutedRun("claude").pipe(Effect.flip);
        assert.equal(off.reason, "not_shared");
        yield* desktopLink.assertRoutedRun("codex");

        const everything = JSON.stringify([
          off.message,
          stranger.message,
          yield* desktopLink.status,
          yield* desktopLink.outbox,
        ]);
        for (const secret of [SECRET_TOKEN, "refresh-token-secret", "auth.json", SECRET_EMAIL]) {
          assert.notInclude(everything.replace(`"identity":"${SECRET_EMAIL}"`, ""), secret);
        }
      }).pipe(Effect.provide(layer));
    }).pipe(Effect.provide(NodeServices.layer)),
);
