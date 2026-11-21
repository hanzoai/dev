import { EnvironmentId } from "@hanzodev/contracts";
import {
  RelayEnvironmentConnectScope,
  RelayEnvironmentStatusScope,
} from "@hanzodev/contracts/relay";
import { describe, expect, it } from "@effect/vitest";
import * as Effect from "effect/Effect";
import * as Fiber from "effect/Fiber";
import * as Layer from "effect/Layer";

import { RELAY_GATEWAY_TIMEOUT_MESSAGE, relayRequestFailureDetail } from "./errorPresentation.ts";
import * as ManagedRelay from "./managedRelay.ts";
import * as RpcHttp from "../rpc/http.ts";

function layerManagedRelayTest(
  fetchFn: typeof globalThis.fetch,
  relayUrl = "https://relay.example.test",
  accessTokenStore?: ManagedRelay.ManagedRelayAccessTokenStore,
) {
  const layerHttpClient = RpcHttp.layerRemoteHttpClient(fetchFn);
  const layerSigner = Layer.succeed(
    ManagedRelay.ManagedRelayDpopSigner,
    ManagedRelay.ManagedRelayDpopSigner.of({
      thumbprint: Effect.succeed("client-thumbprint"),
      createProof: (input: ManagedRelay.ManagedRelayDpopProofInput) =>
        Effect.succeed(`proof:${input.url}`),
    }),
  );
  return ManagedRelay.layer({
    relayUrl,
    clientId: "hanzo-mobile",
    ...(accessTokenStore ? { accessTokenStore } : {}),
  }).pipe(Layer.provide(layerSigner), Layer.provide(layerHttpClient));
}

function iamToken(subject: string, nonce: string): string {
  const encode = (value: unknown) =>
    btoa(JSON.stringify(value)).replaceAll("+", "-").replaceAll("/", "_").replaceAll("=", "");
  return `${encode({ alg: "RS256" })}.${encode({ sub: subject, nonce })}.c2lnbmF0dXJl`;
}

describe("ManagedRelayClient", () => {
  it.effect("rejects unsafe relay URLs before sending credentials", () => {
    let requestCount = 0;
    const fetchFn = (() => {
      requestCount += 1;
      return Promise.resolve(Response.json({}));
    }) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      const error = yield* relayClient.listDevices({ iamToken: "iam-token" }).pipe(Effect.flip);

      expect(error).toMatchObject({
        _tag: "ManagedRelayUrlInvalidError",
        relayUrl: "http://relay.example.test",
        message: "Relay URL must be a secure absolute HTTPS origin.",
      });
      expect(requestCount).toBe(0);
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn, "http://relay.example.test")));
  });

  it.effect("uses a cached token while another scope waits for an exchange", () => {
    const exchangeStarted = Promise.withResolvers<void>();
    const releaseExchange = Promise.withResolvers<void>();
    let tokenExchangeCount = 0;
    const statusTokens: Array<string | null> = [];
    let persistedTokens: ReadonlyArray<ManagedRelay.ManagedRelayAccessTokenCacheEntry> = [
      {
        accountId: "user-1",
        clientId: "hanzo-mobile",
        relayUrl: "https://relay.example.test",
        thumbprint: "client-thumbprint",
        scopes: [RelayEnvironmentStatusScope],
        accessToken: "cached-status-token",
        expiresAtMillis: Number.MAX_SAFE_INTEGER,
      },
    ];
    const accessTokenStore: ManagedRelay.ManagedRelayAccessTokenStore = {
      load: Effect.sync(() => persistedTokens),
      save: (entries) =>
        Effect.sync(() => {
          persistedTokens = entries;
        }),
      clear: Effect.sync(() => {
        persistedTokens = [];
      }),
    };
    const fetchFn = ((input, init) => {
      if (String(input).endsWith("/v1/client/dpop-token")) {
        tokenExchangeCount += 1;
        exchangeStarted.resolve();
        return releaseExchange.promise.then(() =>
          Response.json({
            access_token: "expanded-scope-token",
            issued_token_type: "urn:ietf:params:oauth:token-type:access_token",
            token_type: "DPoP",
            expires_in: 1_800,
            scope: `${RelayEnvironmentStatusScope} ${RelayEnvironmentConnectScope}`,
          }),
        );
      }
      statusTokens.push(new Headers(init?.headers).get("authorization"));
      return Promise.resolve(
        Response.json({
          environmentId: "env-1",
          endpoint: {
            httpBaseUrl: "https://desktop.example.test/",
            wsBaseUrl: "wss://desktop.example.test/ws",
            providerKind: "cloudflare_tunnel",
          },
          status: "online",
          checkedAt: "2026-09-04T00:00:00.000Z",
        }),
      );
    }) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      const statusInput = {
        iamToken: iamToken("user-1", "session-1"),
        scopes: [RelayEnvironmentStatusScope],
        environmentId: EnvironmentId.make("env-1"),
      } as const;
      const expandedScopeInput = {
        ...statusInput,
        scopes: [RelayEnvironmentStatusScope, RelayEnvironmentConnectScope],
      } as const;
      const firstMiss = yield* relayClient
        .getEnvironmentStatus(expandedScopeInput)
        .pipe(Effect.forkChild);
      yield* Effect.promise(() => exchangeStarted.promise);
      const secondMiss = yield* relayClient
        .getEnvironmentStatus(expandedScopeInput)
        .pipe(Effect.forkChild);

      const status = yield* relayClient.getEnvironmentStatus(statusInput);
      expect(status.status).toBe("online");
      expect(statusTokens).toEqual(["DPoP cached-status-token"]);

      releaseExchange.resolve();
      yield* Fiber.join(firstMiss);
      yield* Fiber.join(secondMiss);
      expect(tokenExchangeCount).toBe(1);
      expect(persistedTokens.map((token) => token.accessToken)).toEqual([
        "cached-status-token",
        "expanded-scope-token",
      ]);
      yield* relayClient.resetTokenCache;
      expect(persistedTokens).toEqual([]);
      yield* relayClient.getEnvironmentStatus(expandedScopeInput);
      expect(tokenExchangeCount).toBe(2);
    }).pipe(
      Effect.ensuring(Effect.sync(() => releaseExchange.resolve())),
      Effect.provide(layerManagedRelayTest(fetchFn, undefined, accessTokenStore)),
    );
  });

  it.effect("reuses a persisted token across runtimes and IAM access token rotation", () => {
    let tokenExchangeCount = 0;
    let persistedTokens: ReadonlyArray<ManagedRelay.ManagedRelayAccessTokenCacheEntry> = [];
    const accessTokenStore: ManagedRelay.ManagedRelayAccessTokenStore = {
      load: Effect.sync(() => persistedTokens),
      save: (entries) =>
        Effect.sync(() => {
          persistedTokens = entries;
        }),
      clear: Effect.sync(() => {
        persistedTokens = [];
      }),
    };
    const fetchFn = ((input) => {
      const url = String(input);
      if (url.endsWith("/v1/client/dpop-token")) {
        tokenExchangeCount += 1;
        return Promise.resolve(
          Response.json({
            access_token: "persisted-relay-token",
            issued_token_type: "urn:ietf:params:oauth:token-type:access_token",
            token_type: "DPoP",
            expires_in: 1_800,
            scope: RelayEnvironmentStatusScope,
          }),
        );
      }
      return Promise.resolve(
        Response.json({
          environmentId: "env-1",
          endpoint: {
            httpBaseUrl: "https://desktop.example.test/",
            wsBaseUrl: "wss://desktop.example.test/ws",
            providerKind: "cloudflare_tunnel",
          },
          status: "online",
          checkedAt: "2026-06-05T20:00:00.000Z",
          descriptor: {
            environmentId: "env-1",
            label: "Desktop",
            platform: { os: "darwin", arch: "arm64" },
            serverVersion: "0.0.0-test",
            capabilities: { repositoryIdentity: true },
          },
        }),
      );
    }) satisfies typeof globalThis.fetch;
    const statusInput = (token: string) =>
      ({
        iamToken: token,
        scopes: [RelayEnvironmentStatusScope],
        environmentId: EnvironmentId.make("env-1"),
      }) as const;

    return Effect.gen(function* () {
      yield* Effect.gen(function* () {
        const relayClient = yield* ManagedRelay.ManagedRelayClient;
        yield* relayClient.getEnvironmentStatus(statusInput(iamToken("user-1", "session-1")));
      }).pipe(Effect.provide(layerManagedRelayTest(fetchFn, undefined, accessTokenStore)));

      expect(tokenExchangeCount).toBe(1);
      expect(persistedTokens).toHaveLength(1);

      yield* Effect.gen(function* () {
        const relayClient = yield* ManagedRelay.ManagedRelayClient;
        yield* relayClient.getEnvironmentStatus(statusInput(iamToken("user-1", "session-2")));
      }).pipe(Effect.provide(layerManagedRelayTest(fetchFn, undefined, accessTokenStore)));

      expect(tokenExchangeCount).toBe(1);
    });
  });

  it.effect("refreshes a persisted DPoP token once when the relay rejects it", () => {
    let tokenExchangeCount = 0;
    const statusTokens: Array<string | null> = [];
    let persistedTokens: ReadonlyArray<ManagedRelay.ManagedRelayAccessTokenCacheEntry> = [
      {
        accountId: "user-1",
        clientId: "hanzo-mobile",
        relayUrl: "https://relay.example.test",
        thumbprint: "client-thumbprint",
        scopes: [RelayEnvironmentStatusScope],
        accessToken: "stale-relay-token",
        expiresAtMillis: Number.MAX_SAFE_INTEGER,
      },
    ];
    const accessTokenStore: ManagedRelay.ManagedRelayAccessTokenStore = {
      load: Effect.sync(() => persistedTokens),
      save: (entries) =>
        Effect.sync(() => {
          persistedTokens = entries;
        }),
      clear: Effect.sync(() => {
        persistedTokens = [];
      }),
    };
    const fetchFn = ((input, init) => {
      const url = String(input);
      if (url.endsWith("/v1/client/dpop-token")) {
        tokenExchangeCount += 1;
        return Promise.resolve(
          Response.json({
            access_token: "fresh-relay-token",
            issued_token_type: "urn:ietf:params:oauth:token-type:access_token",
            token_type: "DPoP",
            expires_in: 1_800,
            scope: RelayEnvironmentStatusScope,
          }),
        );
      }

      const authorization = new Headers(init?.headers).get("authorization");
      statusTokens.push(authorization);
      if (authorization === "DPoP stale-relay-token") {
        return Promise.resolve(
          Response.json(
            {
              _tag: "RelayAuthInvalidError",
              code: "auth_invalid",
              reason: "invalid_bearer",
              traceId: "trace-stale-token",
            },
            { status: 401 },
          ),
        );
      }
      return Promise.resolve(
        Response.json({
          environmentId: "env-1",
          endpoint: {
            httpBaseUrl: "https://desktop.example.test/",
            wsBaseUrl: "wss://desktop.example.test/ws",
            providerKind: "cloudflare_tunnel",
          },
          status: "online",
          checkedAt: "2026-06-05T20:00:00.000Z",
          descriptor: {
            environmentId: "env-1",
            label: "Desktop",
            platform: { os: "darwin", arch: "arm64" },
            serverVersion: "0.0.0-test",
            capabilities: { repositoryIdentity: true },
          },
        }),
      );
    }) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      const result = yield* relayClient.getEnvironmentStatus({
        iamToken: iamToken("user-1", "session-1"),
        scopes: [RelayEnvironmentStatusScope],
        environmentId: EnvironmentId.make("env-1"),
      });

      expect(result.status).toBe("online");
      expect(statusTokens).toEqual(["DPoP stale-relay-token", "DPoP fresh-relay-token"]);
      expect(tokenExchangeCount).toBe(1);
      expect(persistedTokens).toMatchObject([
        {
          accessToken: "fresh-relay-token",
        },
      ]);
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn, undefined, accessTokenStore)));
  });

  it.effect("does not persist tokens when the IAM subject cannot be decoded", () => {
    let persistedTokens: ReadonlyArray<ManagedRelay.ManagedRelayAccessTokenCacheEntry> = [];
    const accessTokenStore: ManagedRelay.ManagedRelayAccessTokenStore = {
      load: Effect.succeed([]),
      save: (entries) =>
        Effect.sync(() => {
          persistedTokens = entries;
        }),
      clear: Effect.void,
    };
    const fetchFn = ((input) => {
      const url = String(input);
      if (url.endsWith("/v1/client/dpop-token")) {
        return Promise.resolve(
          Response.json({
            access_token: "relay-token",
            issued_token_type: "urn:ietf:params:oauth:token-type:access_token",
            token_type: "DPoP",
            expires_in: 1_800,
            scope: RelayEnvironmentStatusScope,
          }),
        );
      }
      return Promise.resolve(
        Response.json({
          environmentId: "env-1",
          endpoint: {
            httpBaseUrl: "https://desktop.example.test/",
            wsBaseUrl: "wss://desktop.example.test/ws",
            providerKind: "cloudflare_tunnel",
          },
          status: "online",
          checkedAt: "2026-06-05T20:00:00.000Z",
          descriptor: {
            environmentId: "env-1",
            label: "Desktop",
            platform: { os: "darwin", arch: "arm64" },
            serverVersion: "0.0.0-test",
            capabilities: { repositoryIdentity: true },
          },
        }),
      );
    }) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      yield* relayClient.getEnvironmentStatus({
        iamToken: "not-a-jwt",
        scopes: [RelayEnvironmentStatusScope],
        environmentId: EnvironmentId.make("env-1"),
      });

      expect(persistedTokens).toEqual([]);
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn, undefined, accessTokenStore)));
  });

  it.effect("lists environments from the /v1/sources registry", () => {
    const requests: Array<{ url: string; authorization: string | null }> = [];
    const fetchFn = ((input, init) => {
      requests.push({
        url: String(input),
        authorization: new Headers(init?.headers).get("authorization"),
      });
      return Promise.resolve(
        Response.json({
          sources: [
            {
              id: "mac-1",
              kind: "desktop",
              name: "Zs Mac",
              state: "online",
              linked_at: "2026-11-01T00:00:00Z",
            },
            { id: "gone", kind: "desktop", name: "Old", state: "revoked" },
          ],
        }),
      );
    }) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      const environments = yield* relayClient.listEnvironments({ iamToken: "iam-token" });

      expect(requests).toEqual([
        { url: "https://api.hanzo.ai/v1/sources", authorization: "Bearer iam-token" },
      ]);
      expect(environments).toMatchObject([
        { environmentId: "mac-1", label: "Zs Mac", state: "online" },
      ]);
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn)));
  });

  it.effect("connects a linked machine through /v1/sources at its reported ZAP address", () => {
    const requests: Array<string> = [];
    const fetchFn = ((input) => {
      requests.push(String(input));
      return Promise.resolve(
        Response.json({
          id: "mac-1",
          kind: "desktop",
          name: "Zs Mac",
          state: "online",
          zap: "https://zap.hanzo.ai/v1/dev/zap/mac-1",
        }),
      );
    }) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      const connected = yield* relayClient.connectEnvironment({
        iamToken: "iam-token",
        scopes: [RelayEnvironmentConnectScope],
        environmentId: EnvironmentId.make("mac-1"),
      });
      expect(requests).toEqual(["https://api.hanzo.ai/v1/sources/mac-1"]);
      expect(connected).toMatchObject({
        environmentId: "mac-1",
        endpoint: {
          httpBaseUrl: "https://zap.hanzo.ai/v1/dev/zap/mac-1",
          providerKind: "hanzo_relay",
        },
        credential: "iam-token",
      });
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn)));
  });

  it.effect("lists and connects with no relay configured", () => {
    const fetchFn = (() =>
      Promise.resolve(Response.json({ sources: [] }))) satisfies typeof globalThis.fetch;
    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      expect(yield* relayClient.listEnvironments({ iamToken: "iam-token" })).toEqual([]);
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn, "http://relay.invalid")));
  });

  it.effect("reports an unauthorized /v1/sources as a failed listing", () => {
    const fetchFn = (() =>
      Promise.resolve(
        Response.json({ code: "unauthorized" }, { status: 401 }),
      )) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      const error = yield* relayClient
        .listEnvironments({ iamToken: "iam-token" })
        .pipe(Effect.flip);
      expect(error).toMatchObject({
        _tag: "ManagedRelayRequestFailedError",
        action: "list relay-managed environments",
        status: 401,
      });
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn)));
  });

  it.effect("explains a relay deadline 504 on link and keeps its trace ID", () => {
    const fetchFn = (() =>
      Promise.resolve(
        Response.json(
          { error: "relay_request_deadline_exceeded" },
          {
            status: 504,
            headers: {
              traceparent: "00-4684d7d140f34f0393d42744d240e207-0baf648c44f4bce6-01",
            },
          },
        ),
      )) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      const error = yield* relayClient
        .linkEnvironment({
          iamToken: "iam-token",
          payload: {
            proof: "link-proof",
            notificationsEnabled: true,
            liveActivitiesEnabled: true,
            managedTunnelsEnabled: true,
          },
        })
        .pipe(Effect.flip);

      expect(error).toMatchObject({
        _tag: "ManagedRelayRequestFailedError",
        status: 504,
        traceId: "4684d7d140f34f0393d42744d240e207",
      });
      expect(
        error._tag === "ManagedRelayRequestFailedError" ? relayRequestFailureDetail(error) : null,
      ).toBe(RELAY_GATEWAY_TIMEOUT_MESSAGE);
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn)));
  });

  it.effect("accepts generic DPoP errors from relays without the optional reason", () => {
    const fetchFn = (() =>
      Promise.resolve(
        Response.json(
          {
            _tag: "RelayAuthInvalidError",
            code: "auth_invalid",
            reason: "invalid_dpop",
            traceId: "trace-old-relay",
          },
          { status: 401 },
        ),
      )) satisfies typeof globalThis.fetch;

    return Effect.gen(function* () {
      const relayClient = yield* ManagedRelay.ManagedRelayClient;
      const error = yield* relayClient.listDevices({ iamToken: "iam-token" }).pipe(Effect.flip);

      expect(error).toMatchObject({
        _tag: "ManagedRelayRequestFailedError",
        traceId: "trace-old-relay",
        relayError: {
          _tag: "RelayAuthInvalidError",
          reason: "invalid_dpop",
        },
      });
      if (
        error._tag === "ManagedRelayRequestFailedError" &&
        error.relayError?._tag === "RelayAuthInvalidError"
      ) {
        expect(error.relayError.dpopFailureReason).toBeUndefined();
      }
    }).pipe(Effect.provide(layerManagedRelayTest(fetchFn)));
  });
});
