import { assert, it } from "@effect/vitest";
import * as HttpClient from "effect/http/HttpClient";
import * as HttpClientResponse from "effect/http/HttpClientResponse";
import * as Layer from "effect/Layer";
import * as ConfigProvider from "effect/ConfigProvider";
import * as Effect from "effect/Effect";
import * as Result from "effect/Result";

import {
  hostedAppUrlConfig,
  makeCloudCliOAuthConfig,
  makeRelayUrlConfig,
  resolveRelayClientTracingConfig,
} from "./publicConfig.ts";

const provideEnv = (env: Readonly<Record<string, string>>) =>
  Effect.provide(ConfigProvider.layer(ConfigProvider.fromEnv({ env })));

it.effect("rejects an insecure runtime relay URL override", () =>
  makeRelayUrlConfig("https://embedded.example.test").pipe(
    provideEnv({ HANZO_DEV_RELAY_URL: "http://runtime.example.test" }),
    Effect.flip,
  ),
);

it.effect("rejects malformed or insecure hosted app URLs", () =>
  Effect.gen(function* () {
    for (const value of [
      "hanzo.bot",
      "http://hanzo.bot",
      "https://hanzo.bot/nested",
      "https://hanzo.bot?alias=true",
    ]) {
      const result = yield* hostedAppUrlConfig.pipe(
        provideEnv({ HANZO_DEV_HOSTED_APP_URL: value }),
        Effect.result,
      );
      assert.isTrue(Result.isFailure(result), value);
    }
  }),
);

const discoveryClient = (
  requests: Array<string>,
  document: Readonly<Record<string, string>> | null,
) =>
  Layer.succeed(
    HttpClient.HttpClient,
    HttpClient.make((request) =>
      Effect.sync(() => {
        requests.push(request.url);
        return HttpClientResponse.fromWeb(
          request,
          new Response(document === null ? "" : JSON.stringify(document), {
            status: document === null ? 503 : 200,
          }),
        );
      }),
    ),
  );

it.effect("takes CLI OAuth endpoints from the login host's discovery document", () =>
  Effect.gen(function* () {
    const requests: Array<string> = [];
    const config = yield* makeCloudCliOAuthConfig().pipe(
      provideEnv({ HANZO_DEV_LOGIN_URL: "https://login.example.test" }),
      Effect.provide(
        discoveryClient(requests, {
          issuer: "https://identity.example.test",
          authorization_endpoint: "https://identity.example.test/authorize-here",
          token_endpoint: "https://tokens.example.test/exchange",
          jwks_uri: "https://identity.example.test/keys",
          device_authorization_endpoint: "https://tokens.example.test/device-start",
        }),
      ),
    );

    assert.deepEqual(requests, ["https://login.example.test/.well-known/openid-configuration"]);
    assert.deepEqual(config, {
      tokenEndpoint: "https://tokens.example.test/exchange",
      deviceAuthorizationEndpoint: "https://tokens.example.test/device-start",
      clientId: "hanzo-app",
      loopbackPort: 34338,
      redirectUri: "http://127.0.0.1:34338/callback",
      scopes: ["openid", "profile", "email", "offline_access"],
    });
  }),
);

it.effect("fails closed when discovery is unreachable or names a non-HTTPS endpoint", () =>
  Effect.gen(function* () {
    const unreachable = yield* makeCloudCliOAuthConfig().pipe(
      provideEnv({ HANZO_DEV_LOGIN_URL: "https://down.example.test" }),
      Effect.provide(discoveryClient([], null)),
      Effect.flip,
    );
    assert.equal(unreachable._tag, "IamDiscoveryError");
    const insecure = yield* makeCloudCliOAuthConfig().pipe(
      provideEnv({ HANZO_DEV_LOGIN_URL: "https://insecure.example.test" }),
      Effect.provide(
        discoveryClient([], {
          issuer: "https://identity.example.test",
          authorization_endpoint: "https://identity.example.test/a",
          token_endpoint: "http://identity.example.test/t",
          jwks_uri: "https://identity.example.test/k",
        }),
      ),
      Effect.flip,
    );
    assert.equal(insecure._tag, "IamDiscoveryError");
  }),
);

it("product opt-out disables relay tracing with either built-in or runtime credentials", () => {
  const fallback = {
    tracesUrl: "https://embedded.example.test/v1/traces",
    tracesDataset: "embedded-dataset",
    tracesToken: "embedded-token",
  };
  const env = {
    HANZO_DEV_TELEMETRY_ENABLED: "false",
    HANZO_DEV_OTEL_SDK_DISABLED: "false",
  };

  assert.equal(resolveRelayClientTracingConfig(env, fallback), null);
  assert.equal(
    resolveRelayClientTracingConfig(
      {
        ...env,
        HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_URL: "https://runtime.example.test/v1/traces",
        HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_DATASET: "runtime-dataset",
        HANZO_DEV_RELAY_CLIENT_OTLP_TRACES_TOKEN: "runtime-token",
      },
      fallback,
    ),
    null,
  );
});
