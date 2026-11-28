import * as NodeCrypto from "@effect/platform-node/NodeCrypto";
import * as NodeServices from "@effect/platform-node/NodeServices";
import { assert, it } from "@effect/vitest";
import * as ConfigProvider from "effect/ConfigProvider";
import * as Effect from "effect/Effect";
import * as Layer from "effect/Layer";
import * as Option from "effect/Option";
import * as HttpClient from "effect/http/HttpClient";
import * as HttpClientResponse from "effect/http/HttpClientResponse";

import * as ServerSecretStore from "../auth/ServerSecretStore.ts";
import * as ServerConfig from "../config.ts";
import * as ExternalLauncher from "../process/externalLauncher.ts";
import * as ProviderRegistry from "../provider/ProviderRegistry.ts";
import * as CliTokenManager from "./CliTokenManager.ts";
import * as DesktopLink from "./DesktopLink.ts";
import * as SourceUsageSummary from "./SourceUsageSummary.ts";

// An issuer, token endpoint and login host that match no default.
const DISCOVERY = {
  issuer: "https://issuer.fake.test",
  authorization_endpoint: "https://auth.fake.test/authorize-here",
  token_endpoint: "https://tokens.fake.test/exchange",
  jwks_uri: "https://issuer.fake.test/keys",
  device_authorization_endpoint: "https://tokens.fake.test/device-start",
};

// The user's browser follows the issuer's redirect to the loopback; Node http stands in for it.
const visitLoopback = (state: string) =>
  // @effect-diagnostics-next-line globalFetch:off
  fetch(`http://127.0.0.1:34338/callback?code=issued-code&state=${state}`);

const respond = (
  request: Parameters<typeof HttpClientResponse.fromWeb>[0],
  body: unknown,
  status = 200,
) => HttpClientResponse.fromWeb(request, new Response(JSON.stringify(body), { status }));

it.effect(
  "Link signs in through the browser at the discovered endpoint, then links with the issued bearer",
  () =>
    Effect.gen(function* () {
      const seen: Array<{
        readonly method: string;
        readonly url: string;
        readonly auth: string;
        readonly body: string;
      }> = [];
      const secrets = new Map<string, Uint8Array>();
      let authorizeUrl = "";
      const layer = DesktopLink.layer.pipe(
        Layer.provideMerge(CliTokenManager.layer),
        Layer.provide(
          Layer.mergeAll(
            NodeServices.layer,
            NodeCrypto.layer,
            Layer.mock(ServerSecretStore.ServerSecretStore)({
              get: (name) => Effect.sync(() => Option.fromNullishOr(secrets.get(name))),
              set: (name, value) => Effect.sync(() => void secrets.set(name, value)),
              remove: (name) => Effect.sync(() => void secrets.delete(name)),
            }),
            Layer.mock(ServerConfig.ServerConfig)({
              stateDir: "/nonexistent",
            } as ServerConfig.ServerConfig["Service"]),
            Layer.mock(SourceUsageSummary.SourceUsageSummary)({ read: Effect.never }),
            Layer.mock(ProviderRegistry.ProviderRegistry)({ getProviders: Effect.succeed([]) }),
            // The user's browser: it opens the authorization URL and the issuer redirects to the loopback.
            Layer.mock(ExternalLauncher.ExternalLauncher)({
              launchBrowser: (target) =>
                Effect.promise(async () => {
                  authorizeUrl = target;
                  const state = new URL(target).searchParams.get("state") ?? "";
                  await visitLoopback(state);
                }),
            }),
            Layer.succeed(
              HttpClient.HttpClient,
              HttpClient.make((request) =>
                Effect.sync(() => {
                  const url = new URL(request.url);
                  seen.push({
                    method: request.method,
                    url: request.url,
                    auth: request.headers["authorization"] ?? "",
                    body:
                      request.body._tag === "Uint8Array"
                        ? new TextDecoder().decode(request.body.body)
                        : "",
                  });
                  if (url.pathname === "/.well-known/openid-configuration")
                    return respond(request, DISCOVERY);
                  if (request.url === DISCOVERY.token_endpoint) {
                    return respond(request, {
                      access_token: "issued-access-token",
                      refresh_token: "issued-refresh-token",
                      expires_in: 3600,
                      token_type: "bearer",
                    });
                  }
                  if (url.pathname === "/v1/machines") {
                    return respond(request, { id: "src_1", interval: 30, level: "metadata" }, 201);
                  }
                  return respond(request, { interval: 30, level: "metadata", revoked: false });
                }),
              ),
            ),
          ),
        ),
        Layer.provideMerge(
          ConfigProvider.layer(
            ConfigProvider.fromEnv({ env: { HANZO_DEV_LOGIN_URL: "https://login.fake.test" } }),
          ),
        ),
      );
      yield* Effect.gen(function* () {
        const desktopLink = yield* DesktopLink.DesktopLink;
        const status = yield* desktopLink.link({ signIn: "browser" });
        assert.equal(status.desktop?.id, "src_1");

        const authorize = new URL(authorizeUrl);
        assert.equal(`${authorize.origin}${authorize.pathname}`, DISCOVERY.authorization_endpoint);
        assert.equal(authorize.searchParams.get("response_type"), "code");
        assert.equal(authorize.searchParams.get("code_challenge_method"), "S256");
        assert.equal(authorize.searchParams.get("redirect_uri"), "http://127.0.0.1:34338/callback");

        const exchange = seen.find((request) => request.url === DISCOVERY.token_endpoint);
        assert.include(exchange?.body ?? "", "code=issued-code");
        assert.include(exchange?.body ?? "", "code_verifier=");
        const registration = seen.find(
          (request) => request.method === "POST" && request.url.endsWith("/v1/machines"),
        );
        assert.equal(registration?.auth, "Bearer issued-access-token");
        assert.equal(
          seen.some((request) => request.url.includes("hanzo.id")),
          false,
        );
      }).pipe(Effect.provide(layer));
    }),
);
